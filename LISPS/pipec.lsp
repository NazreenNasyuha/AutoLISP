;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : pipec.lsp
;;; COMMAND     : PIPEC
;;; DESCRIPTION : Fully Automated Civil Road Drainage Network Generator
;;;               (Auto-Segmentation, Sump Placement, Hydraulic Flow Arrows,
;;;                Collision-Aware Pipe Annotations & Parametric SIL Multileaders)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; PIPEC automates the drafting of municipal road stormwater drainage networks:
;;;   1. Automatically inserts standard road drainage sumps/catchpits (R=600mm)
;;;      at junction nodes and at regular intervals when spans exceed maximum limits.
;;;   2. Auto-segments pipe runs that exceed the maximum permissible span (default 30m).
;;;   3. Generates parametric SIL (Sump Invert Level) Multileaders with standard
;;;      "1000-T2" style on layer `#JRK - RD Drain Text IL`.
;;;   4. Draws trimmed centerline drainage pipe lines between sump perimeters.
;;;   5. Generates directional flow arrows pointing in the direction of gravity flow.
;;;   6. Auto-calculates horizontal 2D span and formats pipe MTEXT annotations
;;;      (Drain ID, Diameter, Length, Gradient) with collision-aware offset positioning.
;;;   7. Auto-increments Drainage IDs sequentially (e.g., D1 -> D2 -> D3).
;;;   8. Provides complete live undo history (reverts geometry and ID sequence).
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `PIPEC` in the AutoCAD / GstarCAD command line.
;;;   2. Optionally configure Starting Drain Code, Size (600mm), and Gradient (1:1000),
;;;      or press Enter to accept defaults.
;;;   3. Pick Start Point -> Pick End Point.
;;;   4. System automatically splits pipe, inserts sumps, arrows, pipe labels,
;;;      and parametric SIL Multileaders, then prepares the next sequence ID.
;;;   5. Type 'U' during prompt to undo the previous segment.
;;; ==========================================================================

(defun c:pipec ( / *error* sysVars sysVals 
                   ;; --- settings ---
                   drainLayer drainColor arrowLayer arrowColor arrowWidth
                   sumpLayer sumpColor sumpRad textLayer textColor txtHgt txtStyle txtWidth
                   mlLayer mlStyle mlText mlJustify mlSide mlLeadLen mlLandDist mlArrowSize mlLandGap
                   ilMode unitsPerM maxSegM defCode defSize defGrad pipeTextRot0
                   ;; --- working variables ---
                   askInfo ans currCode prevCode size grad pt1 pt2 ang dist numSeg segDist i
                   sumpPt pA pB segMid segDistM linePt1 linePt2 arrowSize arrowP1 arrowP2
                   txtAngRad finalTxtAng pText str mtextStr distAbove distBelow pTextAbove pTextBelow
                   blockedAbove blockedBelow history stepEnts loop sumpCache silCache c e
                   ;; --- local helper functions ---
                   ensure-layer esc-filter pt2d near-p mk finish-cmd next-seq
                   scan-sumps scan-sils mlstyle-exists-p ensure-mlstyle text-blocked-p
                   make-il make-il-mleader make-il-basic)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq unitsPerM 1000.0)      
  (setq maxSegM   30.0)        

  (setq drainLayer "#JRK - RD Drain Line")
  (setq drainColor 4)

  (setq arrowLayer "#JRK - RD Drain FLOW")
  (setq arrowColor 4)
  (setq arrowWidth 0.0)

  (setq sumpLayer "#JRK - RD Drain Manhole_Sump")
  (setq sumpColor 4)
  (setq sumpRad 600.0)

  (setq textLayer "#JRK - RD Drain Text")
  (setq textColor 7)
  (setq txtHgt 2000.0)
  (setq txtStyle "1000-T2")
  (setq txtWidth 0.5)

  (setq mlLayer     "#JRK - RD Drain Text IL")
  (setq mlStyle     "1000-T2")             
  (setq mlText      "{\\W0.5;SIL00.00}")   
  (setq mlJustify   "Right")               
  (setq mlSide      "Right")               
  (setq mlLeadLen   3000.0)                
  (setq mlLandDist  2000.0)                
  (setq mlArrowSize 2000.0)                
  (setq mlLandGap   1000.0)                
  (setq ilMode      1)                     
  
  (setq pipeTextRot0 nil) 

  (setq defCode "A01") ;; Restored default
  (setq defSize "600")
  (setq defGrad "1:000")
  ;; =========================================================================

  (setq sysVars '("CMDECHO" "CMLEADERSTYLE")
        sysVals (mapcar 'getvar sysVars))

  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))   
          (command "_.-LAYER" "_N" nm "_C" col nm "")))))

  (defun esc-filter (s / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen s))
      (setq ch (substr s k 1))
      (setq out (strcat out (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ",")) "`" "") ch))
      (setq k (1+ k)))
    out)

  (defun pt2d (p) (list (car p) (cadr p) 0.0))

  (defun near-p (pt cache rad / found c)
    (foreach c cache (if (< (distance (pt2d pt) (pt2d c)) rad) (setq found T)))
    found)

  (defun mk (data)
    (if (entmake data) (setq stepEnts (cons (entlast) stepEnts))))

  (defun finish-cmd ()
    (repeat 5 (if (> (getvar "CMDACTIVE") 0) (command "")))
    (if (> (getvar "CMDACTIVE") 0) (command)))

  ;; Helper: Auto-Increments ID (Handles both Numbers like A01 and Letters like 1A)
  (defun next-seq (str / len lastChar asciiVal i numStr preStr val pad s)
    (setq len (strlen str))
    (if (> len 0)
      (progn
        (setq lastChar (substr str len 1)
              asciiVal (ascii lastChar))
        (cond
          ;; Ends in a letter A-Y or a-y (e.g. "1A" -> "1B")
          ((or (and (>= asciiVal 65) (< asciiVal 90)) (and (>= asciiVal 97) (< asciiVal 122)))
           (strcat (substr str 1 (1- len)) (chr (1+ asciiVal))))
          ;; Wrap Z to A
          ((= asciiVal 90) (strcat (substr str 1 (1- len)) "A"))
          ((= asciiVal 122) (strcat (substr str 1 (1- len)) "a"))
          ;; Ends in a number (e.g. "A01" -> "A02")
          ((and (>= asciiVal 48) (<= asciiVal 57))
           (setq i len numStr "")
           (while (and (> i 0) (>= (ascii (substr str i 1)) 48) (<= (ascii (substr str i 1)) 57))
             (setq numStr (strcat (substr str i 1) numStr) i (1- i)))
           (setq preStr (substr str 1 i) val (1+ (atoi numStr)) pad (strlen numStr) s (itoa val))
           (while (< (strlen s) pad) (setq s (strcat "0" s)))
           (strcat preStr s))
          ;; Fallback
          (T str)
        )
      )
      ""
    )
  )

  (defun scan-sumps ( / ss n pts)
    (setq ss (ssget "_X" (list '(0 . "CIRCLE") (cons 8 (esc-filter sumpLayer)) (cons 410 (getvar "CTAB")))))
    (if ss
      (progn (setq n 0)
        (while (< n (sslength ss))
          (setq pts (cons (cdr (assoc 10 (entget (ssname ss n)))) pts))
          (setq n (1+ n)))))
    pts)

  (defun scan-sils ( / ss n ed typ found d pts)
    (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER") (cons 410 (getvar "CTAB")))))
    (if ss
      (progn (setq n 0)
        (while (< n (sslength ss))
          (setq ed (entget (ssname ss n)) typ (cdr (assoc 0 ed)) found nil)
          (foreach d ed
            (if (and (member (car d) '(1 3 304)) (= (type (cdr d)) 'STR)
                     (wcmatch (strcase (cdr d)) "*SIL*"))
              (setq found T)))
          (if found
            (if (= typ "MULTILEADER")
              (foreach d ed
                (if (and (member (car d) '(10 12)) (listp (cdr d)))
                  (setq pts (cons (cdr d) pts))))
              (setq pts (cons (cdr (assoc 10 ed)) pts))))
          (setq n (1+ n)))))
    pts)

  (defun mlstyle-exists-p (nm / d)
    (setq d (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
    (if (and d (dictsearch (cdr (assoc -1 d)) nm)) T nil))

  (defun ensure-mlstyle (nm / dict mlStyles styleObj)
    (vl-load-com)
    (if (and (vl-symbol-value 'vla-get-ActiveDocument)
             (vl-symbol-value 'vlax-get-acad-object))
      (vl-catch-all-apply
        '(lambda ()
           (setq dict (vla-get-Dictionaries (vla-get-ActiveDocument (vlax-get-acad-object))))
           (setq mlStyles (vla-item dict "ACAD_MLEADERSTYLE"))
           (if (vl-catch-all-error-p (setq styleObj (vl-catch-all-apply 'vla-item (list mlStyles nm))))
             (setq styleObj (vla-AddObject mlStyles nm "AcDbMLeaderStyle")))
           (if (and styleObj (not (vl-catch-all-error-p styleObj)))
             (progn
               (if (vlax-property-available-p styleObj 'TextHeight T)
                 (vlax-put-property styleObj 'TextHeight txtHgt))
               (if (vlax-property-available-p styleObj 'ArrowSize T)
                 (vlax-put-property styleObj 'ArrowSize mlArrowSize))
               (if (vlax-property-available-p styleObj 'LandingDistance T)
                 (vlax-put-property styleObj 'LandingDistance mlLandDist))
               (if (vlax-property-available-p styleObj 'LandingGap T)
                 (vlax-put-property styleObj 'LandingGap mlLandGap))
               (if (vlax-property-available-p styleObj 'TextLeftAttachmentType T)
                 (vlax-put-property styleObj 'TextLeftAttachmentType 1))
               (if (vlax-property-available-p styleObj 'TextRightAttachmentType T)
                 (vlax-put-property styleObj 'TextRightAttachmentType 1))
               (if (tblsearch "STYLE" txtStyle)
                 (if (vlax-property-available-p styleObj 'TextStyleName T)
                   (vlax-put-property styleObj 'TextStyleName txtStyle)))))))))

  (defun text-blocked-p (pt / h ss)
    (setq h (* txtHgt 2.0))
    (setq ss (ssget "_CP"
                    (list (trans (list (- (car pt) h) (- (cadr pt) h) 0.0) 0 1)
                          (trans (list (+ (car pt) h) (- (cadr pt) h) 0.0) 0 1)
                          (trans (list (+ (car pt) h) (+ (cadr pt) h) 0.0) 0 1)
                          (trans (list (- (car pt) h) (+ (cadr pt) h) 0.0) 0 1))
                    '((0 . "TEXT,MTEXT"))))
    (if ss T nil))

  ;; -------------------------------------------------------------------------
  ;; NATIVE WCS MLEADER GENERATION
  ;; -------------------------------------------------------------------------
  (defun make-il-mleader (sPt / last0 newEnt ed ok perpAng arrowPt cornerPt dx p2_wcs obj)
    (setq last0 (entlast) ok nil)
    (ensure-mlstyle mlStyle)
    (if (mlstyle-exists-p mlStyle)
      (vl-catch-all-apply 'setvar (list "CMLEADERSTYLE" mlStyle)))

    (setq perpAng (if (= (strcase mlSide) "RIGHT")
                    (- txtAngRad (/ pi 2.0))
                    (+ txtAngRad (/ pi 2.0))))

    (setq arrowPt (polar sPt perpAng sumpRad))
    (setq cornerPt (polar arrowPt perpAng mlLeadLen))

    (setq dx (if (= (strcase mlJustify) "LEFT") 10.0 -10.0))
    (setq p2_wcs (list (+ (car cornerPt) dx) (cadr cornerPt) 0.0))

    (setvar "CLAYER" mlLayer)
    (command "_.MLEADER" "_NON" arrowPt "_NON" p2_wcs mlText "")
    (while (> (getvar "CMDACTIVE") 0) (command ""))

    (setq newEnt (entlast))
    (if (and newEnt (not (equal newEnt last0)))
      (progn
        (setq ed (entget newEnt))
        (if (= (cdr (assoc 0 ed)) "MULTILEADER")
          (progn
            (entmod (subst (cons 8 mlLayer) (assoc 8 ed) ed))
            (vl-catch-all-apply
              '(lambda ()
                 (setq obj (vlax-ename->vla-object newEnt))
                 (if (vlax-property-available-p obj 'StyleName T)
                   (vlax-put-property obj 'StyleName mlStyle))
                 (if (vlax-property-available-p obj 'Layer T)
                   (vlax-put-property obj 'Layer mlLayer))
                 (if (vlax-property-available-p obj 'ArrowheadSize T)
                   (vlax-put-property obj 'ArrowheadSize mlArrowSize))
                 (if (vlax-property-available-p obj 'DoglegLength T)
                   (vlax-put-property obj 'DoglegLength mlLandDist))
                 (if (vlax-property-available-p obj 'LandingGap T)
                   (vlax-put-property obj 'LandingGap mlLandGap))
                 (if (vlax-property-available-p obj 'TextHeight T)
                   (vlax-put-property obj 'TextHeight txtHgt))
                 (if (vlax-property-available-p obj 'TextLeftAttachmentType T)
                   (vlax-put-property obj 'TextLeftAttachmentType 1))
                 (if (vlax-property-available-p obj 'TextRightAttachmentType T)
                   (vlax-put-property obj 'TextRightAttachmentType 1))
                 (if (vlax-property-available-p obj 'TextJustify T)
                   (vlax-put-property obj 'TextJustify (if (= (strcase mlJustify) "LEFT") 1 3)))
                 (if (vlax-property-available-p obj 'ScaleFactor T)
                   (vlax-put-property obj 'ScaleFactor 1.0))
                 (vla-update obj)))
            (setq stepEnts (cons newEnt stepEnts) ok T)))))
    ok)

  (defun make-il-basic (sPt / perpAng arrowPt cornerPt landPt txtPt jCode aP1 aP2 rotAng)
    (setq perpAng (if (= (strcase mlSide) "RIGHT")
                    (- txtAngRad (/ pi 2.0))
                    (+ txtAngRad (/ pi 2.0))))
    (setq arrowPt  (polar sPt perpAng sumpRad)
          cornerPt (polar arrowPt perpAng mlLeadLen))

    (mk (list '(0 . "LINE") (cons 8 mlLayer) (cons 10 arrowPt) (cons 11 cornerPt)))

    (setq aP1 (polar arrowPt (+ perpAng (* pi 0.85)) (* mlArrowSize 0.4))
          aP2 (polar arrowPt (- perpAng (* pi 0.85)) (* mlArrowSize 0.4)))
    (mk (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 mlLayer)
              '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) (cons 43 0.0)
              (list 10 (car arrowPt) (cadr arrowPt))
              (list 10 (car aP1) (cadr aP1))
              (list 10 (car aP2) (cadr aP2))))

    (setq rotAng 0.0)
    (if (= (strcase mlJustify) "LEFT")
      (progn
        (setq landPt (polar cornerPt 0.0 mlLandDist)
              txtPt  (polar landPt 0.0 mlLandGap)
              jCode  1)) 
      (progn
        (setq landPt (polar cornerPt pi mlLandDist)
              txtPt  (polar landPt pi mlLandGap)
              jCode  3))) 

    (mk (list '(0 . "LINE") (cons 8 mlLayer) (cons 10 cornerPt) (cons 11 landPt)))
    (mk (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 mlLayer) '(100 . "AcDbMText")
              (list 10 (car txtPt) (cadr txtPt) 0.0)
              (cons 40 txtHgt) (cons 41 0.0)
              (cons 71 jCode) '(72 . 1) (cons 7 mlStyle) (cons 50 rotAng)
              (list 11 (cos rotAng) (sin rotAng) 0.0)
              (cons 1 mlText))))

  (defun make-il (sPt / ok)
    (setq ok nil)
    (if (= ilMode 1) (setq ok (make-il-mleader sPt)))
    (if (not ok) (make-il-basic sPt)))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nAction Cancelled: " msg)))
    (princ))

  (setvar "CMDECHO" 0)
  (ensure-layer drainLayer drainColor)
  (ensure-layer arrowLayer arrowColor)
  (ensure-layer sumpLayer  sumpColor)
  (ensure-layer textLayer  textColor)
  (ensure-layer mlLayer    textColor)

  (if (not (tblsearch "STYLE" txtStyle))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                   (cons 2 txtStyle) '(70 . 0) '(40 . 0.0) (cons 41 txtWidth) '(50 . 0.0)
                   '(71 . 0) '(3 . "romans.shx") '(4 . ""))))

  (initget "Yes No")
  (setq askInfo (getkword "\nEnter custom drain details? [Yes/No] <No>: "))
  (setq currCode defCode size defSize grad defGrad)
  (if (= askInfo "Yes")
    (progn
      (setq currCode (getstring T (strcat "\nStarting Code <" defCode ">: "))) (if (= currCode "") (setq currCode defCode))
      (setq size (getstring T (strcat "\nSize <" defSize ">: ")))             (if (= size "") (setq size defSize))
      (setq grad (getstring T (strcat "\nGradient <" defGrad ">: ")))         (if (= grad "") (setq grad defGrad))
      (initget "Left Right")
      (setq ans (getkword (strcat "\nSIL Text Justification [Left/Right] <" mlJustify ">: ")))
      (if ans (setq mlJustify ans))
      (initget "Left Right")
      (setq ans (getkword (strcat "\nSIL Leader Offset Side [Left/Right] <" mlSide ">: ")))
      (if ans (setq mlSide ans))))

  (setq sumpCache (scan-sumps) silCache (scan-sils))
  (setq history nil loop T)

  (while loop
    (initget "Side Justify Undo")
    (setq pt1 (getpoint (strcat "\nClick Start Point of Drain [Side/Justify/Undo] <Exit> (Side:" mlSide ", Justify:" mlJustify "): ")))
    (cond
      ((null pt1) (setq loop nil))

      ((= pt1 "Justify")
       (setq mlJustify (if (= (strcase mlJustify) "RIGHT") "Left" "Right"))
       (princ (strcat "\n>> SIL Text Justification set to: " mlJustify)))

      ((= pt1 "Side")
       (setq mlSide (if (= (strcase mlSide) "LEFT") "Right" "Left"))
       (princ (strcat "\n>> SIL Leader Offset Side set to: " mlSide)))

      ((= pt1 "Undo")
       (if history
         (progn
           (setq lastStep (car history))
           (foreach e (car lastStep) (if (and e (entget e)) (entdel e)))
           (setq currCode  (cdr lastStep) 
                 history   (cdr history)
                 sumpCache (scan-sumps)
                 silCache  (scan-sils))
           (princ (strcat "\n>> Last drain segment undone. Sequence reverted to " currCode ".")))
         (princ "\n>> Nothing to undo.")))

      (T
       (initget "Undo")
       (setq pt2 (getpoint pt1 "\nClick End Point of Drain [Undo] <Cancel>: "))
       (if (or (null pt2) (= pt2 "Undo"))
         (princ "\n>> Cancelled point.")
         (progn
           (setq stepEnts nil
                 prevCode currCode 
                 pt1  (pt2d (trans pt1 1 0))
                 pt2  (pt2d (trans pt2 1 0))
                 dist (distance pt1 pt2))
           (if (< dist 1.0e-6)
             (princ "\n>> Zero-length pipe ignored.")
             (progn
               (setq ang       (angle pt1 pt2)
                     arrowSize (* txtHgt 0.8)
                     txtAngRad (if (and (> ang (+ (/ pi 2.0) 1e-9)) (<= ang (+ (* 1.5 pi) 1e-9))) (- ang pi) ang)
                     finalTxtAng (if pipeTextRot0 0.0 txtAngRad) 
                     numSeg    (max 1 (fix (+ (/ dist (* maxSegM unitsPerM)) 0.9999)))
                     segDist   (/ dist numSeg)
                     i 0)

               (while (<= i numSeg)
                 (setq sumpPt (polar pt1 ang (* i segDist)))
                 (if (not (near-p sumpPt sumpCache 100.0))
                   (progn
                     (mk (list '(0 . "CIRCLE") (cons 8 sumpLayer) (cons 10 sumpPt) (cons 40 sumpRad)))
                     (setq sumpCache (cons sumpPt sumpCache))))
                 (if (not (near-p sumpPt silCache 1500.0))
                   (progn
                     (make-il sumpPt)
                     (setq silCache (cons sumpPt silCache))))
                 (setq i (1+ i)))

               (setq i 0)
               (while (< i numSeg)
                 (setq pA (polar pt1 ang (* i segDist))
                       pB (polar pt1 ang (* (1+ i) segDist))
                       segMid  (list (/ (+ (car pA) (car pB)) 2.0) (/ (+ (cadr pA) (cadr pB)) 2.0) 0.0)
                       segDistM (fix (+ (/ (distance pA pB) unitsPerM) 0.5))
                       linePt1 (polar pA ang sumpRad)
                       linePt2 (polar pB (+ ang pi) sumpRad))

                 (mk (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine")
                           (cons 8 drainLayer)
                           (list 10 (car linePt1) (cadr linePt1) 0.0)
                           (list 11 (car linePt2) (cadr linePt2) 0.0)))

                 (setq arrowP1 (polar segMid (+ ang (* pi 0.85)) arrowSize)
                       arrowP2 (polar segMid (- ang (* pi 0.85)) arrowSize))
                 (mk (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 arrowLayer)
                           '(100 . "AcDbPolyline") '(90 . 3) '(70 . 0) (cons 43 arrowWidth)
                           (list 10 (car arrowP1) (cadr arrowP1))
                           (list 10 (car segMid) (cadr segMid))
                           (list 10 (car arrowP2) (cadr arrowP2))))

                 ;; -------------------------------------------------------------
                 ;; DYNAMIC TEXT: Automatically formats sequence and details
                 ;; -------------------------------------------------------------
                 (setq strCode (if (and currCode (/= currCode "")) (strcat currCode ";\\P") ""))
                 (setq strCodeInline (if (and currCode (/= currCode "")) (strcat currCode "; ") ""))

                 (cond
                   ((<= segDistM 6)
                    (setq str (strcat strCode size "%%c\\P" (itoa segDistM) "m\\P" grad)
                          distAbove (* txtHgt 0.25)
                          distBelow (* txtHgt 3.5)))
                   ((<= segDistM 14)
                    (setq str (strcat strCodeInline size "%%c\\P" (itoa segDistM) "m " grad)
                          distAbove (* txtHgt 0.25)
                          distBelow (* txtHgt 1.8)))
                   (T
                    (setq str (strcat strCodeInline size "%%c " (itoa segDistM) "m " grad)
                          distAbove (* txtHgt 0.25)
                          distBelow (* txtHgt 1.25))))

                 (setq pTextAbove (polar segMid (+ txtAngRad (/ pi 2.0)) distAbove)
                       pTextBelow (polar segMid (- txtAngRad (/ pi 2.0)) distBelow)
                       blockedAbove (text-blocked-p pTextAbove)
                       blockedBelow (text-blocked-p pTextBelow))
                 (setq pText (if (and blockedAbove (not blockedBelow)) pTextBelow pTextAbove))

                 (setq mtextStr (strcat "{\\W" (rtos txtWidth 2 4) ";" str "}"))
                 (mk (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 textLayer) '(100 . "AcDbMText")
                           (list 10 (car pText) (cadr pText) 0.0)
                           (cons 40 txtHgt) (cons 41 0.0) '(71 . 8) '(72 . 1)
                           (cons 1 mtextStr) (cons 7 txtStyle)
                           (cons 50 finalTxtAng) (list 11 (cos finalTxtAng) (sin finalTxtAng) 0.0)
                           '(73 . 1) '(44 . 0.8)))
                 
                 ;; Auto-Sequence the Code for the next segment drawn
                 (if (and currCode (/= currCode ""))
                   (setq currCode (next-seq currCode)))
                   
                 (setq i (1+ i)))

               (setq history (cons (cons stepEnts prevCode) history))
               (if (> numSeg 1)
                 (princ (strcat "\nSuccess! Auto-split into " (itoa numSeg) " segments."))
                 (princ "\nSuccess! Pipe generated."))))))))
  )

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)