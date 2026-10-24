;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : pipechouse.lsp
;;; COMMAND     : PIPECHOUSE (Aliases: PIPEC-HOUSE, PHOUSE)
;;; DESCRIPTION : Residential Road Drainage Generator with Live 30m Visual
;;;               Ruler, LWPOLYLINE Drain Segments & Up/Down Text Placement
;;; ==========================================================================

(defun c:pipechouse ( / *error* sysVars sysVals ucsChanged
                        ;; --- settings ---
                        drainLayer drainColor drainWidth arrowLayer arrowColor arrowWidth
                        sumpLayer sumpColor sumpRad textLayer textColor txtHgt txtStyle txtWidth txtPos
                        mlLayer mlStyle mlText mlJustify mlSide mlLeadLen mlLandDist mlArrowSize mlLandGap
                        ilMode unitsPerM maxSegM guideWidth defCode defSize defGrad pipeTextRot0
                        ;; --- working variables ---
                        askInfo ans currCode prevCode size grad pt1 pt2 pickedEnd ang dist maxDist
                        nodePts currPt remDistM splitLoop pPick pPickWcs dAlong dCurr projPt segM
                        numRem remStep k tempEnts
                        numSeg i sumpPt pA pB segMid segDistM linePt1 linePt2 arrowSize arrowP1 arrowP2
                        txtAngRad finalTxtAng pText str strCode strCodeInline mtextStr
                        distAbove distBelow pTextAbove pTextBelow
                        history stepEnts lastStep loop sumpCache silCache e
                        ;; --- local helper functions ---
                        restore-session ensure-layer ensure-textstyle esc-filter pt2d near-p mk mk-temp
                        clear-temp draw-split-hud next-seq scan-sumps scan-sils mlstyle-exists-p
                        ensure-mlstyle force-mleader-text style-mleader-obj
                        make-il make-il-mleader make-il-basic)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq unitsPerM  1000.0)      
  (setq maxSegM    30.0)        
  (setq guideWidth 500.0)       

  (setq drainLayer "#JRK - RD Drain Line")
  (setq drainColor 4)
  (setq drainWidth 0.0)        ;; Polyline width for drainage line

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
  (setq txtPos   "Up")         ;; Default Pipe Text Position: "Up" (Above line) or "Down" (Below line)

  (setq mlLayer     "#JRK - RD Drain Text IL")
  (setq mlStyle     "1000-T2")             
  (setq mlText      "{\\W0.5;SIL00.00}")   
  (setq mlJustify   "Right")               
  (setq mlSide      "Right")               
  (setq mlLeadLen   6000.0)    ;; Extended so MLeader sits farther out
  (setq mlLandDist  2000.0)                
  (setq mlArrowSize 2000.0)                
  (setq mlLandGap   1000.0)                
  (setq ilMode      1)                     
  
  (setq pipeTextRot0 nil) 

  (setq defCode "A01")
  (setq defSize "600")
  (setq defGrad "1:000")
  ;; =========================================================================

  (vl-load-com)
  (setq sysVars    '("CMDECHO" "CMLEADERSTYLE" "CLAYER" "UCSFOLLOW")
        sysVals    (mapcar 'getvar sysVars)
        ucsChanged nil
        tempEnts   nil)

  (defun mk-temp (data)
    (if (entmake data) (setq tempEnts (cons (entlast) tempEnts)))
  )

  (defun clear-temp ( / te)
    (foreach te tempEnts
      (if (and te (entget te)) (entdel te))
    )
    (setq tempEnts nil)
  )

  ;; Helper to restore UCS to Previous and reset system variables
  (defun restore-session ()
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (clear-temp)
    (if ucsChanged
      (progn
        (command "_.UCS" "_Previous")
        (setq ucsChanged nil)
      )
    )
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  )

  (defun draw-split-hud ( / idx pStart pEnd pMid lockedM limitPt perpAng txtPreviewAng
                            t1 t2 tPt tickLen stepM dTick)
    (clear-temp)
    (setq perpAng       (+ txtAngRad (/ pi 2.0))
          txtPreviewAng (if (= (strcase txtPos) "DOWN") (- txtAngRad (/ pi 2.0)) perpAng)
          tickLen       (* txtHgt 1.8))

    (setq idx 0)
    (while (< idx (1- (length nodePts)))
      (setq pStart  (nth idx nodePts)
            pEnd    (nth (1+ idx) nodePts)
            pMid    (list (/ (+ (car pStart) (car pEnd)) 2.0)
                          (/ (+ (cadr pStart) (cadr pEnd)) 2.0) 0.0)
            lockedM (fix (+ (/ (distance pStart pEnd) unitsPerM) 0.5)))

      (mk-temp (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 drainLayer)
                     '(62 . 3) '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0) (cons 43 (* guideWidth 0.7))
                     (list 10 (car pStart) (cadr pStart))
                     (list 10 (car pEnd) (cadr pEnd))))
      (mk-temp (list '(0 . "CIRCLE") (cons 8 sumpLayer) '(62 . 3) (cons 10 pEnd) (cons 40 (* sumpRad 1.2))))
      (setq tPt (polar pMid txtPreviewAng (* txtHgt (if (= (strcase txtPos) "DOWN") 1.8 1.2))))
      (mk-temp (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 textLayer) '(62 . 3) '(100 . "AcDbMText")
                     (list 10 (car tPt) (cadr tPt) 0.0)
                     (cons 40 (* txtHgt 0.9)) '(41 . 0.0) '(71 . 8) '(72 . 1)
                     (cons 1 (strcat "{\\W0.6;LOCKED: " (itoa lockedM) "m (" (strcase txtPos) ")}"))
                     (cons 7 txtStyle) (cons 50 txtAngRad)
                     (list 11 (cos txtAngRad) (sin txtAngRad) 0.0)))
      (setq idx (1+ idx))
    )

    (setq limitPt (polar currPt ang maxDist))

    (mk-temp (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 drainLayer)
                   '(62 . 2) '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0) (cons 43 guideWidth)
                   (list 10 (car currPt) (cadr currPt))
                   (list 10 (car limitPt) (cadr limitPt))))

    (mk-temp (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 drainLayer)
                   '(62 . 1) '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0) (cons 43 guideWidth)
                   (list 10 (car limitPt) (cadr limitPt))
                   (list 10 (car pt2) (cadr pt2))))

    (mk-temp (list '(0 . "CIRCLE") (cons 8 sumpLayer) '(62 . 2) (cons 10 currPt) (cons 40 (* sumpRad 1.3))))
    (setq tPt (polar currPt (- perpAng pi) (* txtHgt 1.5)))
    (mk-temp (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 textLayer) '(62 . 2) '(100 . "AcDbMText")
                   (list 10 (car tPt) (cadr tPt) 0.0)
                   (cons 40 (* txtHgt 0.85)) '(41 . 0.0) '(71 . 2) '(72 . 1)
                   (cons 1 "{\\W0.6;0m}")
                   (cons 7 txtStyle) (cons 50 txtAngRad)
                   (list 11 (cos txtAngRad) (sin txtAngRad) 0.0)))

    (foreach stepM '(10.0 20.0)
      (setq dTick (* stepM unitsPerM))
      (if (< dTick maxDist)
        (progn
          (setq pMid (polar currPt ang dTick)
                t1   (polar pMid perpAng (* tickLen 0.5))
                t2   (polar pMid (- perpAng pi) (* tickLen 0.5))
                tPt  (polar pMid perpAng (* tickLen 0.7)))
          (mk-temp (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 drainLayer)
                         '(62 . 2) '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0) (cons 43 (* guideWidth 0.4))
                         (list 10 (car t1) (cadr t1))
                         (list 10 (car t2) (cadr t2))))
          (mk-temp (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 textLayer) '(62 . 2) '(100 . "AcDbMText")
                         (list 10 (car tPt) (cadr tPt) 0.0)
                         (cons 40 (* txtHgt 0.85)) '(41 . 0.0) '(71 . 8) '(72 . 1)
                         (cons 1 (strcat "{\\W0.6;" (rtos stepM 2 0) "m}"))
                         (cons 7 txtStyle) (cons 50 txtAngRad)
                         (list 11 (cos txtAngRad) (sin txtAngRad) 0.0)))
        )
      )
    )

    (setq t1  (polar limitPt perpAng (* tickLen 1.2))
          t2  (polar limitPt (- perpAng pi) (* tickLen 1.2))
          tPt (polar limitPt perpAng (* tickLen 1.4)))

    (mk-temp (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 drainLayer)
                   '(62 . 1) '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0) (cons 43 (* guideWidth 0.8))
                   (list 10 (car t1) (cadr t1))
                   (list 10 (car t2) (cadr t2))))
    (mk-temp (list '(0 . "CIRCLE") (cons 8 sumpLayer) '(62 . 1) (cons 10 limitPt) (cons 40 (* sumpRad 1.4))))
    (mk-temp (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 textLayer) '(62 . 1) '(100 . "AcDbMText")
                   (list 10 (car tPt) (cadr tPt) 0.0)
                   (cons 40 (* txtHgt 1.15)) '(41 . 0.0) '(71 . 8) '(72 . 1)
                   (cons 1 (strcat "{\\W0.65;" (rtos maxSegM 2 0) "m MAX LIMIT [TEXT: " (strcase txtPos) "]}"))
                   (cons 7 txtStyle) (cons 50 txtAngRad)
                   (list 11 (cos txtAngRad) (sin txtAngRad) 0.0)))
  )

  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))   
          (command "_.-LAYER" "_N" nm "_C" col nm ""))))
    (command "_.-LAYER" "_T" nm "_ON" nm "_U" nm ""))

  (defun ensure-textstyle (nm wid)
    (if (not (tblsearch "STYLE" nm))
      (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                     (cons 2 nm) '(70 . 0) '(40 . 0.0) (cons 41 wid) '(50 . 0.0)
                     '(71 . 0) '(3 . "romans.shx") '(4 . "")))))

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

  (defun next-seq (str / len lastChar asciiVal i numStr preStr val pad s)
    (setq len (strlen str))
    (if (> len 0)
      (progn
        (setq lastChar (substr str len 1)
              asciiVal (ascii lastChar))
        (cond
          ((or (and (>= asciiVal 65) (< asciiVal 90)) (and (>= asciiVal 97) (< asciiVal 122)))
           (strcat (substr str 1 (1- len)) (chr (1+ asciiVal))))
          ((= asciiVal 90) (strcat (substr str 1 (1- len)) "A"))
          ((= asciiVal 122) (strcat (substr str 1 (1- len)) "a"))
          ((and (>= asciiVal 48) (<= asciiVal 57))
           (setq i len numStr "")
           (while (and (> i 0) (>= (ascii (substr str i 1)) 48) (<= (ascii (substr str i 1)) 57))
             (setq numStr (strcat (substr str i 1) numStr) i (1- i)))
           (setq preStr (substr str 1 i) val (1+ (atoi numStr)) pad (strlen numStr) s (itoa val))
           (while (< (strlen s) pad) (setq s (strcat "0" s)))
           (strcat preStr s))
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

  (defun ensure-mlstyle (nm / doc dict mlStyles styleObj)
    (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list (vlax-get-acad-object))))
    (if (and doc (not (vl-catch-all-error-p doc)))
      (progn
        (setq dict (vl-catch-all-apply 'vla-get-Dictionaries (list doc)))
        (if (and dict (not (vl-catch-all-error-p dict)))
          (setq mlStyles (vl-catch-all-apply 'vla-item (list dict "ACAD_MLEADERSTYLE"))))
        (if (and mlStyles (not (vl-catch-all-error-p mlStyles)))
          (progn
            (setq styleObj (vl-catch-all-apply 'vla-item (list mlStyles nm)))
            (if (vl-catch-all-error-p styleObj)
              (setq styleObj (vl-catch-all-apply 'vla-AddObject (list mlStyles nm "AcDbMLeaderStyle"))))
            (if (and styleObj (not (vl-catch-all-error-p styleObj)))
              (progn
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'ContentType 2))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'LeaderLineType 1))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'MaxLeaderSegmentsPoints 2))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'EnableLanding :vlax-true))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'EnableDogleg :vlax-true))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'Annotative :vlax-false))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'ScaleFactor 1.0))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'TextHeight txtHgt))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'ArrowSize mlArrowSize))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'DoglegLength mlLandDist))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'LandingGap mlLandGap))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'TextLeftAttachmentType 1))
                (vl-catch-all-apply 'vlax-put-property (list styleObj 'TextRightAttachmentType 1))
                (if (tblsearch "STYLE" txtStyle)
                  (vl-catch-all-apply 'vlax-put-property (list styleObj 'TextStyleName txtStyle)))
              )
            )
          )
        )
      )
    )
  )

  (defun force-mleader-text (ent str / ed newEd replaced item)
    (setq ed (entget ent) newEd nil replaced nil)
    (foreach item ed
      (if (and (not replaced)
               (= (car item) 304)
               (/= (strcase (cdr item)) "LEADER_LINE{"))
        (progn
          (setq newEd (cons (cons 304 str) newEd))
          (setq replaced T))
        (setq newEd (cons item newEd))))
    (if replaced (progn (entmod (reverse newEd)) (entupd ent)))
  )

  (defun style-mleader-obj (newEnt str / ed obj)
    (setq ed (entget newEnt))
    (entmod (subst (cons 8 mlLayer) (assoc 8 ed) ed))
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list newEnt)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (progn
        (if (mlstyle-exists-p mlStyle)
          (vl-catch-all-apply 'vlax-put-property (list obj 'StyleName mlStyle)))
        (vl-catch-all-apply 'vlax-put-property (list obj 'Layer mlLayer))
        (vl-catch-all-apply 'vlax-put-property (list obj 'ScaleFactor 1.0))
        (vl-catch-all-apply 'vlax-put-property (list obj 'ContentType 2))
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextString str))
        (if (tblsearch "STYLE" txtStyle)
          (vl-catch-all-apply 'vlax-put-property (list obj 'TextStyleName txtStyle)))
        (vl-catch-all-apply 'vlax-put-property (list obj 'ArrowheadSize mlArrowSize))
        (vl-catch-all-apply 'vlax-put-property (list obj 'DoglegLength mlLandDist))
        (vl-catch-all-apply 'vlax-put-property (list obj 'LandingGap mlLandGap))
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextHeight txtHgt))
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextLeftAttachmentType 1))
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextRightAttachmentType 1))
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextJustify (if (= (strcase mlJustify) "LEFT") 1 3)))
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextRotation 0.0))
        (vl-catch-all-apply 'vla-update (list obj))
      )
    )
    (force-mleader-text newEnt str)
  )

  ;; -------------------------------------------------------------------------
  ;; DUAL-ENGINE MULTILEADER GENERATOR (Works in ALL Drawings & UCS States)
  ;; -------------------------------------------------------------------------
  (defun make-il-mleader (sPt / last0 newEnt ok perpAng arrowPt cornerPt dx p1_ucs p2_ucs
                                doc space ptsArr mlObj dogDir)
    (setq last0 (entlast) ok nil)
    (ensure-mlstyle mlStyle)
    (if (mlstyle-exists-p mlStyle)
      (vl-catch-all-apply 'setvar (list "CMLEADERSTYLE" mlStyle)))

    (setq perpAng (if (= (strcase mlSide) "RIGHT")
                    (- txtAngRad (/ pi 2.0))
                    (+ txtAngRad (/ pi 2.0))))

    (setq arrowPt  (polar sPt perpAng sumpRad)
          cornerPt (polar arrowPt perpAng mlLeadLen)
          dx       (if (= (strcase mlJustify) "LEFT") 10.0 -10.0))

    ;; Engine 1: Try UCS-translated _.MLEADER command
    (setq p1_ucs (trans arrowPt 0 1)
          p2_ucs (trans (list (+ (car cornerPt) dx) (cadr cornerPt) 0.0) 0 1))
    (vl-catch-all-apply 'setvar (list "CLAYER" mlLayer))
    (command "_.MLEADER" "_NON" p1_ucs "_NON" p2_ucs mlText "")
    (while (> (getvar "CMDACTIVE") 0) (command ""))

    (setq newEnt (entlast))
    (if (and newEnt (not (equal newEnt last0)) (= (cdr (assoc 0 (entget newEnt))) "MULTILEADER"))
      (progn
        (style-mleader-obj newEnt mlText)
        (setq stepEnts (cons newEnt stepEnts) ok T)
      )
    )

    ;; Engine 2: If command _.MLEADER failed in this drawing, create MULTILEADER via ActiveX vla-AddMLeader
    (if (not ok)
      (progn
        (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list (vlax-get-acad-object))))
        (if (and doc (not (vl-catch-all-error-p doc)))
          (progn
            (setq space (if (= (getvar "CVPORT") 1)
                          (vl-catch-all-apply 'vla-get-PaperSpace (list doc))
                          (vl-catch-all-apply 'vla-get-ModelSpace (list doc))))
            (if (and space (not (vl-catch-all-error-p space)))
              (progn
                (setq ptsArr (vlax-make-safearray vlax-vbDouble '(0 . 5)))
                (vlax-safearray-fill ptsArr
                  (list (car arrowPt) (cadr arrowPt) 0.0
                        (+ (car cornerPt) dx) (cadr cornerPt) 0.0))
                (setq mlObj (vl-catch-all-apply 'vla-AddMLeader (list space ptsArr 0)))
                (if (and mlObj (not (vl-catch-all-error-p mlObj)))
                  (progn
                    (setq dogDir (vlax-3d-point (list (if (= (strcase mlJustify) "LEFT") 1.0 -1.0) 0.0 0.0)))
                    (vl-catch-all-apply 'vla-SetDoglegDirection (list mlObj 0 dogDir))
                    (setq newEnt (vlax-vla-object->ename mlObj))
                    (if newEnt
                      (progn
                        (style-mleader-obj newEnt mlText)
                        (setq stepEnts (cons newEnt stepEnts) ok T)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    ok)

  (defun make-il-basic (sPt / perpAng arrowPt cornerPt landPt txtPt jCode aP1 aP2 rotAng)
    (setq perpAng (if (= (strcase mlSide) "RIGHT")
                    (- txtAngRad (/ pi 2.0))
                    (+ txtAngRad (/ pi 2.0))))
    (setq arrowPt  (polar sPt perpAng sumpRad)
          cornerPt (polar arrowPt perpAng mlLeadLen))

    (mk (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine")
              (cons 8 mlLayer) (cons 10 arrowPt) (cons 11 cornerPt)))

    (setq aP1 (polar arrowPt (+ perpAng (* pi 0.85)) (* mlArrowSize 0.4))
          aP2 (polar arrowPt (- perpAng (* pi 0.85)) (* mlArrowSize 0.4)))
    (mk (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 mlLayer)
              '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) (cons 43 0.0)
              (list 10 (car arrowPt) (cadr arrowPt))
              (list 10 (car aP1) (cadr aP1))
              (list 10 (car aP2) (cadr aP2))))

    (setq rotAng 0.0)
    (if (= (strcase mlJustify) "LEFT")
      (setq landPt (polar cornerPt 0.0 mlLandDist)
            txtPt  (polar landPt 0.0 mlLandGap)
            jCode  1)
      (setq landPt (polar cornerPt pi mlLandDist)
            txtPt  (polar landPt pi mlLandGap)
            jCode  3))

    (mk (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine")
              (cons 8 mlLayer) (cons 10 cornerPt) (cons 11 landPt)))
    (mk (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 mlLayer) '(100 . "AcDbMText")
              (list 10 (car txtPt) (cadr txtPt) 0.0)
              (cons 40 txtHgt) (cons 41 0.0)
              (cons 71 jCode) '(72 . 1) (cons 7 txtStyle) (cons 50 rotAng)
              (list 11 (cos rotAng) (sin rotAng) 0.0)
              (cons 1 mlText))))

  (defun make-il (sPt / ok)
    (setq ok nil)
    (if (= ilMode 1) (setq ok (make-il-mleader sPt)))
    (if (not ok) (make-il-basic sPt)))

  (defun *error* (msg)
    (restore-session)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nAction Cancelled: " msg)))
    (princ))

  ;; -------------------------------------------------------------------------
  ;; INITIALIZE LAYERS, STYLES & SWITCH UCS TO WORLD
  ;; -------------------------------------------------------------------------
  (setvar "CMDECHO" 0)
  (setvar "UCSFOLLOW" 0)

  (if (= (getvar "WORLDUCS") 0)
    (progn
      (command "_.UCS" "_World")
      (setq ucsChanged T)
    )
  )

  (ensure-layer drainLayer drainColor)
  (ensure-layer arrowLayer arrowColor)
  (ensure-layer sumpLayer  sumpColor)
  (ensure-layer textLayer  textColor)
  (ensure-layer mlLayer    textColor)

  (ensure-textstyle txtStyle txtWidth)
  (ensure-textstyle mlStyle  txtWidth)

  (princ "\nCommand: PIPECHOUSE (Live 30m Ruler & Party-Wall Sump Splitter)")

  (initget "Yes No")
  (setq askInfo (getkword "\nEnter custom drain details? [Yes/No] <No>: "))
  (setq currCode defCode size defSize grad defGrad)
  (if (= askInfo "Yes")
    (progn
      (setq currCode (getstring T (strcat "\nStarting Code <" defCode ">: "))) (if (= currCode "") (setq currCode defCode))
      (setq size (getstring T (strcat "\nSize <" defSize ">: ")))             (if (= size "") (setq size defSize))
      (setq grad (getstring T (strcat "\nGradient <" defGrad ">: ")))         (if (= grad "") (setq grad defGrad))
      (initget "Up Down")
      (setq ans (getkword (strcat "\nDrain Text Position [Up/Down] <" txtPos ">: ")))
      (if ans (setq txtPos ans))
      (initget "Left Right")
      (setq ans (getkword (strcat "\nSIL Text Justification [Left/Right] <" mlJustify ">: ")))
      (if ans (setq mlJustify ans))
      (initget "Left Right")
      (setq ans (getkword (strcat "\nSIL Leader Offset Side [Left/Right] <" mlSide ">: ")))
      (if ans (setq mlSide ans))))

  (setq sumpCache (scan-sumps) silCache (scan-sils))
  (setq history nil loop T maxDist (* maxSegM unitsPerM))

  ;; -------------------------------------------------------------------------
  ;; MAIN INTERACTIVE LOOP
  ;; -------------------------------------------------------------------------
  (while loop
    (initget "Text Up Down Side Justify Undo")
    (setq pt1 (getpoint (strcat "\nClick Start Point of Drain [Text/Up/Down/Side/Justify/Undo] <Exit> (Next:" currCode ", Text:" txtPos ", Side:" mlSide ", Justify:" mlJustify "): ")))
    (cond
      ((null pt1) (setq loop nil))

      ((= pt1 "Text")
       (setq txtPos (if (= (strcase txtPos) "UP") "Down" "Up"))
       (princ (strcat "\n>> Drain Text Position set to: " txtPos " (" (if (= (strcase txtPos) "UP") "Above" "Below") " line)")))

      ((or (= pt1 "Up") (= pt1 "Down"))
       (setq txtPos pt1)
       (princ (strcat "\n>> Drain Text Position set to: " txtPos " (" (if (= (strcase txtPos) "UP") "Above" "Below") " line)")))

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
           (princ (strcat "\n>> Last drain run undone. Sequence reverted to " currCode ".")))
         (princ "\n>> Nothing to undo.")))

      (T
       ;; Allow toggling Text (Up/Down) at the End Point prompt as well
       (setq pickedEnd nil)
       (while (not pickedEnd)
         (initget "Text Up Down Undo")
         (setq pt2 (getpoint pt1 (strcat "\nClick End Point of Drain [Text/Up/Down/Undo] <Cancel> (Text:" txtPos "): ")))
         (cond
           ((= pt2 "Text")
            (setq txtPos (if (= (strcase txtPos) "UP") "Down" "Up"))
            (princ (strcat "\n>> Drain Text Position set to: " txtPos " (" (if (= (strcase txtPos) "UP") "Above" "Below") " line)")))
           ((or (= pt2 "Up") (= pt2 "Down"))
            (setq txtPos pt2)
            (princ (strcat "\n>> Drain Text Position set to: " txtPos " (" (if (= (strcase txtPos) "UP") "Above" "Below") " line)")))
           (T
            (setq pickedEnd T))
         )
       )

       (if (or (null pt2) (= pt2 "Undo"))
         (princ "\n>> Cancelled point.")
         (progn
           (setq stepEnts nil
                 prevCode currCode 
                 pt1      (pt2d (trans pt1 1 0))
                 pt2      (pt2d (trans pt2 1 0))
                 dist     (distance pt1 pt2))
           (if (< dist 1.0e-6)
             (princ "\n>> Zero-length pipe ignored.")
             (progn
               (setq ang       (angle pt1 pt2)
                     arrowSize (* txtHgt 0.8)
                     txtAngRad (if (and (> ang (+ (/ pi 2.0) 1e-9)) (<= ang (+ (* 1.5 pi) 1e-9))) (- ang pi) ang)
                     finalTxtAng (if pipeTextRot0 0.0 txtAngRad)
                     nodePts   (list pt1)
                     currPt    pt1)

               (if (> dist maxDist)
                 (progn
                   (princ (strcat "\n>> Total span is " (rtos (/ dist unitsPerM) 2 1) "m (exceeds " (rtos maxSegM 2 0) "m max)."))
                   (princ "\n>> YELLOW = Valid 0-30m Zone | RED = 30m Max Limit | GREEN = Locked Segments")

                   (setq splitLoop T)
                   (while (and splitLoop (> (distance currPt pt2) maxDist))
                     (draw-split-hud)

                     (setq remDistM (/ (distance currPt pt2) unitsPerM))
                     (initget "Auto Done Text Up Down Undo")
                     (setq pPick (getpoint (trans currPt 0 1)
                                           (strcat "\nRemaining: " (rtos remDistM 2 1)
                                                   "m (Text:" txtPos "). Click party wall before RED 30m mark [Auto/Done/Text/Up/Down/Undo] <Auto>: ")))
                     (cond
                       ((or (null pPick) (= pPick "Auto"))
                        (setq numRem  (max 1 (fix (+ (/ (distance currPt pt2) maxDist) 0.9999)))
                              remStep (/ (distance currPt pt2) numRem)
                              k       1)
                        (while (< k numRem)
                          (setq nodePts (append nodePts (list (polar currPt ang (* k remStep)))))
                          (setq k (1+ k))
                        )
                        (setq splitLoop nil))

                       ((= pPick "Done")
                        (setq splitLoop nil))

                       ((= pPick "Text")
                        (setq txtPos (if (= (strcase txtPos) "UP") "Down" "Up"))
                        (princ (strcat "\n>> Drain Text Position switched to: " txtPos " (" (if (= (strcase txtPos) "UP") "Above" "Below") " line)")))

                       ((or (= pPick "Up") (= pPick "Down"))
                        (setq txtPos pPick)
                        (princ (strcat "\n>> Drain Text Position switched to: " txtPos " (" (if (= (strcase txtPos) "UP") "Above" "Below") " line)")))

                       ((= pPick "Undo")
                        (if (> (length nodePts) 1)
                          (progn
                            (setq nodePts (reverse (cdr (reverse nodePts)))
                                  currPt  (last nodePts))
                            (princ "\n>> Removed last intermediate sump. 30m ruler shifted back.")
                          )
                          (princ "\n>> Already at the start point.")
                        ))

                       ((listp pPick)
                        (setq pPickWcs (pt2d (trans pPick 1 0)))
                        (setq dAlong (+ (* (- (car pPickWcs) (car pt1)) (cos ang))
                                        (* (- (cadr pPickWcs) (cadr pt1)) (sin ang)))
                              dCurr  (distance pt1 currPt))

                        (cond
                          ((<= dAlong (+ dCurr 100.0))
                           (princ "\n>> Invalid point: Must be forward along the drain direction!"))
                          ((>= dAlong (- dist 100.0))
                           (princ "\n>> Invalid point: Past the end point of the drain!"))
                          (T
                           (setq projPt (polar pt1 ang dAlong)
                                 segM   (/ (distance currPt projPt) unitsPerM))
                           (setq nodePts (append nodePts (list projPt))
                                 currPt  projPt)
                           (if (> segM maxSegM)
                             (princ (strcat "\n>> WARNING: Segment is " (rtos segM 2 1) "m (exceeds " (rtos maxSegM 2 0) "m)! Type U to re-pick closer, or continue."))
                             (princ (strcat "\n>> Sump locked at " (rtos segM 2 1) "m! 30m ruler updated from new sump."))
                           )
                          )
                        ))
                     )
                   )
                   (clear-temp)
                 )
               )

               (setq nodePts (append nodePts (list pt2))
                     numSeg  (1- (length nodePts)))

               (foreach sumpPt nodePts
                 (if (not (near-p sumpPt sumpCache 100.0))
                   (progn
                     (mk (list '(0 . "CIRCLE") (cons 8 sumpLayer) (cons 10 sumpPt) (cons 40 sumpRad)))
                     (setq sumpCache (cons sumpPt sumpCache))))
                 (if (not (near-p sumpPt silCache 1500.0))
                   (progn
                     (make-il sumpPt)
                     (setq silCache (cons sumpPt silCache))))
               )

               (setq i 0)
               (while (< i numSeg)
                 (setq pA       (nth i nodePts)
                       pB       (nth (1+ i) nodePts)
                       segMid   (list (/ (+ (car pA) (car pB)) 2.0) (/ (+ (cadr pA) (cadr pB)) 2.0) 0.0)
                       segDistM (fix (+ (/ (distance pA pB) unitsPerM) 0.5))
                       linePt1  (polar pA ang sumpRad)
                       linePt2  (polar pB (+ ang pi) sumpRad))

                 ;; Drainage Line created as LWPOLYLINE
                 (mk (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 drainLayer)
                           '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0) (cons 43 drainWidth)
                           (list 10 (car linePt1) (cadr linePt1))
                           (list 10 (car linePt2) (cadr linePt2))))

                 (setq arrowP1 (polar segMid (+ ang (* pi 0.85)) arrowSize)
                       arrowP2 (polar segMid (- ang (* pi 0.85)) arrowSize))
                 (mk (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 arrowLayer)
                           '(100 . "AcDbPolyline") '(90 . 3) '(70 . 0) (cons 43 arrowWidth)
                           (list 10 (car arrowP1) (cadr arrowP1))
                           (list 10 (car segMid) (cadr segMid))
                           (list 10 (car arrowP2) (cadr arrowP2))))

                 (setq strCode       (if (and currCode (/= currCode "")) (strcat currCode ";\\P") ""))
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
                       pText      (if (= (strcase txtPos) "DOWN") pTextBelow pTextAbove))

                 (setq mtextStr (strcat "{\\W" (rtos txtWidth 2 4) ";" str "}"))
                 (mk (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 textLayer) '(100 . "AcDbMText")
                           (list 10 (car pText) (cadr pText) 0.0)
                           (cons 40 txtHgt) (cons 41 0.0) '(71 . 8) '(72 . 1)
                           (cons 1 mtextStr) (cons 7 txtStyle)
                           (cons 50 finalTxtAng) (list 11 (cos finalTxtAng) (sin finalTxtAng) 0.0)
                           '(73 . 1) '(44 . 0.8)))

                 (if (and currCode (/= currCode ""))
                   (setq currCode (next-seq currCode)))

                 (setq i (1+ i)))

               (setq history (cons (cons stepEnts prevCode) history))
               (if (> numSeg 1)
                 (princ (strcat "\nSuccess! Split into " (itoa numSeg) " custom party-wall polyline segments (Text: " txtPos ")."))
                 (princ (strcat "\nSuccess! Drain polyline generated (Text: " txtPos ").")))))))))
  )

  ;; Restore previous UCS and system variables upon exit
  (restore-session)
  (princ)
)

;; Quick Command Aliases
(defun c:pipec-house () (c:pipechouse))
(defun c:phouse () (c:pipechouse))