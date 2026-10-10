;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : sewpipec.lsp
;;; COMMAND     : SEWPIPEC (Alias: PIPEC)
;;; DESCRIPTION : Fully Automated Civil Sewer / Drainage Reticulation Generator
;;;               (Auto-Segmentation, Manhole Placement, Hydraulic Flow Arrows,
;;;                Collision-Aware Pipe Annotations & Parametric Invert Multileaders)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In municipal sewerage and reticulation engineering, drafters model gravity
;;; sewer mains connecting manholes at regulated maximum distances (e.g. 30m).
;;; Each segment requires downstream flow direction arrows, diameter/gradient
;;; annotations, and Manhole Invert Level (IL) multileader callouts.
;;;
;;; SEWPIPEC automates the complete sewer reticulation drafting sequence:
;;;   1. Prompts once per session for pipe attributes (Code, Size, Gradient).
;;;   2. Interactively accepts consecutive link endpoints (P1 -> P2).
;;;   3. Automatically subdivides long spans exceeding `maxSegM` (default 30m)
;;;      into equal segments and places circular manholes at all junction nodes.
;;;   4. Synthesizes 3-vertex polyline flow arrows centered on every pipe link.
;;;   5. Generates professional multi-line MTEXT annotations with intelligent
;;;      collision detection (placing text above or below to avoid clashes).
;;;   6. Spawns Invert Level (IL) multileaders with true native AutoCAD MLEADER
;;;      geometry matching standard Properties palette specifications:
;;;        - Arrowhead size : 2000 mm
;;;        - Landing length : 2000 mm
;;;        - Landing gap    : 1000 mm
;;;        - Justification  : Left or Right
;;;   7. Prevents duplicate manholes and IL leaders at shared junction nodes.
;;;   8. Localized Undo stack (`history` / `stepEnts`) supports step-by-step
;;;      undo (typing 'U') without terminating the session.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type SEWPIPEC (or PIPEC) in the command line.
;;;  2. Enter pipe Code (e.g. A01), Size (e.g. 600), Gradient (e.g. 1:100).
;;;  3. Click initial Start Point (Manhole location).
;;;  4. Click Next Point (Subsequent Manhole or discharge point).
;;;  5. The pipe segment, intermediate manholes, flow arrows, pipe annotation,
;;;     and IL multileaders are generated simultaneously.
;;;  6. Continue clicking downstream points or type U to undo the last segment.
;;;  7. Press Enter to finish.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - unitsPerM   : Drawing units per linear metre (default: 1000.0 = mm).
;;;  - maxSegM     : Maximum pipe run before auto-manhole insertion (default: 30.0 m).
;;;  - drainLayer  : Target layer for sewer linework (default: "#JRK - RD Drain Line").
;;;  - drainColor  : ACI Color index for linework (default: 4 - Cyan).
;;;  - drainWidth  : Polyline global width (default: 250.0 mm).
;;;  - sumpLayer   : Target layer for manhole circles (default: "#JRK - RD Drain Manhole_Sump").
;;;  - sumpRad     : Radius of circular manholes (default: 600.0 mm).
;;;  - arrowLayer  : Target layer for flow arrows (default: "#JRK - RD Drain FLOW").
;;;  - textLayer   : Target layer for pipe labels (default: "#JRK - RD Drain Text").
;;;  - txtHgt      : Text height for annotations (default: 2000.0 mm).
;;;  - mlLayer     : Target layer for IL Multileaders (default: "#JRK - RD Drain Text IL").
;;;  - mlStyle     : Native Multileader style name (default: "1000-T2").
;;;  - mlJustify   : Text justification ("Right" or "Left").
;;;  - mlSide      : Leader orientation ("Right" = -90 deg / below, "Left" = +90 deg / above).
;;;  - mlLeadLen   : Leader radial offset length from manhole rim (default: 3000.0 mm).
;;; ==========================================================================
(defun c:pipec ( / *error* sysVars sysVals ucsSaved
                   ;; --- settings ---
                   drainLayer drainColor drainWidth arrowLayer arrowColor arrowWidth
                   sumpLayer sumpColor sumpRad textLayer textColor txtHgt txtStyle txtWidth
                   mlLayer mlStyle mlText mlJustify mlSide mlLeadLen mlLandDist mlArrowSize mlLandGap
                   ilMode unitsPerM maxSegM defCode defSize defGrad
                   ;; --- working variables ---
                   askInfo ans code size grad pt1 pt2 ang dist numSeg segDist i
                   sumpPt pA pB segMid segDistM linePt1 linePt2 arrowSize arrowP1 arrowP2
                   txtAngRad pText str mtextStr distAbove distBelow pTextAbove pTextBelow
                   blockedAbove blockedBelow history stepEnts loop sumpCache silCache c e
                   ;; --- local helper functions ---
                   ensure-layer esc-filter pt2d near-p mk finish-cmd restore-ucs
                   scan-sumps scan-sils mlstyle-exists-p ensure-mlstyle text-blocked-p
                   make-il make-il-mleader make-il-basic)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq unitsPerM 1000.0)      ;; drawing units per metre (1000 = drawing in mm)
  (setq maxSegM   30.0)        ;; max pipe run (m) before a sump is auto-inserted

  (setq drainLayer "#JRK - RD Drain Line")
  (setq drainColor 4)
  (setq drainWidth 250.0)

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
  (setq mlStyle     "1000-T2")             ;; Multileader style (created automatically if missing)
  (setq mlText      "{\\W0.5;SIL00.00}")   ;; Text content
  (setq mlJustify   "Right")               ;; Text justification: "Right" (text left of landing) or "Left"
  (setq mlSide      "Right")               ;; Leader offset side from pipe: "Right" (-90 deg, below) or "Left" (+90 deg, above)
  (setq mlLeadLen   3000.0)                ;; Perpendicular leader length from sump rim (mm)
  (setq mlLandDist  2000.0)                ;; Horizontal landing length (matches Properties: 2000.0)
  (setq mlArrowSize 2000.0)                ;; Arrowhead size (matches Properties: 2000.0)
  (setq mlLandGap   1000.0)                ;; Landing gap (matches Properties: 1000.0)
  (setq ilMode      1)                     ;; 1 = MLEADER command (auto-fallback), 2 = LINE + MTEXT only

  (setq defCode "A01")
  (setq defSize "600")
  (setq defGrad "1:000")
  ;; =========================================================================

  ;; ---- system variables saved here and restored on exit / ESC / error -----
  (setq sysVars '("CMDECHO" "CMLEADERSTYLE")
        sysVals (mapcar 'getvar sysVars))

  ;; ---- helpers ------------------------------------------------------------
  ;; Create a layer via entmake (does NOT change the current layer)
  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))   ;; fallback if entmake of table records is blocked
          (command "_.-LAYER" "_N" nm "_C" col nm "")))))

  ;; Escape ssget wildcard characters (layer names like "#JRK" start with a wildcard!)
  (defun esc-filter (s / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen s))
      (setq ch (substr s k 1))
      (setq out (strcat out (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ",")) "`" "") ch))
      (setq k (1+ k)))
    out)

  ;; Flatten a point to Z = 0
  (defun pt2d (p) (list (car p) (cadr p) 0.0))

  ;; T if any cached point lies within rad of pt (2D)
  (defun near-p (pt cache rad / found c)
    (foreach c cache (if (< (distance (pt2d pt) (pt2d c)) rad) (setq found T)))
    found)

  ;; entmake + register the new entity for UNDO (only if entmake succeeded)
  (defun mk (data)
    (if (entmake data) (setq stepEnts (cons (entlast) stepEnts))))

  ;; Politely finish any command left waiting for input
  (defun finish-cmd ()
    (repeat 5 (if (> (getvar "CMDACTIVE") 0) (command "")))
    (if (> (getvar "CMDACTIVE") 0) (command)))

  ;; Put the UCS back exactly as the user had it
  (defun restore-ucs ()
    (if ucsSaved
      (progn
        (command "_.UCS" "_W")
        (cond
          ((/= (car ucsSaved) "") (command "_.UCS" "_R" (car ucsSaved)))
          ((not (and (equal (cadr ucsSaved) '(0.0 0.0 0.0) 1e-9)
                     (equal (caddr ucsSaved) '(1.0 0.0 0.0) 1e-9)
                     (equal (cadddr ucsSaved) '(0.0 1.0 0.0) 1e-9)))
           (command "_.UCS" "_3"
                    (cadr ucsSaved)
                    (mapcar '+ (cadr ucsSaved) (caddr ucsSaved))
                    (mapcar '+ (cadr ucsSaved) (cadddr ucsSaved)))))
        (setq ucsSaved nil))))

  ;; Existing manhole/sump circle centres (scanned ONCE, then cached)
  (defun scan-sumps ( / ss n pts)
    (setq ss (ssget "_X" (list '(0 . "CIRCLE") (cons 8 (esc-filter sumpLayer)) (cons 410 (getvar "CTAB")))))
    (if ss
      (progn (setq n 0)
        (while (< n (sslength ss))
          (setq pts (cons (cdr (assoc 10 (entget (ssname ss n)))) pts))
          (setq n (1+ n)))))
    pts)

  ;; Positions of existing "SIL" labels (TEXT / MTEXT / MULTILEADER) - scanned ONCE
  (defun scan-sils ( / ss n ed typ found d pts)
    (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER") (cons 410 (getvar "CTAB")))))
    (if ss
      (progn (setq n 0)
        (while (< n (sslength ss))
          (setq ed (entget (ssname ss n)) typ (cdr (assoc 0 ed)) found nil)
          ;; groups 1 / 3 = TEXT-MTEXT strings, 304 = MULTILEADER content
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

  ;; Does the multileader style exist? (checked via dictionaries - no vla)
  (defun mlstyle-exists-p (nm / d)
    (setq d (dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
    (if (and d (dictsearch (cdr (assoc -1 d)) nm)) T nil))

  ;; Ensure the Multileader Style exists; if not, create/configure it with exact specifications
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

  ;; Any TEXT/MTEXT crossing a square (2*txtHgt) around pt? (crossing polygon = UCS safe)
  (defun text-blocked-p (pt / h ss)
    (setq h (* txtHgt 2.0))
    (setq ss (ssget "_CP"
                    (list (trans (list (- (car pt) h) (- (cadr pt) h) 0.0) 0 1)
                          (trans (list (+ (car pt) h) (- (cadr pt) h) 0.0) 0 1)
                          (trans (list (+ (car pt) h) (+ (cadr pt) h) 0.0) 0 1)
                          (trans (list (- (car pt) h) (+ (cadr pt) h) 0.0) 0 1))
                    '((0 . "TEXT,MTEXT"))))
    (if ss T nil))

  ;; SIL label as a real MLEADER (rotated UCS so text follows the pipe). Returns T on success.
  (defun make-il-mleader (sPt / last0 newEnt ed ok perpAng arrowPt cornerPt p1_ucs p2_ucs dx obj)
    (setq last0 (entlast) ok nil)
    (ensure-mlstyle mlStyle)
    (if (mlstyle-exists-p mlStyle)
      (vl-catch-all-apply 'setvar (list "CMLEADERSTYLE" mlStyle)))

    ;; 1. Calculate perpendicular leader angle (relative to readable text angle)
    (setq perpAng (if (= (strcase mlSide) "RIGHT")
                    (- txtAngRad (/ pi 2.0))
                    (+ txtAngRad (/ pi 2.0))))

    ;; 2. Arrowhead point at sump edge
    (setq arrowPt (polar sPt perpAng sumpRad))

    ;; 3. Corner point (landing junction) offset perpendicularly from arrowhead
    (setq cornerPt (polar arrowPt perpAng mlLeadLen))

    ;; 4. Rotate UCS around Z by txtAngRad so X-axis follows the pipe line
    (setq ucsSaved (list (getvar "UCSNAME") (getvar "UCSORG") (getvar "UCSXDIR") (getvar "UCSYDIR")))
    (command "_.UCS" "_W")
    (command "_.UCS" "_Z" (* 180.0 (/ txtAngRad pi)))

    ;; 5. Transform points to active UCS
    (setq p1_ucs (trans arrowPt 0 1))
    (setq p2_ucs (trans cornerPt 0 1))

    ;; 6. Control landing direction & justification:
    ;; If mlJustify is "Right": landing extends to the LEFT (-X in UCS), text is Right-justified.
    ;; If mlJustify is "Left": landing extends to the RIGHT (+X in UCS), text is Left-justified.
    (setq dx (if (= (strcase mlJustify) "LEFT") 10.0 -10.0))
    (setq p2_ucs (list (+ (car p2_ucs) dx) (cadr p2_ucs) 0.0))

    ;; 7. Run MLEADER command with double Enter to close text entry
    (setvar "CLAYER" mlLayer)
    (command "_.MLEADER" "_NON" p1_ucs "_NON" p2_ucs mlText "")
    (while (> (getvar "CMDACTIVE") 0) (command ""))
    (restore-ucs)

    ;; 8. Post-process created multileader to harden every property
    (setq newEnt (entlast))
    (if (and newEnt (not (equal newEnt last0)))
      (progn
        (setq ed (entget newEnt))
        (if (= (cdr (assoc 0 ed)) "MULTILEADER")
          (progn
            ;; Enforce target layer via entmod
            (entmod (subst (cons 8 mlLayer) (assoc 8 ed) ed))
            ;; Enforce every property via VLA to match the exact properties palette
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
                 (if (vlax-property-available-p obj 'TextRotation T)
                   (vlax-put-property obj 'TextRotation (+ txtAngRad (/ pi 2.0))))
                 (if (vlax-property-available-p obj 'ScaleFactor T)
                   (vlax-put-property obj 'ScaleFactor 1.0))
                 (vla-update obj)))
            (setq stepEnts (cons newEnt stepEnts) ok T)))))
    ok)

  ;; Fallback SIL label: plain LINE + MTEXT (universal fallback)
  (defun make-il-basic (sPt / perpAng arrowPt cornerPt landPt txtPt jCode aP1 aP2 rotAng)
    (setq perpAng (if (= (strcase mlSide) "RIGHT")
                    (- txtAngRad (/ pi 2.0))
                    (+ txtAngRad (/ pi 2.0))))
    (setq arrowPt  (polar sPt perpAng sumpRad)
          cornerPt (polar arrowPt perpAng mlLeadLen))

    ;; Leader line
    (mk (list '(0 . "LINE") (cons 8 mlLayer) (cons 10 arrowPt) (cons 11 cornerPt)))

    ;; Arrowhead (solid polyline wedge pointing toward sump)
    (setq aP1 (polar arrowPt (+ perpAng (* pi 0.85)) (* mlArrowSize 0.4))
          aP2 (polar arrowPt (- perpAng (* pi 0.85)) (* mlArrowSize 0.4)))
    (mk (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 mlLayer)
              '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) (cons 43 0.0)
              (list 10 (car arrowPt) (cadr arrowPt))
              (list 10 (car aP1) (cadr aP1))
              (list 10 (car aP2) (cadr aP2))))

    ;; Horizontal landing line and MText position
    (setq rotAng (+ txtAngRad (/ pi 2.0)))
    (if (= (strcase mlJustify) "LEFT")
      (progn
        ;; Text on Right of landing (Left justified)
        (setq landPt (polar cornerPt txtAngRad mlLandDist)
              txtPt  (polar landPt txtAngRad mlLandGap)
              jCode  1)) ;; Top-Left
      (progn
        ;; Text on Left of landing (Right justified)
        (setq landPt (polar cornerPt (+ txtAngRad pi) mlLandDist)
              txtPt  (polar landPt (+ txtAngRad pi) mlLandGap)
              jCode  3))) ;; Top-Right

    (mk (list '(0 . "LINE") (cons 8 mlLayer) (cons 10 cornerPt) (cons 11 landPt)))
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

  ;; ---- error handler: cancels pending command, restores UCS + sysvars -----
  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (if ucsSaved (restore-ucs))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nAction Cancelled: " msg)))
    (princ))

  ;; ---- setup --------------------------------------------------------------
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
  (setq code defCode size defSize grad defGrad)
  (if (= askInfo "Yes")
    (progn
      (setq code (getstring T (strcat "\nCode <" defCode ">: ")))     (if (= code "") (setq code defCode))
      (setq size (getstring T (strcat "\nSize <" defSize ">: ")))     (if (= size "") (setq size defSize))
      (setq grad (getstring T (strcat "\nGradient <" defGrad ">: "))) (if (= grad "") (setq grad defGrad))
      (initget "Left Right")
      (setq ans (getkword (strcat "\nSIL Text Justification [Left/Right] <" mlJustify ">: ")))
      (if ans (setq mlJustify ans))
      (initget "Left Right")
      (setq ans (getkword (strcat "\nSIL Leader Offset Side [Left/Right] <" mlSide ">: ")))
      (if ans (setq mlSide ans))))

  ;; one-off scans of what is already drawn (fast in heavy drawings)
  (setq sumpCache (scan-sumps) silCache (scan-sils))

  (setq history nil loop T)

  ;; ---- main loop ----------------------------------------------------------
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
           (foreach e (car history) (if (and e (entget e)) (entdel e)))
           (setq history   (cdr history)
                 sumpCache (scan-sumps)
                 silCache  (scan-sils))
           (princ "\n>> Last drain segment undone."))
         (princ "\n>> Nothing to undo.")))

      (T
       (initget "Undo")
       (setq pt2 (getpoint pt1 "\nClick End Point of Drain [Undo] <Cancel>: "))
       (if (or (null pt2) (= pt2 "Undo"))
         (princ "\n>> Cancelled point.")
         (progn
           (setq stepEnts nil
                 pt1  (pt2d (trans pt1 1 0))
                 pt2  (pt2d (trans pt2 1 0))
                 dist (distance pt1 pt2))
           (if (< dist 1.0e-6)
             (princ "\n>> Zero-length pipe ignored.")
             (progn
               (setq ang       (angle pt1 pt2)
                     arrowSize (* txtHgt 0.8)
                     ;; keep text readable (never upside-down)
                     txtAngRad (if (and (> ang (+ (/ pi 2.0) 1e-9)) (<= ang (+ (* 1.5 pi) 1e-9))) (- ang pi) ang)
                     numSeg    (max 1 (fix (+ (/ dist (* maxSegM unitsPerM)) 0.9999)))
                     segDist   (/ dist numSeg)
                     i 0)

               ;; sumps + SIL labels at every run point (skip those that already exist)
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

               ;; pipe segments, flow arrows and labels
               (setq i 0)
               (while (< i numSeg)
                 (setq pA (polar pt1 ang (* i segDist))
                       pB (polar pt1 ang (* (1+ i) segDist))
                       segMid  (list (/ (+ (car pA) (car pB)) 2.0) (/ (+ (cadr pA) (cadr pB)) 2.0) 0.0)
                       segDistM (fix (+ (/ (distance pA pB) unitsPerM) 0.5))
                       linePt1 (polar pA ang sumpRad)
                       linePt2 (polar pB (+ ang pi) sumpRad))

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

                 ;; label layout depends on how long the segment is
                 (cond
                   ((<= segDistM 6)
                    (setq str (strcat code ";\\P" size "%%c\\P" (itoa segDistM) "m\\P" grad)
                          distAbove (* txtHgt 0.25)
                          distBelow (* txtHgt 3.5)))
                   ((<= segDistM 14)
                    (setq str (strcat code "; " size "%%c\\P" (itoa segDistM) "m " grad)
                          distAbove (* txtHgt 0.25)
                          distBelow (* txtHgt 1.8)))
                   (T
                    (setq str (strcat code "; " size "%%c " (itoa segDistM) "m " grad)
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
                           (cons 50 txtAngRad) (list 11 (cos txtAngRad) (sin txtAngRad) 0.0)
                           '(73 . 1) '(44 . 0.8)))
                 (setq i (1+ i)))

               (setq history (cons stepEnts history))
               (if (> numSeg 1)
                 (princ (strcat "\nSuccess! Auto-split into " (itoa numSeg) " segments."))
                 (princ "\nSuccess! Pipe generated."))))))))
  )

  ;; ---- normal exit: restore sysvars --------------------------------------
  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)

;; Alias SEWPIPEC to execute the pipeline engine
(defun c:sewpipec () (c:pipec))

