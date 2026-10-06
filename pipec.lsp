;;; ==========================================================================
;;; PIPEC - Drain line generator (auto-split, sumps, flow arrows, labels, SIL)
;;; Universal build: vanilla AutoLISP (no vla-/vlax-), AutoCAD / LT / GstarCAD.
;;; ==========================================================================
(defun c:pipec ( / *error* sysVars sysVals ucsSaved
                   ;; --- settings ---
                   drainLayer drainColor drainWidth arrowLayer arrowColor arrowWidth
                   sumpLayer sumpColor sumpRad textLayer textColor txtHgt txtStyle txtWidth
                   mlLayer mlStyle mlText ilMode unitsPerM maxSegM defCode defSize defGrad
                   ;; --- working variables ---
                   askInfo code size grad pt1 pt2 ang dist numSeg segDist i
                   sumpPt pA pB segMid segDistM linePt1 linePt2 arrowSize arrowP1 arrowP2
                   txtAngRad pText str mtextStr distAbove distBelow pTextAbove pTextBelow
                   blockedAbove blockedBelow history stepEnts loop sumpCache silCache c e
                   ;; --- local helper functions ---
                   ensure-layer esc-filter pt2d near-p mk finish-cmd restore-ucs
                   scan-sumps scan-sils mlstyle-exists-p text-blocked-p
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

  (setq mlLayer "#JRK - RD Drain Text IL")
  (setq mlStyle "1000-T2")             ;; multileader style (used if it exists)
  (setq mlText "{\\W0.5;SIL00.00}")
  (setq ilMode 1)                      ;; 1 = MLEADER command (auto-fallback), 2 = LINE + MTEXT only

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
  (defun make-il-mleader (sPt tPt / last0 newEnt ed ok)
    (setq last0 (entlast) ok nil)
    (if (mlstyle-exists-p mlStyle) (setvar "CMLEADERSTYLE" mlStyle))
    (setq ucsSaved (list (getvar "UCSNAME") (getvar "UCSORG") (getvar "UCSXDIR") (getvar "UCSYDIR")))
    (command "_.UCS" "_W")
    (command "_.UCS" "_Z" (* 180.0 (/ txtAngRad pi)))
    (command "_.MLEADER" "_NON" (trans sPt 0 1) "_NON" (trans tPt 0 1) mlText)
    (finish-cmd)
    (restore-ucs)
    (setq newEnt (entlast))
    (if (and newEnt (not (equal newEnt last0)))
      (progn
        (setq ed (entget newEnt))
        (if (= (cdr (assoc 0 ed)) "MULTILEADER")
          (progn
            (entmod (subst (cons 8 mlLayer) (assoc 8 ed) ed))
            (setq stepEnts (cons newEnt stepEnts) ok T)))))
    ok)

  ;; Fallback SIL label: plain LINE + MTEXT (works everywhere)
  (defun make-il-basic (sPt tPt)
    (mk (list '(0 . "LINE") (cons 8 mlLayer) (cons 10 sPt) (cons 11 tPt)))
    (mk (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 mlLayer) '(100 . "AcDbMText")
              (list 10 (car tPt) (cadr tPt) 0.0) (cons 40 txtHgt) (cons 41 0.0)
              '(71 . 7) '(72 . 1) (cons 7 txtStyle) (cons 50 txtAngRad)
              (list 11 (cos txtAngRad) (sin txtAngRad) 0.0) (cons 1 mlText))))

  (defun make-il (sPt / tPt ok)
    (setq tPt (polar (polar sPt txtAngRad (* txtHgt 0.8)) (+ txtAngRad (/ pi 2.0)) (* txtHgt 1.5))
          ok  nil)
    (if (= ilMode 1) (setq ok (make-il-mleader sPt tPt)))
    (if (not ok) (make-il-basic sPt tPt)))

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
      (setq grad (getstring T (strcat "\nGradient <" defGrad ">: "))) (if (= grad "") (setq grad defGrad))))

  ;; one-off scans of what is already drawn (fast in heavy drawings)
  (setq sumpCache (scan-sumps) silCache (scan-sils))

  (setq history nil loop T)

  ;; ---- main loop ----------------------------------------------------------
  (while loop
    (initget "Undo")
    (setq pt1 (getpoint "\nClick Start Point of Drain [Undo] <Exit>: "))
    (cond
      ((null pt1) (setq loop nil))

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
