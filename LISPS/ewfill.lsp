;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : ewfill.lsp
;;; COMMAND     : EWFILL (Aliases: LVLFILL, GAPFILL, INTERPLVL)
;;; DESCRIPTION : Earthwork & Road Proposed Level Gap Filler / Interpolator.
;;;               Auto-calculates and fills intermediate proposed levels between
;;;               2 existing level texts (or 2 points) by Distance Interval
;;;               (e.g. every 100m) or by Number of Equal Gaps.
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:ewfill ( / *error* oldCmd oldOs oldDimzin
                    ;; --- user settings & state ---
                    unitsPerM fillMode stepDistM numGaps drawSpot drawBox
                    boxPadFactor alignToLine
                    ;; --- helper functions ---
                    ef:trim ef:fmt2dec get-text-raw parse-level-str
                    get-text-center-wcs draw-rotated-box pick-level-or-point
                    ;; --- working variables ---
                    loop p1Data p2Data pt1 pt2 z1 z2 preStr sufStr
                    refEnt refEd txtLay txtSty txtHgt txtWid txtCol
                    ang txtAng totalUnits totalM slopePct slopeRatio
                    ptsList dM k ratio zVal pIns lblStr fullStr
                    stepEnts history lastStep e ans )

  ;; =========================================================================
  ;; 1. DEFAULT SETTINGS (Toggled Live in Command Prompt)
  ;; =========================================================================
  (setq unitsPerM    1000.0   ;; 1000.0 = Millimeter drawing (1m = 1000 units), 1.0 = Meter drawing
        fillMode     "DIST"   ;; "DIST" = Fixed interval (e.g. every 100m), "GAPS" = Equal N gaps
        stepDistM    100.0    ;; Default interval in meters (100m)
        numGaps      4        ;; Default number of equal gaps when fillMode = "GAPS"
        drawSpot     "No"     ;; "Yes" = Draw cross/circle spot marker at exact point, "No" = Text only
        drawBox      "No"     ;; "Yes" = Draw rotated rectangular box around text (like RFL27.30)
        boxPadFactor 0.35     ;; Box padding factor (x Text Height)
        alignToLine  "Yes")   ;; "Yes" = Rotate text along the line between Pt1 & Pt2, "No" = Keep source angle

  (setq oldCmd    (getvar "CMDECHO")
        oldOs     (getvar "OSMODE")
        oldDimzin (getvar "DIMZIN"))
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0) ;; Prevents AutoCAD from stripping trailing ".00" zeros

  (defun *error* (msg)
    (if oldDimzin (setvar "DIMZIN" oldDimzin))
    (if oldOs     (setvar "OSMODE" oldOs))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EWFILL Error]: " msg))
    )
    (princ)
  )

  ;; =========================================================================
  ;; 2. PURE AUTOLISP STRING, NUMBER & GEOMETRY HELPERS (LT 2024+ Safe)
  ;; =========================================================================
  (defun ef:trim (s / len i1 i2)
    (if (or (null s) (/= (type s) 'STR))
      ""
      (progn
        (setq len (strlen s) i1 1 i2 len)
        (while (and (<= i1 len) (member (substr s i1 1) '(" " "\t" "\r" "\n")))
          (setq i1 (1+ i1))
        )
        (while (and (>= i2 i1) (member (substr s i2 1) '(" " "\t" "\r" "\n")))
          (setq i2 (1- i2))
        )
        (if (> i1 i2) "" (substr s i1 (1+ (- i2 i1))))
      )
    )
  )

  ;; Strict 2-decimal formatter (e.g. 27 -> "27.00", 27.5 -> "27.50")
  (defun ef:fmt2dec (val / s dotPos decLen)
    (setvar "DIMZIN" 0)
    (setq s (rtos (if (numberp val) val 0.0) 2 2))
    (if (= (substr s 1 1) ".")
      (setq s (strcat "0" s))
    )
    (if (= (substr s 1 2) "-.")
      (setq s (strcat "-0." (substr s 3)))
    )
    (setq dotPos (vl-string-search "." s))
    (if (null dotPos)
      (strcat s ".00")
      (progn
        (setq decLen (- (strlen s) dotPos 1))
        (while (< decLen 2)
          (setq s      (strcat s "0")
                decLen (1+ decLen))
        )
        s
      )
    )
  )

  ;; Reads raw text from TEXT, MTEXT, or MULTILEADER
  (defun get-text-raw (ent / elist typ res obj item)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          res   nil)
    (if (and (= typ "MULTILEADER") (boundp 'getpropertyvalue))
      (setq res (vl-catch-all-apply 'getpropertyvalue (list ent "MText/Contents")))
    )
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (progn
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (and obj (not (vl-catch-all-error-p obj)))
          (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
        )
      )
    )
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (cond
        ((= typ "MULTILEADER")
         (setq res "")
         (foreach item elist
           (if (and (= (car item) 304)
                    (= (type (cdr item)) 'STR)
                    (/= (strcase (cdr item)) "LEADER_LINE{")
                    (= res ""))
             (setq res (cdr item))
           )
         ))
        ((= typ "MTEXT")
         (setq res "")
         (foreach item elist
           (if (and (or (= (car item) 3) (= (car item) 1))
                    (= (type (cdr item)) 'STR))
             (setq res (strcat res (cdr item)))
           )
         ))
        (T
         (setq res (if (assoc 1 elist) (cdr (assoc 1 elist)) "")))
      )
    )
    (if (and res (= (type res) 'STR)) res "")
  )

  ;; Strips MTEXT inline codes ({\W0.5;, \f...;) and splits "FFL27.80m" -> (list 27.80 "FFL" "m")
  (defun parse-level-str (raw / len i loopP ch sec semi vis pBrace
                                numStart numStr pre suf hasDot nextCh)
    (setq len (strlen raw) i 1 loopP T)
    (while (and loopP (<= i len))
      (setq ch  (substr raw i 1)
            sec (if (< i len) (substr raw (1+ i) 1) ""))
      (cond
        ((= ch "{") (setq i (1+ i)))
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" raw (1- i)))
         (if semi (setq i (+ semi 2)) (setq loopP nil)))
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k" "~")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )
    (setq vis (substr raw i))
    (if (setq pBrace (vl-string-search "}" vis))
      (setq vis (substr vis 1 pBrace))
    )

    ;; Find the first numeric value inside visible text
    (setq len (strlen vis) i 1 numStart nil)
    (while (and (<= i len) (null numStart))
      (setq ch  (substr vis i 1)
            sec (if (< i len) (substr vis (1+ i) 1) ""))
      (if (or (and (>= (ascii ch) 48) (<= (ascii ch) 57))
              (and (or (= ch "-") (= ch "+"))
                   (< i len)
                   (>= (ascii sec) 48) (<= (ascii sec) 57)))
        (setq numStart i)
        (setq i (1+ i))
      )
    )

    (if (null numStart)
      nil
      (progn
        (setq numStr (substr vis numStart 1)
              hasDot nil
              i      (1+ numStart))
        (while (<= i len)
          (setq nextCh (substr vis i 1))
          (cond
            ((and (>= (ascii nextCh) 48) (<= (ascii nextCh) 57))
             (setq numStr (strcat numStr nextCh) i (1+ i)))
            ((and (= nextCh ".") (not hasDot) (< i len)
                  (>= (ascii (substr vis (1+ i) 1)) 48)
                  (<= (ascii (substr vis (1+ i) 1)) 57))
             (setq hasDot T numStr (strcat numStr nextCh) i (1+ i)))
            (T
             (setq len (- len)))
          )
        )
        (if (< len 0) (setq len (- len)))
        (setq pre (if (> numStart 1) (substr vis 1 (1- numStart)) "")
              suf (if (<= i len) (substr vis i) ""))
        (list (distof numStr) pre suf)
      )
    )
  )

  ;; True visual center of TEXT or MTEXT in WCS (2D Z=0)
  (defun get-text-center-wcs (ent / ed typ p10 ang tb bMin bMax locCx locCy ocsPt wcsPt
                                    w h att dx dy ux uy vx vy cx cy)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed)))
    (cond
      ((= typ "TEXT")
       (setq p10 (cdr (assoc 10 ed))
             ang (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
             tb  (textbox ed))
       (if tb
         (progn
           (setq bMin  (car tb)
                 bMax  (cadr tb)
                 locCx (/ (+ (car bMin) (car bMax)) 2.0)
                 locCy (/ (+ (cadr bMin) (cadr bMax)) 2.0)
                 ocsPt (list (+ (car p10) (- (* locCx (cos ang)) (* locCy (sin ang))))
                             (+ (cadr p10) (+ (* locCx (sin ang)) (* locCy (cos ang))))
                             0.0)
                 wcsPt (trans ocsPt ent 0))
           (list (car wcsPt) (cadr wcsPt) 0.0)
         )
         (progn
           (setq wcsPt (trans p10 ent 0))
           (list (car wcsPt) (cadr wcsPt) 0.0)
         )
       ))

      ((= typ "MTEXT")
       (setq p10 (cdr (assoc 10 ed))
             w   (if (assoc 42 ed) (cdr (assoc 42 ed)) 0.0)
             h   (if (assoc 43 ed) (cdr (assoc 43 ed)) (if (assoc 40 ed) (cdr (assoc 40 ed)) 0.0))
             att (if (assoc 71 ed) (cdr (assoc 71 ed)) 1)
             ang (if (assoc 11 ed)
                   (angle '(0.0 0.0 0.0) (cdr (assoc 11 ed)))
                   (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)))
       (setq dx (cond ((member att '(1 4 7)) (/ w 2.0))
                      ((member att '(2 5 8)) 0.0)
                      (T (/ w -2.0)))
             dy (cond ((member att '(1 2 3)) (/ h -2.0))
                      ((member att '(4 5 6)) 0.0)
                      (T (/ h 2.0))))
       (setq ux (cos ang) uy (sin ang)
             vx (- (sin ang)) vy (cos ang)
             cx (+ (car p10) (* dx ux) (* dy vx))
             cy (+ (cadr p10) (* dx uy) (* dy vy)))
       (list cx cy 0.0))

      (T
       (setq p10 (cdr (assoc 10 ed)))
       (list (car p10) (cadr p10) 0.0))
    )
  )

  ;; Draws a rotated rectangular LWPOLYLINE box around a newly created TEXT/MTEXT
  (defun draw-rotated-box (ent padFactor / ed typ lay col hgt pad p10 ang tb bMin bMax
                                           x1 y1 x2 y2 w h att dxMin dxMax dyMin dyMax
                                           ux uy vx vy c1 c2 c3 c4 dxfBox)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed))
          lay (cdr (assoc 8 ed))
          col (if (assoc 62 ed) (cdr (assoc 62 ed)) nil)
          hgt (if (assoc 40 ed) (cdr (assoc 40 ed)) 1.0)
          pad (* hgt padFactor))
    (cond
      ((= typ "TEXT")
       (setq p10 (cdr (assoc 10 ed))
             ang (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
             tb  (textbox ed))
       (if tb
         (progn
           (setq bMin (car tb) bMax (cadr tb)
                 x1 (- (car bMin) pad) y1 (- (cadr bMin) pad)
                 x2 (+ (car bMax) pad) y2 (+ (cadr bMax) pad)
                 ux (cos ang) uy (sin ang) vx (- (sin ang)) vy (cos ang))
           (setq c1 (trans (list (+ (car p10) (* x1 ux) (* y1 vx)) (+ (cadr p10) (* x1 uy) (* y1 vy)) 0.0) ent 0)
                 c2 (trans (list (+ (car p10) (* x2 ux) (* y1 vx)) (+ (cadr p10) (* x2 uy) (* y1 vy)) 0.0) ent 0)
                 c3 (trans (list (+ (car p10) (* x2 ux) (* y2 vx)) (+ (cadr p10) (* x2 uy) (* y2 vy)) 0.0) ent 0)
                 c4 (trans (list (+ (car p10) (* x1 ux) (* y2 vx)) (+ (cadr p10) (* x1 uy) (* y2 vy)) 0.0) ent 0))
         )
       ))
      ((= typ "MTEXT")
       (setq p10 (cdr (assoc 10 ed))
             w   (if (assoc 42 ed) (cdr (assoc 42 ed)) (* hgt 4.0))
             h   (if (assoc 43 ed) (cdr (assoc 43 ed)) hgt)
             att (if (assoc 71 ed) (cdr (assoc 71 ed)) 5)
             ang (if (assoc 11 ed) (angle '(0.0 0.0 0.0) (cdr (assoc 11 ed))) (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)))
       (cond
         ((member att '(1 4 7)) (setq dxMin (- pad)            dxMax (+ w pad)))
         ((member att '(2 5 8)) (setq dxMin (- (/ w -2.0) pad) dxMax (+ (/ w 2.0) pad)))
         (T                     (setq dxMin (- (- w) pad)      dxMax pad))
       )
       (cond
         ((member att '(1 2 3)) (setq dyMin (- (- h) pad)      dyMax pad))
         ((member att '(4 5 6)) (setq dyMin (- (/ h -2.0) pad) dyMax (+ (/ h 2.0) pad)))
         (T                     (setq dyMin (- pad)            dyMax (+ h pad)))
       )
       (setq ux (cos ang) uy (sin ang) vx (- (sin ang)) vy (cos ang))
       (setq c1 (list (+ (car p10) (* dxMin ux) (* dyMin vx)) (+ (cadr p10) (* dxMin uy) (* dyMin vy)) 0.0)
             c2 (list (+ (car p10) (* dxMax ux) (* dyMin vx)) (+ (cadr p10) (* dxMax uy) (* dyMin vy)) 0.0)
             c3 (list (+ (car p10) (* dxMax ux) (* dyMax vx)) (+ (cadr p10) (* dxMax uy) (* dyMax vy)) 0.0)
             c4 (list (+ (car p10) (* dxMin ux) (* dyMax vx)) (+ (cadr p10) (* dxMin uy) (* dyMax vy)) 0.0)))
    )
    (if (and c1 c2 c3 c4)
      (progn
        (setq dxfBox
          (append
            (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay))
            (if col (list (cons 62 col)) nil)
            (list '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(43 . 0.0)
                  (list 10 (car c1) (cadr c1))
                  (list 10 (car c2) (cadr c2))
                  (list 10 (car c3) (cadr c3))
                  (list 10 (car c4) (cadr c4)))
          )
        )
        (if (entmake dxfBox) (setq stepEnts (cons (entlast) stepEnts)))
      )
    )
  )

  ;; Smart Picker: Click an existing Proposed Level Text (auto-extracts point, level & style)
  ;; OR type "Point" (or click empty space) to pick a coordinate and type the level manually
  (defun pick-level-or-point (tagPrompt defaultPrefix / done sel errCode e pt ssPick raw parsed zVal wcsPt)
    (setq done nil)
    (while (not done)
      (setvar "ERRNO" 0)
      (initget "Point Mode Interval Gaps Units Box Spot Align Undo Exit")
      (setq sel (entsel tagPrompt))
      (setq errCode (getvar "ERRNO"))

      (cond
        ;; User pressed Enter or typed Exit
        ((or (= sel "Exit") (and (null sel) (= errCode 52)))
         (setq done "EXIT"))

        ;; User entered a keyword to adjust settings on the fly
        ((member sel '("Mode" "Interval" "Gaps" "Units" "Box" "Spot" "Align" "Undo"))
         (setq done sel))

        ;; User typed "Point" or clicked empty space -> pick coordinate & type level
        ((or (= sel "Point") (and (null sel) (= errCode 7)))
         (setvar "OSMODE" oldOs)
         (setq pt (getpoint "\n>> Pick exact coordinate point: "))
         (if pt
           (progn
             (setq zVal (getreal "\n>> Enter Proposed Level at this point (e.g. 27.80): "))
             (if zVal
               (progn
                 (setq wcsPt (trans pt 1 0))
                 (setq done (list (list (car wcsPt) (cadr wcsPt) 0.0) zVal defaultPrefix "" nil))
               )
             )
           )
         ))

        ;; User clicked an object -> check if TEXT/MTEXT/MULTILEADER (with click-through fallback)
        ((listp sel)
         (setq e  (car sel)
               pt (cadr sel))
         (if (not (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER")))
           (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT,MULTILEADER"))))
             (setq e (ssname ssPick 0))
           )
         )
         (if (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER"))
           (progn
             (setq raw    (get-text-raw e)
                   parsed (parse-level-str raw))
             (if parsed
               (progn
                 (setq wcsPt (get-text-center-wcs e))
                 (princ (strcat "\n>> Detected Level: "
                                (nth 1 parsed) (ef:fmt2dec (nth 0 parsed)) (nth 2 parsed)))
                 (setq done (list wcsPt (nth 0 parsed) (nth 1 parsed) (nth 2 parsed) e))
               )
               (princ (strcat "\n>> No numeric level found in \"" raw "\". Click a valid level text or type P for Point."))
             )
           )
           (progn
             (setq zVal (getreal "\n>> Clicked geometry point. Enter Proposed Level here (e.g. 27.80): "))
             (if zVal
               (progn
                 (setq wcsPt (trans pt 1 0))
                 (setq done (list (list (car wcsPt) (cadr wcsPt) 0.0) zVal defaultPrefix "" nil))
               )
             )
           )
         ))
      )
    )
    done
  )

  ;; =========================================================================
  ;; 3. MAIN INTERACTIVE LOOP
  ;; =========================================================================
  (princ "\n==================================================================")
  (princ "\n  EWFILL : Earthwork Proposed Level Gap Filler & Interpolator")
  (princ "\n  - Click 2 existing Proposed Level texts (e.g. FFL27.80 & FFL24.20)")
  (princ "\n    OR type P [Point] to click any 2 points and type their levels.")
  (princ "\n==================================================================")

  (setq history nil loop T preStr "FFL " sufStr "")

  (while loop
    (setq p1Data
      (pick-level-or-point
        (strcat "\n[1/2] Click Start Proposed Level Text (or [Point/Mode/Interval/Gaps/Units/Box/Spot/Align/Undo]) <Exit> "
                "(Mode:" (if (= fillMode "DIST") (strcat "Every " (rtos stepDistM 2 0) "m") (strcat (itoa numGaps) " Equal Gaps"))
                ", Units:" (if (= unitsPerM 1000.0) "mm" "m")
                ", Box:" drawBox "): ")
        preStr))

    (cond
      ((or (null p1Data) (= p1Data "EXIT"))
       (setq loop nil))

      ((= p1Data "Mode")
       (initget "Distance Gaps")
       (setq ans (getkword "\nFill Mode - by fixed Distance interval or number of equal Gaps? [Distance/Gaps] <Distance>: "))
       (if (= ans "Gaps")
         (progn
           (setq fillMode "GAPS")
           (initget 6)
           (setq ans (getint (strcat "\nEnter number of equal gaps between the 2 levels <" (itoa numGaps) ">: ")))
           (if ans (setq numGaps ans))
         )
         (progn
           (setq fillMode "DIST")
           (initget 6)
           (setq ans (getreal (strcat "\nEnter distance interval in meters (e.g. 100, 50, 25) <" (rtos stepDistM 2 1) ">: ")))
           (if ans (setq stepDistM ans))
         )
       ))

      ((= p1Data "Interval")
       (setq fillMode "DIST")
       (initget 6)
       (setq ans (getreal (strcat "\nEnter distance interval in meters (e.g. 100, 50, 25) <" (rtos stepDistM 2 1) ">: ")))
       (if ans (setq stepDistM ans))
       (princ (strcat "\n>> Interval set to every " (rtos stepDistM 2 1) "m (Mode: Distance).")))

      ((= p1Data "Gaps")
       (setq fillMode "GAPS")
       (initget 6)
       (setq ans (getint (strcat "\nEnter number of equal gaps between the 2 levels (e.g. 4 = 3 intermediate points) <" (itoa numGaps) ">: ")))
       (if ans (setq numGaps ans))
       (princ (strcat "\n>> Set to " (itoa numGaps) " equal gaps (" (itoa (1- numGaps)) " intermediate levels).")))

      ((= p1Data "Units")
       (initget "Millimeters Meters")
       (setq ans (getkword "\nDrawing units scale [Millimeters (1m=1000) / Meters (1m=1)] <Millimeters>: "))
       (setq unitsPerM (if (= ans "Meters") 1.0 1000.0))
       (princ (strcat "\n>> Drawing scale set to " (rtos unitsPerM 2 0) " CAD units per meter.")))

      ((= p1Data "Box")
       (setq drawBox (if (= drawBox "Yes") "No" "Yes"))
       (princ (strcat "\n>> Draw rotated box around generated levels: " drawBox)))

      ((= p1Data "Spot")
       (setq drawSpot (if (= drawSpot "Yes") "No" "Yes"))
       (princ (strcat "\n>> Draw spot cross/circle marker at each point: " drawSpot)))

      ((= p1Data "Align")
       (setq alignToLine (if (= alignToLine "Yes") "No" "Yes"))
       (princ (strcat "\n>> Align text rotation to line direction: " alignToLine)))

      ((= p1Data "Undo")
       (if history
         (progn
           (setq lastStep (car history)
                 history  (cdr history))
           (foreach e lastStep (if (and e (entget e)) (entdel e)))
           (princ "\n>> Last interpolated level run undone.")
         )
         (princ "\n>> Nothing to undo.")
       ))

      ;; Valid Start Level picked -> Prompt for End Level
      ((listp p1Data)
       (setq pt1    (nth 0 p1Data)
             z1     (nth 1 p1Data)
             preStr (nth 2 p1Data)
             sufStr (nth 3 p1Data)
             refEnt (nth 4 p1Data))

       (setq p2Data
         (pick-level-or-point
           (strcat "\n[2/2] Click End Proposed Level Text (Start = "
                   preStr (ef:fmt2dec z1) sufStr
                   ") or [Point] <Cancel>: ")
           preStr))

       (if (and (listp p2Data) (= (length p2Data) 5))
         (progn
           (setq pt2 (nth 0 p2Data)
                 z2  (nth 1 p2Data))
           (if (and (null refEnt) (nth 4 p2Data))
             (setq refEnt (nth 4 p2Data)
                   preStr (nth 2 p2Data)
                   sufStr (nth 3 p2Data))
           )

           (setq totalUnits (distance pt1 pt2)
                 totalM     (/ totalUnits unitsPerM))

           (if (< totalUnits 1.0e-4)
             (princ "\n>> Start and End points are identical. Cancelled.")
             (progn
               ;; Extract layer, style, height, width factor & color from reference text (or active defaults)
               (if refEnt
                 (setq refEd  (entget refEnt)
                       txtLay (cdr (assoc 8 refEd))
                       txtSty (if (assoc 7 refEd) (cdr (assoc 7 refEd)) (getvar "TEXTSTYLE"))
                       txtHgt (if (assoc 40 refEd) (cdr (assoc 40 refEd)) (* 2.0 unitsPerM))
                       txtWid (if (assoc 41 refEd) (cdr (assoc 41 refEd)) 0.85)
                       txtCol (if (assoc 62 refEd) (cdr (assoc 62 refEd)) nil))
                 (setq txtLay (getvar "CLAYER")
                       txtSty (getvar "TEXTSTYLE")
                       txtHgt (* 2.0 unitsPerM)
                       txtWid 0.85
                       txtCol nil)
               )

               (setq ang (angle pt1 pt2))
               (if (= alignToLine "Yes")
                 ;; Keep text right-side up along the line angle
                 (setq txtAng (if (and (> ang (+ (/ pi 2.0) 1e-9)) (<= ang (+ (* 1.5 pi) 1e-9)))
                                (- ang pi)
                                ang))
                 (setq txtAng (if (and refEnt (assoc 50 (entget refEnt)))
                                (cdr (assoc 50 (entget refEnt)))
                                0.0))
               )

               ;; Build list of intermediate distances (in meters) from pt1
               (setq ptsList nil)
               (if (= fillMode "DIST")
                 (progn
                   (setq dM stepDistM)
                   ;; Stop slightly before totalM so we don't overwrite the existing End Text
                   (while (< dM (- totalM (* stepDistM 0.05)))
                     (setq ptsList (append ptsList (list dM))
                           dM      (+ dM stepDistM))
                   )
                 )
                 (progn
                   (setq k 1)
                   (while (< k numGaps)
                     (setq dM      (* totalM (/ (float k) (float numGaps)))
                           ptsList (append ptsList (list dM))
                           k       (1+ k))
                   )
                 )
               )

               (if (null ptsList)
                 (princ (strcat "\n>> Total distance is " (ef:fmt2dec totalM)
                                "m, which is shorter than the " (ef:fmt2dec stepDistM)
                                "m interval. Lower [Interval] or switch to [Gaps]!"))
                 (progn
                   (command "_.UNDO" "_Begin")
                   (setq stepEnts nil)

                   (foreach dM ptsList
                     (setq ratio   (/ dM totalM)
                           zVal    (+ z1 (* (- z2 z1) ratio))
                           pIns    (polar pt1 ang (* dM unitsPerM))
                           fullStr (strcat preStr (ef:fmt2dec zVal) sufStr))

                     ;; Optional Spot Cross + Circle Marker at exact interpolated coordinate
                     (if (= drawSpot "Yes")
                       (progn
                         (if (entmake (list '(0 . "CIRCLE") (cons 8 txtLay) (cons 10 pIns) (cons 40 (* txtHgt 0.25))))
                           (setq stepEnts (cons (entlast) stepEnts)))
                         (if (entmake (list '(0 . "LINE") (cons 8 txtLay)
                                            (cons 10 (polar pIns 0.0 (* txtHgt 0.4)))
                                            (cons 11 (polar pIns pi  (* txtHgt 0.4)))))
                           (setq stepEnts (cons (entlast) stepEnts)))
                         (if (entmake (list '(0 . "LINE") (cons 8 txtLay)
                                            (cons 10 (polar pIns (/ pi 2.0) (* txtHgt 0.4)))
                                            (cons 11 (polar pIns (* 1.5 pi) (* txtHgt 0.4)))))
                           (setq stepEnts (cons (entlast) stepEnts)))
                       )
                     )

                     ;; Create Middle-Center justified TEXT at the exact interpolated point
                     (if (entmake
                           (append
                             (list '(0 . "TEXT")
                                   '(100 . "AcDbEntity")
                                   (cons 8 txtLay))
                             (if txtCol (list (cons 62 txtCol)) nil)
                             (list '(100 . "AcDbText")
                                   (list 10 (car pIns) (cadr pIns) 0.0)
                                   (cons 40 txtHgt)
                                   (cons 1 fullStr)
                                   (cons 50 txtAng)
                                   (cons 41 txtWid)
                                   (cons 7 txtSty)
                                   '(72 . 1)
                                   (list 11 (car pIns) (cadr pIns) 0.0)
                                   '(100 . "AcDbText")
                                   '(73 . 2))
                           ))
                       (progn
                         (setq stepEnts (cons (entlast) stepEnts))
                         (if (= drawBox "Yes")
                           (draw-rotated-box (entlast) boxPadFactor)
                         )
                       )
                     )
                   )

                   (command "_.UNDO" "_End")
                   (setq history   (cons stepEnts history)
                         slopePct  (* (/ (- z2 z1) totalM) 100.0)
                         slopeRatio (if (> (abs (- z2 z1)) 1.0e-5)
                                      (strcat "1:" (rtos (abs (/ totalM (- z2 z1))) 2 0))
                                      "FLAT"))

                   (princ (strcat "\n>> Filled " (itoa (length ptsList))
                                  " intermediate proposed level(s) across " (ef:fmt2dec totalM) "m"
                                  " | dZ = " (if (>= (- z2 z1) 0.0) "+" "") (ef:fmt2dec (- z2 z1)) "m"
                                  " (" (ef:fmt2dec slopePct) "% / " slopeRatio ")."))
                 )
               )
             )
           )
         )
       ))
    )
  )

  (if oldDimzin (setvar "DIMZIN" oldDimzin))
  (if oldOs     (setvar "OSMODE" oldOs))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Command Aliases
(defun c:lvlfill ()   (c:ewfill))
(defun c:gapfill ()   (c:ewfill))
(defun c:interplvl () (c:ewfill))
(princ "\nEWFILL loaded. Type EWFILL, LVLFILL, GAPFILL, or INTERPLVL to run.")
(princ)