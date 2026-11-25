;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : ewfill.lsp
;;; COMMAND     : EWFILL (Aliases: LVLFILL, GAPFILL, INTERPLVL)
;;; DESCRIPTION : Earthwork & Road Proposed Level Gap Filler / Interpolator (v2.0)
;;;               - Auto-detects Drawing Units (mm vs m) from clicked text height
;;;               - Supports TEXT, MTEXT, MULTILEADER, Block ATTRIB & Manual Points
;;;               - 3 Fill Modes:
;;;                   1. Auto-Even Gaps (uniform gaps <= 100m, no awkward remainder)
;;;                   2. Fixed Interval (e.g. exact 100m steps)
;;;                   3. Manual Equal Gaps (divide span into N equal gaps)
;;;               - Optional Polyline Path mode to follow curved road alignments
;;;               - Safe MTEXT/MULTILEADER property extraction (fixes DXF 41=0 bug)
;;;               - Mathematically centered Rotated Box & strict 0.00 formatting
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:ewfill ( / *error* oldCmd oldOs oldDimzin undoStarted
                    ;; --- settings & state ---
                    unitsMode unitsPerM fillMode stepDistM numGaps
                    drawSpot drawBox boxPadFactor alignToLine
                    ;; --- helper functions ---
                    ef:trim ef:fmt2dec ef:ceil ef:strip-mtext-codes
                    get-text-raw extract-mtext-width parse-level-str
                    get-entity-center-wcs get-text-props draw-centered-box
                    get-poly-pts-wcs get-poly-total-len get-poly-pt-and-ang-at-dist
                    pick-level-or-point ask-fill-method
                    ;; --- working variables ---
                    loop p1Data p2Data pt1 pt2 z1 z2 preStr sufStr
                    refEnt props txtLay txtSty txtHgt txtWid txtCol srcAng
                    usePoly polySel polyEnt polyPts totalUnits totalM
                    ang txtAng slopePct slopeRatio
                    ptsList dM k nAutoGaps ratio zVal pIns segAng fullStr
                    stepEnts history lastStep e ans fillChoice )

  ;; =========================================================================
  ;; 1. DEFAULT SETTINGS (Auto-Units + Live Toggles)
  ;; =========================================================================
  (setq unitsMode    "AUTO"   ;; "AUTO" detects mm vs m from text height; or "MM" / "M"
        unitsPerM    1000.0   ;; Active CAD units per meter (1000.0 = mm, 1.0 = m)
        fillMode     "EVEN"   ;; "EVEN" = Uniform gaps <= stepDistM, "DIST" = Exact stepDistM, "GAPS" = Manual N gaps
        stepDistM    100.0    ;; Default spacing in meters (100m)
        numGaps      4        ;; Default number of equal gaps in "GAPS" mode
        drawSpot     "No"     ;; "Yes" = Draw cross/circle spot marker at exact point
        drawBox      "No"     ;; "Yes" = Draw centered rotated rectangular box around text
        boxPadFactor 0.35     ;; Box padding factor (x Text Height)
        alignToLine  "Yes")   ;; "Yes" = Align text along path, "No" = Match source text rotation

  (setq oldCmd      (getvar "CMDECHO")
        oldOs       (getvar "OSMODE")
        oldDimzin   (getvar "DIMZIN")
        undoStarted nil)
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (redraw)
    (if undoStarted
      (progn
        (if (boundp 'command-s)
          (vl-catch-all-apply '(lambda () (command-s "_.UNDO" "_End")))
          (vl-catch-all-apply '(lambda () (command "_.UNDO" "_End")))
        )
        (setq undoStarted nil)
      )
    )
    (if oldDimzin (setvar "DIMZIN" oldDimzin))
    (if oldOs     (setvar "OSMODE" oldOs))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EWFILL Error]: " msg))
    )
    (princ)
  )

  ;; =========================================================================
  ;; 2. PURE AUTOLISP MATH, STRING & FORMATTING HELPERS
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

  ;; Ceiling function for positive reals (e.g. 3.01 -> 4, 3.0 -> 3)
  (defun ef:ceil (x / n)
    (setq n (fix x))
    (if (> (- x (float n)) 1.0e-6) (1+ n) n)
  )

  ;; Strict 2-decimal formatter (27 -> "27.00", 27.5 -> "27.50")
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

  ;; Strips ALL MTEXT formatting codes anywhere in the string without truncating text
  ;; e.g. "{\W0.5;FFL}{\fArial; 27.80}" -> "FFL 27.80"
  (defun ef:strip-mtext-codes (raw / len i ch sec semi out)
    (if (or (null raw) (/= (type raw) 'STR))
      ""
      (progn
        (setq len (strlen raw) i 1 out "")
        (while (<= i len)
          (setq ch  (substr raw i 1)
                sec (if (< i len) (substr raw (1+ i) 1) ""))
          (cond
            ((or (= ch "{") (= ch "}"))
             (setq i (1+ i)))
            ((and (= ch "\\") (= sec "P"))
             (setq out (strcat out " ") i (+ i 2)))
            ((and (= ch "\\") (= sec "~"))
             (setq out (strcat out " ") i (+ i 2)))
            ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p" "S")))
             (setq semi (vl-string-search ";" raw (1- i)))
             (if semi
               (setq i (+ semi 2))
               (setq i (+ i 2))
             ))
            ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k" "\\" "{" "}")))
             (if (member sec '("\\" "{" "}"))
               (setq out (strcat out sec))
             )
             (setq i (+ i 2)))
            (T
             (setq out (strcat out ch) i (1+ i)))
          )
        )
        (ef:trim out)
      )
    )
  )

  ;; Extracts inline \W0.85; width factor from MTEXT if present
  (defun extract-mtext-width (raw / p semi numStr w)
    (setq w nil)
    (if (and raw (= (type raw) 'STR))
      (progn
        (setq p (vl-string-search "\\W" (strcase raw)))
        (if p
          (progn
            (setq semi (vl-string-search ";" raw p))
            (if (and semi (> semi (+ p 2)))
              (progn
                (setq numStr (substr raw (+ p 3) (- semi (+ p 2)))
                      w      (distof numStr))
                (if (or (null w) (<= w 0.05) (> w 5.0)) (setq w nil))
              )
            )
          )
        )
      )
    )
    w
  )

  ;; =========================================================================
  ;; 3. 3-LAYER TEXT READER, LEVEL PARSER & TRUE PROPERTY EXTRACTOR
  ;; =========================================================================
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

  ;; Parses "FFL27.80m", "RFL 27.30", "+15.20", "{\W0.5;PL 12.50}" -> (list 27.80 "FFL" "m")
  (defun parse-level-str (raw / vis len i numStart numEnd ch sec numStr hasDot nextCh pre suf val)
    (setq vis (ef:strip-mtext-codes raw)
          len (strlen vis)
          i   1
          numStart nil)
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
              i      (1+ numStart)
              numEnd numStart)
        (while (<= i len)
          (setq nextCh (substr vis i 1))
          (cond
            ((and (>= (ascii nextCh) 48) (<= (ascii nextCh) 57))
             (setq numStr (strcat numStr nextCh)
                   numEnd i
                   i      (1+ i)))
            ((and (= nextCh ".") (not hasDot) (< i len)
                  (>= (ascii (substr vis (1+ i) 1)) 48)
                  (<= (ascii (substr vis (1+ i) 1)) 57))
             (setq hasDot T
                   numStr (strcat numStr nextCh)
                   numEnd i
                   i      (1+ i)))
            (T
             (setq i (1+ len)))
          )
        )
        (setq val (distof numStr))
        (if (null val)
          nil
          (progn
            (setq pre (if (> numStart 1) (substr vis 1 (1- numStart)) "")
                  suf (if (< numEnd len) (substr vis (1+ numEnd)) ""))
            (if (and (= (substr numStr 1 1) "+") (not (wcmatch pre "*+")))
              (setq pre (strcat pre "+"))
            )
            (list val pre suf)
          )
        )
      )
    )
  )

  ;; True visual center in 2D WCS for TEXT, MTEXT, MULTILEADER, ATTRIB
  (defun get-entity-center-wcs (ent / ed typ vla pMin pMax pt p10 wcsPt)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed))
          pt  nil)

    ;; Method 1: ActiveX Bounding Box (Works accurately for TEXT, MTEXT, MULTILEADER, ATTRIB)
    (setq *ewf_min* nil *ewf_max* nil)
    (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and vla (not (vl-catch-all-error-p vla))
             (not (vl-catch-all-error-p
                    (vl-catch-all-apply '(lambda () (vla-getboundingbox vla '*ewf_min* '*ewf_max*)))))
             *ewf_min* *ewf_max*)
      (progn
        (setq pMin (vl-catch-all-apply 'vlax-safearray->list (list *ewf_min*))
              pMax (vl-catch-all-apply 'vlax-safearray->list (list *ewf_max*)))
        (if (and (listp pMin) (listp pMax))
          (setq pt (list (/ (+ (car pMin) (car pMax)) 2.0)
                         (/ (+ (cadr pMin) (cadr pMax)) 2.0)
                         0.0))
        )
      )
    )

    ;; Method 2: Pure DXF Fallback
    (if (null pt)
      (cond
        ((= typ "MULTILEADER")
         (setq p10 (cond ((cdr (assoc 12 ed))) ((cdr (assoc 10 ed))) ('(0.0 0.0 0.0))))
         (setq pt (list (car p10) (cadr p10) 0.0)))
        ((member typ '("TEXT" "ATTRIB"))
         (setq p10 (if (and (assoc 11 ed)
                            (or (and (assoc 72 ed) (> (cdr (assoc 72 ed)) 0))
                                (and (assoc 73 ed) (> (cdr (assoc 73 ed)) 0))))
                     (cdr (assoc 11 ed))
                     (cdr (assoc 10 ed))))
         (setq wcsPt (trans p10 ent 0)
               pt    (list (car wcsPt) (cadr wcsPt) 0.0)))
        (T
         (setq p10 (if (assoc 10 ed) (cdr (assoc 10 ed)) '(0.0 0.0 0.0))
               pt  (list (car p10) (cadr p10) 0.0)))
      )
    )
    pt
  )

  ;; Safely extracts (list lay sty hgt wid col rot) from TEXT, MTEXT, MULTILEADER, or ATTRIB
  ;; CRITICAL FIX: Never uses MTEXT DXF 41 (paragraph width, often 0.0!) as TEXT width factor!
  (defun get-text-props (ent rawTxt / ed typ lay sty hgt wid col rot styRec styWid inlineW)
    (if (null ent)
      (list (getvar "CLAYER")
            (getvar "TEXTSTYLE")
            (if (> (getvar "TEXTSIZE") 0.0) (getvar "TEXTSIZE") (* 2.0 unitsPerM))
            0.85
            nil
            0.0)
      (progn
        (setq ed  (entget ent)
              typ (cdr (assoc 0 ed))
              lay (cdr (assoc 8 ed))
              col (if (assoc 62 ed) (cdr (assoc 62 ed)) nil))

        ;; Text Style
        (setq sty (cond
                    ((and (= typ "MULTILEADER") (assoc 340 ed))
                     (getvar "TEXTSTYLE"))
                    ((and (assoc 7 ed) (tblsearch "STYLE" (cdr (assoc 7 ed))))
                     (cdr (assoc 7 ed)))
                    (T (getvar "TEXTSTYLE"))))

        ;; Text Height (MULTILEADER uses DXF 41/140; TEXT/MTEXT use DXF 40)
        (setq hgt (cond
                    ((= typ "MULTILEADER")
                     (cond
                       ((and (assoc 41 ed) (> (cdr (assoc 41 ed)) 0.0)) (cdr (assoc 41 ed)))
                       ((and (assoc 140 ed) (> (cdr (assoc 140 ed)) 0.0)) (cdr (assoc 140 ed)))
                       (T (* 2.0 unitsPerM))))
                    ((and (assoc 40 ed) (> (cdr (assoc 40 ed)) 0.0))
                     (cdr (assoc 40 ed)))
                    (T (* 2.0 unitsPerM))))

        ;; Width Factor (ONLY DXF 41 on TEXT/ATTRIB; for MTEXT extract \W...; or style DXF 41)
        (setq styRec (tblsearch "STYLE" sty)
              styWid (if (and styRec (assoc 41 styRec) (> (cdr (assoc 41 styRec)) 0.0))
                       (cdr (assoc 41 styRec))
                       0.85))
        (setq wid (cond
                    ((member typ '("TEXT" "ATTRIB"))
                     (if (and (assoc 41 ed) (> (cdr (assoc 41 ed)) 0.05) (<= (cdr (assoc 41 ed)) 5.0))
                       (cdr (assoc 41 ed))
                       styWid))
                    (T
                     (setq inlineW (extract-mtext-width rawTxt))
                     (if inlineW inlineW styWid))))
        (if (or (null wid) (<= wid 0.05)) (setq wid 0.85))

        ;; Source Rotation
        (setq rot (cond
                    ((= typ "MTEXT")
                     (if (assoc 11 ed)
                       (angle '(0.0 0.0 0.0) (cdr (assoc 11 ed)))
                       (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)))
                    ((assoc 50 ed) (cdr (assoc 50 ed)))
                    (T 0.0)))

        (list lay sty hgt wid col rot)
      )
    )
  )

  ;; Mathematically centered rotated rectangular LWPOLYLINE box around `pCenter`
  (defun draw-centered-box (pCenter str sty hgt wid rot lay col padFactor /
                            tb bMin bMax halfW halfH ux uy vx vy c1 c2 c3 c4 dxfBox)
    (setq tb (textbox (list '(0 . "TEXT")
                            (cons 1 str)
                            (cons 7 sty)
                            (cons 40 hgt)
                            (cons 41 wid))))
    (if tb
      (setq bMin  (car tb)
            bMax  (cadr tb)
            halfW (+ (/ (- (car bMax) (car bMin)) 2.0) (* hgt padFactor))
            halfH (+ (/ (- (cadr bMax) (cadr bMin)) 2.0) (* hgt padFactor)))
      (setq halfW (+ (* (strlen str) hgt wid 0.38) (* hgt padFactor))
            halfH (+ (* hgt 0.5) (* hgt padFactor)))
    )
    (setq ux (cos rot) uy (sin rot)
          vx (- (sin rot)) vy (cos rot))
    (setq c1 (list (+ (car pCenter) (* (- halfW) ux) (* (- halfH) vx))
                   (+ (cadr pCenter) (* (- halfW) uy) (* (- halfH) vy)))
          c2 (list (+ (car pCenter) (* halfW ux)     (* (- halfH) vx))
                   (+ (cadr pCenter) (* halfW uy)     (* (- halfH) vy)))
          c3 (list (+ (car pCenter) (* halfW ux)     (* halfH vx))
                   (+ (cadr pCenter) (* halfW uy)     (* halfH vy)))
          c4 (list (+ (car pCenter) (* (- halfW) ux) (* halfH vx))
                   (+ (cadr pCenter) (* (- halfW) uy) (* halfH vy))))
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

  ;; =========================================================================
  ;; 4. POLYLINE PATH GEOMETRY ENGINE (Optional Curved Alignment Mode)
  ;; =========================================================================
  (defun get-poly-pts-wcs (ent pStartWcs / ed typ pts d wcsPt sub subEd)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed))
          pts nil)
    (cond
      ((= typ "LINE")
       (setq pts (list (list (car (cdr (assoc 10 ed))) (cadr (cdr (assoc 10 ed))) 0.0)
                       (list (car (cdr (assoc 11 ed))) (cadr (cdr (assoc 11 ed))) 0.0))))
      ((= typ "LWPOLYLINE")
       (foreach d ed
         (if (= (car d) 10)
           (progn
             (setq wcsPt (trans (list (cadr d) (caddr d) 0.0) ent 0))
             (setq pts (append pts (list (list (car wcsPt) (cadr wcsPt) 0.0))))
           )
         )
       ))
      ((= typ "POLYLINE")
       (setq sub (entnext ent))
       (while (and sub (/= (cdr (assoc 0 (setq subEd (entget sub)))) "SEQEND"))
         (if (= (cdr (assoc 0 subEd)) "VERTEX")
           (progn
             (setq wcsPt (trans (cdr (assoc 10 subEd)) sub 0))
             (setq pts (append pts (list (list (car wcsPt) (cadr wcsPt) 0.0))))
           )
         )
         (setq sub (entnext sub))
       ))
    )
    (if (and (>= (length pts) 2) pStartWcs)
      (if (< (distance pStartWcs (last pts)) (distance pStartWcs (car pts)))
        (setq pts (reverse pts))
      )
    )
    pts
  )

  (defun get-poly-total-len (pts / idx tot)
    (setq idx 0 tot 0.0)
    (while (< idx (1- (length pts)))
      (setq tot (+ tot (distance (nth idx pts) (nth (1+ idx) pts)))
            idx (1+ idx))
    )
    tot
  )

  (defun get-poly-pt-and-ang-at-dist (pts targetDistUnits / idx cum dSeg pA pB res)
    (setq idx 0 cum 0.0 res nil)
    (while (and (< idx (1- (length pts))) (null res))
      (setq pA   (nth idx pts)
            pB   (nth (1+ idx) pts)
            dSeg (distance pA pB))
      (if (<= targetDistUnits (+ cum dSeg 1.0e-6))
        (setq res (list (polar pA (angle pA pB) (max 0.0 (- targetDistUnits cum)))
                        (angle pA pB)))
        (setq cum (+ cum dSeg))
      )
      (setq idx (1+ idx))
    )
    (if res res (list (last pts) (angle (nth (- (length pts) 2) pts) (last pts))))
  )

  ;; =========================================================================
  ;; 5. SMART LEVEL / POINT PICKER (Supports TEXT, MTEXT, MLEADER, ATTRIB & Point)
  ;; =========================================================================
  (defun pick-level-or-point (tagPrompt defaultPrefix allowOpts / done sel errCode e pt subEnt ssPick raw parsed zVal wcsPt)
    (setq done nil)
    (while (not done)
      (setvar "ERRNO" 0)
      (if allowOpts
        (initget "Point Units Box Spot Align Undo Exit")
        (initget "Point Exit")
      )
      (setq sel (entsel tagPrompt))
      (setq errCode (getvar "ERRNO"))

      (cond
        ((or (= sel "Exit") (and (null sel) (= errCode 52)))
         (setq done "EXIT"))

        ((member sel '("Units" "Box" "Spot" "Align" "Undo"))
         (setq done sel))

        ((= sel "Point")
         (setvar "OSMODE" oldOs)
         (setq pt (getpoint "\n>> Pick exact coordinate point: "))
         (if pt
           (progn
             (setq zVal (getreal "\n>> Enter Proposed Level at this point (e.g. 27.80): "))
             (if zVal
               (progn
                 (setq wcsPt (trans pt 1 0))
                 (setq done (list (list (car wcsPt) (cadr wcsPt) 0.0) zVal defaultPrefix "" nil ""))
               )
             )
           )
         ))

        ((and (null sel) (= errCode 7))
         (princ "\n>> Missed object. Click directly on a level text, or type [P] for manual Point."))

        ((listp sel)
         (setq e  (car sel)
               pt (cadr sel))

         ;; If Block (INSERT), check nested ATTRIB/TEXT via nentselp
         (if (= (cdr (assoc 0 (entget e))) "INSERT")
           (if (setq subEnt (nentselp pt))
             (if (member (cdr (assoc 0 (entget (car subEnt)))) '("ATTRIB" "TEXT" "MTEXT"))
               (setq e (car subEnt))
             )
           )
         )

         ;; Click-Through fallback if user clicked a line/box overlapping the text
         (if (not (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB")))
           (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT,MULTILEADER"))))
             (setq e (ssname ssPick 0))
           )
         )

         (if (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB"))
           (progn
             (setq raw    (get-text-raw e)
                   parsed (parse-level-str raw))
             (if parsed
               (progn
                 (setq wcsPt (get-entity-center-wcs e))
                 (princ (strcat "\n>> Picked Level: "
                                (nth 1 parsed) (ef:fmt2dec (nth 0 parsed)) (nth 2 parsed)))
                 (setq done (list wcsPt (nth 0 parsed) (nth 1 parsed) (nth 2 parsed) e raw))
               )
               (princ (strcat "\n>> No numeric level found in \"" raw "\". Click a valid level text or type [P] for Point."))
             )
           )
           (princ (strcat "\n>> Clicked a " (cdr (assoc 0 (entget e)))
                          ", not a level text. Click the level text or type [P] for manual Point."))
         ))
      )
    )
    done
  )

  ;; Interactive post-pick prompt: shows exact distance & lets user confirm, type a step, or switch mode
  (defun ask-fill-method (totalM / defLabel ans nVal)
    (setq defLabel
      (cond
        ((= fillMode "EVEN") (strcat "Even <= " (rtos stepDistM 2 1) "m [" (itoa (max 1 (ef:ceil (/ totalM stepDistM)))) " gaps]"))
        ((= fillMode "DIST") (strcat "Exact every " (rtos stepDistM 2 1) "m"))
        (T                   (strcat (itoa numGaps) " Equal Gaps (" (ef:fmt2dec (/ totalM (float numGaps))) "m each)"))
      )
    )
    (initget 6 "Even Distance Gaps Poly Cancel")
    (setq ans (getreal (strcat "\nFill method [Even/Distance/Gaps/Poly/Cancel] or type Interval in meters <" defLabel ">: ")))
    (cond
      ((= ans "Cancel") nil)
      ((= ans "Poly") "POLY")
      ((numberp ans)
       (setq stepDistM ans)
       (if (= fillMode "GAPS") (setq fillMode "EVEN"))
       T)
      ((= ans "Even")
       (setq fillMode "EVEN")
       (initget 6)
       (setq nVal (getreal (strcat "\nMax gap spacing in meters (auto-splits evenly) <" (rtos stepDistM 2 1) ">: ")))
       (if nVal (setq stepDistM nVal))
       T)
      ((= ans "Distance")
       (setq fillMode "DIST")
       (initget 6)
       (setq nVal (getreal (strcat "\nExact fixed interval in meters (e.g. 100, 50, 25) <" (rtos stepDistM 2 1) ">: ")))
       (if nVal (setq stepDistM nVal))
       T)
      ((= ans "Gaps")
       (setq fillMode "GAPS")
       (initget 6)
       (setq nVal (getint (strcat "\nNumber of equal gaps between the 2 levels (e.g. 4 gaps = 3 levels) <" (itoa numGaps) ">: ")))
       (if nVal (setq numGaps nVal))
       T)
      (T T)
    )
  )

  ;; =========================================================================
  ;; 6. MAIN INTERACTIVE LOOP
  ;; =========================================================================
  (princ "\n==================================================================")
  (princ "\n  EWFILL v2.0 : Earthwork Proposed Level Gap Filler & Interpolator")
  (princ "\n  1. Click Start Level Text (or type P for manual Point)")
  (princ "\n  2. Click End Level Text   (or type P for manual Point)")
  (princ "\n  3. Confirm Interval / Equal Gaps (or follow a curved Polyline)")
  (princ "\n==================================================================")

  (setq history nil loop T preStr "FFL " sufStr "")

  (while loop
    (setq p1Data
      (pick-level-or-point
        (strcat "\n[1/2] Click Start Proposed Level Text [Point/Units/Box/Spot/Align/Undo] <Exit> "
                "(Units:" unitsMode
                ", Box:" drawBox
                ", Spot:" drawSpot "): ")
        preStr
        T))

    (cond
      ((or (null p1Data) (= p1Data "EXIT"))
       (setq loop nil))

      ((= p1Data "Units")
       (initget "Auto Millimeters Meters")
       (setq ans (getkword "\nDrawing units scale [Auto / Millimeters (1m=1000) / Meters (1m=1)] <Auto>: "))
       (cond
         ((= ans "Meters")      (setq unitsMode "M"    unitsPerM 1.0))
         ((= ans "Millimeters") (setq unitsMode "MM"   unitsPerM 1000.0))
         (T                     (setq unitsMode "AUTO" unitsPerM 1000.0))
       )
       (princ (strcat "\n>> Units mode set to: " unitsMode)))

      ((= p1Data "Box")
       (setq drawBox (if (= drawBox "Yes") "No" "Yes"))
       (princ (strcat "\n>> Draw centered rotated box around generated levels: " drawBox)))

      ((= p1Data "Spot")
       (setq drawSpot (if (= drawSpot "Yes") "No" "Yes"))
       (princ (strcat "\n>> Draw spot cross/circle marker at each point: " drawSpot)))

      ((= p1Data "Align")
       (setq alignToLine (if (= alignToLine "Yes") "No" "Yes"))
       (princ (strcat "\n>> Align text rotation along path direction: " alignToLine)))

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
                   ") [Point] <Cancel>: ")
           preStr
           nil))

       (if (and (listp p2Data) (= (length p2Data) 6))
         (progn
           (setq pt2 (nth 0 p2Data)
                 z2  (nth 1 p2Data))
           (if (and (null refEnt) (nth 4 p2Data))
             (setq refEnt (nth 4 p2Data)
                   preStr (nth 2 p2Data)
                   sufStr (nth 3 p2Data))
           )

           ;; Extract safe style/height/width factor from reference text
           (setq props  (get-text-props refEnt (if (nth 4 p1Data) (nth 5 p1Data) (nth 5 p2Data)))
                 txtLay (nth 0 props)
                 txtSty (nth 1 props)
                 txtHgt (nth 2 props)
                 txtWid (nth 3 props)
                 txtCol (nth 4 props)
                 srcAng (nth 5 props))

           ;; Auto-detect Millimeters vs Meters from text height (e.g. hgt=1500 -> mm, hgt=2.0 -> m)
           (if (= unitsMode "AUTO")
             (if (< txtHgt 50.0)
               (setq unitsPerM 1.0)
               (setq unitsPerM 1000.0)
             )
           )

           (setq usePoly    nil
                 polyPts    nil
                 totalUnits (distance pt1 pt2)
                 totalM     (/ totalUnits unitsPerM))

           (if (< totalUnits 1.0e-4)
             (princ "\n>> Start and End points are identical. Cancelled.")
             (progn
               ;; Visual rubber-band preview line between Start & End
               (grdraw (trans pt1 0 1) (trans pt2 0 1) 3 1)

               (setq slopePct   (* (/ (- z2 z1) totalM) 100.0)
                     slopeRatio (if (> (abs (- z2 z1)) 1.0e-5)
                                  (strcat "1:" (rtos (abs (/ totalM (- z2 z1))) 2 0))
                                  "FLAT"))

               (princ (strcat "\n>> Span: " (ef:fmt2dec totalM) "m"
                              " (" (if (= unitsPerM 1000.0) "mm" "m") " drawing)"
                              " | " preStr (ef:fmt2dec z1) sufStr " -> " preStr (ef:fmt2dec z2) sufStr
                              " | dZ = " (if (>= (- z2 z1) 0.0) "+" "") (ef:fmt2dec (- z2 z1)) "m"
                              " (" (ef:fmt2dec slopePct) "% / " slopeRatio ")"))

               ;; Ask how to fill the gap (or press Enter for quick default, or P for Polyline)
               (setq fillChoice (ask-fill-method totalM))

               ;; If user chose Polyline path, pick alignment polyline and re-measure totalM
               (if (= fillChoice "POLY")
                 (progn
                   (setq polySel (entsel "\n>> Select Road/Earthwork Polyline or Line to follow: "))
                   (if (and polySel (member (cdr (assoc 0 (entget (car polySel)))) '("LWPOLYLINE" "POLYLINE" "LINE")))
                     (progn
                       (setq polyEnt    (car polySel)
                             polyPts    (get-poly-pts-wcs polyEnt pt1)
                             totalUnits (get-poly-total-len polyPts)
                             totalM     (/ totalUnits unitsPerM)
                             usePoly    T)
                       (princ (strcat "\n>> Polyline Length: " (ef:fmt2dec totalM) "m."))
                       (setq fillChoice (ask-fill-method totalM))
                     )
                     (progn
                       (princ "\n>> Invalid polyline selected. Cancelled.")
                       (setq fillChoice nil)
                     )
                   )
                 )
               )

               (redraw)

               (if fillChoice
                 (progn
                   ;; Build list of intermediate distances (in meters)
                   (setq ptsList nil)
                   (cond
                     ;; Mode 1: Auto-Even Gaps (automatically divides span into uniform gaps <= stepDistM)
                     ((= fillMode "EVEN")
                      (setq nAutoGaps (ef:ceil (/ totalM stepDistM)))
                      (if (> nAutoGaps 1)
                        (progn
                          (setq k 1)
                          (while (< k nAutoGaps)
                            (setq dM      (* totalM (/ (float k) (float nAutoGaps)))
                                  ptsList (append ptsList (list dM))
                                  k       (1+ k))
                          )
                        )
                      ))

                     ;; Mode 2: Exact Fixed Interval (e.g. 100m, 200m, 300m...)
                     ((= fillMode "DIST")
                      (setq dM stepDistM)
                      (while (< dM (- totalM (* stepDistM 0.05)))
                        (setq ptsList (append ptsList (list dM))
                              dM      (+ dM stepDistM))
                      ))

                     ;; Mode 3: Manual Number of Equal Gaps
                     ((= fillMode "GAPS")
                      (setq k 1)
                      (while (< k numGaps)
                        (setq dM      (* totalM (/ (float k) (float numGaps)))
                              ptsList (append ptsList (list dM))
                              k       (1+ k))
                      ))
                   )

                   (if (null ptsList)
                     (princ (strcat "\n>> Total distance (" (ef:fmt2dec totalM)
                                    "m) is shorter than " (ef:fmt2dec stepDistM)
                                    "m. Type a smaller interval or use [Gaps] (and check [Units] mm vs m)."))
                     (progn
                       (command "_.UNDO" "_Begin")
                       (setq undoStarted T
                             stepEnts    nil
                             ang         (angle pt1 pt2))

                       (foreach dM ptsList
                         (setq ratio   (/ dM totalM)
                               zVal    (+ z1 (* (- z2 z1) ratio))
                               fullStr (strcat preStr (ef:fmt2dec zVal) sufStr))

                         (if usePoly
                           (progn
                             (setq ans    (get-poly-pt-and-ang-at-dist polyPts (* dM unitsPerM))
                                   pIns   (car ans)
                                   segAng (cadr ans))
                           )
                           (setq pIns   (polar pt1 ang (* dM unitsPerM))
                                 segAng ang)
                         )

                         (if (= alignToLine "Yes")
                           (setq txtAng (if (and (> segAng (+ (/ pi 2.0) 1.0e-9)) (<= segAng (+ (* 1.5 pi) 1.0e-9)))
                                          (- segAng pi)
                                          segAng))
                           (setq txtAng srcAng)
                         )

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
                               (draw-centered-box pIns fullStr txtSty txtHgt txtWid txtAng txtLay txtCol boxPadFactor)
                             )
                           )
                         )
                       )

                       (command "_.UNDO" "_End")
                       (setq undoStarted nil
                             history     (cons stepEnts history))

                       (princ (strcat "\n>> Filled " (itoa (length ptsList))
                                      " intermediate level(s) across " (ef:fmt2dec totalM) "m"
                                      " (spacing: " (ef:fmt2dec (car ptsList)) "m)"
                                      " | dZ = " (if (>= (- z2 z1) 0.0) "+" "") (ef:fmt2dec (- z2 z1)) "m."))
                     )
                   )
                 )
               )
             )
           )
         )
       ))
    )
  )

  (redraw)
  (if oldDimzin (setvar "DIMZIN" oldDimzin))
  (if oldOs     (setvar "OSMODE" oldOs))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Command Aliases
(defun c:lvlfill ()   (c:ewfill))
(defun c:gapfill ()   (c:ewfill))
(defun c:interplvl () (c:ewfill))
(princ "\nEWFILL v2.0 loaded. Type EWFILL, LVLFILL, GAPFILL, or INTERPLVL to run.")
(princ)