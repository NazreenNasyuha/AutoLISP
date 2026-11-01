;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : roadsec.lsp
;;; COMMAND     : ROADSEC (Aliases: RSEC, ROADLONG, PLINESEC)
;;; DESCRIPTION : Automated Road Longitudinal Section Generator from a
;;;               Plan-View Polyline & Proposed Level Texts in the Drawing.
;;;               - Auto-detects FFL / RFL / RL / PL texts along the Polyline
;;;               - Auto-interpolates regular chainage intervals (e.g. 25m/50m/100m)
;;;               - Draws JRK-standard Profile & Table (2-decimal 0.00 format)
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:roadsec ( / *error* oldCmd oldOs oldDimzin
                     ;; --- settings & state ---
                     unitsPerM hScale vScale searchCorridorM stepIntervalM
                     includeTextStns autoDatum baseDatumUnder10 manualDatum
                     ;; --- helper functions ---
                     rs:esc rs:trim rs:fmt2dec
                     ensure-layer ensure-textstyle mk-line mk-text mk-mtext
                     get-text-raw parse-level-str get-text-center-wcs
                     get-poly-pts-wcs project-pt-to-poly sort-nodes-by-ch
                     interp-z-at-ch build-station-list compute-datum
                     draw-road-section
                     ;; --- working variables ---
                     loop polySel polyEnt pickPtWcs polyPts totalLenM
                     refSel refEnt refLay corridorUnits
                     ssTexts i e ptCen projRes offDistM chM parsed zVal lblTxt
                     rawNodes sortedNodes stnList datumVal ptIns ans )

  ;; =========================================================================
  ;; 1. DEFAULT SETTINGS (Adjustable Live via Command Options)
  ;; =========================================================================
  (setq unitsPerM        1000.0  ;; 1000.0 = Millimeter drawing (1m = 1000 units), 1.0 = Meter drawing
        hScale           1000.0  ;; Horizontal Scale 1:1000
        vScale           200.0   ;; Vertical Scale 1:200
        searchCorridorM  25.0    ;; Max perpendicular search distance (m) from polyline to grab level texts
        stepIntervalM    25.0    ;; Regular chainage interpolation step in meters (e.g. 20m, 25m, 50m, 100m; 0 = Off)
        includeTextStns  "Yes"   ;; "Yes" = Include exact chainages where level texts sit + regular intervals
        autoDatum        "Yes"   ;; "Yes" = Auto +10m Datum step rule
        baseDatumUnder10 0.0     ;; Base datum if min level < 10m
        manualDatum      0.0)    ;; Manual datum if autoDatum = "No"

  (setq oldCmd    (getvar "CMDECHO")
        oldOs     (getvar "OSMODE")
        oldDimzin (getvar "DIMZIN"))
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0) ;; Prevents AutoCAD from suppressing trailing ".00" zeros

  (defun *error* (msg)
    (if oldDimzin (setvar "DIMZIN" oldDimzin))
    (if oldOs     (setvar "OSMODE" oldOs))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[ROADSEC Error]: " msg))
    )
    (princ)
  )

  ;; =========================================================================
  ;; 2. PURE AUTOLISP HELPERS (AutoCAD LT 2024+ & "#JRK" Layer Safe)
  ;; =========================================================================
  (defun rs:esc (str / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen str))
      (setq ch (substr str k 1))
      (setq out (strcat out (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ",")) "`" "") ch))
      (setq k (1+ k))
    )
    out
  )

  (defun rs:trim (s / len i1 i2)
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

  ;; Strict 2-decimal formatter (0 -> "0.00", 27.8 -> "27.80")
  (defun rs:fmt2dec (val / s dotPos decLen)
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

  ;; =========================================================================
  ;; 3. CAD LAYER, STYLE & ENTITY CREATION HELPERS
  ;; =========================================================================
  (defun ensure-layer (nm col ltype)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER")
                       '(100 . "AcDbSymbolTableRecord")
                       '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm)
                       '(70 . 0)
                       (cons 62 col)
                       (cons 6 ltype)))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")
        )
      )
    )
  )

  (defun ensure-textstyle (nm wid)
    (if (not (tblsearch "STYLE" nm))
      (entmake (list '(0 . "STYLE")
                     '(100 . "AcDbSymbolTableRecord")
                     '(100 . "AcDbTextStyleTableRecord")
                     (cons 2 nm)
                     '(70 . 0)
                     '(40 . 0.0)
                     (cons 41 wid)
                     '(50 . 0.0)
                     '(71 . 0)
                     '(3 . "romans.shx")
                     '(4 . "")))
    )
  )

  (defun mk-line (lay p1 p2 / )
    (entmake (list '(0 . "LINE")
                   '(100 . "AcDbEntity")
                   (cons 8 lay)
                   '(100 . "AcDbLine")
                   (list 10 (car p1) (cadr p1) 0.0)
                   (list 11 (car p2) (cadr p2) 0.0)))
  )

  (defun mk-text (lay sty str pt hgt rot wid hJust vJust / data)
    (setq data (list '(0 . "TEXT")
                     '(100 . "AcDbEntity")
                     (cons 8 lay)
                     '(100 . "AcDbText")
                     (list 10 (car pt) (cadr pt) 0.0)
                     (cons 40 hgt)
                     (cons 1 str)
                     (cons 50 rot)
                     (cons 41 wid)
                     (cons 7 sty)
                     (cons 72 hJust)))
    (if (or (/= hJust 0) (/= vJust 0))
      (setq data (append data (list (list 11 (car pt) (cadr pt) 0.0))))
    )
    (setq data (append data (list '(100 . "AcDbText") (cons 73 vJust))))
    (entmake data)
  )

  (defun mk-mtext (lay sty str pt hgt attach / )
    (entmake (list '(0 . "MTEXT")
                   '(100 . "AcDbEntity")
                   (cons 8 lay)
                   '(100 . "AcDbMText")
                   (list 10 (car pt) (cadr pt) 0.0)
                   (cons 40 hgt)
                   '(41 . 0.0)
                   (cons 71 attach)
                   '(72 . 1)
                   (cons 1 str)
                   (cons 7 sty)
                   '(50 . 0.0)
                   '(11 1.0 0.0 0.0)
                   '(73 . 1)
                   '(44 . 1.0)))
  )

  ;; =========================================================================
  ;; 4. TEXT LEVEL PARSER & TRUE VISUAL CENTER
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

  ;; Parses level text like "FFL27.80", "RFL 27.30", "RL14.50", "+12.40"
  ;; Ignores pipe/drain labels like "600%%c" or "1:200"
  (defun parse-level-str (raw / len i loopP ch sec semi vis pBrace uVis
                                numStart numStr hasDot nextCh val)
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
    (setq vis  (rs:trim vis)
          uVis (strcase vis))

    ;; Skip obvious pipe/gradient/chainage strings that are not spot levels
    (if (or (= vis "")
            (wcmatch uVis "*%%C*,*1:#*,*CH *,*CH.*,*{*")
            (and (wcmatch uVis "*M") (not (wcmatch uVis "*FFL*,*RFL*,*RL*,*PL*,*GL*,*IL*,*LVL*"))))
      nil
      (progn
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
            (setq val (distof numStr))
            ;; Require either a decimal point (e.g. 27.80) or a known level prefix (FFL/RFL/RL/PL/GL)
            (if (and val (or hasDot (wcmatch uVis "*FFL*,*RFL*,*RL*,*PL*,*GL*,*IL*,*LVL*")))
              (list val vis)
              nil
            )
          )
        )
      )
    )
  )

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

  ;; =========================================================================
  ;; 5. PURE DXF POLYLINE EXTRACTOR & PERPENDICULAR CHAINAGE PROJECTOR
  ;;    Works on LINE, LWPOLYLINE, and 2D/3D POLYLINE in all CAD versions
  ;; =========================================================================
  (defun get-poly-pts-wcs (ent pickPtWcs / ed typ pts d wcsPt sub subEd)
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
    ;; Auto-orient Chainage 0.00 to the end closest to where the user clicked the polyline
    (if (and (>= (length pts) 2) pickPtWcs)
      (if (< (distance pickPtWcs (last pts)) (distance pickPtWcs (car pts)))
        (progn
          (princ "\n>> Clicked nearer to end of polyline: Chainage 0.00m set to clicked end.")
          (setq pts (reverse pts))
        )
      )
    )
    pts
  )

  ;; Projects 2D WCS point P onto polyline vertex list `polyPts`
  ;; Returns (list minPerpDistUnits bestChainageUnits)
  (defun project-pt-to-poly (pt polyPts / idx cumDist minDist bestCh pA pB dx dy segLenSq tVal projPt dPerp)
    (setq idx     0
          cumDist 0.0
          minDist 1.0e99
          bestCh  0.0)
    (while (< idx (1- (length polyPts)))
      (setq pA       (nth idx polyPts)
            pB       (nth (1+ idx) polyPts)
            dx       (- (car pB) (car pA))
            dy       (- (cadr pB) (cadr pA))
            segLenSq (+ (* dx dx) (* dy dy)))
      (if (> segLenSq 1.0e-8)
        (progn
          (setq tVal (/ (+ (* (- (car pt) (car pA)) dx)
                           (* (- (cadr pt) (cadr pA)) dy))
                        segLenSq))
          (setq tVal   (max 0.0 (min 1.0 tVal))
                projPt (list (+ (car pA) (* tVal dx))
                             (+ (cadr pA) (* tVal dy))
                             0.0)
                dPerp  (distance (list (car pt) (cadr pt) 0.0) projPt))
          (if (< dPerp minDist)
            (setq minDist dPerp
                  bestCh  (+ cumDist (* tVal (sqrt segLenSq))))
          )
          (setq cumDist (+ cumDist (sqrt segLenSq)))
        )
      )
      (setq idx (1+ idx))
    )
    (list minDist bestCh)
  )

  ;; Sorts node list ((chM zVal lbl offDist) ...) by chainage ascending & merges duplicates within 0.25m
  (defun sort-nodes-by-ch (nodes / sorted item out lastItem)
    (setq sorted
      (vl-sort nodes
        '(lambda (a b) (< (car a) (car b)))
      )
    )
    (setq out nil)
    (foreach item sorted
      (if (null out)
        (setq out (list item))
        (progn
          (setq lastItem (last out))
          ;; If two texts project to nearly the exact same chainage (< 0.25m), keep the one closer to the polyline
          (if (< (abs (- (car item) (car lastItem))) 0.25)
            (if (< (nth 3 item) (nth 3 lastItem))
              (setq out (append (reverse (cdr (reverse out))) (list item)))
            )
            (setq out (append out (list item)))
          )
        )
      )
    )
    out
  )

  ;; Linearly interpolates (or clamps) elevation Z at any chainage `chM` from `sortedNodes`
  (defun interp-z-at-ch (chM sortedNodes / n0 nLast idx found nA nB chA chB zA zB)
    (setq n0    (car sortedNodes)
          nLast (last sortedNodes))
    (cond
      ((<= chM (car n0))
       (cadr n0))
      ((>= chM (car nLast))
       (cadr nLast))
      (T
       (setq idx 0 found nil)
       (while (and (< idx (1- (length sortedNodes))) (not found))
         (setq nA  (nth idx sortedNodes)
               nB  (nth (1+ idx) sortedNodes)
               chA (car nA)
               chB (car nB)
               zA  (cadr nA)
               zB  (cadr nB))
         (if (and (>= chM (- chA 1.0e-6)) (<= chM (+ chB 1.0e-6)))
           (if (> (abs (- chB chA)) 1.0e-6)
             (setq found (+ zA (* (- zB zA) (/ (- chM chA) (- chB chA)))))
             (setq found zA)
           )
         )
         (setq idx (1+ idx))
       )
       (if found found (cadr nLast))
      )
    )
  )

  ;; Builds final station list `((chM zVal stnTag) ...)` from 0.00m to totalLenM
  ;; Combines regular interval steps (e.g. every 25m / 100m) + exact picked level text chainages
  (defun build-station-list (totalLenM sortedNodes stepM incText / chList d n uniqueChs out idx ch zVal)
    (setq chList (list 0.0 totalLenM))
    (if (> stepM 0.0)
      (progn
        (setq d stepM)
        (while (< d (- totalLenM 0.25))
          (setq chList (cons d chList)
                d      (+ d stepM))
        )
      )
    )
    (if (= incText "Yes")
      (foreach n sortedNodes
        (setq chList (cons (car n) chList))
      )
    )
    (setq chList (vl-sort chList '<)
          uniqueChs nil)
    (foreach ch chList
      (if (or (null uniqueChs) (> (- ch (last uniqueChs)) 0.25))
        (setq uniqueChs (append uniqueChs (list ch)))
      )
    )
    ;; Ensure the exact end chainage is always the final station
    (if (> (abs (- (last uniqueChs) totalLenM)) 0.01)
      (setq uniqueChs (append (reverse (cdr (reverse uniqueChs))) (list totalLenM)))
    )

    (setq out nil idx 1)
    (foreach ch uniqueChs
      (setq zVal (interp-z-at-ch ch sortedNodes))
      (setq out  (append out (list (list ch zVal (strcat "CH " (rs:fmt2dec ch))))))
      (setq idx  (1+ idx))
    )
    out
  )

  ;; Automatic +10m Datum Calculator (matches SEWSECTION standard)
  (defun compute-datum (stnList isAuto baseU10 manVal / minZ stn tenStep dVal)
    (if (= isAuto "No")
      manVal
      (progn
        (setq minZ 1.0e99)
        (foreach stn stnList
          (if (< (cadr stn) minZ) (setq minZ (cadr stn)))
        )
        (if (>= minZ 10.0)
          (progn
            (setq tenStep (* 10.0 (fix (/ minZ 10.0)))
                  dVal    (+ tenStep baseU10))
            (if (> dVal (- minZ 1.0))
              (setq dVal (max 0.0 (fix (- minZ 2.0))))
            )
            dVal
          )
          (progn
            (setq dVal baseU10)
            (if (> dVal (- minZ 1.0))
              (setq dVal (max 0.0 (fix (- minZ 2.0))))
            )
            dVal
          )
        )
      )
    )
  )

  ;; =========================================================================
  ;; 6. ROAD LONGITUDINAL SECTION GEOMETRY ENGINE (JRK Standard)
  ;; =========================================================================
  (defun draw-road-section (ptBase stnList hSc vSc datumVal /
                            x0 y0 hdrW rowH hMult vMult totalDist xStart xEnd
                            yLine1 yLine2 yLine3 yRow1 yRow2 yRow3
                            maxZ stn yTop idx lastIdx chM zVal stnTag xStn yZ xTxtRight
                            nextStn chNext zNext xNext yZNext segLenM dZ slopeStr xMid)

    (setq x0        (car ptBase)
          y0        (cadr ptBase)
          hdrW      37500.0
          rowH      5000.0
          hMult     (/ 1000000.0 hSc)   ;; 1:1000 -> 1000 CAD units per meter
          vMult     (/ 1000000.0 vSc)   ;; 1:200  -> 5000 CAD units per meter
          totalDist (car (last stnList))
          xStart    (+ x0 hdrW)
          xEnd      (+ xStart (* totalDist hMult))
          yLine1    (- y0 5000.0)
          yLine2    (- y0 10000.0)
          yLine3    (- y0 15000.0)
          yRow1     (- y0 2500.0)
          yRow2     (- y0 7500.0)
          yRow3     (- y0 12500.0)
          lastIdx   (1- (length stnList)))

    (setq maxZ -1.0e99)
    (foreach stn stnList
      (if (> (cadr stn) maxZ) (setq maxZ (cadr stn)))
    )
    (setq yTop (max (+ y0 45000.0)
                    (+ y0 (* (- maxZ datumVal) vMult) 12000.0)))

    ;; 1. Horizontal Table Lines trimmed cleanly at the last station line (xEnd)
    (mk-line "TITLE" (list x0 y0)     (list xEnd y0))
    (mk-line "TITLE" (list x0 yLine1) (list xEnd yLine1))
    (mk-line "TITLE" (list x0 yLine2) (list xEnd yLine2))
    (mk-line "TITLE" (list x0 yLine3) (list xEnd yLine3))

    ;; 2. Left Header Labels
    (mk-text "TITLE" "T3"
             (strcat "DATUM " (rtos datumVal 2 (if (equal datumVal (fix datumVal) 0.01) 0 2)) "m")
             (list x0 (+ y0 1350.0)) 1300.0 0.0 0.85 0 2)

    (mk-text "TITLE" "T3" "PROPOSED ROAD LEVEL" (list x0 (- y0 1468.0)) 1300.0 0.0 0.85 0 2)
    (mk-text "TITLE" "T3" "IN METERS"           (list x0 (- y0 3532.0)) 1300.0 0.0 0.85 0 2)
    (mk-line "P" (list (+ x0 24000.0) yRow1) (list (+ x0 35000.0) yRow1))

    (mk-text "TITLE" "T3" "LONGITUDINAL GRADIENT" (list x0 (- y0 6468.0)) 1300.0 0.0 0.85 0 2)
    (mk-text "TITLE" "T3" "/ SLOPE"               (list x0 (- y0 8532.0)) 1300.0 0.0 0.85 0 2)

    (mk-text "TITLE" "T3" "DISTANCE IN METERS" (list x0 yRow3) 1300.0 0.0 0.85 0 2)

    ;; 3. Loop Through Stations: Vertical Ordinates, Right-Side Table Values & Profile Line
    (setq idx 0)
    (while (<= idx lastIdx)
      (setq stn    (nth idx stnList)
            chM    (car stn)
            zVal   (cadr stn)
            stnTag (caddr stn)
            xStn   (+ xStart (* chM hMult))
            yZ     (+ y0 (* (- zVal datumVal) vMult)))

      ;; Vertical Station Line from bottom of table (yLine3) up to Proposed Road Profile (yZ)
      (mk-line "TITLE" (list xStn yLine3) (list xStn yZ))

      ;; Top Station Chainage Tag above road profile
      (mk-text "M" "T3" stnTag (list (- xStn 250.0) (+ yZ 2000.0)) 1100.0 (/ pi 2.0) 0.85 0 1)

      ;; Table Values (Proposed Level & Distance) on RIGHT of station line (+1050.0) in 0.00 format
      (setq xTxtRight (+ xStn 1050.0))
      (mk-text "DT" "2" (rs:fmt2dec zVal) (list xTxtRight yRow1) 900.0 (/ pi 2.0) 0.85 1 2)
      (mk-text "DT" "2" (rs:fmt2dec chM)  (list xTxtRight yRow3) 900.0 (/ pi 2.0) 0.85 1 2)

      ;; Connect Proposed Road Profile Line & Gradient Label to Next Station
      (if (< idx lastIdx)
        (progn
          (setq nextStn (nth (1+ idx) stnList)
                chNext  (car nextStn)
                zNext   (cadr nextStn)
                xNext   (+ xStart (* chNext hMult))
                yZNext  (+ y0 (* (- zNext datumVal) vMult))
                segLenM (- chNext chM)
                dZ      (- zNext zVal)
                xMid    (/ (+ xStn xNext) 2.0))

          ;; Proposed Road Profile Line
          (mk-line "P" (list xStn yZ) (list xNext yZNext))

          ;; Longitudinal Slope / Gradient Label in Row 2
          (if (> (abs dZ) 1.0e-4)
            (setq slopeStr (strcat "{\\W0.85;1:" (rtos (abs (/ segLenM dZ)) 2 0)
                                   " (" (if (> dZ 0.0) "+" "") (rs:fmt2dec (* (/ dZ segLenM) 100.0)) "%)}"))
            (setq slopeStr "{\\W0.85;FLAT (0.00%)}")
          )
          (if (>= (* segLenM (/ 1000.0 hSc)) 12.0)
            (mk-mtext "DT" "2" slopeStr (list xMid (- y0 7050.0)) 850.0 2)
          )
        )
      )
      (setq idx (1+ idx))
    )

    ;; 4. Bottom Section Title & Scale Annotations
    (mk-text "TITLE" "T3"
             (strcat "LONGITUDINAL SECTION OF ROAD (CH 0.00m TO CH " (rs:fmt2dec totalDist) "m)")
             (list x0 (- yLine3 4000.0)) 2500.0 0.0 0.85 0 0)

    (mk-text "TITLE" "T3"
             (strcat "SCALE   HOR = 1:" (itoa (fix hSc)))
             (list (+ x0 500.0) (- yLine3 6300.0)) 900.0 0.0 0.85 0 0)

    (mk-text "TITLE" "T3"
             (strcat "        VER = 1:" (itoa (fix vSc)))
             (list (+ x0 500.0) (- yLine3 7600.0)) 900.0 0.0 0.85 0 0)
  )

  ;; =========================================================================
  ;; 7. MAIN INTERACTIVE WORKFLOW
  ;; =========================================================================
  (princ "\n==================================================================")
  (princ "\n  ROADSEC : Road Longitudinal Section from Polyline & Level Texts")
  (princ "\n==================================================================")

  (setq loop T)
  (while loop
    (initget "Scale Interval Corridor Units Datum")
    (setq polySel
      (entsel
        (strcat "\nSelect Road Alignment Polyline/Line [Scale/Interval/Corridor/Units/Datum] <Exit> "
                "(HOR 1:" (rtos hScale 2 0)
                ", VER 1:" (rtos vScale 2 0)
                ", Step:" (rtos stepIntervalM 2 0) "m"
                ", Corridor:" (rtos searchCorridorM 2 0) "m): ")))

    (cond
      ((null polySel)
       (setq loop nil))

      ((= polySel "Scale")
       (initget 6)
       (setq ans (getreal (strcat "\nEnter Horizontal Scale 1:X <" (rtos hScale 2 0) ">: ")))
       (if ans (setq hScale ans))
       (initget 6)
       (setq ans (getreal (strcat "\nEnter Vertical Scale 1:Y <" (rtos vScale 2 0) ">: ")))
       (if ans (setq vScale ans)))

      ((= polySel "Interval")
       (initget 4)
       (setq ans (getreal (strcat "\nEnter regular chainage interval in meters (0 = only at level texts) <" (rtos stepIntervalM 2 0) ">: ")))
       (if ans (setq stepIntervalM ans)))

      ((= polySel "Corridor")
       (initget 6)
       (setq ans (getreal (strcat "\nEnter max search distance (m) from polyline to detect level texts <" (rtos searchCorridorM 2 0) ">: ")))
       (if ans (setq searchCorridorM ans)))

      ((= polySel "Units")
       (initget "Millimeters Meters")
       (setq ans (getkword "\nDrawing units [Millimeters (1m=1000) / Meters (1m=1)] <Millimeters>: "))
       (setq unitsPerM (if (= ans "Meters") 1.0 1000.0)))

      ((= polySel "Datum")
       (initget "Auto Manual")
       (setq ans (getkword "\nDatum Mode [Auto/Manual] <Auto>: "))
       (if (= ans "Manual")
         (progn
           (setq autoDatum "No")
           (setq ans (getreal (strcat "\nEnter Manual Datum Elevation (m) <" (rtos manualDatum 2 2) ">: ")))
           (if ans (setq manualDatum ans))
         )
         (setq autoDatum "Yes")
       ))

      ((listp polySel)
       (setq polyEnt   (car polySel)
             pickPtWcs (trans (cadr polySel) 1 0)
             polyPts   (get-poly-pts-wcs polyEnt (list (car pickPtWcs) (cadr pickPtWcs) 0.0)))

       (if (< (length polyPts) 2)
         (princ "\n>> Selected object is not a valid LINE or POLYLINE. Try again.")
         (progn
           (setq totalLenM (/ (cadr (project-pt-to-poly (last polyPts) polyPts)) unitsPerM))
           (princ (strcat "\n>> Alignment Polyline Length: " (rs:fmt2dec totalLenM) "m."))

           ;; Let user click 1 sample Proposed Level text (to lock layer), or Window-select, or press Enter to scan all layers
           (initget "Window All")
           (setq refSel (entsel "\nClick 1 sample Proposed Level text (e.g. FFL27.80) to filter layer, or [Window/All] <All>: "))

           (cond
             ((= refSel "Window")
              (princ "\n>> Select Proposed Level texts along the road alignment: ")
              (setq ssTexts (ssget '((0 . "TEXT,MTEXT,MULTILEADER")))))

             ((listp refSel)
              (setq refEnt  (car refSel)
                    refLay  (cdr (assoc 8 (entget refEnt)))
                    ssTexts (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER")
                                              (cons 8 (rs:esc refLay))
                                              (cons 410 (getvar "CTAB"))))))

             (T
              (setq ssTexts (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER")
                                              (cons 410 (getvar "CTAB"))))))
           )

           (if (not ssTexts)
             (princ "\n>> No text objects found.")
             (progn
               (setq corridorUnits (* searchCorridorM unitsPerM)
                     rawNodes      nil
                     i             0)

               (while (< i (sslength ssTexts))
                 (setq e      (ssname ssTexts i)
                       parsed (parse-level-str (get-text-raw e)))
                 (if parsed
                   (progn
                     (setq ptCen   (get-text-center-wcs e)
                           projRes (project-pt-to-poly ptCen polyPts))
                     (if (<= (car projRes) corridorUnits)
                       (setq offDistM (/ (car projRes) unitsPerM)
                             chM      (/ (cadr projRes) unitsPerM)
                             zVal     (car parsed)
                             lblTxt   (cadr parsed)
                             rawNodes (cons (list chM zVal lblTxt offDistM) rawNodes))
                     )
                   )
                 )
                 (setq i (1+ i))
               )

               (if (< (length rawNodes) 2)
                 (princ (strcat "\n>> Found fewer than 2 Proposed Level texts within "
                                (rtos searchCorridorM 2 0)
                                "m of the polyline. Increase [Corridor] or check [Units]!"))
                 (progn
                   (setq sortedNodes (sort-nodes-by-ch rawNodes)
                         stnList     (build-station-list totalLenM sortedNodes stepIntervalM includeTextStns)
                         datumVal    (compute-datum stnList autoDatum baseDatumUnder10 manualDatum))

                   (princ (strcat "\n>> Detected " (itoa (length sortedNodes))
                                  " Proposed Level text(s) along alignment -> Built "
                                  (itoa (length stnList)) " profile station(s)."))

                   (ensure-layer "TITLE" 7 "Continuous")
                   (ensure-layer "M"     6 "Continuous")
                   (ensure-layer "P"     2 "Continuous")
                   (ensure-layer "DT"    7 "Continuous")
                   (ensure-textstyle "T3" 0.85)
                   (ensure-textstyle "2"  0.85)

                   (setvar "OSMODE" oldOs)
                   (setq ptIns (getpoint "\nClick Insertion Point for Road Longitudinal Section: "))
                   (if ptIns
                     (progn
                       (command "_.UNDO" "_Begin")
                       (setvar "OSMODE" 0)
                       (draw-road-section (trans ptIns 1 0) stnList hScale vScale datumVal)
                       (command "_.UNDO" "_End")
                       (princ (strcat "\n>> ROADSEC Complete! Generated Road Longitudinal Section ("
                                      (rs:fmt2dec totalLenM) "m, Datum " (rs:fmt2dec datumVal) "m)."))
                       (setq loop nil)
                     )
                     (princ "\n>> Insertion cancelled.")
                   )
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
(defun c:rsec ()     (c:roadsec))
(defun c:roadlong () (c:roadsec))
(defun c:plinesec () (c:roadsec))
(princ "\nROADSEC loaded. Type ROADSEC, RSEC, ROADLONG, or PLINESEC to run.")
(princ)