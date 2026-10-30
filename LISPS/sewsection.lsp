;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : sewsection.lsp
;;; COMMAND     : SEWSECTION (Aliases: LONGSEC, SEWLONG)
;;; DESCRIPTION : Automated Sewerage Longitudinal Section Generator
;;;               - Pre-Ticked Toggles for IL, GL, Depth & Distance/Length
;;;               - IL, GL, Depth & Distance on RIGHT of Manhole Line
;;;               - Backdrop (B/D IL) on LEFT of Manhole Line
;;;               - Profile & Shaft Lines Trimmed at Manhole Lines
;;;               - Strict 2-Decimal Point (0.00) Formatting
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:sewsection ( / *error* oldCmd oldOs oldDimzin
                        ;; --- dialog & settings variables ---
                        hScaleIdx vScaleMode autoDatum baseDatumUnder10
                        manualDatum maxDistM splitRule layoutMode
                        showIL showGL showDepth showLen
                        hScaleVal vScaleVal dclFile dclId dlgOK
                        ;; --- file & parsing variables ---
                        inputFile csvFile isTempCsv sections
                        ;; --- placement variables ---
                        ptStart curPt secIdx secW gapX datumVal
                        ;; --- helper functions ---
                        ss:cleanup ss:tmpName ss:writeDCL ss:runDialog
                        ensure-layer ensure-textstyle ensure-dashed-ltype
                        mk-line mk-text mk-mtext fmt-2dec
                        str-trim split-csv-line clean-num clean-mh-id
                        xlsx-to-temp-csv parse-sewer-csv
                        compute-datum draw-long-section )

  (setq oldCmd    (getvar "CMDECHO")
        oldOs     (getvar "OSMODE")
        oldDimzin (getvar "DIMZIN"))
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0) ;; Prevents AutoCAD from suppressing trailing ".00" zeros

  ;; =========================================================================
  ;; 1. DEFAULT SETTINGS (Adjustable in Popup Dialog)
  ;; =========================================================================
  (setq hScaleIdx        "2"      ;; Index 2 = 1:1000 (0=1:200, 1=1:500, 2=1:1000, 3=1:1500, 4=1:2000)
        vScaleMode       "200"    ;; "200" = 1:200 (Default), "500" = 1:500, "1000" = 1000
        autoDatum        "1"      ;; "1" = Automatic (+10m datum step when level >= 10m)
        baseDatumUnder10 "0"      ;; Base datum when min level < 10m (e.g. 0m or 2m)
        manualDatum      "0"      ;; Used only when autoDatum = "0"
        maxDistM         "200"    ;; Max distance per section in meters (200m limit)
        splitRule        "CROSS"  ;; "CROSS" = Finish segment reaching 200m, "STRICT" = <= 200m
        layoutMode       "ROW"    ;; "ROW" = Side-by-side, "COL" = Stack vertically, "PICK" = Pick each
        ;; All 4 table rows ticked ("1") by default:
        showIL           "1"      ;; Tick: Invert Level (IL)
        showGL           "1"      ;; Tick: Ground / Proposed Road Level (GL)
        showDepth        "1"      ;; Tick: Manhole Depth
        showLen          "1")     ;; Tick: Distance / Length

  ;; =========================================================================
  ;; 2. CLEANUP, 2-DECIMAL FORMATTER & ERROR HANDLING
  ;; =========================================================================
  (defun ss:cleanup ()
    (if (and dclId (> dclId 0))
      (progn (unload_dialog dclId) (setq dclId nil))
    )
    (if (and dclFile (findfile dclFile) (boundp 'vl-file-delete))
      (vl-file-delete dclFile)
    )
    (if (and isTempCsv csvFile (findfile csvFile) (boundp 'vl-file-delete))
      (vl-file-delete csvFile)
    )
  )

  (defun *error* (msg)
    (ss:cleanup)
    (if oldDimzin (setvar "DIMZIN" oldDimzin))
    (if oldOs     (setvar "OSMODE" oldOs))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[SEWSECTION Error]: " msg))
    )
    (princ)
  )

  (defun ss:tmpName (ext)
    (if (boundp 'vl-filename-mktemp)
      (vl-filename-mktemp (strcat "sewsec" ext))
      (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) (".")) "\\sewsec_" (itoa (getvar "MILLISECS")) ext)
    )
  )

  ;; Guarantees strict 2-decimal formatting (e.g. 0 -> "0.00", 6.6 -> "6.60", 55 -> "55.00")
  (defun fmt-2dec (val / s dotPos decLen)
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
  ;; 3. POPUP DIALOG (DCL) GENERATOR & CONTROLLER
  ;; =========================================================================
  (defun ss:writeDCL (path / fh)
    (if (setq fh (open path "w"))
      (progn
        (write-line "sewsection : dialog {" fh)
        (write-line "  label = \"Sewerage Longitudinal Section Generator (JRK Standard)\";" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"1. Drawing Scale Settings\";" fh)
        (write-line "    : popup_list { key = \"hscale\"; label = \"Horizontal Scale (HOR):\"; width = 20; }" fh)
        (write-line "    : row {" fh)
        (write-line "      : text { label = \"Vertical Scale (VER):\"; width = 22; }" fh)
        (write-line "      : radio_row {" fh)
        (write-line "        : radio_button { key = \"v200\";  label = \"1:200\"; }" fh)
        (write-line "        : radio_button { key = \"v500\";  label = \"1:500\"; }" fh)
        (write-line "        : radio_button { key = \"v1000\"; label = \"1:1000\"; }" fh)
        (write-line "      }" fh)
        (write-line "    }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"2. Datum & Distance Limit (Meters)\";" fh)
        (write-line "    : toggle { key = \"autodatum\"; label = \"Automatic Datum (Adds 10m step when level >= 10m)\"; }" fh)
        (write-line "    : row {" fh)
        (write-line "      : edit_box { key = \"basedatum\"; label = \"Base Datum if < 10m (m):\"; edit_width = 8; }" fh)
        (write-line "      : edit_box { key = \"mandatum\";  label = \"Manual Datum (m):\"; edit_width = 8; }" fh)
        (write-line "    }" fh)
        (write-line "    : edit_box { key = \"maxdist\"; label = \"Max Distance Limit per Section (m):\"; edit_width = 10; }" fh)
        (write-line "    : row {" fh)
        (write-line "      : text { label = \"200m Split Behavior:\"; width = 20; }" fh)
        (write-line "      : radio_row {" fh)
        (write-line "        : radio_button { key = \"sp_cross\";  label = \"At Manhole >= 200m\"; }" fh)
        (write-line "        : radio_button { key = \"sp_strict\"; label = \"Strict <= 200m\"; }" fh)
        (write-line "      }" fh)
        (write-line "    }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"3. Table Data Rows to Include (All Ticked by Default)\";" fh)
        (write-line "    : row {" fh)
        (write-line "      : column {" fh)
        (write-line "        : toggle { key = \"chk_il\";    label = \"Invert Level (IL) & Pipe Spec\"; }" fh)
        (write-line "        : toggle { key = \"chk_gl\";    label = \"Ground / Road Level (GL)\"; }" fh)
        (write-line "      }" fh)
        (write-line "      : column {" fh)
        (write-line "        : toggle { key = \"chk_depth\"; label = \"Manhole Depth (m)\"; }" fh)
        (write-line "        : toggle { key = \"chk_len\";   label = \"Distance / Length (m)\"; }" fh)
        (write-line "      }" fh)
        (write-line "    }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_radio_column {" fh)
        (write-line "    label = \"4. Multi-Section Placement Arrangement\";" fh)
        (write-line "    : radio_button { key = \"lay_row\";  label = \"Place Side-by-Side Horizontally (Left to Right)\"; }" fh)
        (write-line "    : radio_button { key = \"lay_col\";  label = \"Stack Vertically (Top to Bottom)\"; }" fh)
        (write-line "    : radio_button { key = \"lay_pick\"; label = \"Click Insertion Point Manually for Each Section\"; }" fh)
        (write-line "  }" fh)
        (write-line "  spacer;" fh)
        (write-line "  ok_cancel;" fh)
        (write-line "  errtile;" fh)
        (write-line "}" fh)
        (close fh)
        T
      )
      nil
    )
  )

  (defun ss:runDialog ( / res)
    (setq res nil dclFile (ss:tmpName ".dcl"))
    (if (ss:writeDCL dclFile)
      (progn
        (setq dclId (load_dialog dclFile))
        (if (and dclId (>= dclId 1) (new_dialog "sewsection" dclId))
          (progn
            (start_list "hscale")
            (mapcar 'add_list '("1:200" "1:500" "1:1000 (Default)" "1:1500" "1:2000"))
            (end_list)
            (set_tile "hscale" hScaleIdx)

            (set_tile "v200"  (if (= vScaleMode "200")  "1" "0"))
            (set_tile "v500"  (if (= vScaleMode "500")  "1" "0"))
            (set_tile "v1000" (if (= vScaleMode "1000") "1" "0"))

            (set_tile "autodatum" autoDatum)
            (set_tile "basedatum" baseDatumUnder10)
            (set_tile "mandatum"  manualDatum)
            (if (= autoDatum "1")
              (progn (mode_tile "basedatum" 0) (mode_tile "mandatum" 1))
              (progn (mode_tile "basedatum" 1) (mode_tile "mandatum" 0))
            )

            (set_tile "maxdist"   maxDistM)
            (set_tile "sp_cross"  (if (= splitRule "CROSS")  "1" "0"))
            (set_tile "sp_strict" (if (= splitRule "STRICT") "1" "0"))

            ;; Pre-tick all 4 table row options
            (set_tile "chk_il"    showIL)
            (set_tile "chk_gl"    showGL)
            (set_tile "chk_depth" showDepth)
            (set_tile "chk_len"   showLen)

            (set_tile "lay_row"  (if (= layoutMode "ROW")  "1" "0"))
            (set_tile "lay_col"  (if (= layoutMode "COL")  "1" "0"))
            (set_tile "lay_pick" (if (= layoutMode "PICK") "1" "0"))

            (action_tile "autodatum"
              "(setq autoDatum $value) (if (= autoDatum \"1\") (progn (mode_tile \"basedatum\" 0) (mode_tile \"mandatum\" 1)) (progn (mode_tile \"basedatum\" 1) (mode_tile \"mandatum\" 0)))")

            (action_tile "accept"
              (strcat
                "(setq hScaleIdx (get_tile \"hscale\") "
                "vScaleMode (cond ((= (get_tile \"v500\") \"1\") \"500\") ((= (get_tile \"v1000\") \"1\") \"1000\") (T \"200\")) "
                "autoDatum (get_tile \"autodatum\") "
                "baseDatumUnder10 (get_tile \"basedatum\") "
                "manualDatum (get_tile \"mandatum\") "
                "maxDistM (get_tile \"maxdist\") "
                "splitRule (if (= (get_tile \"sp_strict\") \"1\") \"STRICT\" \"CROSS\") "
                "showIL (get_tile \"chk_il\") "
                "showGL (get_tile \"chk_gl\") "
                "showDepth (get_tile \"chk_depth\") "
                "showLen (get_tile \"chk_len\") "
                "layoutMode (cond ((= (get_tile \"lay_col\") \"1\") \"COL\") ((= (get_tile \"lay_pick\") \"1\") \"PICK\") (T \"ROW\"))) "
                "(cond "
                "((or (null (distof maxDistM)) (<= (distof maxDistM) 0.0)) "
                " (set_tile \"error\" \"Please enter a valid positive Max Distance Limit (e.g. 200).\")) "
                "((and (= showIL \"0\") (= showGL \"0\") (= showDepth \"0\") (= showLen \"0\")) "
                " (set_tile \"error\" \"Please tick at least one Table Data Row to include.\")) "
                "(T (done_dialog 1)))"))

            (action_tile "cancel" "(done_dialog 0)")
            (setq res (start_dialog))
          )
        )
      )
    )
    (ss:cleanup)
    res
  )

  ;; =========================================================================
  ;; 4. CAD LAYER, STYLE & ENTITY CREATION HELPERS
  ;; =========================================================================
  (defun ensure-dashed-ltype ( / ltypeName)
    (setq ltypeName "DASHED")
    (if (not (tblsearch "LTYPE" ltypeName))
      (progn
        (vl-catch-all-apply '(lambda () (command "_.-LINETYPE" "_L" "DASHED" "acad.lin" "")))
        (if (not (tblsearch "LTYPE" "DASHED"))
          (vl-catch-all-apply '(lambda () (command "_.-LINETYPE" "_L" "DASHED" "acadiso.lin" "")))
        )
        (if (not (tblsearch "LTYPE" "DASHED"))
          (if (tblsearch "LTYPE" "HIDDEN")
            (setq ltypeName "HIDDEN")
            (setq ltypeName "Continuous")
          )
        )
      )
    )
    ltypeName
  )

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
          (command "_.-LAYER" "_N" nm "_C" col nm "_LT" ltype nm "")
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

  (defun mk-line (lay p1 p2 ltScale / data)
    (setq data (list '(0 . "LINE")
                     '(100 . "AcDbEntity")
                     (cons 8 lay)
                     '(100 . "AcDbLine")
                     (list 10 (car p1) (cadr p1) 0.0)
                     (list 11 (car p2) (cadr p2) 0.0)))
    (if (and ltScale (> ltScale 0.0))
      (setq data (append data (list (cons 48 ltScale))))
    )
    (entmake data)
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
  ;; 5. CSV & EXCEL (.XLSX / .XLSM) PARSER ENGINE
  ;; =========================================================================
  (defun str-trim (s)
    (if s (vl-string-trim " \t\r\n\"" s) "")
  )

  (defun split-csv-line (line / res cur inQ i len ch)
    (setq res nil cur "" inQ nil i 1 len (strlen line))
    (while (<= i len)
      (setq ch (substr line i 1))
      (cond
        ((= ch "\"")
         (setq inQ (not inQ)))
        ((and (= ch ",") (not inQ))
         (setq res (cons (str-trim cur) res)
               cur ""))
        (T
         (setq cur (strcat cur ch)))
      )
      (setq i (1+ i))
    )
    (setq res (cons (str-trim cur) res))
    (reverse res)
  )

  (defun clean-num (s / i len ch out hasDot)
    (setq s (str-trim s))
    (if (or (= s "") (= s "-") (wcmatch (strcase s) "*#DIV*,*#REF*,*#VALUE*,*#N/A*"))
      nil
      (progn
        (setq i 1 len (strlen s) out "" hasDot nil)
        (while (<= i len)
          (setq ch (substr s i 1))
          (cond
            ((and (= ch "-") (= out ""))
             (setq out "-"))
            ((and (= ch ".") (not hasDot))
             (setq out (strcat out ".") hasDot T))
            ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
             (setq out (strcat out ch)))
          )
          (setq i (1+ i))
        )
        (if (or (= out "") (= out "-") (= out ".")) nil (distof out))
      )
    )
  )

  (defun clean-mh-id (rawPrefix rawSub / s numVal)
    (setq rawPrefix (strcase (str-trim rawPrefix))
          rawSub    (str-trim rawSub))
    (cond
      ((and (= rawPrefix "MH") (/= rawSub "") (clean-num rawSub))
       (setq numVal (fix (+ (clean-num rawSub) 0.0001)))
       (strcat "MH" (itoa numVal)))

      ((wcmatch rawPrefix "MH*")
       (setq s (substr rawPrefix 3)
             s (vl-string-left-trim " -_" s))
       (if (clean-num s)
         (strcat "MH" (itoa (fix (+ (clean-num s) 0.0001))))
         (strcat "MH" s)
       ))

      ((and (/= rawPrefix "") (clean-num rawPrefix))
       (strcat "MH" (itoa (fix (+ (clean-num rawPrefix) 0.0001)))))

      (T "")
    )
  )

  (defun xlsx-to-temp-csv (xlsPath / xlApp wbs wb tmpCsv ok)
    (setq tmpCsv (ss:tmpName ".csv") ok nil)
    (setq xlApp (vl-catch-all-apply 'vlax-get-or-create-object (list "Excel.Application")))
    (if (and xlApp (not (vl-catch-all-error-p xlApp)))
      (progn
        (vl-catch-all-apply 'vlax-put-property (list xlApp 'Visible :vlax-false))
        (vl-catch-all-apply 'vlax-put-property (list xlApp 'DisplayAlerts :vlax-false))
        (setq wbs (vl-catch-all-apply 'vlax-get-property (list xlApp 'Workbooks)))
        (if (and wbs (not (vl-catch-all-error-p wbs)))
          (progn
            (setq wb (vl-catch-all-apply 'vlax-invoke-method (list wbs 'Open xlsPath)))
            (if (and wb (not (vl-catch-all-error-p wb)))
              (progn
                (if (not (vl-catch-all-error-p
                           (vl-catch-all-apply 'vlax-invoke-method (list wb 'SaveAs tmpCsv 6))))
                  (setq ok T)
                )
                (vl-catch-all-apply 'vlax-invoke-method (list wb 'Close :vlax-false))
              )
            )
          )
        )
        (vl-catch-all-apply 'vlax-invoke-method (list xlApp 'Quit))
        (vl-catch-all-apply 'vlax-release-object (list xlApp))
      )
    )
    (if (and ok (findfile tmpCsv)) tmpCsv nil)
  )

  ;; Each MH node: (list mhID cumDist gl ilIn ilOut pipeSize pipeMat pipeGrad depthVal)
  (defun parse-sewer-csv (filePath limitM splitMode / fh line cells rowStr
                          headerFound colMH colMHSub colLen colSize colGrad colMat colGL colIL colDepth colBD
                          idx hCell allSections curRun pendingPipe cumDist
                          rawA rawB mhID lenVal sizeVal gradVal matVal glVal ilVal depthVal bdVal
                          gCell2 gCell3 lastNode ilIn ilOut calcDepth nextDist startNode)
    (setq fh (open filePath "r")
          headerFound nil
          colMH 0 colMHSub 1 colLen 10 colSize 11 colGrad 12 colMat 15 colGL 22 colIL 23 colDepth 24 colBD 25
          allSections nil
          curRun nil
          pendingPipe nil
          cumDist 0.0)

    (while (and fh (setq line (read-line fh)))
      (setq cells  (split-csv-line line)
            rowStr (strcase (apply 'strcat cells)))

      (if (and (not headerFound)
               (or (wcmatch rowStr "*MANHOLE*INVERT*")
                   (wcmatch rowStr "*LENGTH*PIPE*SIZE*")
                   (wcmatch rowStr "*GROUND*LEVEL*INVERT*")))
        (progn
          (setq headerFound T idx 0
                colMH nil colMHSub nil colLen nil colSize nil colGrad nil colMat nil colGL nil colIL nil colDepth nil colBD nil)
          (foreach cell cells
            (setq hCell (strcase (str-trim cell)))
            (cond
              ((and (null colMH) (wcmatch hCell "*MANHOLE*NO*,MH,MANHOLE,MH ID,NODE*"))
               (setq colMH idx))
              ((and (null colLen) (wcmatch hCell "*LENGTH*,LEN*"))
               (setq colLen idx))
              ((and (null colSize) (wcmatch hCell "*PIPE*SIZE*,*DIAMETER*,SIZE,DIA*"))
               (setq colSize idx))
              ((and (null colGrad) (wcmatch hCell "*GRADIENT*,*GRADE*,*SLOPE*"))
               (setq colGrad idx))
              ((and (null colMat) (wcmatch hCell "*PIPE*TYPE*,*MATERIAL*,MAT*"))
               (setq colMat idx))
              ((and (null colGL) (wcmatch hCell "*GROUND*LEVEL*,GL*"))
               (setq colGL idx))
              ((and (null colIL) (wcmatch hCell "*INVERT*LEVEL*,IL*"))
               (setq colIL idx))
              ((and (null colDepth) (wcmatch hCell "*DEPTH*"))
               (setq colDepth idx))
              ((and (null colBD) (wcmatch hCell "*BACKDROP*,B/D*"))
               (setq colBD idx))
            )
            (setq idx (1+ idx))
          )
          (if (null colMH)    (setq colMH 0))
          (if (and (< (1+ colMH) (length cells)) (= (str-trim (nth (1+ colMH) cells)) ""))
            (setq colMHSub (1+ colMH))
          )
          (if (null colLen)   (setq colLen 10))
          (if (null colSize)  (setq colSize 11))
          (if (null colGrad)  (setq colGrad 12))
          (if (null colMat)   (setq colMat 15))
          (if (null colGL)    (setq colGL 22))
          (if (null colIL)    (setq colIL 23))
          (if (null colDepth) (setq colDepth 24))
          (if (null colBD)    (setq colBD 25))
        )
        (if (> (length cells) (max colMH colGL colIL))
          (progn
            (setq rawA (if (< colMH (length cells)) (nth colMH cells) "")
                  rawB (if (and colMHSub (< colMHSub (length cells))) (nth colMHSub cells) "")
                  mhID (clean-mh-id rawA rawB))

            (setq lenVal   (if (and colLen   (< colLen   (length cells))) (clean-num (nth colLen   cells)) nil)
                  sizeVal  (if (and colSize  (< colSize  (length cells))) (clean-num (nth colSize  cells)) nil)
                  glVal    (if (and colGL    (< colGL    (length cells))) (clean-num (nth colGL    cells)) nil)
                  ilVal    (if (and colIL    (< colIL    (length cells))) (clean-num (nth colIL    cells)) nil)
                  depthVal (if (and colDepth (< colDepth (length cells))) (clean-num (nth colDepth cells)) nil)
                  bdVal    (if (and colBD    (< colBD    (length cells))) (clean-num (nth colBD    cells)) nil)
                  matVal   (if (and colMat   (< colMat   (length cells))) (str-trim  (nth colMat   cells)) ""))

            (if (or (= matVal "") (= matVal "-")) (setq matVal "VCP"))

            (setq gradVal nil)
            (if (and colGrad (< colGrad (length cells)))
              (progn
                (setq gCell2 (if (< (1+ colGrad) (length cells)) (str-trim (nth (1+ colGrad) cells)) "")
                      gCell3 (if (< (+ colGrad 2) (length cells)) (str-trim (nth (+ colGrad 2) cells)) ""))
                (cond
                  ((and (= (str-trim (nth colGrad cells)) "1") (= gCell2 ":") (clean-num gCell3))
                   (setq gradVal (itoa (fix (+ (clean-num gCell3) 0.5)))))
                  ((vl-string-search ":" (nth colGrad cells))
                   (setq gradVal (substr (nth colGrad cells) (+ (vl-string-search ":" (nth colGrad cells)) 2)))
                   (if (clean-num gradVal) (setq gradVal (itoa (fix (+ (clean-num gradVal) 0.5))))))
                  ((clean-num (nth colGrad cells))
                   (setq gradVal (itoa (fix (+ (clean-num (nth colGrad cells)) 0.5)))))
                )
              )
            )
            (if (or (null gradVal) (= gradVal "")) (setq gradVal "200"))

            (if (and (/= mhID "") glVal ilVal)
              (cond
                ;; Same Manhole ID as last node -> attach outgoing pipe or backdrop
                ((and curRun (= mhID (nth 0 (last curRun))))
                 (setq lastNode (last curRun))
                 (setq ilIn  (nth 3 lastNode)
                       ilOut (if (< ilVal (- ilIn 0.005)) ilVal (nth 4 lastNode)))
                 (if (and bdVal (> bdVal 0.005) (equal ilIn ilOut 0.005))
                   (setq ilIn (+ ilOut bdVal))
                 )
                 (setq calcDepth (if depthVal depthVal (- (if glVal glVal (nth 2 lastNode)) ilOut)))
                 (setq lastNode (list (nth 0 lastNode)
                                      (nth 1 lastNode)
                                      (if glVal glVal (nth 2 lastNode))
                                      ilIn
                                      ilOut
                                      (if sizeVal (itoa (fix (+ sizeVal 0.5))) (nth 5 lastNode))
                                      matVal
                                      gradVal
                                      calcDepth))
                 (setq curRun (append (reverse (cdr (reverse curRun))) (list lastNode)))
                 (if (and lenVal (> lenVal 0.0))
                   (setq pendingPipe (list lenVal (if sizeVal (itoa (fix (+ sizeVal 0.5))) "225") matVal gradVal))
                 )
                )

                ;; Next Manhole connected by pendingPipe
                ((and curRun pendingPipe)
                 (setq nextDist (+ cumDist (nth 0 pendingPipe)))

                 (if (and (= splitMode "STRICT") (> nextDist limitM) (>= (length curRun) 2))
                   (progn
                     (setq allSections (append allSections (list curRun))
                           lastNode    (last curRun)
                           startNode   (list (nth 0 lastNode) 0.0 (nth 2 lastNode) (nth 4 lastNode) (nth 4 lastNode)
                                             (nth 5 lastNode) (nth 6 lastNode) (nth 7 lastNode) (nth 8 lastNode))
                           curRun      (list startNode)
                           cumDist     (nth 0 pendingPipe))
                   )
                   (setq cumDist nextDist)
                 )

                 (setq ilIn      (if (and bdVal (> bdVal 0.005)) (+ ilVal bdVal) ilVal)
                       ilOut     ilVal
                       calcDepth (if depthVal depthVal (- glVal ilOut)))

                 (setq curRun (append curRun
                                      (list (list mhID cumDist glVal ilIn ilOut
                                                  (if sizeVal (itoa (fix (+ sizeVal 0.5))) "225")
                                                  matVal gradVal calcDepth))))

                 (if (and lenVal (> lenVal 0.0))
                   (setq pendingPipe (list lenVal (if sizeVal (itoa (fix (+ sizeVal 0.5))) "225") matVal gradVal))
                   (setq pendingPipe nil)
                 )

                 (if (and (= splitMode "CROSS") (>= cumDist limitM) pendingPipe)
                   (progn
                     (setq allSections (append allSections (list curRun))
                           lastNode    (last curRun)
                           startNode   (list (nth 0 lastNode) 0.0 (nth 2 lastNode) (nth 4 lastNode) (nth 4 lastNode)
                                             (nth 5 lastNode) (nth 6 lastNode) (nth 7 lastNode) (nth 8 lastNode))
                           curRun      (list startNode)
                           cumDist     0.0)
                   )
                 )
                )

                ;; New Sewer Branch
                (T
                 (if (>= (length curRun) 2)
                   (setq allSections (append allSections (list curRun)))
                 )
                 (setq cumDist   0.0
                       ilIn      (if (and bdVal (> bdVal 0.005)) (+ ilVal bdVal) ilVal)
                       ilOut     ilVal
                       calcDepth (if depthVal depthVal (- glVal ilOut))
                       curRun    (list (list mhID 0.0 glVal ilIn ilOut
                                             (if sizeVal (itoa (fix (+ sizeVal 0.5))) "225")
                                             matVal gradVal calcDepth)))
                 (if (and lenVal (> lenVal 0.0))
                   (setq pendingPipe (list lenVal (if sizeVal (itoa (fix (+ sizeVal 0.5))) "225") matVal gradVal))
                   (setq pendingPipe nil)
                 )
                )
              )
            )
          )
        )
      )
    )
    (if fh (close fh))
    (if (>= (length curRun) 2)
      (setq allSections (append allSections (list curRun)))
    )
    allSections
  )

  ;; =========================================================================
  ;; 6. AUTOMATIC DATUM CALCULATOR (+10m Step Rule when Level >= 10m)
  ;; =========================================================================
  (defun compute-datum (secNodes isAuto baseU10 manVal / minIL node ilOut tenStep dVal)
    (if (= isAuto "0")
      (if (distof manVal) (distof manVal) 0.0)
      (progn
        (setq minIL 1.0e9)
        (foreach node secNodes
          (setq ilOut (nth 4 node))
          (if (< ilOut minIL) (setq minIL ilOut))
        )
        (if (>= minIL 10.0)
          (progn
            (setq tenStep (* 10.0 (fix (/ minIL 10.0)))
                  dVal    (+ tenStep (if (distof baseU10) (distof baseU10) 0.0)))
            (if (> dVal (- minIL 1.0))
              (setq dVal (max 0.0 (fix (- minIL 2.0))))
            )
            dVal
          )
          (progn
            (setq dVal (if (distof baseU10) (distof baseU10) 0.0))
            (if (> dVal (- minIL 1.0))
              (setq dVal (max 0.0 (fix (- minIL 2.0))))
            )
            dVal
          )
        )
      )
    )
  )

  ;; =========================================================================
  ;; 7. LONGITUDINAL SECTION GEOMETRY ENGINE
  ;;    - Dynamically builds ticked rows (IL, GL, Depth, Distance/Length)
  ;;    - IL, GL, Depth & Distance placed on RIGHT of Manhole Line (+1050.0)
  ;;    - Backdrop (B/D IL) placed on LEFT of Manhole Line (-1050.0 & -2150.0)
  ;;    - Profile & Shaft Lines trimmed cleanly at Manhole Shaft Walls (+/-375.0)
  ;; =========================================================================
  (defun draw-long-section (ptBase secNodes hScale vScale datumVal /
                            x0 y0 hdrW rowH shaftHalfW hMult vMult totalDist xStart xEnd
                            curTopY yRowIL yRowGL yRowDepth yRowLen yBottom
                            maxGL n node yTop idx lastIdx mhID dM gl ilIn ilOut depthM xMh
                            yIlIn yIlOut yGl xTxtRight titleMHs
                            prevNode dPrev xPrev yGlPrev yIlPrevOut spanPrev
                            yGlLeftWall yIlLeftWall
                            nextNode dNext xNext yIlNextIn yGlNext segLenM spanNext
                            yGlRightWall yIlRightWall yGlNextLeftWall yIlNextLeftWall
                            xPipe1 xPipe2 xMid pipeStr)

    (setq x0         (car ptBase)
          y0         (cadr ptBase)
          hdrW       37500.0
          rowH       5000.0
          shaftHalfW 375.0                 ;; Half-width of Manhole Shaft (375 CAD units each side)
          hMult      (/ 1000000.0 hScale)  ;; 1:1000 -> 1000 CAD units/m
          vMult      (/ 1000000.0 vScale)  ;; 1:200  -> 5000 CAD units/m
          totalDist  (nth 1 (last secNodes))
          xStart     (+ x0 hdrW)
          xEnd       (+ xStart (* totalDist hMult))
          lastIdx    (1- (length secNodes)))

    ;; Compute uniform top elevation for vertical manhole lines
    (setq maxGL -1.0e9)
    (foreach n secNodes
      (if (> (nth 2 n) maxGL) (setq maxGL (nth 2 n)))
    )
    (setq yTop (max (+ y0 49500.0)
                    (+ y0 (* (- maxGL datumVal) vMult) 15000.0)))

    ;; 1. Draw Top Datum Line & DATUM Label
    (mk-line "TITLE" (list x0 y0) (list xEnd y0) nil)
    (mk-text "TITLE" "T3"
             (strcat "DATUM " (rtos datumVal 2 (if (equal datumVal (fix datumVal) 0.01) 0 2)) "m")
             (list x0 (+ y0 1350.0)) 1300.0 0.0 0.85 0 2)

    ;; 2. Dynamically Build Ticked Table Rows Below Datum (y0)
    (setq curTopY   y0
          yRowIL    nil
          yRowGL    nil
          yRowDepth nil
          yRowLen   nil)

    ;; Row A: Invert Level (IL)
    (if (= showIL "1")
      (progn
        (setq yRowIL (- curTopY 2500.0))
        (mk-text "TITLE" "T3" "MANHOLES INVERT LEVEL" (list x0 (- curTopY 1468.0)) 1300.0 0.0 0.85 0 2)
        (mk-text "TITLE" "T3" "IN METERS"             (list x0 (- curTopY 3532.0)) 1300.0 0.0 0.85 0 2)
        (mk-line "M" (list (+ x0 24000.0) yRowIL) (list (+ x0 35000.0) yRowIL) nil)
        (setq curTopY (- curTopY rowH))
        (mk-line "TITLE" (list x0 curTopY) (list xEnd curTopY) nil)
      )
    )

    ;; Row B: Ground / Proposed Road Level (GL)
    (if (= showGL "1")
      (progn
        (setq yRowGL (- curTopY 2500.0))
        (mk-text "TITLE" "T3" "PROPOSED ROAD LEVEL" (list x0 (- curTopY 1468.0)) 1300.0 0.0 0.85 0 2)
        (mk-text "TITLE" "T3" "IN METERS"           (list x0 (- curTopY 3532.0)) 1300.0 0.0 0.85 0 2)
        (mk-line "P" (list (+ x0 24000.0) yRowGL) (list (+ x0 35000.0) yRowGL) 2500.0)
        (setq curTopY (- curTopY rowH))
        (mk-line "TITLE" (list x0 curTopY) (list xEnd curTopY) nil)
      )
    )

    ;; Row C: Manhole Depth
    (if (= showDepth "1")
      (progn
        (setq yRowDepth (- curTopY 2500.0))
        (mk-text "TITLE" "T3" "MANHOLE DEPTH" (list x0 (- curTopY 1468.0)) 1300.0 0.0 0.85 0 2)
        (mk-text "TITLE" "T3" "IN METERS"     (list x0 (- curTopY 3532.0)) 1300.0 0.0 0.85 0 2)
        (setq curTopY (- curTopY rowH))
        (mk-line "TITLE" (list x0 curTopY) (list xEnd curTopY) nil)
      )
    )

    ;; Row D: Distance / Length in Meters
    (if (= showLen "1")
      (progn
        (setq yRowLen (- curTopY 2500.0))
        (mk-text "TITLE" "T3" "DISTANCE IN METERS" (list x0 yRowLen) 1300.0 0.0 0.85 0 2)
        (setq curTopY (- curTopY rowH))
        (mk-line "TITLE" (list x0 curTopY) (list xEnd curTopY) nil)
      )
    )

    (setq yBottom curTopY)

    ;; 3. Loop Through Manholes: Vertical Lines, Trimmed Shafts, Table Values & Trimmed Profile Lines
    (setq idx 0 titleMHs "")
    (while (<= idx lastIdx)
      (setq node   (nth idx secNodes)
            mhID   (nth 0 node)
            dM     (nth 1 node)
            gl     (nth 2 node)
            ilIn   (nth 3 node)
            ilOut  (nth 4 node)
            depthM (nth 8 node)
            xMh    (+ xStart (* dM hMult))
            yIlIn  (+ y0 (* (- ilIn  datumVal) vMult))
            yIlOut (+ y0 (* (- ilOut datumVal) vMult))
            yGl    (+ y0 (* (- gl    datumVal) vMult)))

      (setq titleMHs (if (= titleMHs "") mhID (strcat titleMHs ", " mhID)))

      ;; 3A. Central Vertical Manhole Line trimmed from bottom table line (yBottom) to top (yTop)
      (mk-line "TITLE" (list xMh yBottom) (list xMh yTop) nil)

      ;; 3B. Compute exact Y intersections at Left Shaft Wall (xMh - 375) and Right Shaft Wall (xMh + 375)
      (if (> idx 0)
        (progn
          (setq prevNode   (nth (1- idx) secNodes)
                dPrev      (nth 1 prevNode)
                xPrev      (+ xStart (* dPrev hMult))
                yGlPrev    (+ y0 (* (- (nth 2 prevNode) datumVal) vMult))
                yIlPrevOut (+ y0 (* (- (nth 4 prevNode) datumVal) vMult))
                spanPrev   (- xMh xPrev))
          (if (> spanPrev (* 2.0 shaftHalfW))
            (setq yGlLeftWall (+ yGl (* (/ (- yGlPrev yGl) spanPrev) shaftHalfW))
                  yIlLeftWall (min yIlOut (+ yIlIn (* (/ (- yIlPrevOut yIlIn) spanPrev) shaftHalfW))))
            (setq yGlLeftWall yGl
                  yIlLeftWall yIlOut)
          )
        )
        (setq yGlLeftWall yGl
              yIlLeftWall yIlOut)
      )

      (if (< idx lastIdx)
        (progn
          (setq nextNode  (nth (1+ idx) secNodes)
                dNext     (nth 1 nextNode)
                xNext     (+ xStart (* dNext hMult))
                yGlNext   (+ y0 (* (- (nth 2 nextNode) datumVal) vMult))
                yIlNextIn (+ y0 (* (- (nth 3 nextNode) datumVal) vMult))
                spanNext  (- xNext xMh))
          (if (> spanNext (* 2.0 shaftHalfW))
            (setq yGlRightWall (+ yGl   (* (/ (- yGlNext   yGl)   spanNext) shaftHalfW))
                  yIlRightWall (+ yIlOut(* (/ (- yIlNextIn yIlOut)spanNext) shaftHalfW)))
            (setq yGlRightWall yGl
                  yIlRightWall yIlOut)
          )
        )
        (setq yGlRightWall yGl
              yIlRightWall yIlOut)
      )

      ;; Draw Trimmed Manhole Shaft Side Lines (Left Wall & Right Wall)
      (if (> yGlLeftWall yIlLeftWall)
        (mk-line "TITLE" (list (- xMh shaftHalfW) yIlLeftWall) (list (- xMh shaftHalfW) yGlLeftWall) nil)
      )
      (if (> yGlRightWall yIlRightWall)
        (mk-line "TITLE" (list (+ xMh shaftHalfW) yIlRightWall) (list (+ xMh shaftHalfW) yGlRightWall) nil)
      )

      ;; 3C. Top Manhole ID Label (Layer "M", Style "T3", Height 1300, Rot 90, Width 0.85, Bottom-Left)
      (mk-text "M" "T3" mhID (list (- xMh 250.0) (- yTop 3700.0)) 1300.0 (/ pi 2.0) 0.85 0 1)

      ;; 3D. Ticked Table Values: IL, GL, Depth & Distance ALWAYS on RIGHT of Manhole Line (+1050.0)
      (setq xTxtRight (+ xMh 1050.0))
      (if yRowIL
        (mk-text "DT" "2" (fmt-2dec ilOut)  (list xTxtRight yRowIL)    900.0 (/ pi 2.0) 0.85 1 2)
      )
      (if yRowGL
        (mk-text "DT" "2" (fmt-2dec gl)     (list xTxtRight yRowGL)    900.0 (/ pi 2.0) 0.85 1 2)
      )
      (if yRowDepth
        (mk-text "DT" "2" (fmt-2dec depthM) (list xTxtRight yRowDepth) 900.0 (/ pi 2.0) 0.85 1 2)
      )
      (if yRowLen
        (mk-text "DT" "2" (fmt-2dec dM)     (list xTxtRight yRowLen)   900.0 (/ pi 2.0) 0.85 1 2)
      )

      ;; 3E. Backdrop Info stays on LEFT of Manhole Line (-1050.0 and -2150.0) + vertical drop line
      (if (and (> idx 0) (> (- ilIn ilOut) 0.01))
        (progn
          (if yRowIL
            (progn
              (mk-text "DT" "2" (fmt-2dec ilIn) (list (- xMh 1050.0) yRowIL) 900.0 (/ pi 2.0) 0.85 1 2)
              (mk-text "DT" "2" "B/D IL"        (list (- xMh 2150.0) yRowIL) 900.0 (/ pi 2.0) 0.85 1 2)
            )
          )
          ;; Vertical backdrop drop line trimmed at the left shaft wall
          (mk-line "M" (list (- xMh shaftHalfW) yIlLeftWall) (list (- xMh shaftHalfW) yIlOut) nil)
        )
      )

      ;; 3F. Connect Profile Lines (Trimmed at Manhole Shaft Walls) & Pipe MTEXT to Next Manhole
      (if (< idx lastIdx)
        (progn
          (setq segLenM (- dNext dM)
                xMid    (/ (+ xMh xNext) 2.0))

          (if (> spanNext (* 2.0 shaftHalfW))
            (setq xPipe1          (+ xMh shaftHalfW)
                  xPipe2          (- xNext shaftHalfW)
                  yGlNextLeftWall (- yGlNext   (* (/ (- yGlNext   yGl)   spanNext) shaftHalfW))
                  yIlNextLeftWall (- yIlNextIn (* (/ (- yIlNextIn yIlOut)spanNext) shaftHalfW)))
            (setq xPipe1          xMh
                  xPipe2          xNext
                  yGlNextLeftWall yGlNext
                  yIlNextLeftWall yIlNextIn)
          )

          ;; Magenta Sewer Invert Profile Line (Trimmed between xMh+375 and xNext-375)
          (mk-line "M" (list xPipe1 yIlRightWall) (list xPipe2 yIlNextLeftWall) nil)

          ;; Yellow Dashed Proposed Road Profile Line (Trimmed between xMh+375 and xNext-375)
          (mk-line "P" (list xPipe1 yGlRightWall) (list xPipe2 yGlNextLeftWall) 2500.0)

          ;; Pipe Specification MTEXT in Invert Level Row (or above datum if IL row unticked)
          (if yRowIL
            (if (< (* segLenM (/ 1000.0 hScale)) 18.0)
              (progn
                (setq pipeStr (strcat "{\\W0.85;" (nth 5 node) "%%C " (nth 6 node) "\\P1:" (nth 7 node) "}"))
                (mk-mtext "DT" "2" pipeStr (list xMid (+ yRowIL 1100.0)) 900.0 2)
              )
              (progn
                (setq pipeStr (strcat "{\\W0.85;" (nth 5 node) "%%C " (nth 6 node) " 1:" (nth 7 node) "}"))
                (mk-mtext "DT" "2" pipeStr (list xMid (+ yRowIL 450.0)) 900.0 2)
              )
            )
          )
        )
      )

      (setq idx (1+ idx))
    )

    ;; 4. Bottom Section Title & Scale Annotations positioned dynamically below yBottom
    (mk-text "TITLE" "T3"
             (strcat "LONG. SECTION OF " titleMHs)
             (list x0 (- yBottom 4000.0)) 2500.0 0.0 0.85 0 0)

    (mk-text "TITLE" "T3"
             (strcat "SCALE   HOR = 1:" (itoa (fix hScale)))
             (list (+ x0 500.0) (- yBottom 6300.0)) 900.0 0.0 0.85 0 0)

    (mk-text "TITLE" "T3"
             (strcat "        VER = 1:" (itoa (fix vScale)))
             (list (+ x0 500.0) (- yBottom 7600.0)) 900.0 0.0 0.85 0 0)

    ;; Return total width of this generated section
    (- xEnd x0)
  )

  ;; =========================================================================
  ;; 8. MAIN EXECUTION FLOW
  ;; =========================================================================
  (princ "\nCommand: SEWSECTION (Automated Sewerage Longitudinal Section Generator)")

  (if (boundp 'load_dialog)
    (setq dlgOK (ss:runDialog))
    (setq dlgOK 1)
  )

  (if (/= dlgOK 1)
    (princ "\n>> SEWSECTION cancelled.")
    (progn
      (setq hScaleVal (nth (atoi hScaleIdx) '(200.0 500.0 1000.0 1500.0 2000.0))
            vScaleVal (distof vScaleMode))

      (setq inputFile (getfiled "Select Sewerage Calculation CSV or Excel Spreadsheet" "" "csv;xlsx;xlsm;xls" 0))
      (if (not inputFile)
        (princ "\n>> No file selected. SEWSECTION cancelled.")
        (progn
          (setq isTempCsv nil csvFile inputFile)
          (if (wcmatch (strcase (vl-filename-extension inputFile)) ".XLS*")
            (progn
              (princ "\n>> Converting Excel spreadsheet to temporary CSV...")
              (setq csvFile (xlsx-to-temp-csv inputFile))
              (if csvFile
                (setq isTempCsv T)
                (progn
                  (alert "Could not convert Excel file via ActiveX. Please save the sheet as CSV (.csv) in Excel and try again.")
                  (exit)
                )
              )
            )
          )

          (setq sections (parse-sewer-csv csvFile (distof maxDistM) splitRule))
          (ss:cleanup)

          (if (null sections)
            (alert "No valid Sewerage Manhole & Pipe runs were found in the selected file.\n\nEnsure the sheet contains Manhole No., Length, Pipe Size, Gradient, Ground Level, and Invert Level columns.")
            (progn
              (ensure-layer "TITLE" 7 "Continuous")
              (ensure-layer "M"     6 "Continuous")
              (ensure-layer "P"     2 (ensure-dashed-ltype))
              (ensure-layer "DT"    7 "Continuous")

              (ensure-textstyle "T3" 0.85)
              (ensure-textstyle "2"  0.85)

              (princ (strcat "\n>> Parsed " (itoa (length sections))
                             " Longitudinal Section(s) from file (Limit: " maxDistM "m)."))

              (setvar "OSMODE" oldOs)
              (if (/= layoutMode "PICK")
                (setq ptStart (getpoint "\nClick Insertion Point for First Longitudinal Section: "))
              )

              (if (or (= layoutMode "PICK") ptStart)
                (progn
                  (command "_.UNDO" "_Begin")
                  (setvar "OSMODE" 0)
                  (if ptStart (setq curPt (trans ptStart 1 0)))
                  (setq secIdx 1 gapX 25000.0)

                  (foreach sec sections
                    (if (= layoutMode "PICK")
                      (progn
                        (setvar "OSMODE" oldOs)
                        (setq ptStart (getpoint (strcat "\nClick Insertion Point for Section ["
                                                        (itoa secIdx) "/" (itoa (length sections))
                                                        "] (" (nth 0 (car sec)) " to " (nth 0 (last sec)) "): ")))
                        (setvar "OSMODE" 0)
                        (if ptStart (setq curPt (trans ptStart 1 0)) (setq curPt nil))
                      )
                    )

                    (if curPt
                      (progn
                        (setq datumVal (compute-datum sec autoDatum baseDatumUnder10 manualDatum))
                        (setq secW (draw-long-section curPt sec hScaleVal vScaleVal datumVal))
                        (cond
                          ((= layoutMode "ROW")
                           (setq curPt (list (+ (car curPt) secW gapX) (cadr curPt) 0.0)))
                          ((= layoutMode "COL")
                           (setq curPt (list (car curPt) (- (cadr curPt) 95000.0) 0.0)))
                        )
                      )
                    )
                    (setq secIdx (1+ secIdx))
                  )

                  (command "_.UNDO" "_End")
                  (princ (strcat "\n>> SEWSECTION Complete! Generated " (itoa (length sections))
                                 " Longitudinal Section(s) at HOR 1:" (itoa (fix hScaleVal))
                                 ", VER 1:" (itoa (fix vScaleVal)) "."))
                )
                (princ "\n>> Insertion cancelled.")
              )
            )
          )
        )
      )
    )
  )

  (if oldDimzin (setvar "DIMZIN" oldDimzin))
  (if oldOs     (setvar "OSMODE" oldOs))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Command Aliases
(defun c:longsec () (c:sewsection))
(defun c:sewlong () (c:sewsection))
(princ "\nSEWSECTION loaded. Type SEWSECTION, LONGSEC, or SEWLONG to run.")
(princ)