;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : mathsim.lsp
;;; COMMAND     : MATHSIM (Aliases: TADD, TPLUS, TMINUS, ADDSIM)
;;; DESCRIPTION : Select Similar TEXT / MTEXT / MULTILEADER & Add or Subtract
;;;               a Numeric Value (Guaranteed Multileader + AutoCAD LT 2024 Safe)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:mathsim ( / *error* oldCmd oldDimzin
                     ;; --- helper functions ---
                     ms:esc ms:trim ms:cleanup ms:tmpName
                     ms:writeDCL ms:runDialog ms:askCmdLine
                     get-text put-text pick-source-text is-alnum-p
                     count-decimals format-num-like update-numbers-in-chunk
                     process-text-math
                     ;; --- state variables ---
                     srcEnt srcData srcType srcLayer rawSrcTxt
                     opMode valStr deltaVal scopeMode parseMode decMode
                     dclFile dclId dlgOK
                     ss filt i cnt e oldTxt newTxt )

  (setq oldCmd    (getvar "CMDECHO")
        oldDimzin (getvar "DIMZIN"))
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0) ;; Prevents AutoCAD LT from suppressing trailing ".00" zeros

  ;; =========================================================================
  ;; 1. STRING & WILDCARD HELPERS (Safe for "#JRK - ..." Layers)
  ;; =========================================================================
  ;; Escape wildcard characters (#, *, ?, @, ~, [, ], `, .) for ssget filters
  (defun ms:esc (str / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen str))
      (setq ch (substr str k 1))
      (setq out (strcat out (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ",")) "`" "") ch))
      (setq k (1+ k))
    )
    out
  )

  ;; Pure AutoLISP string trimmer
  (defun ms:trim (s / len i1 i2)
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

  (defun ms:cleanup ()
    (if (and dclId (> dclId 0))
      (progn (unload_dialog dclId) (setq dclId nil))
    )
    (if (and dclFile (findfile dclFile) (boundp 'vl-file-delete))
      (vl-file-delete dclFile)
    )
  )

  (defun *error* (msg)
    (ms:cleanup)
    (if oldDimzin (setvar "DIMZIN" oldDimzin))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[MATHSIM Error]: " msg))
    )
    (princ)
  )

  (defun ms:tmpName ( / tmpDir)
    (if (boundp 'vl-filename-mktemp)
      (vl-filename-mktemp "mathsim.dcl")
      (progn
        (setq tmpDir (cond ((getenv "TEMP")) ((getenv "TMP")) ((getvar "TEMPPREFIX")) (".")))
        (if (member (substr tmpDir (strlen tmpDir) 1) '("\\" "/"))
          (strcat tmpDir "mathsim.dcl")
          (strcat tmpDir "\\mathsim.dcl")
        )
      )
    )
  )

  ;; =========================================================================
  ;; 2. 3-LAYER UNIVERSAL TEXT READER & WRITER (TEXT, MTEXT, MULTILEADER)
  ;;    Works on AutoCAD LT 2024, Full AutoCAD, and GstarCAD
  ;; =========================================================================
  (defun get-text (ent / elist typ res obj item)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          res   nil)

    ;; Layer 1: Native AutoCAD / AutoCAD LT 2024 getpropertyvalue for MULTILEADER
    (if (and (= typ "MULTILEADER") (boundp 'getpropertyvalue))
      (setq res (vl-catch-all-apply 'getpropertyvalue (list ent "MText/Contents")))
    )

    ;; Layer 2: ActiveX TextString property
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (progn
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (and obj (not (vl-catch-all-error-p obj)))
          (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
        )
      )
    )

    ;; Layer 3: Pure DXF fallback (skips "LEADER_LINE{" in MULTILEADER)
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
         (if (and (assoc 1 elist) (= (type (cdr (assoc 1 elist))) 'STR))
           (setq res (cdr (assoc 1 elist)))
           (setq res "")
         ))
      )
    )
    (if (and res (= (type res) 'STR)) res "")
  )

  (defun put-text (ent str / elist typ newElist replaced item obj ok)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          ok    nil)

    (cond
      ;; A. MULTILEADER: Use setpropertyvalue + ActiveX first so AutoCAD LT 2024 never corrupts the MLeader
      ((= typ "MULTILEADER")
       (if (boundp 'setpropertyvalue)
         (if (not (vl-catch-all-error-p
                    (vl-catch-all-apply 'setpropertyvalue (list ent "MText/Contents" str))))
           (setq ok T)
         )
       )
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
       (if (and obj (not (vl-catch-all-error-p obj)))
         (progn
           (if (not (vl-catch-all-error-p
                      (vl-catch-all-apply 'vlax-put-property (list obj 'TextString str))))
             (setq ok T)
           )
           (vl-catch-all-apply 'vla-update (list obj))
         )
       )
       ;; Fallback to DXF 304 replacement if neither API was available
       (if (not ok)
         (progn
           (setq newElist nil replaced nil)
           (foreach item elist
             (if (and (not replaced)
                      (= (car item) 304)
                      (= (type (cdr item)) 'STR)
                      (/= (strcase (cdr item)) "LEADER_LINE{"))
               (progn
                 (setq newElist (cons (cons 304 str) newElist))
                 (setq replaced T)
               )
               (setq newElist (cons item newElist))
             )
           )
           (entmod (reverse newElist))
         )
       ))

      ;; B. MTEXT: Update DXF 1 (and clear overflow DXF 3 chunks) + sync ActiveX
      ((= typ "MTEXT")
       (setq newElist nil)
       (foreach item elist
         (if (/= (car item) 3)
           (if (= (car item) 1)
             (setq newElist (cons (cons 1 str) newElist))
             (setq newElist (cons item newElist))
           )
         )
       )
       (entmod (reverse newElist))
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
       (if (and obj (not (vl-catch-all-error-p obj)))
         (progn
           (vl-catch-all-apply 'vlax-put-property (list obj 'TextString str))
           (vl-catch-all-apply 'vla-update (list obj))
         )
       ))

      ;; C. Standard TEXT
      (T
       (if (assoc 1 elist)
         (entmod (subst (cons 1 str) (assoc 1 elist) elist))
       ))
    )
    (entupd ent)
  )

  ;; Click-through picker for TEXT, MTEXT, or MULTILEADER
  (defun pick-source-text (promptStr / loop sel e pt ssPick picked)
    (setq loop T picked nil)
    (while loop
      (setq sel (entsel promptStr))
      (if sel
        (progn
          (setq e  (car sel)
                pt (cadr sel))
          (if (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER"))
            (setq picked e loop nil)
            (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT,MULTILEADER"))))
              (setq picked (ssname ssPick 0) loop nil)
              (princ "\n>> Selected object is not TEXT, MTEXT, or MULTILEADER. Try again.")
            )
          )
        )
        (setq loop nil)
      )
    )
    picked
  )

  ;; =========================================================================
  ;; 3. SMART NUMBER PARSER & EXACT ".00" FORMATTER
  ;; =========================================================================
  (defun is-alnum-p (ch / a)
    (if (= ch "")
      nil
      (progn
        (setq a (ascii (strcase ch)))
        (or (and (>= a 48) (<= a 57))
            (and (>= a 65) (<= a 90)))
      )
    )
  )

  (defun count-decimals (numStr / p)
    (if (setq p (vl-string-search "." numStr))
      (- (strlen numStr) p 1)
      0
    )
  )

  ;; Formats newVal and forces exact trailing zeros (e.g. 7 -> "7.00", 7.5 -> "7.50")
  (defun format-num-like (newVal origNumStr deltaStr decSetting isLevelPrefix / decs hasPlus s dotPos curDecs)
    (setq hasPlus (= (substr origNumStr 1 1) "+"))
    (setq decs
      (cond
        ((= decSetting "2") 2)
        ((= decSetting "3") 3)
        ((= decSetting "0") 0)
        (T
         (max (count-decimals origNumStr)
              (count-decimals deltaStr)
              (if isLevelPrefix 2 0)))
      )
    )

    (setvar "DIMZIN" 0)
    (setq s (rtos newVal 2 decs))

    (if (= (substr s 1 1) ".")
      (setq s (strcat "0" s))
    )
    (if (= (substr s 1 2) "-.")
      (setq s (strcat "-0." (substr s 3)))
    )

    (if (> decs 0)
      (progn
        (setq dotPos (vl-string-search "." s))
        (if (null dotPos)
          (progn
            (setq s (strcat s "."))
            (repeat decs (setq s (strcat s "0")))
          )
          (progn
            (setq curDecs (- (strlen s) dotPos 1))
            (while (< curDecs decs)
              (setq s       (strcat s "0")
                    curDecs (1+ curDecs))
            )
          )
        )
      )
    )

    (strcat (if (and hasPlus (>= newVal 0.0)) "+" "") s)
  )

  ;; Updates numeric tokens inside a single line/chunk while skipping MTEXT inline codes
  (defun update-numbers-in-chunk (txt delta deltaStr pMode decSetting / len i out ch sec semi
                                                                        numStart numStr hasDot prevCh nextCh
                                                                        afterCh beforeStr isLevelLbl isGrad isIdToken newNum)
    (setq len (strlen txt) i 1 out "")
    (while (<= i len)
      (setq ch  (substr txt i 1)
            sec (if (< i len) (substr txt (1+ i) 1) ""))
      (cond
        ;; 1. Skip MTEXT inline formatting codes that end with ';' (\W0.5;, \f...;, \H...;, \C...;, \p...;)
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi
           (setq out (strcat out (substr txt i (- (+ semi 2) i)))
                 i   (+ semi 2))
           (setq out (strcat out ch)
                 i   (1+ i))
         ))

        ;; 2. Skip Unicode escape sequences (\U+00F8) and paragraph/format toggles (\P, \L, \O, \K)
        ((and (= ch "\\") (= (strcase sec) "U") (= (substr txt (+ i 2) 1) "+"))
         (setq out (strcat out (substr txt i 7))
               i   (+ i 7)))

        ((and (= ch "\\") (member sec '("P" "p" "L" "l" "O" "o" "K" "k" "~")))
         (setq out (strcat out (substr txt i 2))
               i   (+ i 2)))

        ;; 3. Skip AutoCAD %%c / %%d / %%p control codes
        ((and (= ch "%") (= sec "%"))
         (setq out (strcat out (substr txt i 3))
               i   (+ i 3)))

        ;; 4. Detect a numeric token
        ((or (and (>= (ascii ch) 48) (<= (ascii ch) 57))
             (and (or (= ch "-") (= ch "+"))
                  (< i len)
                  (>= (ascii sec) 48) (<= (ascii sec) 57)
                  (not (is-alnum-p (if (> i 1) (substr txt (1- i) 1) "")))))
         (setq prevCh   (if (> i 1) (substr txt (1- i) 1) "")
               numStart i
               numStr   ch
               hasDot   nil
               i        (1+ i))
         (while (<= i len)
           (setq nextCh (substr txt i 1))
           (cond
             ((and (>= (ascii nextCh) 48) (<= (ascii nextCh) 57))
              (setq numStr (strcat numStr nextCh) i (1+ i)))
             ((and (= nextCh ".") (not hasDot) (< i len)
                   (>= (ascii (substr txt (1+ i) 1)) 48)
                   (<= (ascii (substr txt (1+ i) 1)) 57))
              (setq hasDot T numStr (strcat numStr nextCh) i (1+ i)))
             (T
              (setq len (- len)))
           )
         )
         (if (< len 0) (setq len (- len)))

         (setq afterCh    (if (<= i len) (substr txt i 1) "")
               beforeStr  (strcase (if (> numStart 1) (substr txt 1 (1- numStart)) ""))
               isLevelLbl (wcmatch beforeStr "*FFL*,*SFL*,*DFL*,*SIL*,*IL*,*GL*,*RL*,*CL*,*FL*,*TWL*,*BWL*,*INVERT*,*LEVEL*")
               isGrad     (or (= prevCh ":") (and (= afterCh ":") (= numStr "1")))
               isIdToken  (and (not hasDot)
                               (or (and (/= prevCh "")
                                        (>= (ascii (strcase prevCh)) 65)
                                        (<= (ascii (strcase prevCh)) 90)
                                        (not isLevelLbl)
                                        (not (wcmatch beforeStr "*DATUM*")))
                                   (wcmatch beforeStr "*MH-,*MH ,*DR-,*NODE-,*NODE "))))

         (if (and (= pMode "SMART") (or isGrad isIdToken))
           (setq out (strcat out numStr))
           (progn
             (setq newNum (+ (distof numStr) delta))
             (setq out (strcat out (format-num-like newNum numStr deltaStr decSetting isLevelLbl)))
           )
         ))

        ;; 5. Any other character: copy as-is
        (T
         (setq out (strcat out ch)
               i   (1+ i)))
      )
    )
    out
  )

  ;; Splits multi-line MTEXT/MLeader by \P so Smart Mode updates GL & IL while protecting MH ID and DEPTH
  (defun process-text-math (rawTxt delta deltaStr pMode decSetting / uTxt pPos lines cur rest uLine outLines ln)
    (setq uTxt (strcase rawTxt))
    (if (and (= pMode "SMART")
             (wcmatch uTxt "*\\P*")
             (wcmatch uTxt "*GL*,*IL*"))
      (progn
        (setq rest rawTxt lines nil)
        (while (setq pPos (vl-string-search "\\P" (strcase rest)))
          (setq lines (append lines (list (substr rest 1 pPos)))
                rest  (substr rest (+ pPos 3)))
        )
        (setq lines (append lines (list rest))
              outLines nil)

        (foreach ln lines
          (setq uLine (strcase ln))
          (if (and (wcmatch uLine "*FFL*,*SFL*,*GL=*,*GL:*,*IL=*,*IL:*,*SIL*,*RL*,*INV*")
                   (not (wcmatch uLine "*D=*,*DEPTH*")))
            (setq outLines (append outLines (list (update-numbers-in-chunk ln delta deltaStr pMode decSetting))))
            (setq outLines (append outLines (list ln)))
          )
        )

        (setq cur (car outLines))
        (foreach ln (cdr outLines)
          (setq cur (strcat cur "\\P" ln))
        )
        cur
      )
      (update-numbers-in-chunk rawTxt delta deltaStr pMode decSetting)
    )
  )

  ;; =========================================================================
  ;; 4. INTERACTIVE POPUP DIALOG (DCL) & CLI FALLBACK
  ;; =========================================================================
  (defun ms:writeDCL (path / fh)
    (if (setq fh (open path "w"))
      (progn
        (write-line "mathsim : dialog {" fh)
        (write-line "  label = \"MATHSIM - Select Similar Text/MText/MLeader & Add / Minus Value\";" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"1. Source Object Reference\";" fh)
        (write-line "    : text { key = \"srclay\"; label = \"Layer: \"; width = 48; }" fh)
        (write-line "    : edit_box { key = \"srctxt\"; label = \"Sample Text:\"; edit_width = 36; is_enabled = false; }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"2. Math Operation & Value\";" fh)
        (write-line "    : radio_row {" fh)
        (write-line "      : radio_button { key = \"op_add\"; label = \"+ Add Value\"; }" fh)
        (write-line "      : radio_button { key = \"op_sub\"; label = \"- Minus Value\"; }" fh)
        (write-line "    }" fh)
        (write-line "    : edit_box { key = \"valstr\"; label = \"Value to Add / Minus (e.g. 2 or 0.50):\"; edit_width = 16; }" fh)
        (write-line "    : popup_list { key = \"decmode\"; label = \"Decimal Precision:\"; width = 28; }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_radio_column {" fh)
        (write-line "    label = \"3. Number Detection Mode\";" fh)
        (write-line "    : radio_button { key = \"pm_smart\"; label = \"Smart Level Mode (Updates FFL/SIL/GL/IL; protects IDs & Depth)\"; }" fh)
        (write-line "    : radio_button { key = \"pm_all\";   label = \"All Visible Numbers (Updates every number in text)\"; }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_radio_column {" fh)
        (write-line "    label = \"4. Select Similar Scope\";" fh)
        (write-line "    : radio_button { key = \"sc_window\"; label = \"Window / Select on Screen (Filtered to Same Layer)\"; }" fh)
        (write-line "    : radio_button { key = \"sc_layer\";  label = \"All Similar Text on Same Layer (Entire Drawing)\"; }" fh)
        (write-line "    : radio_button { key = \"sc_exact\";  label = \"Only Identical Text Value on Same Layer\"; }" fh)
        (write-line "    : radio_button { key = \"sc_single\"; label = \"Only the Picked Object\"; }" fh)
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

  (defun ms:runDialog ( / res)
    (setq res nil dclFile (ms:tmpName))
    (if (ms:writeDCL dclFile)
      (progn
        (setq dclId (load_dialog dclFile))
        (if (and dclId (>= dclId 1) (new_dialog "mathsim" dclId))
          (progn
            (set_tile "srclay" (strcat "Layer: " srcLayer "  |  Type: " srcType))
            (set_tile "srctxt" rawSrcTxt)

            (set_tile "op_add" (if (= opMode "ADD") "1" "0"))
            (set_tile "op_sub" (if (= opMode "SUB") "1" "0"))
            (set_tile "valstr" valStr)

            (start_list "decmode")
            (mapcar 'add_list '("Auto (Keep 0.00 / Original Decimals)" "2 Decimal Places (0.00)" "3 Decimal Places (0.000)" "Integer (0)"))
            (end_list)
            (set_tile "decmode" "0")

            (set_tile "pm_smart" (if (= parseMode "SMART") "1" "0"))
            (set_tile "pm_all"   (if (= parseMode "ALL")   "1" "0"))

            (set_tile "sc_window" (if (= scopeMode "WINDOW") "1" "0"))
            (set_tile "sc_layer"  (if (= scopeMode "LAYER")  "1" "0"))
            (set_tile "sc_exact"  (if (= scopeMode "EXACT")  "1" "0"))
            (set_tile "sc_single" (if (= scopeMode "SINGLE") "1" "0"))

            (mode_tile "valstr" 2)

            (action_tile "accept"
              (strcat
                "(setq opMode (if (= (get_tile \"op_sub\") \"1\") \"SUB\" \"ADD\") "
                "valStr (ms:trim (get_tile \"valstr\")) "
                "decMode (cond ((= (get_tile \"decmode\") \"1\") \"2\") ((= (get_tile \"decmode\") \"2\") \"3\") ((= (get_tile \"decmode\") \"3\") \"0\") (T \"AUTO\")) "
                "parseMode (if (= (get_tile \"pm_all\") \"1\") \"ALL\" \"SMART\") "
                "scopeMode (cond ((= (get_tile \"sc_layer\") \"1\") \"LAYER\") ((= (get_tile \"sc_exact\") \"1\") \"EXACT\") ((= (get_tile \"sc_single\") \"1\") \"SINGLE\") (T \"WINDOW\"))) "
                "(if (or (= valStr \"\") (null (distof valStr))) "
                "(set_tile \"error\" \"Please enter a valid number (e.g. 2 or 0.50).\") "
                "(done_dialog 1))"))

            (action_tile "cancel" "(done_dialog 0)")
            (setq res (start_dialog))
          )
        )
      )
    )
    (ms:cleanup)
    res
  )

  (defun ms:askCmdLine ( / ans)
    (initget "Add Minus")
    (setq ans (getkword "\nSelect operation [Add/Minus] <Add>: "))
    (setq opMode (if (= ans "Minus") "SUB" "ADD"))
    (setq valStr (ms:trim (getstring T (strcat "\nEnter value to " (if (= opMode "SUB") "Minus" "Add") " (e.g. 2 or 0.50): "))))
    (if (or (null valStr) (= valStr "") (null (distof valStr)))
      0
      (progn
        (initget "Window Layer Exact Single")
        (setq ans (getkword "\nApply to [Window on layer/Layer all/Exact match/Single] <Window>: "))
        (setq scopeMode (cond ((= ans "Layer") "LAYER") ((= ans "Exact") "EXACT") ((= ans "Single") "SINGLE") (T "WINDOW")))
        1
      )
    )
  )

  ;; =========================================================================
  ;; 5. MAIN EXECUTION WORKFLOW
  ;; =========================================================================
  (princ "\nCommand: MATHSIM (Select Similar Text/MText/MLeader & Add or Minus Value)")

  (setq srcEnt (pick-source-text "\nClick reference TEXT, MTEXT, or MULTILEADER: "))
  (if (not srcEnt)
    (princ "\n>> Nothing selected. MATHSIM cancelled.")
    (progn
      (setq srcData   (entget srcEnt)
            srcType   (cdr (assoc 0 srcData))
            srcLayer  (cdr (assoc 8 srcData))
            rawSrcTxt (get-text srcEnt)
            opMode    "ADD"
            valStr    "0.50"
            decMode   "AUTO"
            parseMode "SMART"
            scopeMode "WINDOW")

      (setq dlgOK nil)
      (if (boundp 'load_dialog)
        (setq dlgOK (ms:runDialog))
      )
      (if (null dlgOK)
        (setq dlgOK (ms:askCmdLine))
      )

      (if (/= dlgOK 1)
        (princ "\n>> MATHSIM cancelled.")
        (progn
          ;; Calculate signed delta value
          (setq deltaVal (distof valStr))
          (if (and (= opMode "SUB") (> deltaVal 0.0))
            (setq deltaVal (- deltaVal))
          )

          ;; Build target selection set with wildcard-escaped layer name (critical for "#JRK - ..." layers!)
          (setq filt (list '(0 . "TEXT,MTEXT,MULTILEADER") (cons 8 (ms:esc srcLayer))))
          (cond
            ((= scopeMode "SINGLE")
             (setq ss (ssadd srcEnt (ssadd))))

            ((= scopeMode "WINDOW")
             (princ (strcat "\n>> Select similar objects on layer [" srcLayer "] (or press Enter to update picked object only): "))
             (setq ss (ssget filt))
             (if (not ss)
               (setq ss (ssadd srcEnt (ssadd)))
             ))

            ((or (= scopeMode "LAYER") (= scopeMode "EXACT"))
             (setq ss (ssget "_X" (append filt (list (cons 410 (getvar "CTAB")))))))
          )

          (if ss
            (progn
              (command "_.UNDO" "_Begin")
              (setq i 0 cnt 0)
              (while (< i (sslength ss))
                (setq e      (ssname ss i)
                      oldTxt (get-text e))
                (if (or (/= scopeMode "EXACT") (= oldTxt rawSrcTxt))
                  (progn
                    (setq newTxt (process-text-math oldTxt deltaVal valStr parseMode decMode))
                    (if (/= newTxt oldTxt)
                      (progn
                        (put-text e newTxt)
                        (setq cnt (1+ cnt))
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
              (command "_.UNDO" "_End")
              (princ (strcat "\n>> MATHSIM Complete! Applied "
                             (if (>= deltaVal 0.0) (strcat "+" (rtos deltaVal 2 2)) (rtos deltaVal 2 2))
                             " to " (itoa cnt) " object(s) on layer [" srcLayer "]."))
            )
            (princ "\n>> No matching objects found.")
          )
        )
      )
    )
  )

  (if oldDimzin (setvar "DIMZIN" oldDimzin))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Command Aliases
(defun c:tadd ()   (c:mathsim))
(defun c:tplus ()  (c:mathsim))
(defun c:tminus () (c:mathsim))
(defun c:addsim () (c:mathsim))
(princ "\nMATHSIM loaded. Type MATHSIM, TADD, TMINUS, or ADDSIM to run.")
(princ)