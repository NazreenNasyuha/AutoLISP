;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : repsim.lsp
;;; COMMAND     : REPSIM (Alias: CPSTYLE)
;;; DESCRIPTION : Intelligent Global Text / MText / MLeader Search & Replace
;;;               + Canva/Word-Style Format Painter (Layer, Color, Text Style,
;;;               Height, Width Factor, Inline MTEXT Font/Width, Linetype)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:REPSIM ( / *error* oldCmd
                    ;; --- helper functions ---
                    rs:cleanup rs:tmpName rs:writeDCL
                    get-text put-text split-mtext-style
                    extract-style set-dxf rem-dxf apply-style-to-ent color-name
                    pick-object run-dialog ask-cmdline
                    ;; --- target & donor variables ---
                    targetEnt targetData targetType rawTargetTxt
                    targetParsed targetPre targetBody targetSuf srcLayer
                    donorEnt styleData hasDonor
                    ;; --- dialog state variables ---
                    newTxt keepTextOnly scopeMode
                    doLayer doColor doTxtStyle doHeight doWidth doInline doProps
                    dclFile dclId dlgStatus loopDialog
                    ;; --- execution variables ---
                    ss i cnt e ed eType eLayer eRaw eParsed ePre eBody eSuf
                    finalBody finalPre finalSuf finalFullTxt )

  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; =========================================================================
  ;; 1. RESOURCE CLEANUP & ERROR HANDLING
  ;; =========================================================================
  (defun rs:cleanup ()
    (if (and dclId (> dclId 0))
      (progn (unload_dialog dclId) (setq dclId nil))
    )
    (if (and dclFile (findfile dclFile) (boundp 'vl-file-delete))
      (vl-file-delete dclFile)
    )
  )

  (defun *error* (msg)
    (rs:cleanup)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[REPSIM Error]: " msg))
    )
    (princ)
  )

  (defun rs:tmpName ()
    (if (boundp 'vl-filename-mktemp)
      (vl-filename-mktemp "repsim.dcl")
      (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) (".")) "\\repsim_" (itoa (getvar "MILLISECS")) ".dcl")
    )
  )

  ;; =========================================================================
  ;; 2. UNIVERSAL TEXT READER, WRITER & MTEXT INLINE STYLE PARSER
  ;; =========================================================================
  (defun get-text (ent / elist typ obj res item)
    (setq elist (entget ent) typ (cdr (assoc 0 elist)) res nil)
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
    )
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (cond
        ((= typ "MULTILEADER")
         (setq res "")
         (foreach item elist
           (if (and (= (car item) 304)
                    (/= (strcase (cdr item)) "LEADER_LINE{")
                    (= res ""))
             (setq res (cdr item))
           )
         ))
        ((= typ "MTEXT")
         (setq res "")
         (foreach item elist
           (if (or (= (car item) 3) (= (car item) 1))
             (setq res (strcat res (cdr item)))
           )
         ))
        (T
         (setq res (cdr (assoc 1 elist))))
      )
    )
    (if (and res (= (type res) 'STR)) res "")
  )

  (defun put-text (ent str / elist typ newElist replaced obj)
    (setq elist (entget ent) typ (cdr (assoc 0 elist)))
    (cond
      ((= typ "MULTILEADER")
       (setq newElist nil replaced nil)
       (foreach item elist
         (if (and (not replaced)
                  (= (car item) 304)
                  (/= (strcase (cdr item)) "LEADER_LINE{"))
           (progn
             (setq newElist (cons (cons 304 str) newElist))
             (setq replaced T)
           )
           (setq newElist (cons item newElist))
         )
       )
       (entmod (reverse newElist)))

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
       (entmod (reverse newElist)))

      ((member typ '("TEXT" "ATTRIB" "ATTDEF"))
       (if (assoc 1 elist)
         (entmod (subst (cons 1 str) (assoc 1 elist) elist))
       ))
    )
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (progn
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextString str))
        (vl-catch-all-apply 'vla-update (list obj))
      )
    )
    (entupd ent)
  )

  ;; Splits "{\fArial;\W0.5;Hello}" -> (list "{\fArial;\W0.5;" "Hello" "}")
  (defun split-mtext-style (txt / len i braceCnt loopP ch sec semi pre rem endIdx stripped body suf)
    (setq len (strlen txt) i 1 braceCnt 0 loopP T)
    (while (and loopP (<= i len))
      (setq ch  (substr txt i 1))
      (setq sec (if (< i len) (substr txt (1+ i) 1) ""))
      (cond
        ((= ch "{")
         (setq braceCnt (1+ braceCnt) i (1+ i)))
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi (setq i (+ semi 2)) (setq loopP nil)))
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )
    (setq pre (if (> i 1) (substr txt 1 (1- i)) ""))
    (setq rem (substr txt i))

    (setq endIdx (strlen rem) stripped 0)
    (while (and (< stripped braceCnt) (> endIdx 0) (= (substr rem endIdx 1) "}"))
      (setq endIdx (1- endIdx) stripped (1+ stripped))
    )
    (setq body (if (> endIdx 0) (substr rem 1 endIdx) ""))
    (setq suf "")
    (repeat braceCnt (setq suf (strcat suf "}")))
    (list pre body suf)
  )

  ;; Readable color name for the Dialog UI
  (defun color-name (c / )
    (cond
      ((or (null c) (= c 256)) "ByLayer")
      ((= c 0) "ByBlock")
      ((= c 1) "1 (Red)")
      ((= c 2) "2 (Yellow)")
      ((= c 3) "3 (Green)")
      ((= c 4) "4 (Cyan)")
      ((= c 5) "5 (Blue)")
      ((= c 6) "6 (Magenta)")
      ((= c 7) "7 (White)")
      ((= c 8) "8 (Grey)")
      (T (strcat "ACI " (itoa c)))
    )
  )

  ;; =========================================================================
  ;; 3. STYLE EXTRACTOR & APPLIER (CANVA / WORD FORMAT PAINTER ENGINE)
  ;; =========================================================================
  (defun extract-style (ent / ed typ obj raw parsed lay col trueCol ltype lscale lweight
                              tStyle tHgt tWid tObl lineSpc mlStyle)
    (setq ed      (entget ent)
          typ     (cdr (assoc 0 ed))
          obj     (vl-catch-all-apply 'vlax-ename->vla-object (list ent))
          raw     (if (member typ '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB" "ATTDEF")) (get-text ent) "")
          parsed  (split-mtext-style raw)
          lay     (cdr (assoc 8 ed))
          col     (if (assoc 62 ed) (cdr (assoc 62 ed)) 256)
          trueCol (if (assoc 420 ed) (cdr (assoc 420 ed)) nil)
          ltype   (if (assoc 6 ed) (cdr (assoc 6 ed)) "BYLAYER")
          lscale  (if (assoc 48 ed) (cdr (assoc 48 ed)) 1.0)
          lweight (if (assoc 370 ed) (cdr (assoc 370 ed)) -1)
          tStyle  (if (assoc 7 ed) (cdr (assoc 7 ed)) (getvar "TEXTSTYLE"))
          tHgt    (cond
                    ((= typ "MULTILEADER")
                     (if (and obj (not (vl-catch-all-error-p obj)) (vlax-property-available-p obj 'TextHeight))
                       (vl-catch-all-apply 'vlax-get-property (list obj 'TextHeight))
                       (cdr (assoc 140 ed))))
                    ((assoc 40 ed) (cdr (assoc 40 ed)))
                    (T nil))
          tWid    (if (and (= typ "TEXT") (assoc 41 ed)) (cdr (assoc 41 ed)) 1.0)
          tObl    (if (assoc 51 ed) (cdr (assoc 51 ed)) 0.0)
          lineSpc (if (assoc 44 ed) (cdr (assoc 44 ed)) nil)
          mlStyle (if (= typ "MULTILEADER")
                    (if (and obj (not (vl-catch-all-error-p obj)) (vlax-property-available-p obj 'StyleName))
                      (vl-catch-all-apply 'vlax-get-property (list obj 'StyleName))
                      nil)
                    nil))

    ;; If MULTILEADER, also read its TextStyleName via ActiveX
    (if (and (= typ "MULTILEADER") obj (not (vl-catch-all-error-p obj)) (vlax-property-available-p obj 'TextStyleName))
      (setq tStyle (vl-catch-all-apply 'vlax-get-property (list obj 'TextStyleName)))
    )

    (list
      (cons "TYPE"      typ)
      (cons "LAYER"     lay)
      (cons "COLOR"     col)
      (cons "TRUECOLOR" trueCol)
      (cons "LTYPE"     ltype)
      (cons "LSCALE"    lscale)
      (cons "LWEIGHT"   lweight)
      (cons "TSTYLE"    (if (and tStyle (= (type tStyle) 'STR)) tStyle "Standard"))
      (cons "THGT"      (if (and tHgt (numberp tHgt) (> tHgt 0.0)) tHgt nil))
      (cons "TWID"      tWid)
      (cons "TOBL"      tObl)
      (cons "LINESP"    lineSpc)
      (cons "MLSTYLE"   mlStyle)
      (cons "PRE"       (nth 0 parsed))
      (cons "BODY"      (nth 1 parsed))
      (cons "SUF"       (nth 2 parsed))
    )
  )

  ;; Helper to set or replace a DXF group code on an entity list
  (defun set-dxf (ed code val)
    (if (assoc code ed)
      (subst (cons code val) (assoc code ed) ed)
      (append ed (list (cons code val)))
    )
  )

  ;; Remove a DXF group code from an entity list
  (defun rem-dxf (ed code)
    (vl-remove-if '(lambda (x) (= (car x) code)) ed)
  )

  ;; Apply checked style properties from styleData onto target ent
  (defun apply-style-to-ent (ent sData / ed typ obj col trueCol ltype lscale lweight
                                         tStyle tHgt tWid tObl lineSpc mlStyle curTxt)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed)))

    ;; 1. Layer
    (if (= doLayer "1")
      (setq ed (set-dxf ed 8 (cdr (assoc "LAYER" sData))))
    )

    ;; 2. Color (Handles both ByLayer and explicit ACI / TrueColor)
    (if (= doColor "1")
      (progn
        (setq col     (cdr (assoc "COLOR" sData))
              trueCol (cdr (assoc "TRUECOLOR" sData)))
        (setq ed (set-dxf ed 62 col))
        (if trueCol
          (setq ed (set-dxf ed 420 trueCol))
          (setq ed (rem-dxf ed 420))
        )
      )
    )

    ;; 3. Linetype, Linetype Scale & Lineweight
    (if (= doProps "1")
      (progn
        (setq ltype   (cdr (assoc "LTYPE" sData))
              lscale  (cdr (assoc "LSCALE" sData))
              lweight (cdr (assoc "LWEIGHT" sData)))
        (if ltype   (setq ed (set-dxf ed 6 ltype)))
        (if lscale  (setq ed (set-dxf ed 48 lscale)))
        (if lweight (setq ed (set-dxf ed 370 lweight)))
      )
    )

    ;; 4. Text Style
    (if (and (= doTxtStyle "1") (member typ '("TEXT" "MTEXT" "ATTRIB" "ATTDEF")))
      (progn
        (setq tStyle (cdr (assoc "TSTYLE" sData)))
        (if (and tStyle (tblsearch "STYLE" tStyle))
          (setq ed (set-dxf ed 7 tStyle))
        )
      )
    )

    ;; 5. Text Height & Line Spacing
    (if (= doHeight "1")
      (progn
        (setq tHgt   (cdr (assoc "THGT" sData))
              lineSpc (cdr (assoc "LINESP" sData)))
        (if (and tHgt (member typ '("TEXT" "MTEXT" "ATTRIB" "ATTDEF")))
          (setq ed (set-dxf ed 40 tHgt))
        )
        (if (and lineSpc (= typ "MTEXT"))
          (setq ed (set-dxf ed 44 lineSpc))
        )
      )
    )

    ;; 6. Width Factor & Oblique Angle (for single-line TEXT)
    (if (and (= doWidth "1") (member typ '("TEXT" "ATTRIB" "ATTDEF")))
      (progn
        (setq tWid (cdr (assoc "TWID" sData))
              tObl (cdr (assoc "TOBL" sData)))
        (if tWid (setq ed (set-dxf ed 41 tWid)))
        (if tObl (setq ed (set-dxf ed 51 tObl)))
      )
    )

    (entmod ed)

    ;; 7. ActiveX Sync (Essential for MULTILEADER and TrueColor/ByLayer refresh)
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (progn
        (if (= doLayer "1")
          (vl-catch-all-apply 'vlax-put-property (list obj 'Layer (cdr (assoc "LAYER" sData))))
        )
        (if (= doColor "1")
          (vl-catch-all-apply 'vlax-put-property (list obj 'Color (cdr (assoc "COLOR" sData))))
        )
        (if (= typ "MULTILEADER")
          (progn
            (setq curTxt  (get-text ent)
                  mlStyle (cdr (assoc "MLSTYLE" sData))
                  tStyle  (cdr (assoc "TSTYLE" sData))
                  tHgt    (cdr (assoc "THGT" sData)))
            (if (and (= doTxtStyle "1") mlStyle)
              (progn
                (vl-catch-all-apply 'vlax-put-property (list obj 'StyleName mlStyle))
                (vl-catch-all-apply 'vlax-put-property (list obj 'TextString curTxt))
              )
            )
            (if (and (= doTxtStyle "1") tStyle (tblsearch "STYLE" tStyle))
              (vl-catch-all-apply 'vlax-put-property (list obj 'TextStyleName tStyle))
            )
            (if (and (= doHeight "1") tHgt)
              (vl-catch-all-apply 'vlax-put-property (list obj 'TextHeight tHgt))
            )
          )
        )
        (vl-catch-all-apply 'vla-update (list obj))
      )
    )
    (entupd ent)
  )

  ;; Click-through object picker
  (defun pick-object (promptStr / loop sel e pt ssPick picked)
    (setq loop T picked nil)
    (while loop
      (setq sel (entsel promptStr))
      (if sel
        (progn
          (setq e  (car sel)
                pt (cadr sel))
          (if (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB" "ATTDEF"))
            (setq picked e loop nil)
            (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT,MULTILEADER"))))
              (setq picked (ssname ssPick 0) loop nil)
              (setq picked e loop nil)
            )
          )
        )
        (setq loop nil)
      )
    )
    picked
  )

  ;; =========================================================================
  ;; 4. DYNAMIC DCL DIALOG (WITH CANVA/WORD STYLE PAINTER)
  ;; =========================================================================
  (defun rs:writeDCL (path / fh)
    (if (setq fh (open path "w"))
      (progn
        (write-line "repsim : dialog {" fh)
        (write-line "  label = \"REPSIM v2.0 - Replace Similar & Copy/Paste Style (Format Painter)\";" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"1. Text Content Replacement\";" fh)
        (write-line "    : edit_box { key = \"oldtxt\"; label = \"Target Text:\"; edit_width = 42; is_enabled = false; }" fh)
        (write-line "    : edit_box { key = \"newtxt\"; label = \"Replace With:\"; edit_width = 42; }" fh)
        (write-line "    : toggle { key = \"styleonly\"; label = \"Style Painter Only (Keep each object's existing text wording)\"; }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"2. Copy & Paste Style (Like Canva / Word Format Painter)\";" fh)
        (write-line "    : row {" fh)
        (write-line "      : button { key = \"pickstyle\"; label = \"Pick Object to Copy Style From...\"; width = 30; }" fh)
        (write-line "      : text { key = \"stylesum\"; label = \"Source: Target's Own Style\"; width = 32; }" fh)
        (write-line "    }" fh)
        (write-line "    : row {" fh)
        (write-line "      : column {" fh)
        (write-line "        : toggle { key = \"dolayer\";  label = \"Layer\"; }" fh)
        (write-line "        : toggle { key = \"docolor\";  label = \"Color (ACI / TrueColor)\"; }" fh)
        (write-line "        : toggle { key = \"dostyle\";  label = \"Text Style (Font)\"; }" fh)
        (write-line "      }" fh)
        (write-line "      : column {" fh)
        (write-line "        : toggle { key = \"doheight\"; label = \"Text Height & Line Spacing\"; }" fh)
        (write-line "        : toggle { key = \"doinline\"; label = \"MTEXT Inline Style ({\\\\W0.5;, {\\\\f...;)\"; }" fh)
        (write-line "        : toggle { key = \"doprops\";  label = \"Width Factor, Linetype & Weight\"; }" fh)
        (write-line "      }" fh)
        (write-line "    }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_radio_column {" fh)
        (write-line "    label = \"3. Where to Apply (Target Scope)\";" fh)
        (write-line "    : radio_button { key = \"sc_layer\";  label = \"All Identical Text on Same Layer\"; }" fh)
        (write-line "    : radio_button { key = \"sc_global\"; label = \"All Identical Text in Entire Drawing\"; }" fh)
        (write-line "    : radio_button { key = \"sc_alllay\"; label = \"ALL Text Objects on Target Layer (Regardless of wording)\"; }" fh)
        (write-line "    : radio_button { key = \"sc_manual\"; label = \"Manually Select Target Objects on Screen (Paintbrush Mode)\"; }" fh)
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

  (defun run-dialog ( / result sumStr)
    (setq result nil dclFile (rs:tmpName))
    (if (rs:writeDCL dclFile)
      (progn
        (setq dclId (load_dialog dclFile))
        (if (and dclId (>= dclId 1) (new_dialog "repsim" dclId))
          (progn
            (set_tile "oldtxt" targetBody)
            (set_tile "newtxt" newTxt)
            (set_tile "styleonly" keepTextOnly)
            (if (= keepTextOnly "1") (mode_tile "newtxt" 1) (mode_tile "newtxt" 0))

            ;; Style Summary Banner
            (setq sumStr
              (strcat (if hasDonor "Copied: " "Current: ")
                      (cdr (assoc "LAYER" styleData)) " | "
                      (color-name (cdr (assoc "COLOR" styleData))) " | "
                      (cdr (assoc "TSTYLE" styleData))
                      (if (cdr (assoc "THGT" styleData))
                        (strcat " | H=" (rtos (cdr (assoc "THGT" styleData)) 2 1))
                        "")))
            (set_tile "stylesum" sumStr)

            ;; Style Checkboxes
            (set_tile "dolayer"  doLayer)
            (set_tile "docolor"  doColor)
            (set_tile "dostyle"  doTxtStyle)
            (set_tile "doheight" doHeight)
            (set_tile "doinline" doInline)
            (set_tile "doprops"  doProps)

            ;; Scope Radio Buttons
            (set_tile "sc_layer"  (if (= scopeMode "LAYER")  "1" "0"))
            (set_tile "sc_global" (if (= scopeMode "GLOBAL") "1" "0"))
            (set_tile "sc_alllay" (if (= scopeMode "ALLLAY") "1" "0"))
            (set_tile "sc_manual" (if (= scopeMode "MANUAL") "1" "0"))

            (mode_tile "newtxt" 2)

            ;; Toggle "Style Painter Only" disables/enables Replace With box
            (action_tile "styleonly"
              "(setq keepTextOnly $value) (if (= keepTextOnly \"1\") (mode_tile \"newtxt\" 1) (mode_tile \"newtxt\" 0))")

            ;; Button: Pick Style Donor Object on screen (returns code 2)
            (action_tile "pickstyle"
              (strcat
                "(setq newTxt (get_tile \"newtxt\") keepTextOnly (get_tile \"styleonly\") "
                "doLayer (get_tile \"dolayer\") doColor (get_tile \"docolor\") "
                "doTxtStyle (get_tile \"dostyle\") doHeight (get_tile \"doheight\") "
                "doInline (get_tile \"doinline\") doProps (get_tile \"doprops\") "
                "scopeMode (cond ((= (get_tile \"sc_global\") \"1\") \"GLOBAL\") "
                "((= (get_tile \"sc_alllay\") \"1\") \"ALLLAY\") "
                "((= (get_tile \"sc_manual\") \"1\") \"MANUAL\") (T \"LAYER\"))) "
                "(done_dialog 2)"))

            ;; Button: OK (returns code 1)
            (action_tile "accept"
              (strcat
                "(setq newTxt (get_tile \"newtxt\") keepTextOnly (get_tile \"styleonly\") "
                "doLayer (get_tile \"dolayer\") doColor (get_tile \"docolor\") "
                "doTxtStyle (get_tile \"dostyle\") doHeight (get_tile \"doheight\") "
                "doInline (get_tile \"doinline\") doProps (get_tile \"doprops\") "
                "scopeMode (cond ((= (get_tile \"sc_global\") \"1\") \"GLOBAL\") "
                "((= (get_tile \"sc_alllay\") \"1\") \"ALLLAY\") "
                "((= (get_tile \"sc_manual\") \"1\") \"MANUAL\") (T \"LAYER\"))) "
                "(if (and (= keepTextOnly \"0\") (= newTxt \"\")) "
                "(set_tile \"error\" \"Enter replacement text or check 'Style Painter Only'.\") "
                "(done_dialog 1))"))

            (action_tile "cancel" "(done_dialog 0)")
            (setq result (start_dialog))
          )
        )
      )
    )
    (rs:cleanup)
    result
  )

  ;; CLI Fallback if DCL is unavailable
  (defun ask-cmdline ( / ans dEnt)
    (setq ans (getstring T (strcat "\nReplace Text With <keep \"" targetBody "\">: ")))
    (if (= ans "")
      (setq keepTextOnly "1" newTxt targetBody)
      (setq keepTextOnly "0" newTxt ans)
    )
    (initget "Yes No")
    (setq ans (getkword "\nCopy style (Layer, Color, Font, Height) from another object? [Yes/No] <No>: "))
    (if (= ans "Yes")
      (if (setq dEnt (pick-object "\nClick Donor Object to copy style from: "))
        (setq styleData (extract-style dEnt)
              hasDonor  T
              doLayer   "1" doColor "1" doTxtStyle "1" doHeight "1" doWidth "1" doInline "1" doProps "1")
      )
    )
    (initget "Layer Global AllLayer Manual")
    (setq ans (getkword "\nApply to [Layer similar/Global similar/AllLayer texts/Manual pick] <Layer>: "))
    (setq scopeMode
      (cond
        ((= ans "Global")   "GLOBAL")
        ((= ans "AllLayer") "ALLLAY")
        ((= ans "Manual")   "MANUAL")
        (T "LAYER")
      )
    )
    1
  )

  ;; =========================================================================
  ;; 5. MAIN EXECUTION FLOW
  ;; =========================================================================
  (princ "\nCommand: REPSIM v2.0 (Replace Similar + Canva/Word Style Painter)")

  (setq targetEnt (pick-object "\nSelect Target Object (to find similar or restyle): "))
  (cond
    ((null targetEnt)
     (princ "\nNothing selected. REPSIM cancelled."))

    (T
     (setq targetData   (entget targetEnt)
           targetType   (cdr (assoc 0 targetData))
           srcLayer     (cdr (assoc 8 targetData))
           rawTargetTxt (get-text targetEnt)
           targetParsed (split-mtext-style rawTargetTxt)
           targetPre    (nth 0 targetParsed)
           targetBody   (nth 1 targetParsed)
           targetSuf    (nth 2 targetParsed)
           styleData    (extract-style targetEnt)
           hasDonor     nil
           newTxt       targetBody
           keepTextOnly (if (= targetBody "") "1" "0")
           scopeMode    "LAYER"
           doLayer      "0"
           doColor      "0"
           doTxtStyle   "0"
           doHeight     "0"
           doWidth      "0"
           doInline     "1"
           doProps      "0")

     ;; Interactive Dialog Loop (Allows jumping out to pick Donor Style object & returning)
     (setq loopDialog T dlgStatus nil)
     (while loopDialog
       (if (boundp 'load_dialog)
         (setq dlgStatus (run-dialog))
         (setq dlgStatus (ask-cmdline))
       )
       (cond
         ;; User clicked "Pick Object to Copy Style From..."
         ((= dlgStatus 2)
          (setq donorEnt (pick-object "\n>> Click DONOR Object to copy its Style (Layer, Color, Font, Height, Width): "))
          (if donorEnt
            (progn
              (setq styleData  (extract-style donorEnt)
                    hasDonor   T
                    doLayer    "1"
                    doColor    "1"
                    doTxtStyle "1"
                    doHeight   "1"
                    doWidth    "1"
                    doInline   "1"
                    doProps    "1")
              (princ (strcat "\n>> Copied style from [" (cdr (assoc "TYPE" styleData))
                             "] on Layer: " (cdr (assoc "LAYER" styleData))
                             " | Color: " (color-name (cdr (assoc "COLOR" styleData)))
                             " | Style: " (cdr (assoc "TSTYLE" styleData))))
            )
            (princ "\n>> No donor object picked. Keeping previous style settings.")
          ))
         ;; User clicked OK or Cancel
         (T
          (setq loopDialog nil))
       )
     )

     ;; Step 6: Apply Text & Style Updates across the chosen Scope
     (if (= dlgStatus 1)
       (progn
         (setq doWidth doProps)
         (cond
           ((= scopeMode "MANUAL")
            (princ "\n>> Select Target Objects to paint style/text onto: ")
            (setq ss (ssget)))
           (T
            (setq ss (ssget "_X" '((0 . "TEXT,MTEXT,MULTILEADER,ATTRIB,ATTDEF")))))
         )

         (if ss
           (progn
             (command "_.UNDO" "_Begin")
             (setq i 0 cnt 0)
             (while (< i (sslength ss))
               (setq e       (ssname ss i)
                     ed      (entget e)
                     eType   (cdr (assoc 0 ed))
                     eLayer  (cdr (assoc 8 ed))
                     eRaw    (get-text e)
                     eParsed (split-mtext-style eRaw)
                     ePre    (nth 0 eParsed)
                     eBody   (nth 1 eParsed)
                     eSuf    (nth 2 eParsed))

               ;; Check if entity matches the chosen Scope filter
               (if (cond
                     ((= scopeMode "MANUAL") T)
                     ((= scopeMode "ALLLAY") (= (strcase eLayer) (strcase srcLayer)))
                     ((= scopeMode "GLOBAL") (or (= eBody targetBody) (= eRaw rawTargetTxt)))
                     (T                      (and (= (strcase eLayer) (strcase srcLayer))
                                                  (or (= eBody targetBody) (= eRaw rawTargetTxt))))
                   )
                 (progn
                   ;; 1. Update Text Content & Inline MTEXT Prefix if it's a text-bearing object
                   (if (member eType '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB" "ATTDEF"))
                     (progn
                       (setq finalBody (if (= keepTextOnly "1") eBody newTxt))
                       (if (member eType '("MTEXT" "MULTILEADER"))
                         (progn
                           ;; If copying inline MTEXT style from Donor, use Donor's {\W...;\f...; prefix
                           (if (and (= doInline "1") hasDonor)
                             (setq finalPre (cdr (assoc "PRE" styleData))
                                   finalSuf (cdr (assoc "SUF" styleData)))
                             (if (= doInline "1")
                               (setq finalPre ePre
                                     finalSuf eSuf)
                               (setq finalPre ""
                                     finalSuf "")
                             )
                           )
                           (setq finalFullTxt (strcat finalPre finalBody finalSuf))
                         )
                         ;; Single-line TEXT does not use MTEXT {\W...;} braces
                         (setq finalFullTxt finalBody)
                       )
                       (put-text e finalFullTxt)
                     )
                   )

                   ;; 2. Apply Checked Style Properties (Layer, Color, Font, Height, Width, Linetype)
                   (if (or (= doLayer "1") (= doColor "1") (= doTxtStyle "1")
                           (= doHeight "1") (= doWidth "1") (= doProps "1"))
                     (apply-style-to-ent e styleData)
                   )

                   (setq cnt (1+ cnt))
                 )
               )
               (setq i (1+ i))
             )
             (command "_.UNDO" "_End")
             (princ (strcat "\n>> REPSIM Complete: Updated " (itoa cnt) " object(s)."))
           )
           (princ "\n>> No matching objects found.")
         )
       )
       (princ "\n>> REPSIM cancelled.")
     )
    )
  )

  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Alias for Direct Canva/Word Style Painter
(defun c:cpstyle () (c:REPSIM))
(princ "\nREPSIM v2.0 loaded. Type REPSIM or CPSTYLE to run.")
(princ)