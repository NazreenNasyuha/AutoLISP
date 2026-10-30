;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tbox.lsp
;;; COMMAND     : TBOX (Aliases: BOXSIM, TEXTBOX)
;;; DESCRIPTION : Select Similar TEXT / MTEXT & Draw Rotated Rectangular
;;;               Polyline Boxes Around Each Text (AutoCAD LT 2024 & GstarCAD)
;;; COMPATIBILITY: Universal (100% Pure AutoLISP / DXF - Zero ActiveX Needed)
;;; ==========================================================================

(defun c:tbox ( / *error* oldCmd
                  ;; --- settings ---
                  defOffFactor boxWidth
                  ;; --- helper functions ---
                  tb:esc tb:trim tb:cleanup tb:tmpName
                  tb:writeDCL tb:runDialog tb:askCmdLine
                  get-clean-text extract-prefix pick-source-text
                  has-box-already-p draw-rotated-box
                  ;; --- state variables ---
                  srcEnt srcData srcType srcLayer srcTxt srcPrefix
                  offFactorStr offFactor scopeMode matchPrefix
                  dclFile dclId dlgOK
                  ss filt i cnt skipped e ed eTxt )

  ;; =========================================================================
  ;; 1. USER SETTINGS (Defaults - Adjustable in Popup Dialog)
  ;; =========================================================================
  (setq defOffFactor 0.35) ;; Default padding around text (0.35 x Text Height)
  (setq boxWidth     0.0)  ;; Polyline global width (0.0 = standard crisp line)
  ;; =========================================================================

  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; -------------------------------------------------------------------------
  ;; 2. PURE AUTOLISP HELPERS (AutoCAD LT 2024 & "#JRK" Layer Safe)
  ;; -------------------------------------------------------------------------
  (defun tb:esc (str / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen str))
      (setq ch (substr str k 1))
      (setq out (strcat out (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ",")) "`" "") ch))
      (setq k (1+ k))
    )
    out
  )

  (defun tb:trim (s / len i1 i2)
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

  (defun tb:cleanup ()
    (if (and dclId (> dclId 0))
      (progn (unload_dialog dclId) (setq dclId nil))
    )
    (if (and dclFile (findfile dclFile) (boundp 'vl-file-delete))
      (vl-file-delete dclFile)
    )
  )

  (defun *error* (msg)
    (tb:cleanup)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[TBOX Error]: " msg))
    )
    (princ)
  )

  (defun tb:tmpName ( / tmpDir)
    (if (boundp 'vl-filename-mktemp)
      (vl-filename-mktemp "tbox.dcl")
      (progn
        (setq tmpDir (cond ((getenv "TEMP")) ((getenv "TMP")) ((getvar "TEMPPREFIX")) (".")))
        (if (member (substr tmpDir (strlen tmpDir) 1) '("\\" "/"))
          (strcat tmpDir "tbox.dcl")
          (strcat tmpDir "\\tbox.dcl")
        )
      )
    )
  )

  ;; Strips leading MTEXT formatting ({\W0.5;, {\f...;) to read clean visible text
  (defun get-clean-text (ed / typ raw item len i loopP ch sec semi)
    (setq typ (cdr (assoc 0 ed)) raw "")
    (if (= typ "MTEXT")
      (foreach item ed
        (if (and (or (= (car item) 3) (= (car item) 1)) (= (type (cdr item)) 'STR))
          (setq raw (strcat raw (cdr item)))
        )
      )
      (if (and (assoc 1 ed) (= (type (cdr (assoc 1 ed))) 'STR))
        (setq raw (cdr (assoc 1 ed)))
      )
    )
    (setq len (strlen raw) i 1 loopP T)
    (while (and loopP (<= i len))
      (setq ch  (substr raw i 1)
            sec (if (< i len) (substr raw (1+ i) 1) ""))
      (cond
        ((= ch "{") (setq i (1+ i)))
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" raw (1- i)))
         (if semi (setq i (+ semi 2)) (setq loopP nil)))
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )
    (substr raw i)
  )

  ;; Extracts leading alphabetic prefix (e.g. "FFL27.80" -> "FFL", "RFL27.30" -> "RFL")
  (defun extract-prefix (s / len i ch out)
    (setq s (tb:trim s) len (strlen s) i 1 out "")
    (while (<= i len)
      (setq ch (strcase (substr s i 1)))
      (if (and (>= (ascii ch) 65) (<= (ascii ch) 90))
        (setq out (strcat out ch) i (1+ i))
        (setq i (1+ len))
      )
    )
    out
  )

  (defun pick-source-text (promptStr / loop sel e pt ssPick picked)
    (setq loop T picked nil)
    (while loop
      (setq sel (entsel promptStr))
      (if sel
        (progn
          (setq e  (car sel)
                pt (cadr sel))
          (if (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT"))
            (setq picked e loop nil)
            (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT"))))
              (setq picked (ssname ssPick 0) loop nil)
              (princ "\n>> Selected object is not TEXT or MTEXT. Try again.")
            )
          )
        )
        (setq loop nil)
      )
    )
    picked
  )

  ;; =========================================================================
  ;; 3. ROTATED TEXT BOUNDING BOX & DUPLICATE CHECK ENGINE
  ;; =========================================================================
  ;; Checks if a 4-corner closed LWPOLYLINE already exists right at the text center
  (defun has-box-already-p (wcsCen lay hgt / rad ss i e ed pts cx cy found)
    (setq rad (* hgt 0.8) found nil)
    (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 (tb:esc lay)) '(90 . 4) (cons 410 (getvar "CTAB")))))
    (if ss
      (progn
        (setq i 0)
        (while (and (< i (sslength ss)) (not found))
          (setq e   (ssname ss i)
                ed  (entget e)
                pts nil)
          (foreach item ed
            (if (= (car item) 10)
              (setq pts (cons (cdr item) pts))
            )
          )
          (if (= (length pts) 4)
            (progn
              (setq cx (/ (apply '+ (mapcar 'car pts)) 4.0)
                    cy (/ (apply '+ (mapcar 'cadr pts)) 4.0))
              (if (< (distance (list (car wcsCen) (cadr wcsCen)) (list cx cy)) rad)
                (setq found T)
              )
            )
          )
          (setq i (1+ i))
        )
      )
    )
    found
  )

  ;; Computes exact rotated 4 corners around TEXT or MTEXT and draws LWPOLYLINE box
  (defun draw-rotated-box (ent padFactor / ed typ lay col trueCol hgt pad
                                           p10 ang tb bMin bMax x1 y1 x2 y2
                                           w h att dxMin dxMax dyMin dyMax
                                           ux uy vx vy c1 c2 c3 c4 wcsCen dxfData)
    (setq ed      (entget ent)
          typ     (cdr (assoc 0 ed))
          lay     (cdr (assoc 8 ed))
          col     (if (assoc 62 ed) (cdr (assoc 62 ed)) nil)
          trueCol (if (assoc 420 ed) (cdr (assoc 420 ed)) nil)
          hgt     (if (assoc 40 ed) (cdr (assoc 40 ed)) 1.0)
          pad     (* hgt padFactor))

    (cond
      ;; A. Single-Line TEXT: Exact local bounding box via native (textbox ed)
      ((= typ "TEXT")
       (setq p10 (cdr (assoc 10 ed))
             ang (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
             tb  (textbox ed))
       (if tb
         (progn
           (setq bMin (car tb)
                 bMax (cadr tb)
                 x1   (- (car bMin) pad)
                 y1   (- (cadr bMin) pad)
                 x2   (+ (car bMax) pad)
                 y2   (+ (cadr bMax) pad)
                 ux   (cos ang)
                 uy   (sin ang)
                 vx   (- (sin ang))
                 vy   (cos ang))
           (setq c1 (trans (list (+ (car p10) (* x1 ux) (* y1 vx)) (+ (cadr p10) (* x1 uy) (* y1 vy)) (caddr p10)) ent 0)
                 c2 (trans (list (+ (car p10) (* x2 ux) (* y1 vx)) (+ (cadr p10) (* x2 uy) (* y1 vy)) (caddr p10)) ent 0)
                 c3 (trans (list (+ (car p10) (* x2 ux) (* y2 vx)) (+ (cadr p10) (* x2 uy) (* y2 vy)) (caddr p10)) ent 0)
                 c4 (trans (list (+ (car p10) (* x1 ux) (* y2 vx)) (+ (cadr p10) (* x1 uy) (* y2 vy)) (caddr p10)) ent 0))
         )
       ))

      ;; B. MTEXT: Exact rotated bounding box from DXF 42 (ActualWidth), DXF 43 (ActualHeight) & DXF 71 (Attachment)
      ((= typ "MTEXT")
       (setq p10 (cdr (assoc 10 ed))
             w   (if (assoc 42 ed) (cdr (assoc 42 ed)) (* hgt 4.0))
             h   (if (assoc 43 ed) (cdr (assoc 43 ed)) hgt)
             att (if (assoc 71 ed) (cdr (assoc 71 ed)) 1)
             ang (if (assoc 11 ed)
                   (angle '(0.0 0.0 0.0) (cdr (assoc 11 ed)))
                   (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)))

       ;; Horizontal local bounds relative to MTEXT insertion point
       (cond
         ((member att '(1 4 7)) (setq dxMin (- pad)            dxMax (+ w pad)))
         ((member att '(2 5 8)) (setq dxMin (- (/ w -2.0) pad) dxMax (+ (/ w 2.0) pad)))
         (T                     (setq dxMin (- (- w) pad)      dxMax pad))
       )
       ;; Vertical local bounds relative to MTEXT insertion point
       (cond
         ((member att '(1 2 3)) (setq dyMin (- (- h) pad)      dyMax pad))
         ((member att '(4 5 6)) (setq dyMin (- (/ h -2.0) pad) dyMax (+ (/ h 2.0) pad)))
         (T                     (setq dyMin (- pad)            dyMax (+ h pad)))
       )

       (setq ux (cos ang) uy (sin ang)
             vx (- (sin ang)) vy (cos ang))

       (setq c1 (list (+ (car p10) (* dxMin ux) (* dyMin vx)) (+ (cadr p10) (* dxMin uy) (* dyMin vy)) 0.0)
             c2 (list (+ (car p10) (* dxMax ux) (* dyMin vx)) (+ (cadr p10) (* dxMax uy) (* dyMin vy)) 0.0)
             c3 (list (+ (car p10) (* dxMax ux) (* dyMax vx)) (+ (cadr p10) (* dxMax uy) (* dyMax vy)) 0.0)
             c4 (list (+ (car p10) (* dxMin ux) (* dyMax vx)) (+ (cadr p10) (* dxMin uy) (* dyMax vy)) 0.0)))
    )

    (if (and c1 c2 c3 c4)
      (progn
        (setq wcsCen (list (/ (+ (car c1) (car c3)) 2.0)
                           (/ (+ (cadr c1) (cadr c3)) 2.0)
                           0.0))
        (if (has-box-already-p wcsCen lay hgt)
          "SKIPPED"
          (progn
            (setq dxfData
              (append
                (list '(0 . "LWPOLYLINE")
                      '(100 . "AcDbEntity")
                      (cons 8 lay))
                (if col (list (cons 62 col)) nil)
                (if trueCol (list (cons 420 trueCol)) nil)
                (list '(100 . "AcDbPolyline")
                      '(90 . 4)
                      '(70 . 1)
                      (cons 43 boxWidth)
                      (list 10 (car c1) (cadr c1))
                      (list 10 (car c2) (cadr c2))
                      (list 10 (car c3) (cadr c3))
                      (list 10 (car c4) (cadr c4)))
              )
            )
            (if (entmake dxfData) "CREATED" nil)
          )
        )
      )
      nil
    )
  )

  ;; =========================================================================
  ;; 4. INTERACTIVE POPUP DIALOG (DCL) & CLI FALLBACK
  ;; =========================================================================
  (defun tb:writeDCL (path / fh)
    (if (setq fh (open path "w"))
      (progn
        (write-line "tbox : dialog {" fh)
        (write-line "  label = \"TBOX - Select Similar Text & Draw Box Around Text\";" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"1. Reference Text Selected\";" fh)
        (write-line "    : text { key = \"srclay\"; label = \"Layer:\"; width = 46; }" fh)
        (write-line "    : edit_box { key = \"srctxt\"; label = \"Sample Text:\"; edit_width = 34; is_enabled = false; }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_column {" fh)
        (write-line "    label = \"2. Box Padding & Prefix Filter\";" fh)
        (write-line "    : edit_box { key = \"offset\"; label = \"Box Margin / Offset Factor (x Text Height):\"; edit_width = 10; }" fh)
        (write-line "    : toggle { key = \"matchpre\"; label = \"Only box texts with same prefix (e.g. FFL* or RFL*)\"; }" fh)
        (write-line "  }" fh)
        (write-line "  : boxed_radio_column {" fh)
        (write-line "    label = \"3. Select Similar Scope\";" fh)
        (write-line "    : radio_button { key = \"sc_layer\";  label = \"All Similar Text on Same Layer (Entire Drawing)\"; }" fh)
        (write-line "    : radio_button { key = \"sc_window\"; label = \"Window / Select on Screen (Filtered to Same Layer)\"; }" fh)
        (write-line "    : radio_button { key = \"sc_single\"; label = \"Only the Picked Reference Text\"; }" fh)
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

  (defun tb:runDialog ( / res)
    (setq res nil dclFile (tb:tmpName))
    (if (tb:writeDCL dclFile)
      (progn
        (setq dclId (load_dialog dclFile))
        (if (and dclId (>= dclId 1) (new_dialog "tbox" dclId))
          (progn
            (set_tile "srclay" (strcat "Layer: " srcLayer "  |  Prefix: " (if (/= srcPrefix "") (strcat srcPrefix "*") "Any")))
            (set_tile "srctxt" srcTxt)
            (set_tile "offset" offFactorStr)
            (set_tile "matchpre" matchPrefix)
            (if (= srcPrefix "") (mode_tile "matchpre" 1) (mode_tile "matchpre" 0))

            (set_tile "sc_layer"  (if (= scopeMode "LAYER")  "1" "0"))
            (set_tile "sc_window" (if (= scopeMode "WINDOW") "1" "0"))
            (set_tile "sc_single" (if (= scopeMode "SINGLE") "1" "0"))

            (action_tile "accept"
              (strcat
                "(setq offFactorStr (tb:trim (get_tile \"offset\")) "
                "matchPrefix (get_tile \"matchpre\") "
                "scopeMode (cond ((= (get_tile \"sc_window\") \"1\") \"WINDOW\") ((= (get_tile \"sc_single\") \"1\") \"SINGLE\") (T \"LAYER\"))) "
                "(if (or (= offFactorStr \"\") (null (distof offFactorStr)) (< (distof offFactorStr) 0.0)) "
                "(set_tile \"error\" \"Please enter a valid positive offset factor (e.g. 0.35).\") "
                "(done_dialog 1))"))

            (action_tile "cancel" "(done_dialog 0)")
            (setq res (start_dialog))
          )
        )
      )
    )
    (tb:cleanup)
    res
  )

  (defun tb:askCmdLine ( / ans)
    (setq ans (getreal (strcat "\nSpecify box margin factor (x Text Height) <" offFactorStr ">: ")))
    (if ans (setq offFactorStr (rtos ans 2 2)))
    (initget "Layer Window Single")
    (setq ans (getkword "\nApply to [Layer all/Window on layer/Single] <Layer>: "))
    (setq scopeMode (cond ((= ans "Window") "WINDOW") ((= ans "Single") "SINGLE") (T "LAYER")))
    1
  )

  ;; =========================================================================
  ;; 5. MAIN EXECUTION WORKFLOW
  ;; =========================================================================
  (princ "\nCommand: TBOX (Select Similar Text & Draw Box Around Text)")

  (setq srcEnt (pick-source-text "\nClick reference TEXT or MTEXT (e.g. FFL27.80 or RFL27.30): "))
  (if (not srcEnt)
    (princ "\n>> Nothing selected. TBOX cancelled.")
    (progn
      (setq srcData      (entget srcEnt)
            srcType      (cdr (assoc 0 srcData))
            srcLayer     (cdr (assoc 8 srcData))
            srcTxt       (get-clean-text srcData)
            srcPrefix    (extract-prefix srcTxt)
            offFactorStr (rtos defOffFactor 2 2)
            matchPrefix  (if (/= srcPrefix "") "1" "0")
            scopeMode    "LAYER")

      (setq dlgOK nil)
      (if (boundp 'load_dialog)
        (setq dlgOK (tb:runDialog))
      )
      (if (null dlgOK)
        (setq dlgOK (tb:askCmdLine))
      )

      (if (/= dlgOK 1)
        (princ "\n>> TBOX cancelled.")
        (progn
          (setq offFactor (distof offFactorStr))
          (setq filt (list '(0 . "TEXT,MTEXT") (cons 8 (tb:esc srcLayer))))

          (cond
            ((= scopeMode "SINGLE")
             (setq ss (ssadd srcEnt (ssadd))))

            ((= scopeMode "WINDOW")
             (princ (strcat "\n>> Select similar texts on layer [" srcLayer "] (or press Enter for all on layer): "))
             (setq ss (ssget filt))
             (if (not ss)
               (setq ss (ssget "_X" (append filt (list (cons 410 (getvar "CTAB"))))))
             ))

            (T
             (setq ss (ssget "_X" (append filt (list (cons 410 (getvar "CTAB")))))))
          )

          (if ss
            (progn
              (command "_.UNDO" "_Begin")
              (setq i 0 cnt 0 skipped 0)
              (while (< i (sslength ss))
                (setq e    (ssname ss i)
                      ed   (entget e)
                      eTxt (get-clean-text ed))

                ;; Check optional prefix match (e.g., only FFL* or RFL*)
                (if (or (= matchPrefix "0")
                        (= srcPrefix "")
                        (= (extract-prefix eTxt) srcPrefix))
                  (progn
                    (setq status (draw-rotated-box e offFactor))
                    (cond
                      ((= status "CREATED") (setq cnt (1+ cnt)))
                      ((= status "SKIPPED") (setq skipped (1+ skipped)))
                    )
                  )
                )
                (setq i (1+ i))
              )
              (command "_.UNDO" "_End")
              (princ (strcat "\n>> TBOX Complete! Created " (itoa cnt)
                             " rotated box(es) on layer [" srcLayer "]"
                             (if (> skipped 0) (strcat " (" (itoa skipped) " existing box(es) skipped).") ".")))
            )
            (princ "\n>> No matching text objects found.")
          )
        )
      )
    )
  )

  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Command Aliases
(defun c:boxsim ()  (c:tbox))
(defun c:textbox () (c:tbox))
(princ "\nTBOX loaded. Type TBOX, BOXSIM, or TEXTBOX to run.")
(princ)