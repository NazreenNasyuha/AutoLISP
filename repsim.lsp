;;; ==========================================================================
;;; REPSIM - Replace Similar Text
;;; Select a TEXT/MTEXT object, then replace every identical string in the
;;; drawing (optionally restricted to the same layer) via a dynamic DCL dialog.
;;; ==========================================================================
(defun c:REPSIM ( / *error* rs:esc rs:writeDCL
                    sel ent entData entType oldTxt srcLayer newTxt sameLayer
                    dclFile dclId dlgOK f filt ss i cnt ed e)

  (vl-load-com) ; ensures vl-filename-mktemp / vl-file-delete exist (harmless otherwise)

  ;; Cleanup helper used by both normal exit and the error handler
  (defun rs:cleanup ()
    (if (and dclId (> dclId 0)) (progn (unload_dialog dclId) (setq dclId nil)))
    (if (and dclFile (findfile dclFile)) (vl-file-delete dclFile))
  )

  ;; Error handler: always unload dialog and remove temp DCL
  (defun *error* (msg)
    (rs:cleanup)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nREPSIM error: " msg)))
    (princ)
  )

  ;; Escape ssget wildcard characters so the string is matched literally
  (defun rs:esc (str / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen str))
      (setq ch (substr str k 1))
      (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ","))
        (setq out (strcat out "`" ch))
        (setq out (strcat out ch)))
      (setq k (1+ k)))
    out
  )

  ;; Write the dialog definition to the temp file (quotes escaped as \")
  (defun rs:writeDCL (path / fh)
    (if (setq fh (open path "w"))
      (progn
        (write-line "repsim : dialog {" fh)
        (write-line "  label = \"Replace Similar Text\";" fh)
        (write-line "  : edit_box { key = \"oldtxt\"; label = \"Target Text:\"; edit_width = 40; is_enabled = false; }" fh)
        (write-line "  : edit_box { key = \"newtxt\"; label = \"Replace With:\"; edit_width = 40; }" fh)
        (write-line "  : toggle { key = \"samelayer\"; label = \"Restrict to same layer\"; value = \"1\"; }" fh)
        (write-line "  spacer;" fh)
        (write-line "  ok_cancel;" fh)
        (write-line "  errtile;" fh)
        (write-line "}" fh)
        (close fh)
        T)
      nil)
  )

  ;; ---- 1. Entity selection ------------------------------------------------
  (setq sel (entsel "\nSelect source TEXT or MTEXT object: "))
  (cond
    ((null sel) (princ "\nNothing selected. REPSIM cancelled."))
    (t
      (setq ent     (car sel)
            entData (entget ent)
            entType (cdr (assoc 0 entData)))
      (cond
        ((not (member entType '("TEXT" "MTEXT")))
         (princ "\nSelected object is not TEXT or MTEXT. REPSIM cancelled."))
        ;; MTEXT over 250 chars stores overflow in group 3; (1 . ) alone would be partial
        ((assoc 3 entData)
         (princ "\nMTEXT too long (multi-chunk) for safe matching. REPSIM cancelled."))
        (t
          (setq oldTxt   (cdr (assoc 1 entData))
                srcLayer (cdr (assoc 8 entData)))

          ;; ---- 2. Build & load dynamic DCL ---------------------------------
          (setq dclFile (vl-filename-mktemp "repsim.dcl"))
          (if (not (rs:writeDCL dclFile))
            (princ "\nCould not write temporary DCL file.")
            (progn
              (setq dclId (load_dialog dclFile))
              (if (or (null dclId) (< dclId 1) (not (new_dialog "repsim" dclId)))
                (princ "\nCould not load REPSIM dialog.")
                (progn
                  ;; Pre-fill tiles and set focus on the replacement box
                  (set_tile "oldtxt" oldTxt)
                  (set_tile "newtxt" "")
                  (set_tile "samelayer" "1")
                  (mode_tile "newtxt" 2)

                  ;; OK captures inputs (rejects empty replacement); Cancel returns 0
                  (action_tile "accept"
                    "(setq newTxt (get_tile \"newtxt\") sameLayer (get_tile \"samelayer\")) (if (= newTxt \"\") (set_tile \"error\" \"Please enter replacement text.\") (done_dialog 1))")
                  (action_tile "cancel" "(done_dialog 0)")

                  (setq dlgOK (start_dialog))))))

          ;; ---- 5. Cleanup immediately after UI closes (OK or Cancel) -------
          (rs:cleanup)

          ;; ---- 3. Build ssget filter and 4. execute ------------------------
          (if (= dlgOK 1)
            (progn
              ;; Always filter by type and exact string (wildcards escaped)
              (setq filt (list '(0 . "TEXT,MTEXT") (cons 1 (rs:esc oldTxt))))
              ;; Optionally restrict to source layer
              (if (= sameLayer "1")
                (setq filt (append filt (list (cons 8 (rs:esc srcLayer))))))

              (setq ss  (ssget "_X" filt)
                    cnt 0)

              ;; Only loop if matches were found (ss may be nil)
              (if ss
                (progn
                  (setq i 0)
                  (while (< i (sslength ss))
                    (setq e  (ssname ss i)
                          ed (entget e))
                    ;; Safety: exact match and not a multi-chunk MTEXT
                    (if (and ed
                             (not (assoc 3 ed))
                             (= (cdr (assoc 1 ed)) oldTxt))
                      (if (entmod (subst (cons 1 newTxt) (assoc 1 ed) ed))
                        (setq cnt (1+ cnt))))
                    (setq i (1+ i)))))

              (princ (strcat "\nREPSIM: " (itoa cnt) " text object(s) modified."))
            )
            (princ "\nREPSIM cancelled."))
        )
      )
    )
  )
  (princ)
)

(princ "\nREPSIM loaded. Type REPSIM to run.")
(princ)