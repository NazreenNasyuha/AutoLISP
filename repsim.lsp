;;; ==========================================================================
;;; REPSIM - Replace Similar Text
;;; Select a TEXT/MTEXT object, then replace every identical string in the
;;; drawing (optionally restricted to the same layer).
;;; Uses a dynamic DCL dialog; falls back to command-line prompts where DCL is
;;; unavailable (e.g. AutoCAD LT).
;;; ==========================================================================
(defun c:REPSIM ( / *error* rs:esc rs:writeDCL rs:cleanup rs:tmpName rs:runDialog rs:askCmdLine
                    sel ent entData entType oldTxt srcLayer newTxt sameLayer
                    dclFile dclId dlgOK ans filt ss i cnt ed e)

  ;; Remove temp DCL / unload dialog (used on normal exit AND on error)
  (defun rs:cleanup ()
    (if (and dclId (> dclId 0)) (progn (unload_dialog dclId) (setq dclId nil)))
    (if (and dclFile (findfile dclFile) (boundp 'vl-file-delete)) (vl-file-delete dclFile)))

  (defun *error* (msg)
    (rs:cleanup)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nREPSIM error: " msg)))
    (princ))

  ;; Escape ssget wildcard characters so strings/layers are matched literally
  (defun rs:esc (str / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen str))
      (setq ch (substr str k 1))
      (setq out (strcat out (if (member ch '("`" "*" "?" "#" "@" "." "~" "[" "]" ",")) "`" "") ch))
      (setq k (1+ k)))
    out)

  ;; Temp file name (vl-filename-mktemp when present, manual otherwise)
  (defun rs:tmpName ()
    (if (boundp 'vl-filename-mktemp)
      (vl-filename-mktemp "repsim.dcl")
      (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) (".")) "\\repsim_" (itoa (getvar "MILLISECS")) ".dcl")))

  ;; Write the dialog definition (quotes escaped as \")
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
      nil))

  ;; DCL path. Returns 1 (OK), 0 (Cancel) or nil if the dialog could not be shown.
  (defun rs:runDialog ( / result)
    (setq result nil dclFile (rs:tmpName))
    (if (rs:writeDCL dclFile)
      (progn
        (setq dclId (load_dialog dclFile))
        (if (and dclId (>= dclId 1) (new_dialog "repsim" dclId))
          (progn
            (set_tile "oldtxt" oldTxt)
            (set_tile "newtxt" "")
            (set_tile "samelayer" "1")
            (mode_tile "newtxt" 2)                       ;; focus on Replace With
            (action_tile "accept"
              "(setq newTxt (get_tile \"newtxt\") sameLayer (get_tile \"samelayer\")) (if (= newTxt \"\") (set_tile \"error\" \"Please enter replacement text.\") (done_dialog 1))")
            (action_tile "cancel" "(done_dialog 0)")
            (setq result (start_dialog))))))
    (rs:cleanup)                                         ;; unload + delete .dcl right after the UI closes
    result)

  ;; Command-line fallback. Returns 1 (OK) or 0 (Cancel).
  (defun rs:askCmdLine ()
    (setq newTxt (getstring T "\nReplace With: "))
    (if (or (null newTxt) (= newTxt ""))
      0
      (progn
        (initget "Yes No")
        (setq ans (getkword "\nRestrict to same layer? [Yes/No] <Yes>: "))
        (setq sameLayer (if (= ans "No") "0" "1"))
        1)))

  ;; ---- 1. Select source object -------------------------------------------
  (setq sel (entsel "\nSelect source TEXT or MTEXT object: "))
  (cond
    ((null sel) (princ "\nNothing selected. REPSIM cancelled."))
    (T
     (setq ent (car sel) entData (entget ent) entType (cdr (assoc 0 entData)))
     (cond
       ((not (member entType '("TEXT" "MTEXT")))
        (princ "\nSelected object is not TEXT or MTEXT. REPSIM cancelled."))
       ((assoc 3 entData)   ;; MTEXT > 250 chars keeps overflow in group 3
        (princ "\nMTEXT too long (multi-chunk) for safe matching. REPSIM cancelled."))
       (T
        (setq oldTxt (cdr (assoc 1 entData)) srcLayer (cdr (assoc 8 entData)))

        ;; ---- 2. UI (DCL if available, otherwise command line) --------------
        (setq dlgOK nil)
        (if (boundp 'load_dialog) (setq dlgOK (rs:runDialog)))
        (if (null dlgOK) (setq dlgOK (rs:askCmdLine)))

        ;; ---- 3/4. Filter + replace -----------------------------------------
        (if (= dlgOK 1)
          (progn
            (setq filt (list '(0 . "TEXT,MTEXT") (cons 1 (rs:esc oldTxt))))
            (if (= sameLayer "1")
              (setq filt (append filt (list (cons 8 (rs:esc srcLayer))))))

            (setq ss (ssget "_X" filt) cnt 0)
            (if ss
              (progn
                (setq i 0)
                (while (< i (sslength ss))
                  (setq e (ssname ss i) ed (entget e))
                  (if (and ed (not (assoc 3 ed)) (= (cdr (assoc 1 ed)) oldTxt))
                    (if (entmod (subst (cons 1 newTxt) (assoc 1 ed) ed))
                      (setq cnt (1+ cnt))))
                  (setq i (1+ i)))))
            (princ (strcat "\nREPSIM: " (itoa cnt) " text object(s) modified.")))
          (princ "\nREPSIM cancelled."))))))
  (princ)
)

(princ "\nREPSIM loaded. Type REPSIM to run.")
(princ)
