;;; ==========================================================================
;;; IMPORTCADCSV.LSP - Import 2-Column AutoCAD CSV into Drawing
;;; Reads "AutoCAD_Import.csv" produced by the AutoCAD XLS-to-CSV Bridge.
;;; Matches Column 1 (ID placeholder) and updates text with Column 2 (CAD String).
;;; Universal Vanilla AutoLISP: Compatible with AutoCAD, LT (2024+), and GstarCAD.
;;; ==========================================================================
(defun c:importcadcsv ( / *error* sysVars sysVals fn fh line pos id cadStr
                          clean-csv-token parse-csv-line
                          ss ent ed cnt notFoundList )

  (setq sysVars '("CMDECHO") sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (if (and fh (setq fh nil)) (close fh))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[IMPORTCADCSV] Error: " msg)))
    (princ)
  )

  ;; Strips enclosing double quotes if present and unescapes doubled quotes
  (defun clean-csv-token (tok / len)
    (setq tok (vl-string-trim " \t\r\n" tok) len (strlen tok))
    (if (and (> len 1) (= (substr tok 1 1) "\"") (= (substr tok len 1) "\""))
      (setq tok (substr tok 2 (- len 2))))
    ;; unescape doubled quotes
    (while (vl-string-search "\"\"" tok)
      (setq tok (vl-string-subst "\"" "\"\"" tok)))
    tok)

  ;; Parses a 2-column CSV line taking into account quoted strings
  (defun parse-csv-line (str / k len inQuote ch idStr valStr)
    (setq len (strlen str) k 1 inQuote nil idStr "" valStr "")
    ;; Read Col 1 (ID)
    (while (and (<= k len) (or inQuote (/= (substr str k 1) ",")))
      (setq ch (substr str k 1))
      (if (= ch "\"") (setq inQuote (not inQuote)))
      (setq idStr (strcat idStr ch))
      (setq k (1+ k)))
    ;; Remainder is Col 2 (CAD String)
    (if (< k len)
      (setq valStr (substr str (1+ k))))
    (list (clean-csv-token idStr) (clean-csv-token valStr)))

  ;; ---- Main Execution ----------------------------------------------------
  (setq fn (getfiled "Select AutoCAD CSV Export (.csv)" "AutoCAD_Import.csv" "csv" 0))
  (cond
    ((null fn) (princ "\nNo CSV file selected."))
    ((null (setq fh (open fn "r"))) (princ "\nCould not open the selected CSV file."))
    (T
     (setvar "CMDECHO" 0)
     (setq cnt 0 notFoundList nil)

     (while (setq line (read-line fh))
       (if (/= (vl-string-trim " \t\r\n" line) "")
         (progn
           (setq parsed (parse-csv-line line)
                 id     (car parsed)
                 cadStr (cadr parsed))

           (if (and (/= id "") (/= cadStr ""))
             (progn
               ;; Search for any TEXT or MTEXT whose exact string matches ID
               (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 1 id))))
               (if ss
                 (progn
                   (setq i 0)
                   (while (< i (sslength ss))
                     (setq ent (ssname ss i) ed (entget ent))
                     (if (= (cdr (assoc 1 ed)) id)
                       (progn
                         (entmod (subst (cons 1 cadStr) (assoc 1 ed) ed))
                         (setq cnt (1+ cnt))))
                     (setq i (1+ i))))
                 (setq notFoundList (cons id notFoundList))))))))

     (close fh)
     (setq fh nil)

     (princ (strcat "\n========================================================"))
     (princ (strcat "\n [IMPORTCADCSV] Update Completed!"))
     (princ (strcat "\n Successfully updated : " (itoa cnt) " text entity/entities."))
     (if notFoundList
       (princ (strcat "\n IDs not found in CAD : " (itoa (length notFoundList)) " (e.g. " (car notFoundList) ")")))
     (princ (strcat "\n========================================================\n"))
    )
  )

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)

(princ "\n[IMPORTCADCSV] Loaded. Run 'IMPORTCADCSV' to batch-replace text placeholders.")
(princ)
