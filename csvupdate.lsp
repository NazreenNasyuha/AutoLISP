;;; ==========================================================================
;;; CSVUPDATE - Batch Replace Text Placeholders from Engineer's CSV
;;; Reads 2-column CSV exported by the AutoCAD XLS-to-CSV Bridge.
;;; Prompts drafter to select TEXT/MTEXT (or type ALL), looks up ID in dictionary,
;;; and updates matching text entities with the formatted Excel MTEXT string.
;;; ==========================================================================
(defun c:csvupdate ( / *error* filename file line pos id val dict ss count i ent elist entType oldTxt newTxt updated cleanTxt match oldCmd )
  (vl-load-com)
  
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and file (not (vl-catch-all-error-p (vl-catch-all-apply 'close (list file))))))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (princ "\nCommand: CSVUPDATE (Batch Replace Placeholders from Excel)")

  ;; 1. Prompt user to select the CSV file
  (setq filename (getfiled "Select Engineer's CSV File" "" "csv" 0))
  (if (not filename)
    (progn (princ "\nNo file selected. Command cancelled.") (exit))
  )

  ;; 2. Read the CSV file into memory
  (setq file (open filename "r"))
  (setq dict nil)
  
  (while (setq line (read-line file))
    ;; Find the first comma separating Column A (ID) and Column B (Data)
    (setq pos (vl-string-search "," line))
    (if pos
      (progn
        (setq id (vl-string-trim " \"\t" (substr line 1 pos)))
        (setq val (vl-string-trim " \"\t" (substr line (+ pos 2))))
        ;; Add to dictionary if both exist
        (if (and (/= id "") (/= val ""))
          (setq dict (cons (cons (strcase id) val) dict))
        )
      )
    )
  )
  (close file)
  (setq file nil)

  (if (not dict)
    (progn (princ "\nError: No valid data found in CSV.") (exit))
  )

  ;; 3. Prompt drafter to select the texts (or type ALL)
  (princ "\nSelect the placeholder TEXT/MTEXT to update (or type ALL): ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  
  (if ss
    (progn
      (setq count (sslength ss) i 0 updated 0)
      
      ;; 4. Scan through selection and replace matching IDs
      (while (< i count)
        (setq ent (ssname ss i))
        (setq elist (entget ent))
        (setq oldTxt (cdr (assoc 1 elist)))
        
        ;; Strip formatting from MTEXT to read the raw placeholder ID
        (setq cleanTxt (strcase (vl-string-trim " \t" oldTxt)))
        
        ;; Check if the CAD text exists in our Excel dictionary
        (setq match (assoc cleanTxt dict))
        
        (if match
          (progn
            (setq newTxt (cdr match))
            ;; Update the CAD entity with the new Excel string
            (setq elist (subst (cons 1 newTxt) (assoc 1 elist) elist))
            (entmod elist)
            (setq updated (1+ updated))
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nSuccess! " (itoa updated) " placeholders updated from Excel."))
    )
    (princ "\nNo text selected.")
  )
  (princ)
)

(princ "\n[CSVUPDATE] Loaded. Type CSVUPDATE to batch-update text placeholders.")
(princ)
