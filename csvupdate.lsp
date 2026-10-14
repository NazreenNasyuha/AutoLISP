(defun c:csvupdate ( / *error* filename file line pos id val dict refSel refEnt refLayer ss count i ent elist oldTxt cleanTxt match newTxt updated missing pt p1 p2 p3 p4 gap)
  (vl-load-com)
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg) (if oldCmd (setvar "CMDECHO" oldCmd)) (princ))

  (princ "\nCommand: CSVUPDATE (Batch Replace with QA/QC Check)")

  ;; 1. Read CSV
  (setq filename (getfiled "Select Engineer's CSV File" "" "csv" 0))
  (if (not filename) (progn (princ "\nNo file selected.") (exit)))

  (setq file (open filename "r") dict nil)
  (while (setq line (read-line file))
    (setq pos (vl-string-search "," line))
    (if pos
      (progn
        (setq id (vl-string-trim " \"\t" (substr line 1 pos)) val (vl-string-trim " \"\t" (substr line (+ pos 2))))
        (if (and (/= id "") (/= val "")) (setq dict (cons (cons (strcase id) val) dict)))
      )
    )
  )
  (close file)
  (if (not dict) (progn (princ "\nError: No valid data found in CSV.") (exit)))

  ;; 2. Select Similar Automation
  (princ "\nSelect ONE placeholder text to target its layer (e.g. Drainage or Sewerage): ")
  (setq refSel (ssget ":S" '((0 . "TEXT,MTEXT"))))
  (if (not refSel) (progn (princ "\nNo reference text selected.") (exit)))
  
  (setq refEnt (ssname refSel 0))
  (setq refLayer (cdr (assoc 8 (entget refEnt))))
  
  ;; Select ALL text on that specific layer
  (setq ss (ssget "X" (list '(0 . "TEXT,MTEXT") (cons 8 refLayer))))
  
  (if ss
    (progn
      (setq count (sslength ss) i 0 updated 0 missing 0)
      
      (while (< i count)
        (setq ent (ssname ss i) elist (entget ent) oldTxt (cdr (assoc 1 elist)))
        (setq cleanTxt (strcase (vl-string-trim " \t" oldTxt)))
        (setq match (assoc cleanTxt dict))
        
        (if match
          (progn
            ;; Update successful
            (setq newTxt (cdr match))
            (setq elist (subst (cons 1 newTxt) (assoc 1 elist) elist))
            (entmod elist)
            (setq updated (1+ updated))
          )
          (progn
            ;; NOT FOUND IN EXCEL: Draw Thick Red QA/QC Box
            (setq pt (cdr (assoc 10 elist)))
            (setq gap 1500.0) ;; Box size padding
            (setq p1 (list (- (car pt) gap) (- (cadr pt) gap) 0.0)
                  p2 (list (+ (car pt) (* gap 3.0)) (- (cadr pt) gap) 0.0)
                  p3 (list (+ (car pt) (* gap 3.0)) (+ (cadr pt) gap) 0.0)
                  p4 (list (- (car pt) gap) (+ (cadr pt) gap) 0.0))
            
            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                           (cons 8 refLayer) (cons 62 1) '(90 . 4) '(70 . 1) (cons 43 50.0)
                           (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2)) 
                           (list 10 (car p3) (cadr p3)) (list 10 (car p4) (cadr p4))))
            (setq missing (1+ missing))
          )
        )
        (setq i (1+ i))
      )
      
      ;; 3. Popup Status Alert
      (alert (strcat "CSV UPDATE COMPLETE\n\nTarget Layer: " refLayer 
                     "\n\nTexts Updated: " (itoa updated) 
                     "\n\nMissing from CSV (Red Boxed): " (itoa missing)))
    )
  )
  (princ)
)