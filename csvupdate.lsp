;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : csvupdate.lsp
;;; COMMAND     : CSVUPDATE
;;; DESCRIPTION : Automated Batch Placeholder Replacement from Engineer's CSV
;;;               with Visual QA/QC Audit Bounding Boxes for Missing IDs
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In civil engineering workflows (drainage sizing, sewer pipe schedules, invert
;;; level calculations), hydraulic models and spreadsheets generate final pipe
;;; parameters (e.g. "C123; 1200Ø 27m 1:75"). Drafting these manually across
;;; hundreds of pipes is slow and prone to human error.
;;;
;;; CSVUPDATE provides a seamless 2-way bridge between Excel/CSV and AutoCAD:
;;;   1. Reads an engineer's 2-column CSV file (Column A = Placeholder ID,
;;;      Column B = Formatted CAD Text).
;;;   2. Allows the drafter to click a single reference text object in CAD.
;;;      The script automatically detects its layer (e.g., "#JRK - RD Drain Text").
;;;   3. Scans the entire drawing for all TEXT and MTEXT entities residing on
;;;      that specific layer.
;;;   4. Performs case-insensitive matching against the CSV lookup dictionary.
;;;   5. Matched entities are updated in-place via entmod with zero coordinate drift.
;;;   6. QA/QC AUDIT SAFEGUARD: If a text placeholder exists on the drawing layer
;;;      but has no corresponding entry in the engineer's CSV, the script
;;;      automatically draws a prominent thick red polyline boundary box around it.
;;;   7. Displays an immediate summary dialog showing total updated and missing count.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type CSVUPDATE in the command line.
;;;  2. Select the engineer's CSV file via the standard file dialog.
;;;  3. Click ONE text object on the target layer (e.g., click any placeholder).
;;;  4. The tool updates all matching labels instantly and highlights missing IDs
;;;     with red QA/QC boxes.
;;; ==========================================================================

(defun c:csvupdate ( / *error* filename file line pos id val dict refSel refEnt refLayer
                        ss count i ent elist oldTxt cleanTxt match newTxt updated missing
                        pt p1 p2 p3 p4 gap oldCmd )
  (vl-load-com)

  ;; =========================================================================
  ;; 1. ERROR HANDLING & SYSTEM SAFEGUARDS
  ;; =========================================================================
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[CSVUPDATE] Error: " msg)))
    (princ)
  )

  (princ "\nCommand: CSVUPDATE (Batch Replace Placeholders with QA/QC Audit)")

  ;; =========================================================================
  ;; 2. CSV PARSING & LOOKUP DICTIONARY GENERATION
  ;; =========================================================================
  (setq filename (getfiled "Select Engineer's CSV File" "" "csv" 0))
  (if (not filename)
    (progn (princ "\nNo file selected. Command cancelled.") (exit)))

  (setq file (open filename "r")
        dict nil)

  (while (setq line (read-line file))
    ;; Locate comma separator between Column A (ID) and Column B (Formatted Data)
    (setq pos (vl-string-search "," line))
    (if pos
      (progn
        (setq id  (vl-string-trim " \"\t" (substr line 1 pos))
              val (vl-string-trim " \"\t" (substr line (+ pos 2))))
        ;; Add key-value pair to lookup table
        (if (and (/= id "") (/= val ""))
          (setq dict (cons (cons (strcase id) val) dict)))
      )
    )
  )
  (close file)

  (if (not dict)
    (progn (princ "\nError: No valid data found in CSV file.") (exit)))

  ;; =========================================================================
  ;; 3. LAYER TARGETING & AUTOMATED SELECTION
  ;; =========================================================================
  (princ "\nSelect ONE placeholder text to target its layer (e.g. Drainage or Sewerage): ")
  (setq refSel (ssget ":S" '((0 . "TEXT,MTEXT"))))
  (if (not refSel)
    (progn (princ "\nNo reference text selected. Command cancelled.") (exit)))

  (setq refEnt   (ssname refSel 0))
  (setq refLayer (cdr (assoc 8 (entget refEnt))))

  ;; Query the entire drawing for all text entities on the target layer
  (setq ss (ssget "X" (list '(0 . "TEXT,MTEXT") (cons 8 refLayer))))

  ;; =========================================================================
  ;; 4. BATCH REPLACEMENT & QA/QC HIGHLIGHT LOOP
  ;; =========================================================================
  (if ss
    (progn
      (setq count (sslength ss)
            i 0
            updated 0
            missing 0)

      (while (< i count)
        (setq ent   (ssname ss i)
              elist (entget ent)
              oldTxt (cdr (assoc 1 elist)))
        (setq cleanTxt (strcase (vl-string-trim " \t" oldTxt)))
        (setq match    (assoc cleanTxt dict))

        (if match
          (progn
            ;; Replacement match found: update text entity in-place
            (setq newTxt (cdr match))
            (setq elist (subst (cons 1 newTxt) (assoc 1 elist) elist))
            (entmod elist)
            (setq updated (1+ updated))
          )
          (progn
            ;; QA/QC Alert: Placeholder exists in CAD but missing in CSV
            ;; Draw a prominent thick red polyline box around the missing text
            (setq pt (cdr (assoc 10 elist)))
            (setq gap 1500.0) ;; Box padding offset
            (setq p1 (list (- (car pt) gap)          (- (cadr pt) gap) 0.0)
                  p2 (list (+ (car pt) (* gap 3.0))   (- (cadr pt) gap) 0.0)
                  p3 (list (+ (car pt) (* gap 3.0))   (+ (cadr pt) gap) 0.0)
                  p4 (list (- (car pt) gap)          (+ (cadr pt) gap) 0.0))

            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                           (cons 8 refLayer) (cons 62 1) '(90 . 4) '(70 . 1) (cons 43 50.0)
                           (list 10 (car p1) (cadr p1))
                           (list 10 (car p2) (cadr p2))
                           (list 10 (car p3) (cadr p3))
                           (list 10 (car p4) (cadr p4))))
            (setq missing (1+ missing))
          )
        )
        (setq i (1+ i))
      )

      ;; =====================================================================
      ;; 5. STATUS SUMMARY DIALOG
      ;; =====================================================================
      (alert (strcat "CSV UPDATE COMPLETE\n"
                     "==============================\n"
                     "Target Layer: " refLayer "\n\n"
                     "Texts Updated: " (itoa updated) "\n"
                     "Missing from CSV (Red Boxed): " (itoa missing)))
    )
    (princ (strcat "\nNo text objects found on layer: " refLayer))
  )

  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)