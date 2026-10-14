;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getallaream.lsp
;;; COMMAND     : GETALLAREAM
;;; DESCRIPTION : Batch Closed Polyline Area Calculation & Cumulative Summation
;;;               (Auto Centroid M2 Text Label Inserter + Summary Total)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In civil site grading, road reserve allocation, and master planning, drafters
;;; must often calculate areas for dozens or hundreds of parcels, building pads,
;;; or catchment sub-basins at once.
;;;
;;; GETALLAREAM streamlines batch area extraction:
;;;   1. Prompts for a window or cross selection of closed polylines.
;;;   2. Filters out open polylines and invalid geometries automatically.
;;;   3. Loops through each polyline, querying native object area.
;;;   4. Converts square millimeters (drawing units) to square meters (m2).
;;;   5. Computes the geometric centroid vertex average of each polygon.
;;;   6. Injects a centered, cleanly formatted text label (e.g. "125.500 m2")
;;;      directly into the center of each polyline.
;;;   7. Aggregates all individual areas and prints the Grand Total to the
;;;      CAD command prompt.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `GETALLAREAM` in the CAD command line.
;;;   2. Select all closed polylines using a crossing window.
;;;   3. Individual m2 text labels are injected at polygon centers.
;;;   4. Cumulative total m2 is reported at the command prompt.
;;; ==========================================================================

(defun c:getallaream ( / ss i ent totalArea area mArea center txtHgt oldCmd)
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq txtHgt 1500.0)
  (setq decimals 3)
  ;; =========================================================================
  
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg) (if oldCmd (setvar "CMDECHO" oldCmd)) (princ))
  
  ;; Helper: Finds the center by averaging the polyline vertices
  (defun get-poly-center (e / elist sumX sumY count)
    (setq elist (entget e) sumX 0.0 sumY 0.0 count 0)
    (foreach item elist
      (if (= (car item) 10)
        (setq sumX (+ sumX (cadr item)) sumY (+ sumY (caddr item)) count (1+ count))
      )
    )
    (if (> count 0) (list (/ sumX count) (/ sumY count) 0.0) nil)
  )

  (princ "\nSelect closed polylines to calculate area: ")
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE") (-4 . "&") (70 . 1))))
    (progn
      (setq i 0 totalArea 0.0)
      (setvar "CMDECHO" 0)
      
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (command "_.AREA" "_O" ent)
        (setq area (getvar "AREA"))
        (setq totalArea (+ totalArea area))
        
        ;; Format individual area & find center
        (setq mArea (/ area 1000000.0))
        (setq center (get-poly-center ent))
        
        ;; Draw Text in the middle
        (if center
          (entmake (list '(0 . "TEXT") (cons 1 (strcat (rtos mArea 2 decimals) " m2"))
                         (list 10 (car center) (cadr center) 0.0) (list 11 (car center) (cadr center) 0.0)
                         (cons 40 txtHgt) '(72 . 1) '(73 . 2)))
        )
        (setq i (1+ i))
      )
      (setvar "CMDECHO" oldCmd)
      (princ (strcat "\n>> Processed " (itoa i) " polylines. TOTAL AREA: " (rtos (/ totalArea 1000000.0) 2 decimals) " m2"))
    )
    (princ "\nNo closed polylines selected.")
  )
  (princ)
)