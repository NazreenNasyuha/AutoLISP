;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getallaream.lsp
;;; COMMAND     : GETALLAREAM
;;; DESCRIPTION : Batch Polyline Area Extractor with Instant Centered Labels (m²)
;;;               and Cumulative Total Area Output
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; When calculating land use zoning, sub-catchments, or building lot areas across
;;; an entire master plan, extracting areas one polyline at a time is inefficient.
;;;
;;; GETALLAREAM enables rapid batch extraction:
;;;   1. Prompts the drafter to select any number of closed polylines (window selection).
;;;   2. Validates closure flags (`(70 . 1)`) across all selected LWPOLYLINE and
;;;      POLYLINE entities.
;;;   3. Computes the geometric centroid of each closed polygon by averaging its
;;;      boundary vertices.
;;;   4. Measures the exact surface area, converts from drawing units (mm²) to
;;;      square metres (m²), and creates a centered Middle-Center aligned text
;;;      annotation (`"xxx.xxx m2"`) directly in the center of each shape.
;;;   5. Accumulates all individual areas and prints the grand total area in m²
;;;      to the command line.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type GETALLAREAM in the command line.
;;;  2. Select all closed polylines (window, crossing, or individual clicks).
;;;  3. Press Enter. Each polygon is immediately annotated with its m² area,
;;;     and the total combined area is displayed on the command line.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - txtHgt   : Text height for the generated area annotations (mm).
;;;  - decimals : Decimal precision for the area value (default 3).
;;; ==========================================================================

(defun c:getallaream ( / ss i ent totalArea area mArea center txtHgt decimals oldCmd get-poly-center )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq txtHgt   1500.0) ;; Height of the injected area text (mm)
  (setq decimals 3)      ;; Decimal precision for the area value
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SAFEGUARDS
  ;; =========================================================================
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETALLAREAM] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. GEOMETRIC CENTROID HELPER
  ;; =========================================================================
  ;; Calculates polygon centroid by averaging all boundary vertex coordinates
  (defun get-poly-center (e / elist sumX sumY count)
    (setq elist (entget e) sumX 0.0 sumY 0.0 count 0)
    (foreach item elist
      (if (= (car item) 10)
        (setq sumX (+ sumX (cadr item))
              sumY (+ sumY (caddr item))
              count (1+ count))
      )
    )
    (if (> count 0) (list (/ sumX count) (/ sumY count) 0.0) nil)
  )

  ;; =========================================================================
  ;; 4. BATCH PROCESSING & TEXT INJECTION LOOP
  ;; =========================================================================
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

        ;; Format individual area & locate centroid
        (setq mArea (/ area 1000000.0))  ;; mm² to m²
        (setq center (get-poly-center ent))

        ;; Inject centered text label (Middle-Center alignment: 72=1, 73=2)
        (if center
          (entmake (list '(0 . "TEXT")
                         (cons 1 (strcat (rtos mArea 2 decimals) " m2"))
                         (list 10 (car center) (cadr center) 0.0)
                         (list 11 (car center) (cadr center) 0.0)
                         (cons 40 txtHgt)
                         '(72 . 1)
                         '(73 . 2)))
        )
        (setq i (1+ i))
      )
      (setvar "CMDECHO" oldCmd)
      (princ (strcat "\n>> Processed " (itoa i) " polylines. TOTAL AREA: "
                     (rtos (/ totalArea 1000000.0) 2 decimals) " m2"))
    )
    (princ "\nNo closed polylines selected.")
  )

  (princ)
)