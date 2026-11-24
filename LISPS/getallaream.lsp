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

(vl-load-com)

(defun c:getallaream ( / *error* ss i ent totalArea area mArea center txtHgt decimals
                         oldCmd oldDimZin undoStarted fmt-dec get-poly-center )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq txtHgt   1500.0) ;; Height of the injected area text (mm)
  (setq decimals 3)      ;; Decimal precision for the area value
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SAFEGUARDS
  ;; =========================================================================
  (setq oldCmd    (getvar "CMDECHO")
        oldDimZin (getvar "DIMZIN")
  )
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (if undoStarted (progn (command "_.UNDO" "_End") (setq undoStarted nil)))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETALLAREAM] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. ZERO-PADDED FORMATTER (Rule #4) & TRUE SHOELACE CENTROID IN WCS (Rule #5 & #6)
  ;; =========================================================================
  (defun fmt-dec (val dec / s dotPos fracLen)
    (if (fboundp 'jrk:fmt-dec)
      (jrk:fmt-dec val dec)
      (progn
        (setq s (rtos val 2 dec)
              dotPos (vl-string-search "." s)
        )
        (if (null dotPos)
          (progn
            (setq s (strcat s "."))
            (repeat dec (setq s (strcat s "0")))
          )
          (progn
            (setq fracLen (- (strlen s) dotPos 1))
            (while (< fracLen dec)
              (setq s (strcat s "0") fracLen (1+ fracLen))
            )
          )
        )
        s
      )
    )
  )

  (defun get-poly-center (e / elist pts elev n k p1 p2 cross areaSum cxSum cySum cx cy)
    (if (fboundp 'jrk:get-poly-centroid-wcs)
      (jrk:get-poly-centroid-wcs e)
      (progn
        (setq elist (entget e)
              pts   nil
              elev  (if (assoc 38 elist) (cdr (assoc 38 elist)) 0.0)
        )
        (foreach item elist
          (if (= (car item) 10)
            (setq pts (cons (cdr item) pts))
          )
        )
        (setq pts (reverse pts)
              n   (length pts)
        )
        (if (>= n 3)
          (progn
            (setq areaSum 0.0 cxSum 0.0 cySum 0.0 k 0)
            (while (< k n)
              (setq p1 (nth k pts)
                    p2 (nth (rem (1+ k) n) pts)
                    cross (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
                    areaSum (+ areaSum cross)
                    cxSum (+ cxSum (* (+ (car p1) (car p2)) cross))
                    cySum (+ cySum (* (+ (cadr p1) (cadr p2)) cross))
                    k (1+ k)
              )
            )
            (if (> (abs areaSum) 1e-8)
              (setq cx (/ cxSum (* 3.0 areaSum))
                    cy (/ cySum (* 3.0 areaSum))
              )
              (setq cx (/ (apply '+ (mapcar 'car pts)) (float n))
                    cy (/ (apply '+ (mapcar 'cadr pts)) (float n))
              )
            )
            (trans (list cx cy elev) e 0)
          )
          (if (> n 0)
            (trans (list (caar pts) (cadar pts) elev) e 0)
            nil
          )
        )
      )
    )
  )

  ;; =========================================================================
  ;; 4. BATCH PROCESSING & TEXT INJECTION LOOP
  ;; =========================================================================
  (princ "\nSelect closed polylines to calculate area: ")
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE") (-4 . "&") (70 . 1))))
    (progn
      (command "_.UNDO" "_Begin")
      (setq undoStarted T)
      (setq i 0 totalArea 0.0)

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (command "_.AREA" "_O" ent)
        (setq area (getvar "AREA"))
        (setq totalArea (+ totalArea area))

        ;; Format individual area & locate true WCS centroid
        (setq mArea (/ area 1000000.0))  ;; mm² to m²
        (setq center (get-poly-center ent))

        ;; Inject centered text label (Middle-Center alignment: 72=1, 73=2)
        (if center
          (entmake (list '(0 . "TEXT")
                         (cons 1 (strcat (fmt-dec mArea decimals) " m2"))
                         (list 10 (car center) (cadr center) (caddr center))
                         (list 11 (car center) (cadr center) (caddr center))
                         (cons 40 txtHgt)
                         '(72 . 1)
                         '(73 . 2)))
        )
        (setq i (1+ i))
      )
      (command "_.UNDO" "_End")
      (setq undoStarted nil)
      (princ (strcat "\n>> Processed " (itoa i) " polylines. TOTAL AREA: "
                     (fmt-dec (/ totalArea 1000000.0) decimals) " m2"))
    )
    (princ "\nNo closed polylines selected.")
  )

  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (princ)
)