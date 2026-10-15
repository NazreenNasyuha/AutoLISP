;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getaream.lsp
;;; COMMAND     : GETAREAM
;;; DESCRIPTION : Single-Click Polyline Area Extractor to Square Metres (m²)
;;;               with Automatic Geometric Centroid Text Injection & Clipboard Copy
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In standard civil infrastructure drafting, hydraulic design, building
;;; footprints, and pavement schedule quantification, areas are reported in
;;; Square Metres (m²). With drawings scaled in standard millimetres:
;;;   1 m² = 1,000 mm * 1,000 mm = 1,000,000 mm²
;;;
;;; GETAREAM provides an instant, zero-friction measurement workflow:
;;;   1. Prompts the drafter to select any closed or open lightweight polyline
;;;      (LWPOLYLINE or 2D/3D POLYLINE).
;;;   2. Queries drawing geometric area via native AutoCAD `_.AREA _O`.
;;;   3. Converts raw millimetre area into metric Square Metres (m²).
;;;   4. Computes the geometric vertex centroid of the boundary.
;;;   5. Uses atomic DXF `entmake` to inject a clean, middle-centered TEXT entity
;;;      ("X.XXXX m2") directly inside the boundary footprint.
;;;   6. Pipes the exact numerical m² string directly into the Windows
;;;      Clipboard using native Windows `clip.exe` for immediate Excel pasting.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type GETAREAM in the AutoCAD / GstarCAD command line.
;;;  2. Pick the lot, road pavement, or structure polyline boundary.
;;;  3. The text label is automatically created at the parcel centroid, and
;;;     the numeric value is copied to the Windows clipboard for instant use.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - txtHgt   : Height of the injected CAD text entity (default: 1500.0 mm).
;;;  - decimals : Precision decimal places for square metre formatting (default: 4).
;;; ==========================================================================

(defun c:getaream ( / *error* txtHgt decimals ss ent rawArea mArea outText center oldCmd get-poly-center )
  
  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq txtHgt 1500.0) ;; Height of the injected area text
  (setq decimals 4)    ;; Decimal places for the final number
  ;; =========================================================================
  
  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SETUP
  ;; =========================================================================
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. GEOMETRIC CENTROID CALCULATION
  ;; =========================================================================
  ;; Helper to calculate the vertex centroid of the polyline
  (defun get-poly-center (e / elist sumX sumY count)
    (setq elist (entget e) sumX 0.0 sumY 0.0 count 0)
    (foreach item elist 
      (if (= (car item) 10) 
        (setq sumX (+ sumX (cadr item)) sumY (+ sumY (caddr item)) count (1+ count))
      )
    )
    (if (> count 0) (list (/ sumX count) (/ sumY count) 0.0) nil)
  )

  ;; =========================================================================
  ;; 4. BOUNDARY SELECTION & EXECUTION ENGINE
  ;; =========================================================================
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      
      (setvar "CMDECHO" 0) 
      (command "_.AREA" "_O" ent) 
      (if oldCmd (setvar "CMDECHO" oldCmd))
      
      (setq rawArea (getvar "AREA"))
      (setq mArea (/ rawArea 1000000.0))
      (setq outText (rtos mArea 2 decimals))
      
      ;; Find centroid and inject middle-centered text
      (setq center (get-poly-center ent))
      (if center
        (entmake (list '(0 . "TEXT")
                       (cons 1 (strcat outText " m2")) 
                       (list 10 (car center) (cadr center) 0.0) 
                       (list 11 (car center) (cadr center) 0.0) 
                       (cons 40 txtHgt)
                       '(72 . 1)
                       '(73 . 2)))
      )
      
      ;; Pipe result to Windows Clipboard
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Area: " outText " m2 (Copied to Clipboard!)"))
    )
    (princ "\nNo polyline selected.")
  )
  (princ)
)