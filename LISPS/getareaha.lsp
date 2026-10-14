;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getareaha.lsp
;;; COMMAND     : GETAREAHA
;;; DESCRIPTION : Dedicated Polyline Area Extraction in Hectares (ha) with
;;;               Centroid Text Annotation & Windows Clipboard Sync
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In municipal township planning, environmental impact assessments, and
;;; bulk earthwork catchment zoning, parcel and pond areas are measured in Hectares.
;;;
;;; GETAREAHA provides instantaneous Hectare extraction:
;;;   1. Prompts the drafter to select any polyline boundary.
;;;   2. Queries native object area in square millimeters.
;;;   3. Converts to Hectares using metric conversion (10,000,000,000 mm2/ha).
;;;   4. Places a centered text entity directly at the polyline geometric center
;;;      formatted with 4 decimal places and "ha" suffix.
;;;   5. Pipes the numerical Hectare value directly to the Windows Clipboard.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `GETAREAHA` in the CAD command line.
;;;   2. Click any closed polyline.
;;;   3. Centroid text label is placed automatically, and result is copied to clipboard.
;;; ==========================================================================

(defun c:getareaha ( / ss ent rawArea haArea outText center oldCmd )
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq txtHgt 1500.0) ;; Height of the injected area text
  (setq decimals 4)    ;; Decimal places for the final number
  ;; =========================================================================
  
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg) (if oldCmd (setvar "CMDECHO" oldCmd)) (princ))

  ;; Helper to calculate the center of the polyline
  (defun get-poly-center (e / elist sumX sumY count)
    (setq elist (entget e) sumX 0.0 sumY 0.0 count 0)
    (foreach item elist 
      (if (= (car item) 10) 
        (setq sumX (+ sumX (cadr item)) sumY (+ sumY (caddr item)) count (1+ count))
      )
    )
    (if (> count 0) (list (/ sumX count) (/ sumY count) 0.0) nil)
  )

  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      
      (setvar "CMDECHO" 0) 
      (command "_.AREA" "_O" ent) 
      (if oldCmd (setvar "CMDECHO" oldCmd))
      
      (setq rawArea (getvar "AREA"))
      (setq haArea (/ rawArea 10000000000.0))
      (setq outText (rtos haArea 2 decimals))
      
      ;; Find center and inject text
      (setq center (get-poly-center ent))
      (if center
        (entmake (list '(0 . "TEXT") (cons 1 (strcat outText " ha")) 
                       (list 10 (car center) (cadr center) 0.0) 
                       (list 11 (car center) (cadr center) 0.0) 
                       (cons 40 txtHgt) '(72 . 1) '(73 . 2)))
      )
      
      ;; Copy to clipboard
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Area: " outText " Hectares (Copied to Clipboard!)"))
    )
    (princ "\nNo polyline selected.")
  )
  (princ)
)