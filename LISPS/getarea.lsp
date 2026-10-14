;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getarea.lsp
;;; COMMAND     : GETAREA
;;; DESCRIPTION : Universal Polyline Area Query (m2 / Hectares / Acres) with
;;;               Centroid Text Annotation & Windows Clipboard Sync
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In civil engineering, statutory submissions often require multi-unit area
;;; declarations (square meters for local authorities, hectares for planning,
;;; and acres for land titles).
;;;
;;; GETAREA provides an instantaneous multi-unit readout:
;;;   1. Prompts the drafter to select any polyline boundary.
;;;   2. Queries native object area and computes:
;;;        - Square Meters (m2)
;;;        - Hectares (ha)
;;;        - Imperial Acres (ac)
;;;   3. Compiles a composite string: `{m2} m2 / {ha} ha / {ac} ac`.
;;;   4. Places a centered text entity directly at the polyline geometric center.
;;;   5. Pipes the formatted result directly to the Windows Clipboard via `clip.exe`.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `GETAREA` in the CAD command line.
;;;   2. Click any closed polyline.
;;;   3. Centroid text label is placed automatically, and result is copied to clipboard.
;;; ==========================================================================

(defun c:getarea ( / ss ent rawArea mArea haArea acreArea outText center txtHgt oldCmd)
  (setq txtHgt 1500.0 decimals 3 oldCmd (getvar "CMDECHO"))
  
  (defun get-poly-center (e / elist sumX sumY count)
    (setq elist (entget e) sumX 0.0 sumY 0.0 count 0)
    (foreach item elist (if (= (car item) 10) (setq sumX (+ sumX (cadr item)) sumY (+ sumY (caddr item)) count (1+ count))))
    (if (> count 0) (list (/ sumX count) (/ sumY count) 0.0) nil)
  )

  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      (setvar "CMDECHO" 0) (command "_.AREA" "_O" ent) (if oldCmd (setvar "CMDECHO" oldCmd))
      (setq rawArea (getvar "AREA") mArea (/ rawArea 1000000.0) haArea (/ rawArea 10000000000.0) acreArea (/ rawArea 4046856422.4))
      
      (setq outText (strcat (rtos mArea 2 decimals) " m2 / " (rtos haArea 2 decimals) " ha / " (rtos acreArea 2 decimals) " ac"))
      (setq center (get-poly-center ent))
      (if center
        (entmake (list '(0 . "TEXT") (cons 1 outText) (list 10 (car center) (cadr center) 0.0) (list 11 (car center) (cadr center) 0.0) (cons 40 txtHgt) '(72 . 1) '(73 . 2)))
      )
      
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Area Copied to Clipboard: " outText))
    )
  )
  (princ)
)