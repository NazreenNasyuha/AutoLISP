;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : guideoffset.lsp
;;; COMMAND     : GUIDEOFFSET
;;; DESCRIPTION : Interactive Boundary Trace & Automated Offset Guide Polyline
;;;               Generator with Dynamic Dedicated Layer Management
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In road, drainage, and infrastructure layouts, civil engineers frequently
;;; need to draw reference guide lines (e.g. 1.5 m or 3.0 m setbacks from plot
;;; boundaries, fence lines, or building reserves) to guide drainage routes,
;;; water mains, or pavement kerbs.
;;;
;;; Manually copying, offsetting, trimming, and moving lines to dedicated
;;; reference layers is slow and tedious. GUIDEOFFSET automates the process:
;;;   1. Prompts the drafter for an offset clearance distance (retaining the
;;;      configured default, e.g. 1500 mm).
;;;   2. Automatically provisions a dedicated reference layer named dynamically
;;;      after the clearance (e.g. "DRN-GUIDE-1500") with muted drafting color.
;;;   3. Prompts the drafter to click successive boundary reference points
;;;      (UCS/WCS coordinate translation safe).
;;;   4. Synthesizes an atomic lightweight polyline directly on the target layer.
;;;   5. Automatically computes the normal offset vector and executes a clean
;;;      native AutoCAD `_.OFFSET`, discarding the temporary construction trace.
;;;   6. Leaves a clean, permanent offset guideline without polluting the
;;;      current working layer or altering user drafting settings.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type GUIDEOFFSET in the command line.
;;;  2. Specify the offset distance (or press Enter to accept default, e.g. 1500).
;;;  3. Click boundary reference points in sequence (Tip: Clockwise to offset outside).
;;;  4. Press Enter to finish tracing.
;;;  5. The offset guide polyline is instantly placed on its dedicated layer.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - defOff     : Default offset clearance distance in drawing units (1500.0 mm).
;;;  - guideColor : ACI Color index for the generated guide layer (default: 8 - Gray).
;;; ==========================================================================

(defun c:guideoffset ( / *error* sysVars sysVals ensure-layer
                         defOff guideColor userOff offsetDist layerName
                         pt ptList wpts p data plEnt ptOut ang p1 p2 newEnt)

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq defOff 1500.0)   ;; Default offset distance (drawing units, e.g. 1500 mm)
  (setq guideColor 8)    ;; Guide layer color (ACI 8 = 8-bit Gray)
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SETUP
  ;; =========================================================================
  (setq sysVars '("CMDECHO") sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nAction Cancelled: " msg)))
    (princ))

  ;; =========================================================================
  ;; 3. LAYER PROVISIONING ENGINE
  ;; =========================================================================
  ;; Create layer by entmake - does not alter the current active layer
  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER")
                       '(100 . "AcDbSymbolTableRecord")
                       '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm)
                       '(70 . 0)
                       (cons 62 col)
                       '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")))))

  ;; =========================================================================
  ;; 4. INTERACTIVE TRACE & OFFSET ENGINE
  ;; =========================================================================
  (princ "\nCommand: GUIDEOFFSET (Continuous Pick Offset)")

  (setq userOff (getreal (strcat "\nSpecify offset distance <" (rtos defOff 2 0) ">: ")))
  (setq offsetDist (if userOff userOff defOff))

  (if (<= offsetDist 0.0)
    (princ "\nOffset distance must be greater than zero.")
    (progn
      (setq layerName (strcat "DRN-GUIDE-" (rtos offsetDist 2 0)))
      (setvar "CMDECHO" 0)
      (ensure-layer layerName guideColor)

      (princ "\nTip: Trace sequence CLOCKWISE to offset OUTSIDE.")

      ;; Collect points in Current UCS
      (setq ptList nil)
      (while (setq pt (getpoint (if ptList "\nClick next point (or Enter to finish): " "\nClick 1st point: ")))
        (setq ptList (append ptList (list pt))))

      (if (< (length ptList) 2)
        (princ "\nNot enough points clicked.")
        (progn
          ;; Temporary polyline built with entmake directly on the guide layer (WCS points)
          (setq wpts (mapcar '(lambda (q) (trans q 1 0)) ptList))
          (setq data (list '(0 . "LWPOLYLINE")
                           '(100 . "AcDbEntity")
                           (cons 8 layerName)
                           '(100 . "AcDbPolyline")
                           (cons 90 (length wpts))
                           '(70 . 0)
                           (cons 38 (caddr (car wpts)))))
          (foreach p wpts
            (setq data (append data (list (list 10 (car p) (cadr p))))))
          (entmake data)
          (setq plEnt (entlast))

          ;; Pick-side point: normal vector perpendicular to first segment (UCS)
          (setq p1 (nth 0 ptList)
                p2 (nth 1 ptList)
                ang (angle p1 p2)
                ptOut (polar p1 (+ ang (/ pi 2.0)) (* offsetDist 2.0)))

          (command "_.OFFSET" offsetDist plEnt "_NON" ptOut "")
          (setq newEnt (entlast))

          (if (not (equal plEnt newEnt))
            (progn
              (entdel plEnt)   ;; Offset result inherits the guide layer from its source
              (princ "\nGuide drawn successfully."))
            (progn
              (entdel plEnt)
              (princ "\nError: Invalid shape geometry."))))))
  )

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
