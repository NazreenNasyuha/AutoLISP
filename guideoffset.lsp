;;; ==========================================================================
;;; GUIDEOFFSET - Click a boundary, get an offset guide polyline on its own layer
;;; Universal build: vanilla AutoLISP, AutoCAD / LT / GstarCAD.
;;; ==========================================================================
(defun c:guideoffset ( / *error* sysVars sysVals ensure-layer
                         defOff guideColor userOff offsetDist layerName
                         pt ptList wpts p data plEnt ptOut ang p1 p2 newEnt)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq defOff 1500.0)   ;; default offset distance (drawing units)
  (setq guideColor 8)    ;; guide layer colour
  ;; =========================================================================

  (setq sysVars '("CMDECHO") sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nAction Cancelled: " msg)))
    (princ))

  ;; Create layer by entmake - does not alter the current layer
  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")))))

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

      ;; collect points (UCS)
      (setq ptList nil)
      (while (setq pt (getpoint (if ptList "\nClick next point (or Enter to finish): " "\nClick 1st point: ")))
        (setq ptList (append ptList (list pt))))

      (if (< (length ptList) 2)
        (princ "\nNot enough points clicked.")
        (progn
          ;; temp polyline built with entmake directly on the guide layer (WCS points)
          (setq wpts (mapcar '(lambda (q) (trans q 1 0)) ptList))
          (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layerName)
                           '(100 . "AcDbPolyline") (cons 90 (length wpts)) '(70 . 0)
                           (cons 38 (caddr (car wpts)))))
          (foreach p wpts
            (setq data (append data (list (list 10 (car p) (cadr p))))))
          (entmake data)
          (setq plEnt (entlast))

          ;; pick-side point: left of first segment (UCS)
          (setq p1 (nth 0 ptList) p2 (nth 1 ptList)
                ang (angle p1 p2)
                ptOut (polar p1 (+ ang (/ pi 2.0)) (* offsetDist 2.0)))

          (command "_.OFFSET" offsetDist plEnt "_NON" ptOut "")
          (setq newEnt (entlast))

          (if (not (equal plEnt newEnt))
            (progn
              (entdel plEnt)   ;; offset result inherits the guide layer from its source
              (princ "\nGuide drawn successfully."))
            (progn
              (entdel plEnt)
              (princ "\nError: Invalid shape geometry."))))))
  )

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
