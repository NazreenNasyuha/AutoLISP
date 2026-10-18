;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : guidemiddle.lsp
;;; COMMAND     : GUIDEMIDDLE (Alias: GMID)
;;; DESCRIPTION : 4-Corner Road Centerline Generator with Start/End Extension
;;;               (Auto-Detects Road Width vs. Corridor Axis & Extends Ends)
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:guidemiddle ( / *error* oldCmd oldOs
                        ;; --- settings ---
                        defExt layerName guideColor guideWidth
                        ;; --- working variables ---
                        userExt extDist pts pt lastPt tempEnts
                        wPts sorted p0 p1 p2 p3 midA midB ang startPt endPt
                        ;; --- local helpers ---
                        ensure-layer clear-temp mark-corner midpt sort-ccw )

  ;; =========================================================================
  ;; 1. USER SETTINGS
  ;; =========================================================================
  (setq defExt     500.0)              ;; Default extension at start & end (500.0 = 500 units/mm; use 500000.0 for 500m in mm)
  (setq layerName  "DRN-GUIDE-MIDDLE") ;; Target layer for road middle guide line
  (setq guideColor 8)                  ;; 8 = Grey (AutoCAD Color Index)
  (setq guideWidth 0.0)                ;; Polyline global width (0.0 = standard thin line)
  ;; =========================================================================

  (setq oldCmd   (getvar "CMDECHO")
        oldOs    (getvar "OSMODE")
        tempEnts nil)

  ;; -------------------------------------------------------------------------
  ;; 2. ERROR HANDLING & GEOMETRY HELPERS
  ;; -------------------------------------------------------------------------
  (defun clear-temp ( / e)
    (foreach e tempEnts
      (if (and e (entget e)) (entdel e))
    )
    (setq tempEnts nil)
  )

  (defun *error* (msg)
    (clear-temp)
    (if oldOs  (setvar "OSMODE" oldOs))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[GUIDEMIDDLE] Action Cancelled: " msg))
    )
    (princ)
  )

  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")
        )
      )
    )
  )

  ;; Draw a small temporary yellow X/circle marker at each clicked corner
  (defun mark-corner (p / wPt)
    (setq wPt (trans p 1 0))
    (if (entmake (list '(0 . "POINT") (cons 8 layerName) '(62 . 2) (cons 10 wPt)))
      (setq tempEnts (cons (entlast) tempEnts))
    )
  )

  (defun midpt (pA pB)
    (list (/ (+ (car pA) (car pB)) 2.0)
          (/ (+ (cadr pA) (cadr pB)) 2.0)
          0.0)
  )

  ;; Sort 4 points counter-clockwise around their centroid
  (defun sort-ccw (ptList / cen)
    (setq cen (list (/ (apply '+ (mapcar 'car ptList)) 4.0)
                    (/ (apply '+ (mapcar 'cadr ptList)) 4.0)
                    0.0))
    (vl-sort ptList (function (lambda (a b) (< (angle cen a) (angle cen b)))))
  )

  ;; =========================================================================
  ;; 3. MAIN WORKFLOW
  ;; =========================================================================
  (setvar "CMDECHO" 0)
  (ensure-layer layerName guideColor)

  (princ "\nCommand: GUIDEMIDDLE (4-Corner Road Centerline Generator)")

  ;; Step 1: Prompt for start/end extension distance
  (setq userExt (getreal (strcat "\nSpecify start/end extension distance <" (rtos defExt 2 0) ">: ")))
  (setq extDist (if (null userExt) defExt userExt))

  ;; Step 2: Pick 4 road corners (in any order, with Undo support)
  (setq pts nil)
  (while (< (length pts) 4)
    (setq lastPt (if pts (last pts) nil))
    (if pts
      (progn
        (initget "Undo")
        (setq pt (getpoint lastPt (strcat "\nClick Road Corner " (itoa (1+ (length pts))) " of 4 [Undo]: ")))
      )
      (setq pt (getpoint "\nClick Road Corner 1 of 4: "))
    )

    (cond
      ((null pt)
       (clear-temp)
       (princ "\n>> Command cancelled.")
       (exit))

      ((= pt "Undo")
       (if pts
         (progn
           (setq pts (reverse (cdr (reverse pts))))
           (if (and tempEnts (entget (car tempEnts)))
             (progn (entdel (car tempEnts)) (setq tempEnts (cdr tempEnts)))
           )
           (princ "\n>> Last corner undone.")
         )
         (princ "\n>> No corners to undo.")
       ))

      ((listp pt)
       (setq pts (append pts (list pt)))
       (mark-corner pt))
    )
  )

  (clear-temp)

  ;; Step 3: Translate to WCS, sort corners, find road ends, and extend centerline
  (setq wPts   (mapcar '(lambda (p) (list (car (trans p 1 0)) (cadr (trans p 1 0)) 0.0)) pts)
        sorted (sort-ccw wPts)
        p0     (nth 0 sorted)
        p1     (nth 1 sorted)
        p2     (nth 2 sorted)
        p3     (nth 3 sorted))

  ;; Compare opposite side pairs: shorter pair = road width ends, longer pair = road sides
  (if (< (+ (distance p0 p1) (distance p2 p3))
         (+ (distance p1 p2) (distance p3 p0)))
    (setq midA (midpt p0 p1)
          midB (midpt p2 p3))
    (setq midA (midpt p1 p2)
          midB (midpt p3 p0))
  )

  ;; Extend both ends outward by extDist
  (setq ang     (angle midA midB)
        startPt (polar midA (+ ang pi) extDist)
        endPt   (polar midB ang extDist))

  ;; Draw the extended road centerline
  (if (> guideWidth 0.0)
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                   (cons 8 layerName) '(90 . 2) '(70 . 0) (cons 43 guideWidth)
                   (list 10 (car startPt) (cadr startPt))
                   (list 10 (car endPt) (cadr endPt))))
    (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine")
                   (cons 8 layerName)
                   (cons 10 startPt)
                   (cons 11 endPt)))
  )

  (princ (strcat "\n>> Success! Road centerline generated on layer " layerName
                 " with " (rtos extDist 2 0) " extension at both ends."))

  (setvar "OSMODE" oldOs)
  (setvar "CMDECHO" oldCmd)
  (princ)
)

;; Quick Command Alias
(defun c:gmid () (c:guidemiddle))