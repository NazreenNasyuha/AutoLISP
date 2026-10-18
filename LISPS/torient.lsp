;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : torient.lsp
;;; COMMAND     : TORIENT (Alias: TROT)
;;; DESCRIPTION : Universal Object Orientation Tool (AutoCAD, LT & GstarCAD)
;;;               Orients Blocks, MText, Text, Multileaders, Lines & Polylines
;;;               in-place by typed angle, 2 picked points, or reference line.
;;; ==========================================================================

(vl-load-com)

(defun c:torient ( / *error* oldCmd ss inp pt2 ucsAng wAng count i ent elist entType
                     pick-line-angle orient-entity )

  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[TORIENT Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; HELPER 1: Extract angle in UCS by clicking an existing Line or Polyline
  ;; -------------------------------------------------------------------------
  (defun pick-line-angle ( / sel ent pPick ed typ p1 p2 param deriv obj)
    (setq sel (entsel "\nClick reference Line or Polyline segment to match angle: "))
    (if sel
      (progn
        (setq ent   (car sel)
              pPick (trans (cadr sel) 1 0)
              ed    (entget ent)
              typ   (cdr (assoc 0 ed)))
        (cond
          ((= typ "LINE")
           (setq p1 (trans (cdr (assoc 10 ed)) 0 1)
                 p2 (trans (cdr (assoc 11 ed)) 0 1))
           (angle p1 p2))

          ((wcmatch typ "*POLYLINE,ARC,SPLINE")
           (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
           (if (and obj (not (vl-catch-all-error-p obj)))
             (progn
               (setq pPick (vlax-curve-getClosestPointTo ent pPick)
                     param (vlax-curve-getParamAtPoint ent pPick)
                     deriv (vlax-curve-getFirstDeriv ent param))
               (angle '(0.0 0.0 0.0) (trans deriv 0 1 T))
             )
             0.0
           ))

          ((member typ '("TEXT" "MTEXT" "INSERT"))
           (setq p1 (cdr (assoc 50 ed)))
           (if p1
             (angle '(0.0 0.0 0.0) (trans (list (cos p1) (sin p1) 0.0) 0 1 T))
             0.0
           ))

          (T 0.0)
        )
      )
      nil
    )
  )

  ;; -------------------------------------------------------------------------
  ;; HELPER 2: Orient any entity in-place to target WCS angle (wAng)
  ;; -------------------------------------------------------------------------
  (defun orient-entity (ent wAng / elist typ obj p1 p2 mid halfLen newP1 newP2
                                   verts sumX sumY cnt cen curAng dAng newElist
                                   v1 v2 v3 midWing maxD k pA pB d)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          obj   (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))

    (cond
      ;; 1. MTEXT (Must update BOTH DXF 11 direction vector and DXF 50 angle)
      ((= typ "MTEXT")
       (if (assoc 11 elist)
         (setq elist (subst (list 11 (cos wAng) (sin wAng) 0.0) (assoc 11 elist) elist))
         (setq elist (append elist (list (list 11 (cos wAng) (sin wAng) 0.0))))
       )
       (if (assoc 50 elist)
         (setq elist (subst (cons 50 wAng) (assoc 50 elist) elist))
         (setq elist (append elist (list (cons 50 wAng))))
       )
       (entmod elist)
       (if (and obj (not (vl-catch-all-error-p obj)))
         (vl-catch-all-apply 'vlax-put-property (list obj 'Rotation wAng))
       )
       T)

      ;; 2. TEXT, BLOCK (INSERT), ATTRIB, ATTDEF, SHAPE
      ((member typ '("TEXT" "INSERT" "ATTRIB" "ATTDEF" "SHAPE"))
       (if (assoc 50 elist)
         (setq elist (subst (cons 50 wAng) (assoc 50 elist) elist))
         (setq elist (append elist (list (cons 50 wAng))))
       )
       (entmod elist)
       (if (and obj (not (vl-catch-all-error-p obj)))
         (vl-catch-all-apply 'vlax-put-property (list obj 'Rotation wAng))
       )
       (entupd ent)
       T)

      ;; 3. MULTILEADER
      ((= typ "MULTILEADER")
       (if (and obj (not (vl-catch-all-error-p obj)))
         (progn
           (vl-catch-all-apply 'vlax-put-property (list obj 'TextRotation wAng))
           (vl-catch-all-apply 'vla-update (list obj))
           T
         )
         nil
       ))

      ;; 4. LINE (Pivots around its own midpoint to match target angle)
      ((= typ "LINE")
       (setq p1      (cdr (assoc 10 elist))
             p2      (cdr (assoc 11 elist))
             mid     (list (/ (+ (car p1) (car p2)) 2.0)
                           (/ (+ (cadr p1) (cadr p2)) 2.0)
                           (/ (+ (caddr p1) (caddr p2)) 2.0))
             halfLen (/ (distance p1 p2) 2.0)
             newP1   (polar mid (+ wAng pi) halfLen)
             newP2   (polar mid wAng halfLen))
       (setq elist (subst (cons 10 newP1) (assoc 10 elist) elist))
       (setq elist (subst (cons 11 newP2) (assoc 11 elist) elist))
       (entmod elist)
       T)

      ;; 5. LWPOLYLINE (Flow arrows, boxes, guide polylines - rotates around centroid)
      ((= typ "LWPOLYLINE")
       (setq verts nil sumX 0.0 sumY 0.0 cnt 0)
       (foreach item elist
         (if (= (car item) 10)
           (setq verts (append verts (list (cdr item)))
                 sumX  (+ sumX (cadr item))
                 sumY  (+ sumY (caddr item))
                 cnt   (1+ cnt))
         )
       )
       (if (>= cnt 2)
         (progn
           (setq cen (list (/ sumX cnt) (/ sumY cnt)))
           ;; Detect 3-vertex flow arrow vs standard polyline
           (if (and (= cnt 3)
                    (= (logand (cdr (assoc 70 elist)) 1) 0)
                    (equal (distance (nth 0 verts) (nth 1 verts))
                           (distance (nth 1 verts) (nth 2 verts)) 1.0))
             (progn
               (setq v1      (nth 0 verts)
                     v2      (nth 1 verts)
                     v3      (nth 2 verts)
                     midWing (list (/ (+ (car v1) (car v3)) 2.0)
                                   (/ (+ (cadr v1) (cadr v3)) 2.0))
                     curAng  (angle midWing v2)
                     cen     v2)
             )
             (progn
               ;; Find longest segment angle as baseline orientation
               (setq maxD -1.0 curAng 0.0 k 0)
               (while (< k (1- cnt))
                 (setq pA (nth k verts)
                       pB (nth (1+ k) verts)
                       d  (distance pA pB))
                 (if (> d maxD)
                   (setq maxD d curAng (angle pA pB))
                 )
                 (setq k (1+ k))
               )
             )
           )
           (setq dAng (- wAng curAng)
                 newElist nil)
           (foreach item elist
             (if (= (car item) 10)
               (progn
                 (setq p1    (cdr item)
                       newP1 (polar cen (+ (angle cen p1) dAng) (distance cen p1)))
                 (setq newElist (cons (list 10 (car newP1) (cadr newP1)) newElist))
               )
               (setq newElist (cons item newElist))
             )
           )
           (entmod (reverse newElist))
           T
         )
         nil
       ))

      ;; 6. Any Other Object (Rotates around DXF 10 base point if present)
      (T
       (if (and obj (not (vl-catch-all-error-p obj)) (vlax-property-available-p obj 'Rotation T))
         (progn
           (vl-catch-all-apply 'vlax-put-property (list obj 'Rotation wAng))
           T
         )
         nil
       ))
    )
  )

  ;; =========================================================================
  ;; MAIN WORKFLOW
  ;; =========================================================================
  (princ "\nCommand: TORIENT (Universal Object Orientation - Blocks, MText, Text, Lines)")

  ;; 1. Select ANY objects (No restrictive filter)
  (setq ss (ssget))
  (if (not ss)
    (progn (princ "\nNo objects selected.") (exit))
  )

  ;; 2. Unified Prompt: Click 1st point, Type Angle in degrees, or type O for Object
  (initget 128 "Object Line")
  (setq inp (getpoint "\nSpecify 1st alignment point, type Angle (deg), or [Object] <0>: "))

  (setq ucsAng
    (cond
      ;; User pressed Enter -> 0 degrees
      ((null inp) 0.0)

      ;; User typed "O" or "L" to pick a reference Line/Object
      ((or (= inp "Object") (= inp "Line"))
       (pick-line-angle))

      ;; User clicked a 1st point on screen -> Ask for 2nd point
      ((listp inp)
       (setq pt2 (getpoint inp "\nSpecify 2nd alignment point: "))
       (if pt2 (angle inp pt2) nil))

      ;; User typed a number directly (e.g. "0", "45", "90", "-30")
      ((= (type inp) 'STR)
       (if (distof inp)
         (* pi (/ (distof inp) 180.0))
         (progn (princ "\nInvalid angle input.") nil)
       ))

      (T nil)
    )
  )

  ;; 3. Convert UCS Angle to WCS Angle & Orient All Selected Entities
  (if ucsAng
    (progn
      (setq wAng  (angle '(0.0 0.0 0.0) (trans (list (cos ucsAng) (sin ucsAng) 0.0) 1 0 T))
            count 0
            i     0)

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (orient-entity ent wAng)
          (setq count (1+ count))
        )
        (setq i (1+ i))
      )

      (princ (strcat "\n>> TORIENT: Oriented " (itoa count) " object(s) to "
                     (rtos (* 180.0 (/ ucsAng pi)) 2 1) " deg."))
    )
    (princ "\n>> TORIENT cancelled.")
  )

  (setvar "CMDECHO" oldCmd)
  (princ)
)

;; Alias TROT
(defun c:trot () (c:torient))