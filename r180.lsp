;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : r180.lsp
;;; COMMAND     : R180
;;; DESCRIPTION : In-Place 180-Degree Entity Reversal with Entity-Aware
;;;               Geometric Centroid / Insertion Point Detection
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In road, drainage, and water network drafting, arrows, text labels, blocks,
;;; and flow markers frequently point in the reverse direction after copying
;;; or mirroring across road alignments. Rotating entities manually requires
;;; selecting the object, issuing ROTATE, finding or snapping to its center,
;;; and typing 180.
;;;
;;; R180 eliminates this friction with a single-click in-place rotation loop:
;;;   1. Prompts the drafter to click any object.
;;;   2. Inspects the underlying entity type (assoc 0) and computes its true
;;;      natural geometric rotation base point:
;;;        - LINE       : Exact 3D midpoint between (assoc 10) and (assoc 11)
;;;        - LWPOLYLINE : Center of vertex bounding box (trans OCS -> WCS -> UCS)
;;;        - TEXT/INSERT: Insertion base point (trans OCS -> WCS -> UCS)
;;;        - MTEXT/PT   : Primary insertion point (trans WCS -> UCS)
;;;        - Others     : Click pick-point (UCS) fallback
;;;   3. Executes native `_.ROTATE` by exactly 180.0 degrees about this point.
;;;   4. Seamlessly remains in a continuous pick loop so dozens of reversed
;;;      elements can be flipped rapidly with individual clicks.
;;;   5. Exits cleanly upon pressing Enter or Spacebar.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type R180 in the AutoCAD / GstarCAD command line.
;;;  2. Click any flow arrow, block, text, or line to flip it 180° in place.
;;;  3. Continue clicking other inverted elements.
;;;  4. Press Enter to exit.
;;;
;;; COMPATIBILITY & SAFETY:
;;; --------------------------------------------------------------------------
;;;  - 100% vanilla AutoLISP, no ActiveX/COM required.
;;;  - Flawlessly handles arbitrary User Coordinate Systems (UCS) and Entity
;;;    Object Coordinate Systems (OCS/Ecs).
;;; ==========================================================================

(defun c:r180 ( / *error* sysVars sysVals loop sel baseU get-base)

  ;; =========================================================================
  ;; 1. ERROR HANDLING & SYSTEM STATE RESTORATION
  ;; =========================================================================
  (setq sysVars '("CMDECHO") sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  ;; =========================================================================
  ;; 2. GEOMETRIC BASE POINT CALCULATOR (ENTITY-AWARE)
  ;; =========================================================================
  ;; Rotation base point (returned in current UCS) for the picked entity
  (defun get-base (pick / ent ed typ xs ys d)
    (setq ent (car pick) ed (entget ent) typ (cdr (assoc 0 ed)))
    (cond
      ;; LINE: midpoint, so it flips truly "in place"
      ((= typ "LINE")
       (trans (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (cdr (assoc 10 ed)) (cdr (assoc 11 ed))) 0 1))
      ;; LWPOLYLINE: centre of its vertex bounding box (stored in OCS)
      ((= typ "LWPOLYLINE")
       (setq xs nil ys nil)
       (foreach d ed
         (if (= (car d) 10) (setq xs (cons (cadr d) xs) ys (cons (caddr d) ys))))
       (trans (trans (list (/ (+ (apply 'min xs) (apply 'max xs)) 2.0)
                           (/ (+ (apply 'min ys) (apply 'max ys)) 2.0)
                           (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
                     ent 0)
              0 1))
      ;; Insertion-point entities whose group 10 is in OCS
      ((member typ '("TEXT" "CIRCLE" "ARC" "INSERT" "ATTDEF"))
       (trans (trans (cdr (assoc 10 ed)) ent 0) 0 1))
      ;; Insertion-point entities whose group 10 is in WCS
      ((member typ '("MTEXT" "POINT" "ELLIPSE"))
       (trans (cdr (assoc 10 ed)) 0 1))
      ;; Fallback: the click pick point (already in current UCS)
      (T (cadr pick))))

  ;; =========================================================================
  ;; 3. INTERACTIVE FLIP SELECTION LOOP
  ;; =========================================================================
  (princ "\nCommand: R180 (Click to Flip 180 deg In Place)")
  (setq loop T)
  (while loop
    (setq sel (entsel "\nClick object to flip 180 deg (or press Enter to exit): "))
    (if sel
      (progn
        (setq baseU (get-base sel))
        (setvar "CMDECHO" 0)
        (command "_.ROTATE" (car sel) "" "_NON" baseU 180.0))
      (setq loop nil)))

  ;; Restore system state
  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
