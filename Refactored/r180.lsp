;;; ==========================================================================
;;; R180 - Click objects to flip them 180 degrees in place
;;; Universal build: base point chosen per entity type (centre/midpoint/insertion).
;;; ==========================================================================
(defun c:r180 ( / *error* sysVars sysVals loop sel baseU get-base)

  (setq sysVars '("CMDECHO") sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  ;; Rotation base point (returned in UCS) for the picked entity
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
      ;; insertion-point entities whose group 10 is in OCS
      ((member typ '("TEXT" "CIRCLE" "ARC" "INSERT" "ATTDEF"))
       (trans (trans (cdr (assoc 10 ed)) ent 0) 0 1))
      ;; insertion-point entities whose group 10 is in WCS
      ((member typ '("MTEXT" "POINT" "ELLIPSE"))
       (trans (cdr (assoc 10 ed)) 0 1))
      ;; anything else: the pick point (already UCS)
      (T (cadr pick))))

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

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
