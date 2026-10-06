;;; ==========================================================================
;;; CATCHMENTAREA - Split a roof/road catchment into 4 polygons (inner, outer, ridge)
;;; Universal build: no vl-sort / COM, no OSMODE or current-layer side effects.
;;; ==========================================================================
(defun c:catchmentarea ( / *error* layerName col ensure-layer
                           iPt1 iPt2 iPt3 iPt4 oPt1 oPt2 oPt3 oPt4 rPtUser1 rPtUser2 rPt1 rPt2
                           iList oList oSorted iSorted
                           i0 i1 i2 i3 o0 o1 o2 o3 r0 r1 r2 r3 axisA axisB
                           midpt sort-ccw align-lists get-closest make-poly project-pt)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq layerName "DRN-TOTALAREA")
  (setq col 4)   ;; 4 = Cyan
  ;; =========================================================================

  (defun *error* (msg)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  (defun ensure-layer (nm cl)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 cl) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" cl nm "")))))

  (defun midpt (pA pB)
    (list (/ (+ (car pA) (car pB)) 2.0) (/ (+ (cadr pA) (cadr pB)) 2.0) 0.0))

  ;; Sort 4 points counter-clockwise about their centroid (selection sort, no vl-sort)
  (defun sort-ccw (pts / cen lst out m k rest)
    (setq cen (list (/ (apply '+ (mapcar 'car pts)) 4.0)
                    (/ (apply '+ (mapcar 'cadr pts)) 4.0) 0.0))
    (setq lst (mapcar '(lambda (q) (cons (angle cen q) q)) pts) out nil)
    (while lst
      (setq m (car lst))
      (foreach k lst (if (< (car k) (car m)) (setq m k)))
      (setq out (cons (cdr m) out) rest nil)
      (foreach k lst (if (not (eq k m)) (setq rest (cons k rest))))
      (setq lst rest))
    (reverse out))

  ;; Rotate matchList so that point-to-point distance to refList is minimal
  (defun align-lists (refList matchList / minSum bestList curList shift k curSum)
    (setq minSum 1e99 bestList matchList shift 0)
    (while (< shift 4)
      (setq curList matchList curSum 0.0 k 0)
      (repeat shift (setq curList (append (cdr curList) (list (car curList)))))
      (while (< k 4)
        (setq curSum (+ curSum (distance (nth k refList) (nth k curList))))
        (setq k (1+ k)))
      (if (< curSum minSum) (setq minSum curSum bestList curList))
      (setq shift (1+ shift)))
    bestList)

  (defun get-closest (pt pA pB)
    (if (< (distance pt pA) (distance pt pB)) pA pB))

  ;; Closed LWPOLYLINE from a point list (removes duplicate consecutive vertices)
  (defun make-poly (ptList / cleanList lastPt entData p)
    (setq cleanList nil lastPt nil)
    (foreach p ptList
      (if (not (equal p lastPt 1e-6))
        (setq cleanList (cons p cleanList) lastPt p)))
    (setq cleanList (reverse cleanList))
    (if (and (> (length cleanList) 2) (equal (car cleanList) (last cleanList) 1e-6))
      (setq cleanList (reverse (cdr (reverse cleanList)))))
    (if (> (length cleanList) 2)
      (progn
        (setq entData (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layerName)
                            '(100 . "AcDbPolyline") (cons 90 (length cleanList)) '(70 . 1)
                            (cons 38 (caddr (car cleanList)))))
        (foreach p cleanList
          (setq entData (append entData (list (list 10 (car p) (cadr p))))))
        (entmake entData))))

  ;; Orthogonal projection of P onto line A-B
  (defun project-pt (P A B / vAB vAP dotABAB tVal)
    (setq vAB (list (- (car B) (car A)) (- (cadr B) (cadr A)) 0.0)
          vAP (list (- (car P) (car A)) (- (cadr P) (cadr A)) 0.0)
          dotABAB (+ (* (car vAB) (car vAB)) (* (cadr vAB) (cadr vAB))))
    (if (= dotABAB 0.0)
      A
      (progn
        (setq tVal (/ (+ (* (car vAP) (car vAB)) (* (cadr vAP) (cadr vAB))) dotABAB))
        (list (+ (car A) (* tVal (car vAB))) (+ (cadr A) (* tVal (cadr vAB))) (caddr A)))))

  ;; ---- user input ---------------------------------------------------------
  (princ "\n--- SELECT 4 CORNERS OF THE INNER HOUSE/SUMP ---")
  (setq iPt1 (getpoint "\nClick 1st Inner Corner: "))
  (if iPt1 (setq iPt2 (getpoint iPt1 "\nClick 2nd Inner Corner: ")))
  (if iPt2 (setq iPt3 (getpoint iPt2 "\nClick 3rd Inner Corner: ")))
  (if iPt3 (setq iPt4 (getpoint iPt3 "\nClick 4th Inner Corner: ")))

  (if (and iPt1 iPt2 iPt3 iPt4)
    (progn
      (princ "\n--- SELECT 4 CORNERS OF THE OUTER ROAD/BOUNDARY ---")
      (setq oPt1 (getpoint "\nClick 1st Outer Corner: "))
      (if oPt1 (setq oPt2 (getpoint oPt1 "\nClick 2nd Outer Corner: ")))
      (if oPt2 (setq oPt3 (getpoint oPt2 "\nClick 3rd Outer Corner: ")))
      (if oPt3 (setq oPt4 (getpoint oPt3 "\nClick 4th Outer Corner: ")))

      (if (and oPt1 oPt2 oPt3 oPt4)
        (progn
          (princ "\n--- SELECT 2 POINTS FOR THE ROOF RIDGE ---")
          (setq rPtUser1 (getpoint "\nClick roughly where Roof Ridge STARTS: "))
          (if rPtUser1 (setq rPtUser2 (getpoint rPtUser1 "\nClick roughly where Roof Ridge ENDS: ")))

          (if (and rPtUser1 rPtUser2)
            (progn
              ;; UCS -> WCS, flattened
              (setq iList (mapcar '(lambda (q) (trans q 1 0)) (list iPt1 iPt2 iPt3 iPt4))
                    oList (mapcar '(lambda (q) (trans q 1 0)) (list oPt1 oPt2 oPt3 oPt4))
                    rPtUser1 (trans rPtUser1 1 0)
                    rPtUser2 (trans rPtUser2 1 0))

              (ensure-layer layerName col)

              (setq oSorted (sort-ccw oList)
                    iSorted (align-lists oSorted (sort-ccw iList)))
              (setq o0 (nth 0 oSorted) o1 (nth 1 oSorted) o2 (nth 2 oSorted) o3 (nth 3 oSorted)
                    i0 (nth 0 iSorted) i1 (nth 1 iSorted) i2 (nth 2 iSorted) i3 (nth 3 iSorted))

              ;; ridge axis runs along the longer inner side
              (if (< (distance i0 i1) (distance i1 i2))
                (setq axisA (midpt i0 i1) axisB (midpt i2 i3))
                (setq axisA (midpt i1 i2) axisB (midpt i3 i0)))

              (setq rPt1 (project-pt rPtUser1 axisA axisB)
                    rPt2 (project-pt rPtUser2 axisA axisB))
              (setq r0 (get-closest i0 rPt1 rPt2) r1 (get-closest i1 rPt1 rPt2)
                    r2 (get-closest i2 rPt1 rPt2) r3 (get-closest i3 rPt1 rPt2))

              (make-poly (list o0 o1 i1 r1 r0 i0))
              (make-poly (list o1 o2 i2 r2 r1 i1))
              (make-poly (list o2 o3 i3 r3 r2 i2))
              (make-poly (list o3 o0 i0 r0 r3 i3))

              (princ "\nSuccess! Catchments drawn."))))))
  )
  (princ)
)
