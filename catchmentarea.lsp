(defun c:catchmentarea( / *error* layerName col oldOsnap
                       iPt1 iPt2 iPt3 iPt4 oPt1 oPt2 oPt3 oPt4
                       rPtUser1 rPtUser2 rPt1 rPt2
                       iList oList oSorted iSorted 
                       i0 i1 i2 i3 o0 o1 o2 o3 r0 r1 r2 r3
                       axisA axisB
                       sort-ccw align-lists get-closest make-poly midpt project-pt)
  (vl-load-com)
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq layerName "DRN-TOTALAREA")
  (setq col 4) ;; 4 = Cyan
  ;; =========================================================================

  (setq oldOsnap (getvar "OSMODE"))
  (defun *error* (msg)
    (if oldOsnap (setvar "OSMODE" oldOsnap))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (if (not (tblsearch "LAYER" layerName)) (command "_.LAYER" "_M" layerName "_C" col "" ""))

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
              (setq iPt1 (trans iPt1 1 0)) (setq iPt2 (trans iPt2 1 0)) 
              (setq iPt3 (trans iPt3 1 0)) (setq iPt4 (trans iPt4 1 0))
              (setq oPt1 (trans oPt1 1 0)) (setq oPt2 (trans oPt2 1 0)) 
              (setq oPt3 (trans oPt3 1 0)) (setq oPt4 (trans oPt4 1 0))
              (setq rPtUser1 (trans rPtUser1 1 0)) (setq rPtUser2 (trans rPtUser2 1 0))
              (setvar "OSMODE" 0)

              (defun midpt (pA pB) (list (/ (+ (car pA) (car pB)) 2.0) (/ (+ (cadr pA) (cadr pB)) 2.0) 0.0))
              (defun sort-ccw (pts / cen)
                (setq cen (list (/ (apply '+ (mapcar 'car pts)) 4.0) (/ (apply '+ (mapcar 'cadr pts)) 4.0) 0.0))
                (vl-sort pts (function (lambda (a b) (< (angle cen a) (angle cen b))))))
              (defun align-lists (refList matchList / minSum bestList curList shift i curSum)
                (setq minSum 1e99 bestList matchList shift 0)
                (while (< shift 4)
                  (setq curList matchList curSum 0 i 0)
                  (repeat shift (setq curList (append (cdr curList) (list (car curList)))))
                  (while (< i 4)
                    (setq curSum (+ curSum (distance (nth i refList) (nth i curList))))
                    (setq i (1+ i)))
                  (if (< curSum minSum) (setq minSum curSum bestList curList))
                  (setq shift (1+ shift))) bestList)
              (defun get-closest (pt pA pB) (if (< (distance pt pA) (distance pt pB)) pA pB))
              (defun make-poly (ptList / cleanList lastPt entData)
                (setq cleanList nil lastPt nil)
                (foreach p ptList (if (not (equal p lastPt 1e-6)) (setq cleanList (cons p cleanList) lastPt p)))
                (setq cleanList (reverse cleanList))
                (if (and (> (length cleanList) 2) (equal (car cleanList) (last cleanList) 1e-6))
                  (setq cleanList (reverse (cdr (reverse cleanList)))))
                (setq entData (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                    (cons 8 layerName) (cons 90 (length cleanList)) '(70 . 1)))
                (foreach p cleanList (setq entData (append entData (list (list 10 (car p) (cadr p))))))
                (entmake entData))
              (defun project-pt (P A B / vAB vAP dotABAB dotAPAB t-val)
                (setq vAB (list (- (car B) (car A)) (- (cadr B) (cadr A)) 0.0) vAP (list (- (car P) (car A)) (- (cadr P) (cadr A)) 0.0)
                      dotABAB (+ (* (car vAB) (car vAB)) (* (cadr vAB) (cadr vAB))))
                (if (= dotABAB 0.0) A
                  (progn (setq dotAPAB (+ (* (car vAP) (car vAB)) (* (cadr vAP) (cadr vAB))) t-val (/ dotAPAB dotABAB))
                         (list (+ (car A) (* t-val (car vAB))) (+ (cadr A) (* t-val (cadr vAB))) 0.0))))

              (setq iList (list iPt1 iPt2 iPt3 iPt4) oList (list oPt1 oPt2 oPt3 oPt4))
              (setq oSorted (sort-ccw oList) iSorted (align-lists oSorted (sort-ccw iList)))
              (setq o0 (nth 0 oSorted) o1 (nth 1 oSorted) o2 (nth 2 oSorted) o3 (nth 3 oSorted))
              (setq i0 (nth 0 iSorted) i1 (nth 1 iSorted) i2 (nth 2 iSorted) i3 (nth 3 iSorted))

              (if (< (distance i0 i1) (distance i1 i2))
                (progn (setq axisA (midpt i0 i1) axisB (midpt i2 i3)))
                (progn (setq axisA (midpt i1 i2) axisB (midpt i3 i0))))

              (setq rPt1 (project-pt rPtUser1 axisA axisB) rPt2 (project-pt rPtUser2 axisA axisB))
              (setq r0 (get-closest i0 rPt1 rPt2) r1 (get-closest i1 rPt1 rPt2)
                    r2 (get-closest i2 rPt1 rPt2) r3 (get-closest i3 rPt1 rPt2))

              (make-poly (list o0 o1 i1 r1 r0 i0)) (make-poly (list o1 o2 i2 r2 r1 i1))
              (make-poly (list o2 o3 i3 r3 r2 i2)) (make-poly (list o3 o0 i0 r0 r3 i3))

              (princ "\nSuccess! Catchments drawn.")
            )
          )
        )
      )
    )
  )
  (if oldOsnap (setvar "OSMODE" oldOsnap))
  (princ)
)
