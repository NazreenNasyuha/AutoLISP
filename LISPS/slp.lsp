;;; ---------------------------------------------------------------
;;; SLP v3.1 - Slope semicircle layout generator
;;; Pick facing line -> pick border line -> enter radius
;;; - works on open lines AND closed polylines (pond, rectangle)
;;; - semicircles are cut at the border line (no TRIM used)
;;; - near the ends, cells move closer so they touch each other
;;; - too-short slope lines are removed
;;; - one polyline per slope, layer of the facing line
;;; ---------------------------------------------------------------
(vl-load-com)

;; slope lines shorter than (this x radius) are removed
(setq *slp-minfactor* 0.2)

(defun slp:pick (msg / s res)
  (while (and (not res) (setq s (entsel msg)))
    (if (member (cdr (assoc 0 (entget (car s))))
                '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE"))
      (setq res s)
      (princ "\nMust be a line / polyline / arc / spline.")))
  res)

(defun slp:len (e)
  (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))

(defun slp:unit (d / l)
  (setq l (sqrt (+ (* (car d) (car d)) (* (cadr d) (cadr d)))))
  (if (< l 1e-12) (list 1.0 0.0) (list (/ (car d) l) (/ (cadr d) l))))

(defun slp:best (res / lst best bl o l)
  (setq lst (vlax-safearray->list (vlax-variant-value res)) bl -1.0)
  (foreach o lst
    (setq l (slp:len o))
    (if (> l bl) (setq bl l best o)))
  (foreach o lst
    (if (not (eq o best)) (vl-catch-all-apply 'vla-delete (list o))))
  best)

(defun slp:off (obj dist / res)
  (setq res (vl-catch-all-apply 'vla-offset (list obj dist)))
  (if (vl-catch-all-error-p res) nil (slp:best res)))

;; average distance of a curve from the border (20 sample points)
(defun slp:avgd (o eB / L k sum p)
  (setq L (slp:len o) k 0 sum 0.0)
  (while (< k 20)
    (setq p (vlax-curve-getPointAtDist o (* L (/ (+ k 0.5) 20.0))))
    (setq sum (+ sum (distance p (vlax-curve-getClosestPointTo eB p))))
    (setq k (1+ k)))
  (/ sum 20.0))

;; closed path: wrap the distance around
(defun slp:wrap (d len)
  (if *slp-cl* (- d (* len (fix (/ d len)))) d))

;; point on guide path M at distance d (open path extends straight)
(defun slp:mp (M d len eu / e)
  (setq d (slp:wrap d len))
  (if (<= d len)
    (vlax-curve-getPointAtDist M d)
    (progn
      (setq e (vlax-curve-getEndPoint M))
      (list (+ (car e) (* (- d len) (car eu)))
            (+ (cadr e) (* (- d len) (cadr eu)))
            (caddr e)))))

;; direction of guide path at distance d
(defun slp:mu (M d len eu / prm)
  (setq d (slp:wrap d len))
  (if (>= d len)
    eu
    (progn
      (setq prm (vlax-curve-getParamAtDist M d))
      (slp:unit (vlax-curve-getFirstDeriv M prm)))))

;; next circle centre on M: first point (after passing closer than R)
;; that is exactly R away from the start point S
(defun slp:ctr (M dprev len eu S R maxd / h eps dp d dd below res lo hi md k)
  (setq h (/ R 20.0) eps (* R 1e-4) dp dprev below nil)
  (if (< (distance (slp:mp M dp len eu) S) (- R eps)) (setq below T))
  (while (and (not res) (< dp maxd))
    (setq d  (min maxd (+ dp h))
          dd (distance (slp:mp M d len eu) S))
    (if (< dd (- R eps)) (setq below T))
    (if (and below (>= dd R))
      (progn
        (setq lo dp hi d k 0)
        (while (< k 40)
          (setq md (/ (+ lo hi) 2.0))
          (if (>= (distance (slp:mp M md len eu) S) R)
            (setq hi md)
            (setq lo md))
          (setq k (1+ k)))
        (setq res hi))
      (setq dp d)))
  res)

;; signed side of point pt relative to curve E
(defun slp:side (E pt / cp prm d dl)
  (setq cp  (vlax-curve-getClosestPointTo E pt)
        prm (vlax-curve-getParamAtPoint E cp))
  (if (null prm)
    0.0
    (progn
      (setq d  (vlax-curve-getFirstDeriv E prm)
            dl (sqrt (+ (* (car d) (car d)) (* (cadr d) (cadr d)))))
      (if (< dl 1e-12)
        0.0
        (/ (- (* (car d) (- (cadr pt) (cadr cp)))
              (* (cadr d) (- (car pt) (car cp))))
           dl)))))

(defun slp:out (eB pt sr)
  (< (* (slp:side eB pt) sr) -1e-4))

;; where the circle (centre C, radius R) crosses the border line,
;; on the facing half. want = 'L (behind) or 'R (ahead)
(defun slp:cross (C R eB u nf want / tmp pts k p v al lw best ba)
  (setq tmp (entmakex (list '(0 . "CIRCLE") (cons 10 C) (cons 40 R)))
        pts (vl-catch-all-apply 'vlax-invoke
              (list (vlax-ename->vla-object tmp) 'IntersectWith
                    (vlax-ename->vla-object eB) 0)))
  (entdel tmp)
  (if (and pts (not (vl-catch-all-error-p pts)) (>= (length pts) 3))
    (progn
      (setq k 0)
      (while (< (+ k 2) (length pts))
        (setq p  (list (nth k pts) (nth (+ k 1) pts) (nth (+ k 2) pts))
              v  (list (- (car p) (car C)) (- (cadr p) (cadr C)))
              lw (+ (* (car v) (car nf)) (* (cadr v) (cadr nf)))
              al (+ (* (car v) (car u)) (* (cadr v) (cadr u))))
        (if (>= lw (* -1e-3 R))
          (if (or (null best)
                  (if (= want 'R) (> al ba) (< al ba)))
            (setq best p ba al)))
        (setq k (+ k 3)))))
  best)

;; point where a ray from P hits the border; falls back to the
;; closest point on the border if the ray misses
(defun slp:border (P ang eB / cpB L far tmp pts best bd k Q)
  (setq cpB (vlax-curve-getClosestPointTo eB P)
        L   (max 1.0 (* 20.0 (distance P cpB)))
        far (list (+ (car P) (* L (cos ang))) (+ (cadr P) (* L (sin ang))) 0.0)
        tmp (entmakex (list '(0 . "LINE") (cons 10 P) (cons 11 far)))
        pts (vl-catch-all-apply 'vlax-invoke
              (list (vlax-ename->vla-object tmp) 'IntersectWith
                    (vlax-ename->vla-object eB) 0))
        bd  1e99)
  (entdel tmp)
  (if (and pts (not (vl-catch-all-error-p pts)) (>= (length pts) 3))
    (progn
      (setq k 0)
      (while (< (+ k 2) (length pts))
        (setq Q (list (nth k pts) (nth (+ k 1) pts) (nth (+ k 2) pts)))
        (if (< (distance P Q) bd) (setq bd (distance P Q) best Q))
        (setq k (+ k 3)))))
  (if best best cpB))

(defun slp:pl (lay vl / data v)
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                   '(100 . "AcDbPolyline") (cons 90 (length vl)) '(70 . 0)))
  (foreach v vl
    (setq data (append data
                 (list (cons 10 (list (car (car v)) (cadr (car v))))
                       (cons 42 (cdr v))))))
  (entmake data)
  (entlast))

(defun c:SLP (/ *error* doc selA selB eA eB lay R o1 o2 eM len eu
                chord sgn bul cr P Pn cp ref sr d dc S S0 S2 C u nb nf E Tp cells
                cnt ok pts bpts m i B ang minl cell v1 v2 crs dt th q bg n)

  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (princ))

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (if (and (setq selA (slp:pick "\nClick the FACING line (side where semicircles touch): "))
           (setq selB (slp:pick "\nClick the BORDER line (other side): ")))
    (progn
      (setq eA (car selA) eB (car selB)
            lay (cdr (assoc 8 (entget eA)))
            *slp-cl* (= (vlax-curve-isClosed eA) :vlax-true))

      (initget 6)
      (setq R (getdist (strcat "\nSemicircle radius"
                               (if *slp-r* (strcat " <" (rtos *slp-r*) ">") "")
                               ": ")))
      (if (null R) (setq R *slp-r*))

      (if (null R)
        (princ "\nNo radius given.")
        (progn
          (setq *slp-r* R chord (* 2.0 R) minl (* R *slp-minfactor*))
          (vla-startundomark doc)

          ;; guide line = facing line offset by R toward the border
          (setq o1 (slp:off (vlax-ename->vla-object eA) R)
                o2 (slp:off (vlax-ename->vla-object eA) (- R)))
          (cond
            ((and o1 o2)
             (if (<= (slp:avgd o1 eB) (slp:avgd o2 eB))
               (progn (vla-delete o2) (setq eM o1))
               (progn (vla-delete o1) (setq eM o2))))
            (o1 (setq eM o1))
            (o2 (setq eM o2)))

          (if (null eM)
            (princ "\nOffset failed - check radius / lines.")
            (progn
              (setq len (slp:len eM)
                    eu  (slp:unit (vlax-curve-getFirstDeriv eM (vlax-curve-getEndParam eM)))
                    ref (vlax-curve-getPointAtDist eA (/ (slp:len eA) 2.0))
                    sr  (slp:side eB ref))

              ;; which side is the facing line (bulge direction)
              (setq P  (vlax-curve-getStartPoint eM)
                    Pn (vlax-curve-getPointAtDist eM (min len chord))
                    cp (vlax-curve-getClosestPointTo eA
                         (list (/ (+ (car P) (car Pn)) 2.0)
                               (/ (+ (cadr P) (cadr Pn)) 2.0) 0.0))
                    cr (- (* (- (car Pn) (car P)) (- (cadr cp) (cadr P)))
                          (* (- (cadr Pn) (cadr P)) (- (car cp) (car P)))))
              (if (> cr 0)
                (setq bul -1.0 sgn -1.0)
                (setq bul  1.0 sgn  1.0))

              ;; build the cells one by one
              (setq d 0.0 S P S0 P cells nil cnt 0 ok T)
              (while (and ok (< cnt 2000)
                          (setq dc (slp:ctr eM d len eu S R (+ len chord))))
                (if (and *slp-cl* (>= dc len))
                  ;; closed path: last cell closes back to the first point
                  (progn
                    (if (> (distance S S0) (* 0.3 R))
                      (setq C (list (/ (+ (car S) (car S0)) 2.0)
                                    (/ (+ (cadr S) (cadr S0)) 2.0)
                                    (caddr S))
                            cells (append cells (list (list S S0 C)))
                            cnt (1+ cnt)))
                    (setq ok nil))
                  (progn
                    (setq C  (slp:mp eM dc len eu)
                          u  (slp:mu eM dc len eu)
                          nb (list (* -1.0 sgn (cadr u)) (* sgn (car u)))
                          nf (list (- (car nb)) (- (cadr nb))))
                    (setq S2 (if (slp:out eB S sr)
                               (slp:cross C R eB u nf 'L)
                               S))
                    (setq E  (list (+ (car C) (* R (car u)))
                                   (+ (cadr C) (* R (cadr u)))
                                   (caddr C))
                          Tp (if (slp:out eB E sr)
                               (slp:cross C R eB u nf 'R)
                               E))
                    (if (and S2 Tp
                             (> (+ (* (- (car Tp) (car S2)) (car u))
                                   (* (- (cadr Tp) (cadr S2)) (cadr u)))
                                (* 0.05 R)))
                      (setq cells (append cells (list (list S2 Tp C)))
                            S Tp d dc cnt (1+ cnt))
                      (setq ok nil)))))
              (vla-delete eM)

              (if (null cells)
                (princ "\nNo slopes could be made - check the lines / radius.")
                (progn
                  (setq n (length cells)
                        pts (cons (car (car cells))
                                  (mapcar '(lambda (c) (cadr c)) cells))
                        m (length pts))

                  ;; border end of every slope line
                  (setq i 0 bpts nil)
                  (while (< i m)
                    (setq P (nth i pts)
                          ang (cond ((= i 0) (angle P (nth 1 pts)))
                                    ((= i (1- m)) (angle (nth (- i 1) pts) P))
                                    (t (angle (nth (- i 1) pts) (nth (+ i 1) pts)))))
                    (setq bpts (append bpts
                                 (list (slp:border P (+ ang (* sgn (/ pi 2.0))) eB))))
                    (setq i (1+ i)))

                  ;; one polyline per slope: line (if long enough) + semicircle
                  (setq i 0)
                  (while (< i n)
                    (setq cell (nth i cells)
                          S2 (car cell) Tp (cadr cell) C (caddr cell)
                          v1 (list (- (car S2) (car C)) (- (cadr S2) (cadr C)))
                          v2 (list (- (car Tp) (car C)) (- (cadr Tp) (cadr C)))
                          crs (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
                          dt  (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)))
                          th  (atan (abs crs) dt)
                          q   (/ th 4.0)
                          bg  (* bul (/ (sin q) (cos q)))
                          B   (nth i bpts))
                    (if (and B (> (distance S2 B) minl))
                      (slp:pl lay (list (cons B 0.0) (cons S2 bg) (cons Tp 0.0)))
                      (slp:pl lay (list (cons S2 bg) (cons Tp 0.0))))
                    (setq i (1+ i)))

                  ;; closing line at the very end (open lines only)
                  (if (not *slp-cl*)
                    (progn
                      (setq P (nth (1- m) pts) B (nth (1- m) bpts))
                      (if (and B (> (distance P B) minl))
                        (slp:pl lay (list (cons B 0.0) (cons P 0.0))))))

                  (princ (strcat "\nDone. " (itoa n) " slopes."))))))
          (vla-endundomark doc)))))
  (princ))

(princ "\nType SLP to run.")
(princ)