;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : r180.lsp
;;; COMMAND     : R180
;;; DESCRIPTION : Universal 180-Degree Flipper Around True Visual Middle
;;;               + Live Cursor-Attached Placement (Click to Place / Enter = Keep)
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:r180 ( / *error* oldCmd oldOs preSS loop sel errCode ss
                  get-vla-bbox get-dxf-bbox get-entity-bbox
                  get-true-center-ucs flip-single flip-selection )

  (setq oldCmd (getvar "CMDECHO")
        oldOs  (getvar "OSMODE"))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (if oldOs  (setvar "OSMODE" oldOs))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[R180 Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; 1. ACTIVEX BOUNDING BOX EXTRACTOR (Returns (list pMinWcs pMaxWcs) or nil)
  ;;    Uses global symbols *r180_min* / *r180_max* to prevent AutoCAD local
  ;;    safearray scoping bugs inside vl-catch-all-apply.
  ;; -------------------------------------------------------------------------
  (defun get-vla-bbox (ent / obj ok pMin pMax)
    (setq *r180_min* nil *r180_max* nil ok nil)
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (progn
        (setq ok (vl-catch-all-apply
                   '(lambda () (vla-GetBoundingBox obj '*r180_min* '*r180_max*))))
        (if (and (not (vl-catch-all-error-p ok)) *r180_min* *r180_max*)
          (progn
            (setq pMin (vl-catch-all-apply 'vlax-safearray->list (list *r180_min*))
                  pMax (vl-catch-all-apply 'vlax-safearray->list (list *r180_max*)))
            (if (and (listp pMin) (listp pMax))
              (list pMin pMax)
              nil
            )
          )
          nil
        )
      )
      nil
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 2. PURE DXF BOUNDING BOX & TRUE VISUAL MIDDLE CALCULATOR (WCS)
  ;;    Calculates exact visual center for MTEXT, TEXT, INSERT (Blocks),
  ;;    LINE, LWPOLYLINE, POLYLINE, ARC, CIRCLE, ELLIPSE, SPLINE, LEADER,
  ;;    MULTILEADER, SOLID, TRACE, 3DFACE, HATCH, and DIMENSION.
  ;; -------------------------------------------------------------------------
  (defun get-dxf-bbox (ent / ed typ p10 p11 ang w h att ux uy vx vy dx dy cx cy
                             tb bMin bMax locCx locCy ocsPt wcsPt
                             pts d sub subEd vPt r a1 a2 dAng aMid pStart pEnd pApex
                             bName bRec insPt sx sy rot bPts subBox)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed)))

    (cond
      ;; A. MTEXT: True visual middle from DXF 42 (ActualWidth), DXF 43 (ActualHeight) & DXF 71 (Attachment)
      ((= typ "MTEXT")
       (setq p10 (cdr (assoc 10 ed))
             w   (if (assoc 42 ed) (cdr (assoc 42 ed)) 0.0)
             h   (if (assoc 43 ed) (cdr (assoc 43 ed)) (if (assoc 40 ed) (cdr (assoc 40 ed)) 0.0))
             att (if (assoc 71 ed) (cdr (assoc 71 ed)) 1)
             ang (if (assoc 11 ed)
                   (angle '(0.0 0.0 0.0) (cdr (assoc 11 ed)))
                   (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)))
       (setq dx (cond ((member att '(1 4 7)) (/ w 2.0))
                      ((member att '(2 5 8)) 0.0)
                      (T (/ w -2.0)))
             dy (cond ((member att '(1 2 3)) (/ h -2.0))
                      ((member att '(4 5 6)) 0.0)
                      (T (/ h 2.0))))
       (setq ux (cos ang) uy (sin ang)
             vx (- (sin ang)) vy (cos ang)
             cx (+ (car p10) (* dx ux) (* dy vx))
             cy (+ (cadr p10) (* dx uy) (* dy vy)))
       (list (list (- cx (/ w 2.0)) (- cy (/ h 2.0)) (caddr p10))
             (list (+ cx (/ w 2.0)) (+ cy (/ h 2.0)) (caddr p10))))

      ;; B. TEXT / ATTDEF / ATTRIB: True visual middle via native (textbox ed)
      ((member typ '("TEXT" "ATTDEF" "ATTRIB"))
       (setq p10 (cdr (assoc 10 ed))
             ang (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
             tb  (textbox ed))
       (if tb
         (progn
           (setq bMin  (car tb)
                 bMax  (cadr tb)
                 locCx (/ (+ (car bMin) (car bMax)) 2.0)
                 locCy (/ (+ (cadr bMin) (cadr bMax)) 2.0)
                 ocsPt (list (+ (car p10) (- (* locCx (cos ang)) (* locCy (sin ang))))
                             (+ (cadr p10) (+ (* locCx (sin ang)) (* locCy (cos ang))))
                             (caddr p10))
                 wcsPt (trans ocsPt ent 0))
           (list wcsPt wcsPt)
         )
         (progn
           (setq wcsPt (trans p10 ent 0))
           (list wcsPt wcsPt)
         )
       ))

      ;; C. LINE: Exact 3D midpoint between start (10) and end (11)
      ((= typ "LINE")
       (setq p10 (cdr (assoc 10 ed))
             p11 (cdr (assoc 11 ed)))
       (list (mapcar 'min p10 p11) (mapcar 'max p10 p11)))

      ;; D. LWPOLYLINE: Bounding box of all vertices in WCS
      ((= typ "LWPOLYLINE")
       (setq pts nil)
       (foreach d ed
         (if (= (car d) 10)
           (setq pts (cons (trans (list (cadr d) (caddr d) (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0)) ent 0) pts))
         )
       )
       (if pts
         (list (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)) (apply 'min (mapcar 'caddr pts)))
               (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)) (apply 'max (mapcar 'caddr pts))))
         nil
       ))

      ;; E. Heavy 2D/3D POLYLINE: Walk VERTEX sub-entities
      ((= typ "POLYLINE")
       (setq pts nil sub (entnext ent))
       (while (and sub (/= (cdr (assoc 0 (setq subEd (entget sub)))) "SEQEND"))
         (if (= (cdr (assoc 0 subEd)) "VERTEX")
           (setq vPt (cdr (assoc 10 subEd))
                 pts (cons (trans vPt sub 0) pts))
         )
         (setq sub (entnext sub))
       )
       (if pts
         (list (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)) (apply 'min (mapcar 'caddr pts)))
               (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)) (apply 'max (mapcar 'caddr pts))))
         nil
       ))

      ;; F. ARC: Bounding box of start point, end point, and arc apex
      ((= typ "ARC")
       (setq p10    (cdr (assoc 10 ed))
             r      (cdr (assoc 40 ed))
             a1     (cdr (assoc 50 ed))
             a2     (cdr (assoc 51 ed))
             dAng   (rem (+ (- a2 a1) (* 2.0 pi)) (* 2.0 pi))
             aMid   (+ a1 (/ dAng 2.0))
             pStart (trans (polar p10 a1 r) ent 0)
             pEnd   (trans (polar p10 a2 r) ent 0)
             pApex  (trans (polar p10 aMid r) ent 0)
             pts    (list pStart pEnd pApex))
       (list (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)) (apply 'min (mapcar 'caddr pts)))
             (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)) (apply 'max (mapcar 'caddr pts)))))

      ;; G. CIRCLE / ELLIPSE / POINT
      ((member typ '("CIRCLE" "ELLIPSE" "POINT"))
       (setq wcsPt (if (= typ "CIRCLE") (trans (cdr (assoc 10 ed)) ent 0) (cdr (assoc 10 ed)))
             r     (if (assoc 40 ed) (cdr (assoc 40 ed)) 0.0))
       (list (list (- (car wcsPt) r) (- (cadr wcsPt) r) (caddr wcsPt))
             (list (+ (car wcsPt) r) (+ (cadr wcsPt) r) (caddr wcsPt))))

      ;; H. SOLID / TRACE / 3DFACE / LEADER / SPLINE: Collect all group 10..13 points
      ((member typ '("SOLID" "TRACE" "3DFACE" "LEADER" "SPLINE"))
       (setq pts nil)
       (foreach d ed
         (if (and (member (car d) '(10 11 12 13)) (listp (cdr d)))
           (setq pts (cons (if (member typ '("SOLID" "TRACE")) (trans (cdr d) ent 0) (cdr d)) pts))
         )
       )
       (if pts
         (list (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)) (apply 'min (mapcar 'caddr pts)))
               (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)) (apply 'max (mapcar 'caddr pts))))
         nil
       ))

      ;; I. INSERT (Blocks): Scan block definition sub-entities to find true visual middle
      ((= typ "INSERT")
       (setq bName (cdr (assoc 2 ed))
             insPt (trans (cdr (assoc 10 ed)) ent 0)
             sx    (if (assoc 41 ed) (cdr (assoc 41 ed)) 1.0)
             sy    (if (assoc 42 ed) (cdr (assoc 42 ed)) 1.0)
             rot   (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
             bRec  (tblsearch "BLOCK" bName)
             sub   (if bRec (cdr (assoc -2 bRec)) nil)
             bPts  nil)
       (while sub
         (setq subBox (get-dxf-bbox sub))
         (if subBox
           (foreach p subBox
             (setq cx (+ (car insPt) (- (* (* (car p) sx) (cos rot)) (* (* (cadr p) sy) (sin rot))))
                   cy (+ (cadr insPt) (+ (* (* (car p) sx) (sin rot)) (* (* (cadr p) sy) (cos rot))))
                   bPts (cons (list cx cy (caddr insPt)) bPts))
           )
         )
         (setq sub (entnext sub))
       )
       (if bPts
         (list (list (apply 'min (mapcar 'car bPts)) (apply 'min (mapcar 'cadr bPts)) (apply 'min (mapcar 'caddr bPts)))
               (list (apply 'max (mapcar 'car bPts)) (apply 'max (mapcar 'cadr bPts)) (apply 'max (mapcar 'caddr bPts))))
         (list insPt insPt)
       ))

      ;; J. MULTILEADER / DIMENSION / OTHERS
      (T
       (setq pts nil)
       (foreach d ed
         (if (and (member (car d) '(10 11 12 13)) (listp (cdr d)))
           (setq pts (cons (cdr d) pts))
         )
       )
       (if pts
         (list (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)) (apply 'min (mapcar 'caddr pts)))
               (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)) (apply 'max (mapcar 'caddr pts))))
         nil
       ))
    )
  )

  ;; Combine exact DXF math + ActiveX Bounding Box so every entity type succeeds
  (defun get-entity-bbox (ent / typ box)
    (setq typ (cdr (assoc 0 (entget ent))))
    ;; For MTEXT, TEXT, LINE, LWPOLYLINE, ARC, exact DXF math gives the true geometric center
    (if (member typ '("MTEXT" "TEXT" "ATTDEF" "ATTRIB" "LINE" "LWPOLYLINE" "ARC"))
      (setq box (get-dxf-bbox ent))
    )
    ;; For Blocks (INSERT), Multileaders, Hatches, Dimensions, Splines, etc., try ActiveX BBox then DXF BBox
    (if (null box)
      (setq box (get-vla-bbox ent))
    )
    (if (null box)
      (setq box (get-dxf-bbox ent))
    )
    box
  )

  (defun get-true-center-ucs (ent / box pMin pMax wcsCen)
    (setq box (get-entity-bbox ent))
    (if box
      (progn
        (setq pMin   (car box)
              pMax   (cadr box)
              wcsCen (list (/ (+ (car pMin) (car pMax)) 2.0)
                           (/ (+ (cadr pMin) (cadr pMax)) 2.0)
                           (/ (+ (caddr pMin) (caddr pMax)) 2.0)))
        (trans wcsCen 0 1)
      )
      nil
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 3. FLIP + STICK-TO-CURSOR PLACEMENT ENGINE
  ;;    1. Rotates 180 deg around true visual middle.
  ;;    2. Attaches the object's middle to the cursor via _.MOVE so you see a
  ;;       live preview and can click to place (or press Enter to keep in-place).
  ;; -------------------------------------------------------------------------
  (defun flip-single (ent fallbackPt / baseU)
    (setq baseU (get-true-center-ucs ent))
    (if (null baseU) (setq baseU fallbackPt))
    (if baseU
      (progn
        (setvar "CMDECHO" 0)
        (command "_.UNDO" "_Begin")

        ;; Step 1: Rotate 180 deg around exact middle with OSNAP disabled
        (setvar "OSMODE" 0)
        (command "_.ROTATE" ent "" "_NON" baseU 180.0)

        ;; Step 2: Re-enable user's OSNAP and stick middle of object to cursor until clicked
        (setvar "OSMODE" oldOs)
        (princ "\n>> Click to place rotated object (or press Enter to keep in place): ")
        (command "_.MOVE" ent "" "_NON" baseU pause)
        (while (> (getvar "CMDACTIVE") 0)
          (command baseU)
        )

        (command "_.UNDO" "_End")
        (setvar "OSMODE" oldOs)
        (setvar "CMDECHO" oldCmd)
      )
    )
  )

  (defun flip-selection (selSet asGroup / i n ent baseU allPts box grpWcs grpUcs)
    (setq n (sslength selSet))
    (setvar "CMDECHO" 0)
    (command "_.UNDO" "_Begin")

    ;; Collect bounding box points of the selection to find shared group middle
    (setq i 0 allPts nil)
    (while (< i n)
      (setq box (get-entity-bbox (ssname selSet i)))
      (if box (setq allPts (cons (car box) (cons (cadr box) allPts))))
      (setq i (1+ i))
    )
    (if allPts
      (setq grpWcs (list (/ (+ (apply 'min (mapcar 'car allPts)) (apply 'max (mapcar 'car allPts))) 2.0)
                         (/ (+ (apply 'min (mapcar 'cadr allPts)) (apply 'max (mapcar 'cadr allPts))) 2.0)
                         0.0)
            grpUcs (trans grpWcs 0 1))
    )

    (setvar "OSMODE" 0)
    (if (and asGroup (> n 1))
      ;; GROUP FLIP: Rotate the entire selection together 180 deg around its combined middle
      (if grpUcs
        (command "_.ROTATE" selSet "" "_NON" grpUcs 180.0)
      )
      ;; INDIVIDUAL FLIP: Rotate each object 180 deg in-place around its own true middle
      (progn
        (setq i 0)
        (while (< i n)
          (setq ent   (ssname selSet i)
                baseU (get-true-center-ucs ent))
          (if baseU
            (command "_.ROTATE" ent "" "_NON" baseU 180.0)
          )
          (setq i (1+ i))
        )
      )
    )

    ;; Stick the middle of the rotated selection to the cursor until clicked to place
    (setvar "OSMODE" oldOs)
    (if grpUcs
      (progn
        (princ (strcat "\n>> Flipped " (itoa n) " object(s) 180 deg. Click to place (or press Enter to keep in place): "))
        (command "_.MOVE" selSet "" "_NON" grpUcs pause)
        (while (> (getvar "CMDACTIVE") 0)
          (command grpUcs)
        )
      )
    )

    (command "_.UNDO" "_End")
    (setvar "OSMODE" oldOs)
    (setvar "CMDECHO" oldCmd)
  )

  ;; =========================================================================
  ;; 4. MAIN INTERACTIVE LOOP (1-Click Flip + Stick-to-Cursor Placement)
  ;; =========================================================================
  (princ "\nCommand: R180 (Universal True-Middle 180-Deg Flipper + Cursor Place)")

  ;; If objects are already highlighted before running R180, flip & stick to cursor immediately
  (if (setq preSS (ssget "_I"))
    (progn
      (sssetfirst nil nil)
      (flip-selection preSS nil)
    )
    (progn
      (setq loop T)
      (while loop
        (setvar "ERRNO" 0)
        (initget "Multiple Group Undo")
        (setq sel (entsel "\nClick object to flip 180 deg & place (or click empty space / [Multiple/Group/Undo]) <Exit>: "))
        (setq errCode (getvar "ERRNO"))

        (cond
          ;; User pressed Enter or Spacebar -> Exit cleanly
          ((and (null sel) (= errCode 52))
           (setq loop nil))

          ;; User clicked empty space (ERRNO = 7) or typed "M" -> Window/Multiple selection (Individual middles)
          ((or (= sel "Multiple") (and (null sel) (= errCode 7)))
           (princ "\nSelect objects to flip 180 deg individually around each object's middle: ")
           (if (setq ss (ssget))
             (flip-selection ss nil)
           ))

          ;; User typed "G" -> Window/Multiple selection (Shared Group middle)
          ((= sel "Group")
           (princ "\nSelect objects to flip 180 deg together as a single group: ")
           (if (setq ss (ssget))
             (flip-selection ss T)
           ))

          ;; User typed "U" -> Undo last flip + placement
          ((= sel "Undo")
           (setvar "CMDECHO" 0)
           (command "_.U")
           (setvar "CMDECHO" oldCmd)
           (princ "\n>> Last 180-deg flip & placement undone."))

          ;; User clicked directly on an object -> Instant 1-Click Flip + Stick to Cursor
          ((listp sel)
           (flip-single (car sel) (cadr sel)))

          (T
           (setq loop nil))
        )
      )
    )
  )

  (if oldOs  (setvar "OSMODE" oldOs))
  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)