;; ============================================================================
;; JRK-CORE.LSP — Shared AutoLISP Civil Engineering Core Kernel
;; ============================================================================
;; Provides reusable, standards-compliant utilities across the AutoLISP suite
;; in accordance with autolisp_rules_standards.md (Rules #1 through #9):
;;   - jrk:esc-filter            : Rule #1 Wildcard Escaping for ssget filters
;;   - jrk:get-text              : Rule #2 3-Layer Read Hierarchy (TEXT/MTEXT/MULTILEADER)
;;   - jrk:put-text              : Rule #2 3-Layer Write Hierarchy
;;   - jrk:split-mtext-style     : Rule #3 Inline MTEXT Formatting Preservation
;;   - jrk:fmt-dec / jrk:fmt-2dec: Rule #4 Zero-Padded Engineering Decimal Formatter
;;   - jrk:get-poly-centroid-wcs : Rule #5 & #6 True Shoelace Centroid + OCS->WCS
;;   - jrk:copy-clip             : Hardened 2-Stage Clipboard Helper (No static temp file)
;; ============================================================================

(vl-load-com)

;; ----------------------------------------------------------------------------
;; Rule #1: Wildcard Escaping for ssget Layer & Text Filters
;; ----------------------------------------------------------------------------
(defun jrk:esc-filter (str / i ch out)
  (setq out "" i 1)
  (if str
    (while (<= i (strlen str))
      (setq ch (substr str i 1))
      (if (wcmatch ch "[#*?@.~`[\\],`]")
        (setq out (strcat out "`" ch))
        (setq out (strcat out ch))
      )
      (setq i (1+ i))
    )
  )
  out
)

;; ----------------------------------------------------------------------------
;; Rule #2: 3-Layer Entity Text Extraction (TEXT, MTEXT, MULTILEADER)
;; ----------------------------------------------------------------------------
(defun jrk:get-text (ent / ed typ str vla res rest)
  (if (and ent (setq ed (entget ent)))
    (progn
      (setq typ (cdr (assoc 0 ed))
            str nil
      )
      ;; Layer 1: Modern Property Engine
      (if (and (null str) (fboundp 'getpropertyvalue))
        (progn
          (setq res
            (cond
              ((= typ "MULTILEADER") (vl-catch-all-apply 'getpropertyvalue (list ent "MText/Contents")))
              ((= typ "MTEXT")       (vl-catch-all-apply 'getpropertyvalue (list ent "Contents")))
              ((= typ "TEXT")        (vl-catch-all-apply 'getpropertyvalue (list ent "TextString")))
              (t nil)
            )
          )
          (if (and res (not (vl-catch-all-error-p res)) (= (type res) 'STR) (/= res ""))
            (setq str res)
          )
        )
      )
      ;; Layer 2: ActiveX VLA COM Fallback
      (if (null str)
        (progn
          (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
          (if (and vla (not (vl-catch-all-error-p vla)))
            (progn
              (setq res (vl-catch-all-apply 'vlax-get-property (list vla 'TextString)))
              (if (and res (not (vl-catch-all-error-p res)) (= (type res) 'STR))
                (setq str res)
              )
            )
          )
        )
      )
      ;; Layer 3: Pure DXF Fallback (handles DXF 304 skipping "LEADER_LINE{" and DXF 3 + 1 chunks)
      (if (null str)
        (cond
          ((= typ "MULTILEADER")
           (setq rest ed)
           (while rest
             (if (and (= (caar rest) 304) (/= (cdar rest) "LEADER_LINE{"))
               (setq str (cdar rest) rest nil)
               (setq rest (cdr rest))
             )
           )
          )
          ((or (= typ "TEXT") (= typ "MTEXT"))
           (setq str "")
           (foreach pair ed
             (if (or (= (car pair) 3) (= (car pair) 1))
               (setq str (strcat str (cdr pair)))
             )
           )
          )
        )
      )
      str
    )
  )
)

;; ----------------------------------------------------------------------------
;; Rule #2: 3-Layer Entity Text Modification (TEXT, MTEXT, MULTILEADER)
;; ----------------------------------------------------------------------------
(defun jrk:put-text (ent newstr / ed typ ok res vla out replaced pair)
  (if (and ent newstr (setq ed (entget ent)))
    (progn
      (setq typ (cdr (assoc 0 ed))
            ok  nil
      )
      ;; Layer 1: Modern Property Engine
      (if (and (not ok) (fboundp 'setpropertyvalue))
        (progn
          (setq res
            (cond
              ((= typ "MULTILEADER") (vl-catch-all-apply 'setpropertyvalue (list ent "MText/Contents" newstr)))
              ((= typ "MTEXT")       (vl-catch-all-apply 'setpropertyvalue (list ent "Contents" newstr)))
              ((= typ "TEXT")        (vl-catch-all-apply 'setpropertyvalue (list ent "TextString" newstr)))
              (t nil)
            )
          )
          (if (and (not (vl-catch-all-error-p res))
                   (= (jrk:get-text ent) newstr))
            (setq ok T)
          )
        )
      )
      ;; Layer 2: ActiveX VLA COM Fallback
      (if (not ok)
        (progn
          (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
          (if (and vla (not (vl-catch-all-error-p vla)))
            (progn
              (setq res (vl-catch-all-apply 'vlax-put-property (list vla 'TextString newstr)))
              (if (not (vl-catch-all-error-p res))
                (progn
                  (vl-catch-all-apply 'vla-update (list vla))
                  (if (= (jrk:get-text ent) newstr)
                    (setq ok T)
                  )
                )
              )
            )
          )
        )
      )
      ;; Layer 3: Pure DXF Fallback
      (if (not ok)
        (cond
          ((= typ "MULTILEADER")
           (setq out nil replaced nil)
           (foreach pair ed
             (if (and (not replaced) (= (car pair) 304) (/= (cdr pair) "LEADER_LINE{"))
               (progn
                 (setq out (cons (cons 304 newstr) out))
                 (setq replaced T)
               )
               (setq out (cons pair out))
             )
           )
           (if replaced
             (progn (entmod (reverse out)) (entupd ent) (setq ok T))
           )
          )
          ((or (= typ "TEXT") (= typ "MTEXT"))
           (setq out nil)
           (foreach pair ed
             (if (/= (car pair) 3)
               (setq out (cons pair out))
             )
           )
           (setq ed (reverse out))
           (if (assoc 1 ed)
             (setq ed (subst (cons 1 newstr) (assoc 1 ed) ed))
             (setq ed (append ed (list (cons 1 newstr))))
           )
           (entmod ed)
           (entupd ent)
           (setq ok T)
          )
        )
      )
      (if ok (entupd ent))
      ok
    )
  )
)

;; ----------------------------------------------------------------------------
;; Rule #3: MTEXT Inline Formatting Preservation (Splits prefix/content/suffix)
;; ----------------------------------------------------------------------------
(defun jrk:split-mtext-style (str / pfx sfx core semiPos loopP)
  (setq pfx "" sfx "" core (if str str ""))
  (if (and (> (strlen core) 2)
           (= (substr core 1 1) "{")
           (= (substr core (strlen core) 1) "}"))
    (progn
      (setq pfx "{"
            sfx "}"
            core (substr core 2 (- (strlen core) 2))
      )
    )
  )
  (setq loopP T)
  (while (and loopP
              (> (strlen core) 2)
              (= (substr core 1 1) "\\")
              (wcmatch (substr core 1 2) "\\[ACFHQTWfpc]"))
    (setq semiPos (vl-string-search ";" core))
    (if semiPos
      (setq pfx  (strcat pfx (substr core 1 (1+ semiPos)))
            core (substr core (+ semiPos 2))
      )
      (setq loopP nil)
    )
  )
  (list pfx core sfx)
)

;; ----------------------------------------------------------------------------
;; Rule #4: Zero-Padded Engineering Decimal Formatting
;; ----------------------------------------------------------------------------
(defun jrk:fmt-dec (val dec / s dotPos fracLen)
  (setq s (rtos val 2 dec))
  (if (> dec 0)
    (progn
      (setq dotPos (vl-string-search "." s))
      (if (null dotPos)
        (progn
          (setq s (strcat s "."))
          (repeat dec (setq s (strcat s "0")))
        )
        (progn
          (setq fracLen (- (strlen s) dotPos 1))
          (while (< fracLen dec)
            (setq s (strcat s "0")
                  fracLen (1+ fracLen)
            )
          )
        )
      )
    )
  )
  s
)

(defun jrk:fmt-2dec (val)
  (jrk:fmt-dec val 2)
)

;; ----------------------------------------------------------------------------
;; Rule #5 & #6: True Polygon Area Centroid (Shoelace Formula) + OCS->WCS
;; ----------------------------------------------------------------------------
(defun jrk:get-poly-centroid-wcs (ent / ed pts elev n i p1 p2 cross areaSum cxSum cySum cx cy ptOcs vla minPt maxPt pMin pMax)
  (setq ed (entget ent)
        pts nil
        elev (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0)
  )
  (foreach item ed
    (if (= (car item) 10)
      (setq pts (cons (cdr item) pts))
    )
  )
  (setq pts (reverse pts)
        n   (length pts)
  )
  (cond
    ;; Case 1: LWPOLYLINE with >= 3 vertices -> Shoelace true area centroid in OCS, then (trans pt ent 0)
    ((>= n 3)
     (setq areaSum 0.0 cxSum 0.0 cySum 0.0 i 0)
     (while (< i n)
       (setq p1 (nth i pts)
             p2 (nth (rem (1+ i) n) pts)
             cross (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
             areaSum (+ areaSum cross)
             cxSum (+ cxSum (* (+ (car p1) (car p2)) cross))
             cySum (+ cySum (* (+ (cadr p1) (cadr p2)) cross))
             i (1+ i)
       )
     )
     (if (> (abs areaSum) 1e-8)
       (setq cx (/ cxSum (* 3.0 areaSum))
             cy (/ cySum (* 3.0 areaSum))
       )
       (progn
         (setq cx (/ (apply '+ (mapcar 'car pts)) (float n))
               cy (/ (apply '+ (mapcar 'cadr pts)) (float n))
         )
       )
     )
     (setq ptOcs (list cx cy elev))
     (trans ptOcs ent 0)
    )
    ;; Case 2: Non-LWPOLYLINE or < 3 vertices -> ActiveX BoundingBox midpoint (already WCS)
    (t
     (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
     (if (and vla (not (vl-catch-all-error-p vla))
              (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list vla 'minPt 'maxPt)))))
       (progn
         (setq pMin (vlax-safearray->list minPt)
               pMax (vlax-safearray->list maxPt)
         )
         (list (/ (+ (car pMin) (car pMax)) 2.0)
               (/ (+ (cadr pMin) (cadr pMax)) 2.0)
               (/ (+ (caddr pMin) (caddr pMax)) 2.0))
       )
       (if pts
         (trans (list (caar pts) (cadar pts) elev) ent 0)
         '(0.0 0.0 0.0)
       )
     )
    )
  )
)

;; ----------------------------------------------------------------------------
;; Security-Hardened 2-Stage Clipboard Helper (No static temp file, no echo injection)
;; ----------------------------------------------------------------------------
(defun jrk:copy-clip (str / html pw clip ok tmp f wsh cmd)
  (setq ok nil)
  ;; Stage 1: Direct in-memory COM HTMLFile ClipboardData API
  (setq html (vl-catch-all-apply 'vlax-create-object (list "htmlfile")))
  (if (and html (not (vl-catch-all-error-p html)))
    (progn
      (setq pw (vl-catch-all-apply 'vlax-get (list html 'ParentWindow)))
      (if (and pw (not (vl-catch-all-error-p pw)))
        (progn
          (setq clip (vl-catch-all-apply 'vlax-get (list pw 'ClipboardData)))
          (if (and clip (not (vl-catch-all-error-p clip)))
            (if (not (vl-catch-all-error-p
                       (vl-catch-all-apply 'vlax-invoke (list clip 'setData "Text" str))))
              (setq ok T)
            )
          )
        )
      )
      (vl-catch-all-apply 'vlax-release-object (list html))
    )
  )
  ;; Stage 2: Randomized temp file piped into clip.exe and deleted immediately
  (if (not ok)
    (progn
      (setq tmp (vl-filename-mktemp "jrk_clip_" (getenv "TEMP") ".txt"))
      (if (and tmp (setq f (open tmp "w")))
        (progn
          (princ str f)
          (close f)
          (setq cmd (strcat "cmd.exe /c clip < \"" tmp "\""))
          (setq wsh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
          (if (and wsh (not (vl-catch-all-error-p wsh)))
            (progn
              (vl-catch-all-apply 'vlax-invoke (list wsh 'Run cmd 0 :vlax-true))
              (vl-catch-all-apply 'vlax-release-object (list wsh))
              (setq ok T)
            )
          )
          (vl-catch-all-apply 'vl-file-delete (list tmp))
        )
      )
    )
  )
  ok
)

(princ)
