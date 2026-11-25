;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getareaha.lsp
;;; COMMAND     : GETAREAHA
;;; DESCRIPTION : Single-Click Polyline Area Extractor to Hectares (ha)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:getareaha ( / *error* txtHgt decimals ss ent rawArea convArea outText center
                       oldCmd oldDimZin undoStarted fmt-dec copy-clip get-poly-center )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq txtHgt 1500.0) ;; Height of the injected area text
  (setq decimals 4)    ;; Decimal places for the final number
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SETUP (AutoCAD 2024+ / LT & GstarCAD Safe)
  ;; =========================================================================
  (setq oldCmd      (getvar "CMDECHO")
        oldDimZin   (getvar "DIMZIN")
        undoStarted nil
  )
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (if undoStarted
      (progn
        (if (boundp 'command-s)
          (vl-catch-all-apply 'command-s (list "_.UNDO" "_End"))
          (vl-catch-all-apply 'command (list "_.UNDO" "_End"))
        )
        (setq undoStarted nil)
      )
    )
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETAREAHA] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. ZERO-PADDED FORMATTER, 4-LAYER CLIPBOARD & TRUE SHOELACE CENTROID
  ;; =========================================================================
  (defun fmt-dec (val dec / s dotPos fracLen)
    (if (boundp 'jrk:fmt-dec)
      (jrk:fmt-dec val dec)
      (progn
        (setvar "DIMZIN" 0)
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
                  (setq s (strcat s "0") fracLen (1+ fracLen))
                )
              )
            )
          )
        )
        s
      )
    )
  )

  (defun copy-clip (txt / html pw clip ok tmpDir tmp fh wsh cmd pid safeTxt i ch tmpEnt)
    (if (boundp 'jrk:copy-clip)
      (jrk:copy-clip txt)
      (progn
        (setq ok nil)

        ;; Layer 1: htmlfile COM ClipboardData
        (setq html (vl-catch-all-apply 'vlax-create-object (list "htmlfile")))
        (if (and html (not (vl-catch-all-error-p html)))
          (progn
            (setq pw (vl-catch-all-apply 'vlax-get (list html 'ParentWindow)))
            (if (and pw (not (vl-catch-all-error-p pw)))
              (progn
                (setq clip (vl-catch-all-apply 'vlax-get (list pw 'ClipboardData)))
                (if (and clip (not (vl-catch-all-error-p clip)))
                  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke (list clip 'setData "Text" txt))))
                    (setq ok T)
                  )
                )
              )
            )
            (vl-catch-all-apply 'vlax-release-object (list html))
          )
        )

        ;; Layer 2: WScript.Shell synchronous clip.exe
        (if (not ok)
          (progn
            (setq tmpDir (cond ((getenv "TEMP")) ((getenv "TMP")) ((getvar "TEMPPREFIX")) (".")))
            (setq tmp (if (boundp 'vl-filename-mktemp)
                        (vl-filename-mktemp "jrk_clip_" tmpDir ".txt")
                        (strcat tmpDir "\\jrk_clip.txt")))
            (if (and tmp (setq fh (open tmp "w")))
              (progn
                (princ txt fh)
                (close fh)
                (setq cmd (strcat "cmd.exe /c clip < \"" tmp "\""))
                (setq wsh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
                (if (and wsh (not (vl-catch-all-error-p wsh)))
                  (progn
                    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke (list wsh 'Run cmd 0 :vlax-true))))
                      (setq ok T)
                    )
                    (vl-catch-all-apply 'vlax-release-object (list wsh))
                  )
                )
                (if (boundp 'vl-file-delete) (vl-file-delete tmp))
              )
            )
          )
        )

        ;; Layer 3: AutoCAD LT 2024+ & Windows 11 Safe startapp + clip.exe
        (if (and (not ok) (boundp 'startapp))
          (progn
            (setq safeTxt "" i 1)
            (while (<= i (strlen txt))
              (setq ch (substr txt i 1))
              (if (not (member ch '("\"" "&" "|" "<" ">" "^" "%")))
                (setq safeTxt (strcat safeTxt ch))
              )
              (setq i (1+ i))
            )
            (setq pid (vl-catch-all-apply 'startapp
                        (list "cmd.exe" (strcat "/c <nul set /p=\"" safeTxt "\" | clip"))))
            (if (and pid (not (vl-catch-all-error-p pid)) (numberp pid) (> pid 0))
              (setq ok T)
            )
          )
        )

        ;; Layer 4: Pure CAD Native _.COPYCLIP Fallback
        (if (not ok)
          (if (entmake (list '(0 . "TEXT")
                             '(10 0.0 0.0 0.0)
                             '(40 . 1.0)
                             (cons 1 txt)))
            (progn
              (setq tmpEnt (entlast))
              (vl-catch-all-apply 'command (list "_.COPYCLIP" tmpEnt ""))
              (entdel tmpEnt)
              (setq ok T)
            )
          )
        )

        ok
      )
    )
  )

  (defun get-poly-center (e / elist typ pts elev sub subEd n k p1 p2 cross areaSum cxSum cySum cx cy)
    (if (boundp 'jrk:get-poly-centroid-wcs)
      (jrk:get-poly-centroid-wcs e)
      (progn
        (setq elist (entget e)
              typ   (cdr (assoc 0 elist))
              pts   nil
              elev  (if (assoc 38 elist) (cdr (assoc 38 elist)) 0.0)
        )
        (if (= typ "POLYLINE")
          (progn
            (setq sub (entnext e))
            (while (and sub (/= (cdr (assoc 0 (setq subEd (entget sub)))) "SEQEND"))
              (if (= (cdr (assoc 0 subEd)) "VERTEX")
                (setq pts (cons (cdr (assoc 10 subEd)) pts))
              )
              (setq sub (entnext sub))
            )
          )
          (foreach item elist
            (if (= (car item) 10)
              (setq pts (cons (cdr item) pts))
            )
          )
        )
        (setq pts (reverse pts)
              n   (length pts)
        )
        (if (>= n 3)
          (progn
            (setq areaSum 0.0 cxSum 0.0 cySum 0.0 k 0)
            (while (< k n)
              (setq p1 (nth k pts)
                    p2 (nth (rem (1+ k) n) pts)
                    cross (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
                    areaSum (+ areaSum cross)
                    cxSum (+ cxSum (* (+ (car p1) (car p2)) cross))
                    cySum (+ cySum (* (+ (cadr p1) (cadr p2)) cross))
                    k (1+ k)
              )
            )
            (if (> (abs areaSum) 1e-8)
              (setq cx (/ cxSum (* 3.0 areaSum))
                    cy (/ cySum (* 3.0 areaSum))
              )
              (setq cx (/ (apply '+ (mapcar 'car pts)) (float n))
                    cy (/ (apply '+ (mapcar 'cadr pts)) (float n))
              )
            )
            (trans (list cx cy elev) e 0)
          )
          (if (> n 0)
            (trans (list (caar pts) (cadar pts) elev) e 0)
            nil
          )
        )
      )
    )
  )

  ;; =========================================================================
  ;; 4. BOUNDARY SELECTION & EXECUTION ENGINE
  ;; =========================================================================
  (if (setq ss (ssget "_:S" '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (command "_.UNDO" "_Begin")
      (setq undoStarted T)
      (setq ent (ssname ss 0))

      (command "_.AREA" "_O" ent)
      (setq rawArea (getvar "AREA"))
      (setq convArea (/ rawArea 10000000000.0))
      (setq outText (fmt-dec convArea decimals))

      (setq center (get-poly-center ent))
      (if center
        (entmake (list '(0 . "TEXT")
                       (cons 1 (strcat outText " ha"))
                       (list 10 (car center) (cadr center) (caddr center))
                       (list 11 (car center) (cadr center) (caddr center))
                       (cons 40 txtHgt)
                       '(72 . 1)
                       '(73 . 2)))
      )

      (command "_.UNDO" "_End")
      (setq undoStarted nil)

      (if (copy-clip outText)
        (princ (strcat "\n>> Area: " outText " Hectares (Copied to Clipboard!)"))
        (princ (strcat "\n>> Area: " outText " Hectares (clipboard unavailable)"))
      )
    )
    (princ "\nNo polyline selected.")
  )
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (princ)
)