;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getareaacre.lsp
;;; COMMAND     : GETAREAACRE
;;; DESCRIPTION : Single-Click Polyline Area Extractor to Acres (ac) with
;;;               Automatic Geometric Centroid Text Injection & Clipboard Copy
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In master planning, land titling, estate development, and regional hydrology,
;;; parcel areas are frequently documented in Acres (ac). When drawings are
;;; prepared in standard civil millimetres (1 m = 1,000 mm):
;;;   1 m²     = 1,000,000 mm²
;;;   1 acre   = 4,046.8564224 m² = 4,046,856,422.4 mm²
;;;
;;; GETAREAACRE provides an instant, zero-friction measurement workflow:
;;;   1. Prompts the drafter to select any closed or open lightweight polyline
;;;      (LWPOLYLINE or 2D/3D POLYLINE).
;;;   2. Queries drawing geometric area via native AutoCAD `_.AREA _O`.
;;;   3. Converts raw millimetre area into imperial Acres (ac).
;;;   4. Computes the geometric vertex centroid of the boundary.
;;;   5. Uses atomic DXF `entmake` to inject a clean, middle-centered TEXT entity
;;;      ("X.XXXX ac") directly inside the boundary footprint.
;;;   6. Pipes the exact numerical acre string directly into the Windows
;;;      Clipboard using native Windows `clip.exe` for immediate Excel pasting.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type GETAREAACRE in the AutoCAD / GstarCAD command line.
;;;  2. Pick the parcel, catchment, or lot polyline boundary.
;;;  3. The text label is automatically created at the parcel centroid, and
;;;     the numeric value is copied to the Windows clipboard for instant use.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - txtHgt   : Height of the injected CAD text entity (default: 1500.0 mm).
;;;  - decimals : Precision decimal places for acre formatting (default: 4).
;;; ==========================================================================

(vl-load-com)

(defun c:getareaacre ( / *error* txtHgt decimals ss ent rawArea acreArea outText center
                         oldCmd oldDimZin undoStarted fmt-dec copy-clip get-poly-center )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq txtHgt 1500.0) ;; Height of the injected area text
  (setq decimals 4)    ;; Decimal places for the final number
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SETUP
  ;; =========================================================================
  (setq oldCmd    (getvar "CMDECHO")
        oldDimZin (getvar "DIMZIN")
  )
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (if undoStarted (progn (command "_.UNDO" "_End") (setq undoStarted nil)))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETAREAACRE] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. ZERO-PADDED FORMATTER, CLIPBOARD & TRUE SHOELACE CENTROID IN WCS
  ;; =========================================================================
  (defun fmt-dec (val dec / s dotPos fracLen)
    (if (fboundp 'jrk:fmt-dec)
      (jrk:fmt-dec val dec)
      (progn
        (setq s (rtos val 2 dec)
              dotPos (vl-string-search "." s)
        )
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
        s
      )
    )
  )

  (defun copy-clip (txt / html pw clip ok tmp fh wsh cmd)
    (if (fboundp 'jrk:copy-clip)
      (jrk:copy-clip txt)
      (progn
        (setq ok nil)
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
        (if (not ok)
          (progn
            (setq tmp (vl-filename-mktemp "jrk_clip_" (getenv "TEMP") ".txt"))
            (if (and tmp (setq fh (open tmp "w")))
              (progn
                (princ txt fh)
                (close fh)
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
    )
  )

  (defun get-poly-center (e / elist pts elev n k p1 p2 cross areaSum cxSum cySum cx cy)
    (if (fboundp 'jrk:get-poly-centroid-wcs)
      (jrk:get-poly-centroid-wcs e)
      (progn
        (setq elist (entget e)
              pts   nil
              elev  (if (assoc 38 elist) (cdr (assoc 38 elist)) 0.0)
        )
        (foreach item elist
          (if (= (car item) 10)
            (setq pts (cons (cdr item) pts))
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
      (setq acreArea (/ rawArea 4046856422.4))
      (setq outText (fmt-dec acreArea decimals))

      ;; Find true WCS centroid and inject middle-centered text
      (setq center (get-poly-center ent))
      (if center
        (entmake (list '(0 . "TEXT")
                       (cons 1 (strcat outText " ac"))
                       (list 10 (car center) (cadr center) (caddr center))
                       (list 11 (car center) (cadr center) (caddr center))
                       (cons 40 txtHgt)
                       '(72 . 1)
                       '(73 . 2)))
      )

      (command "_.UNDO" "_End")
      (setq undoStarted nil)

      ;; Pipe result safely to Windows Clipboard
      (if (copy-clip outText)
        (princ (strcat "\n>> Area: " outText " Acres (Copied to Clipboard!)"))
        (princ (strcat "\n>> Area: " outText " Acres (clipboard unavailable)"))
      )
    )
    (princ "\nNo polyline selected.")
  )
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (princ)
)