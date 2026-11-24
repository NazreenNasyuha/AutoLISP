;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getlength.lsp
;;; COMMAND     : GETLENGTH
;;; DESCRIPTION : Interactive Multi-Point Path Distance Accumulator with
;;;               Automatic Unit Scaling and Direct Windows Clipboard Piping
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In civil drafting, estimating, and hydraulic design, engineers frequently
;;; need to measure non-linear paths, pipe alignments, kerb lines, or corridor
;;; lengths without creating throwaway polylines or cluttering the drawing with
;;; temporary dimensions.
;;;
;;; GETLENGTH provides an interactive continuous distance accumulator:
;;;   1. Prompts the drafter to click an initial start point.
;;;   2. Interactively prompts for subsequent vertices, drawing standard rubber-
;;;      band cursor lines from vertex to vertex.
;;;   3. Computes 2D planar Euclidean distance (flattening elevation Z to 0.0)
;;;      to avoid false slope-length inflation on 3D civil surveys.
;;;   4. Scales raw drawing units (e.g. mm) to real-world engineering units (m)
;;;      using a configurable unit divisor (`divideBy`).
;;;   5. Automatically pipes the formatted distance directly to the Windows
;;;      Clipboard (`clip.exe`) via a safe, isolated temporary text buffer.
;;;   6. Gracefully loops, allowing drafters to measure multiple independent
;;;      runs in rapid succession until pressing Enter on an empty prompt.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type GETLENGTH in the AutoCAD / GstarCAD command line.
;;;  2. Click the start point of your alignment or pipe path.
;;;  3. Click consecutive turn points or junction nodes.
;;;  4. Press Enter to finish the path.
;;;  5. The accumulated length is printed to the command prompt and immediately
;;;     available on the Windows Clipboard (Ctrl+V) for Excel, Word, or BQ.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - divideBy : Drawing units per reporting unit (default: 1000.0 for mm -> m).
;;;  - decimals : Precision decimal places for output string (default: 2).
;;;  - suffix   : Unit suffix label printed to command line (default: "m").
;;; ==========================================================================

(vl-load-com)

(defun c:getlength ( / *error* oldDimZin divideBy decimals suffix pt1 nextPt lastPt
                       wLast wNext totalLen outText fmt-dec copy-clip)

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq divideBy 1000.0)   ;; Drawing units per reporting unit (1000.0 = mm -> m)
  (setq decimals 2)        ;; Precision decimal places for formatted text
  (setq suffix   "m")      ;; Unit text label
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & CLEANUP
  ;; =========================================================================
  (setq oldDimZin (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETLENGTH] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. ZERO-PADDED FORMATTER & HARDENED WINDOWS CLIPBOARD HELPER
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

  ;; =========================================================================
  ;; 4. INTERACTIVE POINT PICK & ACCUMULATION ENGINE (Rule #5 UCS -> WCS)
  ;; =========================================================================
  (while (setq pt1 (getpoint "\nClick Start Point (or Enter to exit): "))
    (setq totalLen 0.0 lastPt pt1)
    (while (setq nextPt (getpoint lastPt "\nClick Next Point (or Enter to finish): "))
      ;; Transform UCS pick points to WCS before computing 2D planar length
      (setq wLast (trans lastPt 1 0)
            wNext (trans nextPt 1 0)
            totalLen (+ totalLen (distance (list (car wLast) (cadr wLast) 0.0)
                                           (list (car wNext) (cadr wNext) 0.0)))
            lastPt nextPt
      )
    )
    (if (> totalLen 0.0)
      (progn
        (setq outText (fmt-dec (/ totalLen divideBy) decimals))
        (if (copy-clip outText)
          (princ (strcat "\n>> Length: " outText " " suffix " (Copied to Clipboard!)"))
          (princ (strcat "\n>> Length: " outText " " suffix " (clipboard unavailable)")))
        )
      (princ "\nNot enough points clicked.")))
  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (princ)
)
