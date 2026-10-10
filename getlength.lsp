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

(defun c:getlength ( / *error* divideBy decimals suffix pt1 nextPt lastPt totalLen
                       outText copy-clip)

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
  (defun *error* (msg)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. WINDOWS CLIPBOARD PIPE HELPER
  ;; =========================================================================
  ;; Copy text to the Windows clipboard through a temp file (safe for all characters)
  (defun copy-clip (txt / path fh)
    (setq path (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) ("C:\\Temp")) "\\acad_clip.txt"))
    (if (and (boundp 'startapp) (setq fh (open path "w")))
      (progn
        (princ txt fh)
        (close fh)
        (startapp (strcat "cmd.exe /c clip < \"" path "\""))
        T)
      nil))

  ;; =========================================================================
  ;; 4. INTERACTIVE POINT PICK & ACCUMULATION ENGINE
  ;; =========================================================================
  (while (setq pt1 (getpoint "\nClick Start Point (or Enter to exit): "))
    (setq totalLen 0.0 lastPt pt1)
    (while (setq nextPt (getpoint lastPt "\nClick Next Point (or Enter to finish): "))
      ;; 2D planar length (Z elevation ignored)
      (setq totalLen (+ totalLen (distance (list (car lastPt) (cadr lastPt) 0.0)
                                           (list (car nextPt) (cadr nextPt) 0.0))))
      (setq lastPt nextPt))
    (if (> totalLen 0.0)
      (progn
        (setq outText (rtos (/ totalLen divideBy) 2 decimals))
        (if (copy-clip outText)
          (princ (strcat "\n>> Length: " outText " " suffix " (Copied to Clipboard!)"))
          (princ (strcat "\n>> Length: " outText " " suffix " (clipboard unavailable)")))
        )
      (princ "\nNot enough points clicked.")))
  (princ)
)
