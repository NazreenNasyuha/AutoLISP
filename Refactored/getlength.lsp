;;; ==========================================================================
;;; GETLENGTH - Click a path, total length is copied to the clipboard
;;; Universal build: no COM; clipboard via CLIP.EXE (Windows) with command-line fallback.
;;; ==========================================================================
(defun c:getlength ( / *error* divideBy decimals suffix pt1 nextPt lastPt totalLen
                       outText copy-clip)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq divideBy 1000.0)   ;; drawing units per output unit (1000 = mm -> m)
  (setq decimals 2)
  (setq suffix   "m")
  ;; =========================================================================

  (defun *error* (msg)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  ;; Copy text to the Windows clipboard through a temp file (safe for any characters)
  (defun copy-clip (txt / path fh)
    (setq path (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) ("C:\\Temp")) "\\acad_clip.txt"))
    (if (and (boundp 'startapp) (setq fh (open path "w")))
      (progn
        (princ txt fh)
        (close fh)
        (startapp (strcat "cmd.exe /c clip < \"" path "\""))
        T)
      nil))

  (while (setq pt1 (getpoint "\nClick Start Point (or Enter to exit): "))
    (setq totalLen 0.0 lastPt pt1)
    (while (setq nextPt (getpoint lastPt "\nClick Next Point (or Enter to finish): "))
      ;; 2D length (Z ignored)
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
