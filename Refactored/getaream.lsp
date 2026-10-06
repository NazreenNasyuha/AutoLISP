;;; ==========================================================================
;;; GETAREAM - Select a polyline, copy its area in square metres
;;; ==========================================================================
(defun c:getaream ( / *error* sysVars sysVals decimals unitsPerM ss ent
                      rawArea outText copy-clip)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq decimals 4)
  (setq unitsPerM 1000.0)   ;; drawing units per metre (1000 = drawing in mm)
  ;; =========================================================================

  (setq sysVars '("CMDECHO") sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  (defun copy-clip (txt / path fh)
    (setq path (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) ("C:\\Temp")) "\\acad_clip.txt"))
    (if (and (boundp 'startapp) (setq fh (open path "w")))
      (progn (princ txt fh) (close fh)
             (startapp (strcat "cmd.exe /c clip < \"" path "\""))
             T)
      nil))

  (if (setq ss (ssget "_:S" '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      (setvar "CMDECHO" 0)
      (command "_.AREA" "_O" ent)
      (setq rawArea (getvar "AREA"))
      (setq outText (rtos (/ rawArea (* unitsPerM unitsPerM)) 2 decimals))
      (if (copy-clip outText)
        (princ (strcat "\n>> Area: " outText " m2 (Copied to Clipboard!)"))
        (princ (strcat "\n>> Area: " outText " m2 (clipboard unavailable)"))))
    (princ "\nNo polyline selected."))

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
