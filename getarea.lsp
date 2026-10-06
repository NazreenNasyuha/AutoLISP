;;; ==========================================================================
;;; GETAREA - Select a polyline, copy its area in mm2 / m2 / ha / acres
;;; Universal build: AREA command + CLIP.EXE (no COM).
;;; ==========================================================================
(defun c:getarea ( / *error* sysVars sysVals decimals unitsPerM ss ent
                     rawArea sqm haArea acreArea outText rawLabel copy-clip)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq decimals 3)
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

  (princ "\nCommand: GETAREA (Multi-Unit Extractor)")

  (if (setq ss (ssget "_:S" '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      (setvar "CMDECHO" 0)
      (command "_.AREA" "_O" ent)
      (setq rawArea (getvar "AREA"))

      (setq sqm      (/ rawArea (* unitsPerM unitsPerM))    ;; square metres
            haArea   (/ sqm 10000.0)
            acreArea (/ sqm 4046.8564224)
            rawLabel (if (equal unitsPerM 1000.0 1e-9) " mm2 / " " du2 / "))

      (setq outText (strcat (rtos rawArea 2 2) rawLabel
                            (rtos sqm 2 decimals) " m2 / "
                            (rtos haArea 2 decimals) " ha / "
                            (rtos acreArea 2 decimals) " ac"))
      (if (copy-clip outText)
        (princ (strcat "\n>> Copied to Clipboard: " outText))
        (princ (strcat "\n>> " outText " (clipboard unavailable)"))))
    (princ "\nNo valid polyline selected."))

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
