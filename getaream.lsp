(defun c:getaream ( / ss ent rawArea mArea outText oldCmd )
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq decimals 4)
  ;; =========================================================================
  
  (setq oldCmd (getvar "CMDECHO"))
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      (setvar "CMDECHO" 0) (command "_.AREA" "_O" ent) (if oldCmd (setvar "CMDECHO" oldCmd))
      (setq rawArea (getvar "AREA"))
      (setq mArea (/ rawArea 1000000.0))
      (setq outText (rtos mArea 2 decimals))
      
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Area: " outText " m2 (Copied to Clipboard!)"))
    )
    (princ "\nNo polyline selected.")
  )
  (princ)
)
