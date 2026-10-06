(defun c:getareaacre ( / ss ent rawArea acreArea outText oldCmd )
  
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
      (setq acreArea (/ rawArea 4046856422.4))
      (setq outText (rtos acreArea 2 decimals))
      
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Area: " outText " Acres (Copied to Clipboard!)"))
    )
    (princ "\nNo polyline selected.")
  )
  (princ)
)
