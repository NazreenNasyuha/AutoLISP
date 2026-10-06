(defun c:getareaha ( / ss ent rawArea haArea outText oldCmd )
  
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
      (setq haArea (/ rawArea 10000000000.0))
      (setq outText (rtos haArea 2 decimals))
      
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Area: " outText " Hectares (Copied to Clipboard!)"))
    )
    (princ "\nNo polyline selected.")
  )
  (princ)
)
