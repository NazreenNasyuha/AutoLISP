(defun c:getarea ( / ss ent rawArea mArea haArea acreArea outText oldCmd )
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq decimals 3)
  ;; =========================================================================

  (setq oldCmd (getvar "CMDECHO"))
  (princ "\nCommand: GETAREA (Multi-Unit Extractor)")
  
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      
      (setvar "CMDECHO" 0)
      (command "_.AREA" "_O" ent)
      (if oldCmd (setvar "CMDECHO" oldCmd))
      
      (setq rawArea (getvar "AREA"))
      (setq mArea (/ rawArea 1000000.0) haArea (/ rawArea 10000000000.0) acreArea (/ rawArea 4046856422.4))
      
      (setq outText (strcat (rtos rawArea 2 2) " mm2 / " (rtos mArea 2 decimals) " m2 / " (rtos haArea 2 decimals) " ha / " (rtos acreArea 2 decimals) " ac"))
      
      (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
      (princ (strcat "\n>> Copied to Clipboard: " outText))
    )
    (princ "\nNo valid polyline selected.")
  )
  (princ)
)
