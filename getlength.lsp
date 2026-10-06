(defun c:getlength ( / *error* divideBy decimals suffix pt1 nextPt lastPt totalLen finalLen outText)
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq divideBy 1000.0) 
  (setq decimals 2)      
  (setq suffix "m")      
  ;; =========================================================================

  (defun *error* (msg)
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (while (setq pt1 (getpoint "\nClick Start Point (or Enter to exit): "))
    (setq totalLen 0.0 lastPt pt1)
    (while (setq nextPt (getpoint lastPt "\nClick Next Point (or Enter to finish): "))
      (setq totalLen (+ totalLen (distance (list (car lastPt) (cadr lastPt) 0.0) 
                                           (list (car nextPt) (cadr nextPt) 0.0))))
      (setq lastPt nextPt)
    )
    (if (> totalLen 0.0)
      (progn
        (setq finalLen (/ totalLen divideBy))
        (setq outText (rtos finalLen 2 decimals))
        
        (startapp (strcat "cmd.exe /c echo | set /p=" outText "| clip"))
        
        (princ (strcat "\n>> Length: " outText " " suffix " (Copied to Clipboard!)"))
      )
      (princ "\nNot enough points clicked.")
    )
  )
  (princ)
)
