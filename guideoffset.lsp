(defun c:guideoffset ( / *error* defOff userOff offsetDist layerName 
                       pt ptList oldOs oldCmd plEnt ptOut ang p1 p2)
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq defOff 1500.0) ;; Default offset distance
  ;; =========================================================================

  (setq oldCmd (getvar "CMDECHO") oldOs (getvar "OSMODE"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldOs (setvar "OSMODE" oldOs))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nAction Cancelled: " msg)))
    (princ)
  )

  (princ "\nCommand: GUIDEOFFSET (Continuous Pick Offset)")
  
  (setq userOff (getreal (strcat "\nSpecify offset distance <" (rtos defOff 2 0) ">: ")))
  (setq offsetDist (if (not userOff) defOff userOff))
  (setq layerName (strcat "DRN-GUIDE-" (rtos offsetDist 2 0)))
  
  (if (not (tblsearch "LAYER" layerName))
    (command "_.LAYER" "_M" layerName "_C" 8 "" "")
    (command "_.LAYER" "_C" 8 layerName "")
  )

  (princ "\nTip: Trace sequence CLOCKWISE to offset OUTSIDE.")
  
  (setq ptList nil)
  (while (setq pt (getpoint (if ptList "\nClick next point (or Enter to finish): " "\nClick 1st point: ")))
    (setq ptList (append ptList (list pt)))
  )
  
  (if (and ptList (> (length ptList) 1))
    (progn
      (setvar "OSMODE" 0) (setvar "CMDECHO" 0)
      
      (command "_.PLINE")
      (foreach p ptList (command "_NON" p))
      (command "") 
      (setq plEnt (entlast))
      
      (setq p1 (nth 0 ptList) p2 (nth 1 ptList))
      (setq ang (angle p1 p2) ptOut (polar p1 (+ ang (/ pi 2.0)) (* offsetDist 2.0)))
      
      (command "_.OFFSET" offsetDist plEnt "_NON" ptOut "")
      
      (if (not (equal plEnt (entlast)))
        (progn
          (setq guideEnt (entlast))
          (command "_.CHPROP" guideEnt "" "_LA" layerName "")
          (entdel plEnt)
          (princ "\nGuide drawn successfully.")
        )
        (progn (entdel plEnt) (princ "\nError: Invalid shape geometry."))
      )
      (setvar "OSMODE" oldOs) (setvar "CMDECHO" oldCmd)
    )
    (princ "\nNot enough points clicked.")
  )
  (princ)
)
