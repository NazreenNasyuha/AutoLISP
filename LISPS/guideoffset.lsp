(defun c:guideoffset ( / *error* defOff userOff offsetDist layerName 
                       pt ptList oldOs oldCmd plEnt ptOut guideEnt)
  
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

  (princ "\nCommand: GUIDEOFFSET (Continuous Pick Offset - Closed Shape)")
  
  ;; 1. Type distance directly
  (setq userOff (getreal (strcat "\nSpecify offset distance <" (rtos defOff 2 0) ">: ")))
  (setq offsetDist (if (not userOff) defOff userOff))
  (setq layerName (strcat "DRN-GUIDE-" (rtos offsetDist 2 0)))
  
  (if (not (tblsearch "LAYER" layerName))
    (command "_.LAYER" "_M" layerName "_C" 8 "" "")
    (command "_.LAYER" "_C" 8 layerName "")
  )
  
  ;; 2. Infinite loop to collect your trace points
  (setq ptList nil)
  (while (setq pt (getpoint (if ptList "\nClick next point (or Enter to finish): " "\nClick 1st point: ")))
    (setq ptList (append ptList (list pt)))
  )
  
  (if (and ptList (> (length ptList) 2)) ;; Requires at least 3 points to close a shape
    (progn
      (setvar "OSMODE" 0) (setvar "CMDECHO" 0)
      
      ;; 3. Draw the polyline and FORCE it to Close
      (command "_.PLINE")
      (foreach p ptList (command "_NON" p))
      (command "_C") ;; "_C" triggers the "Close" command, sealing the gap
      (setq plEnt (entlast))
      
      (setvar "OSMODE" oldOs) ;; Turn OSNAP back on so user can comfortably click
      
      ;; 4. Wait for user to manually pick the offset direction
      (setq ptOut (getpoint "\nClick to specify offset direction (Inside or Outside): "))
      
      (if ptOut
        (progn
          (setvar "OSMODE" 0) 
          (command "_.OFFSET" offsetDist plEnt "_NON" ptOut "")
          
          (if (not (equal plEnt (entlast)))
            (progn
              (setq guideEnt (entlast))
              (command "_.CHPROP" guideEnt "" "_LA" layerName "")
              (entdel plEnt) ;; Delete the original trace line
              (princ "\nGuide drawn successfully.")
            )
            (progn (entdel plEnt) (princ "\nError: Invalid shape geometry or offset distance too large."))
          )
        )
        (progn (entdel plEnt) (princ "\nAction cancelled. Shape deleted.")) ;; Cleanup if user presses Esc
      )
      
      (setvar "OSMODE" oldOs) (setvar "CMDECHO" oldCmd)
    )
    (princ "\nYou need to click at least 3 points to create a closed shape.")
  )
  (princ)
)