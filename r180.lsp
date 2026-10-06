(defun c:r180 ( / *error* loop sel ent elist basePt basePt_UCS oldCmd)
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (princ "\nCommand: R180 (Click to Flip 180° In Place)")
  (setq loop T)
  (while loop
    (setq sel (entsel "\nClick object to flip 180° (or press Enter to exit): "))
    (if sel
      (progn
        (setq ent (car sel))
        (setq elist (entget ent))
        
        (setq basePt (cdr (assoc 10 elist)))
        (if (not basePt) (setq basePt (cadr sel))) 
        
        (setq basePt_UCS (trans basePt 0 1))
        
        (setvar "CMDECHO" 0)
        (command "_.ROTATE" ent "" "_NON" basePt_UCS 180)
        (setvar "CMDECHO" oldCmd)
      )
      (setq loop nil) 
    )
  )
  (princ)
)
