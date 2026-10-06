(defun c:tseq ( / *error* ent elist entType currStr parsed seqType pre val pad loop pt pt_ucs oldCmd insPt insPt_ucs newEnt)
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )
  
  (defun split-str (str / len i numStr preStr lst)
    (setq len (strlen str))
    (if (> len 0)
      (progn (setq lst (ascii (substr str len 1)))
        (cond
          ((and (>= lst 48) (<= lst 57))
           (setq i len numStr "")
           (while (and (> i 0) (>= (ascii (substr str i 1)) 48) (<= (ascii (substr str i 1)) 57))
             (setq numStr (strcat (substr str i 1) numStr) i (1- i)))
           (list "NUM" (substr str 1 i) (atoi numStr) (strlen numStr)))
          ((or (and (>= lst 65) (<= lst 90)) (and (>= lst 97) (<= lst 122)))
           (list "ALPHA" (substr str 1 (1- len)) lst 0))
          (T (list "NONE" str 0 0))))
      (list "NONE" str 0 0)))

  (defun pad-num (num padLen / s)
    (setq s (itoa num)) (while (< (strlen s) padLen) (setq s (strcat "0" s))) s)
  
  (setq ent (car (entsel "\nSelect source text to copy (TEXT or MTEXT): ")))
  (if (not ent) (progn (princ "\nNothing selected.") (exit)))
  
  (setq elist (entget ent) entType (cdr (assoc 0 elist)))
  (if (not (or (= entType "TEXT") (= entType "MTEXT")))
    (progn (princ "\nSelected object must be TEXT or MTEXT.") (exit)))
  
  (setq insPt (cdr (assoc 10 elist)) currStr (cdr (assoc 1 elist)) parsed (split-str currStr)
        seqType (nth 0 parsed) pre (nth 1 parsed) val (nth 2 parsed) pad (nth 3 parsed))
  
  (setq loop T)
  (while loop
    (setq pt (getpoint (strcat "\nClick insertion point for [" currStr "] or press Enter to exit: ")))
    (if pt
      (progn
        (setq insPt_ucs (trans insPt 0 1))
        (setq pt_ucs pt)
        
        (setvar "CMDECHO" 0)
        (command "_.COPY" ent "" "_NON" insPt_ucs "_NON" pt_ucs)
        (setvar "CMDECHO" oldCmd)
        
        (setq newEnt (entlast) elist (entget newEnt) elist (subst (cons 1 currStr) (assoc 1 elist) elist))
        (entmod elist)
        
        (cond
          ((= seqType "NUM") (setq val (1+ val) currStr (strcat pre (pad-num val pad))))
          ((= seqType "ALPHA")
           (setq val (1+ val))
           (if (= val 91) (setq val 65)) (if (= val 123) (setq val 97))
           (setq currStr (strcat pre (chr val))))
        )
      )
      (setq loop nil) 
    )
  )
  (princ)
)
