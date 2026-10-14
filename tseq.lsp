(defun c:tseq ( / *error* defSeq userSeq currStr parsed seqType pre val pad loop sel ent elist entType oldTxt newTxt oldCmd replace-id)
  (vl-load-com)
  (setq oldCmd (getvar "CMDECHO"))
  
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )
  
  ;; -------------------------------------------------------------------------
  ;; SMART PARSER: Detects the semicolon boundary to protect pipe details
  ;; -------------------------------------------------------------------------
  (defun replace-id (currTxt newID / pos1 pos2 prefix suffix)
    ;; Check if the text starts with the hidden MTEXT width formatting (e.g. {\W0.5;)
    (if (= (substr currTxt 1 3) "{\\W")
      (progn
        ;; Find the semicolon after the Width factor, then the semicolon after the ID
        (setq pos1 (vl-string-search ";" currTxt))
        (if pos1 (setq pos2 (vl-string-search ";" currTxt (1+ pos1))))
        
        (if pos2
          (progn
            (setq prefix (substr currTxt 1 (1+ pos1)))   ;; Keep "{\W0.5;"
            (setq suffix (substr currTxt (1+ pos2)))     ;; Keep "; \P600%%c\P15m\P1:100}"
            (strcat prefix newID suffix)                 ;; Sandwich the new ID in the middle
          )
          newID ;; Fallback if it's not a pipe label
        )
      )
      (progn
        ;; Normal text (No MTEXT formatting). Find the first semicolon.
        (setq pos1 (vl-string-search ";" currTxt))
        (if pos1
          (strcat newID (substr currTxt (1+ pos1)))      ;; Swap everything before the semicolon
          newID ;; Fallback if it's a raw placeholder (like "P1")
        )
      )
    )
  )

  ;; Helper: Separates the text prefix from the numeric suffix for sequencing
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

  ;; Helper: Maintains zero-padding (e.g. 01, 02)
  (defun pad-num (num padLen / s)
    (setq s (itoa num)) (while (< (strlen s) padLen) (setq s (strcat "0" s))) s)
  
  (princ "\nCommand: TSEQ (Rapid Smart-Rename Sequencer)")
  
  ;; =========================================================================
  ;; 1. PROMPT FOR NEW ID SEQUENCE
  ;; =========================================================================
  (setq defSeq "D01") 
  (setq userSeq (getstring (strcat "\nEnter starting ID sequence <" defSeq ">: ")))
  (setq currStr (if (= userSeq "") defSeq userSeq))
  
  ;; Parse the sequence to figure out how to count it
  (setq parsed (split-str currStr)
        seqType (nth 0 parsed) pre (nth 1 parsed) val (nth 2 parsed) pad (nth 3 parsed))
  
  ;; =========================================================================
  ;; 2. CONTINUOUS CLICK-TO-REPLACE LOOP
  ;; =========================================================================
  (setq loop T)
  (while loop
    (setq sel (entsel (strcat "\nClick pipe text to rename ID to [" currStr "] (or press Enter to exit): ")))
    (if sel
      (progn
        (setq ent (car sel) elist (entget ent) entType (cdr (assoc 0 elist)))
        
        ;; Check if they clicked TEXT or MTEXT
        (if (or (= entType "TEXT") (= entType "MTEXT"))
          (progn
            (setq oldTxt (cdr (assoc 1 elist)))
            
            ;; Execute the smart replacement
            (setq newTxt (replace-id oldTxt currStr))
            
            ;; Update the entity
            (setq elist (subst (cons 1 newTxt) (assoc 1 elist) elist))
            (entmod elist)
            
            ;; Auto-increment the sequence for the next click
            (cond
              ((= seqType "NUM") 
               (setq val (1+ val) currStr (strcat pre (pad-num val pad))))
              ((= seqType "ALPHA")
               (setq val (1+ val))
               (if (= val 91) (setq val 65)) (if (= val 123) (setq val 97))
               (setq currStr (strcat pre (chr val))))
            )
          )
          (princ "\nError: Selected object is not TEXT or MTEXT.")
        )
      )
      (setq loop nil) ;; Exits loop when the user presses Enter
    )
  )
  (princ)
)