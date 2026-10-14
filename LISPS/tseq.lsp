;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tseq.lsp
;;; COMMAND     : TSEQ
;;; DESCRIPTION : Smart Alphanumeric Sequence Renumbering Engine for
;;;               TEXT, MTEXT, and Parametric MULTILEADER Entities
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; During civil drawing revisions, pipe or manhole numbering frequently changes
;;; when new connections or sumps are inserted mid-run. Manually editing dozens
;;; of TEXT, MTEXT, and MULTILEADER labels is slow and error-prone.
;;;
;;; TSEQ automates sequential renumbering with intelligent formatting protection:
;;;   1. Prompts the drafter for a starting sequence string (e.g., `D1`, `MH01`,
;;;      `P-100`, or `A`).
;;;   2. Supports numeric and alphabetic sequences, retaining zero-padding
;;;      (e.g., `MH-01` -> `MH-02` -> `...` -> `MH-10`).
;;;   3. Features a non-destructive regex/substring parser (`replace-id`):
;;;        - Preserves MTEXT width factors (e.g., `{\W0.5;...}`).
;;;        - Preserves multi-line engineering breaks (`\P`).
;;;        - Preserves pipe specifications (e.g., `;600mm;25m;1000`).
;;;        - Replaces ONLY the ID segment, leaving engineering data intact.
;;;   4. Seamlessly supports standard TEXT, MTEXT, and ActiveX MULTILEADERs.
;;;   5. Operates in an infinite continuous pick loop: click each label to
;;;      stamp the current sequence and advance to the next number.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `TSEQ` in the CAD command line.
;;;   2. Enter the starting sequence (e.g. `D1` or `MH-01`).
;;;   3. Click each drain label or manhole multileader along the run in sequence.
;;;   4. Press Enter or Spacebar when finished.
;;; ==========================================================================

(defun c:tseq ( / *error* defSeq userSeq currStr parsed seqType pre val pad loop sel ent entType obj oldTxt newTxt oldCmd replace-id)
  (vl-load-com)
  (setq oldCmd (getvar "CMDECHO"))
  
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )
  
  ;; -------------------------------------------------------------------------
  ;; SMART PARSER: Universally protects Drain details (;) and Manhole details (\P)
  ;; -------------------------------------------------------------------------
  (defun replace-id (currTxt newID / pPos prefix remTxt semiPos semi2 slashP endId suffix)
    (setq pPos 0 prefix "")
    
    ;; 1. Check if the text starts with hidden MTEXT formatting (e.g., {\W0.5; or {\C1;)
    (if (= (substr currTxt 1 2) "{\\")
      (if (setq semiPos (vl-string-search ";" currTxt))
        (setq pPos (1+ semiPos) prefix (substr currTxt 1 pPos))
      )
    )
    
    (setq remTxt (substr currTxt (1+ pPos)))
    
    ;; 2. Find where the ID ends: either at the next semicolon (Drainage) or newline (Manhole)
    (setq semi2 (vl-string-search ";" remTxt))
    (setq slashP (vl-string-search "\\P" remTxt))
    
    (setq endId (cond 
                  ((and semi2 slashP) (min semi2 slashP))
                  (semi2 semi2)
                  (slashP slashP)
                  (T (strlen remTxt))
                ))
                
    ;; 3. Sandwich the new ID between the protected prefix and suffix
    (setq suffix (substr remTxt (1+ endId)))
    (strcat prefix newID suffix)
  )

  ;; Helper: Separates the text prefix from the numeric/alphabetic suffix for sequencing
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
  
  (princ "\nCommand: TSEQ (Rapid Smart-Rename Sequencer - Drains & Manholes)")
  
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
    (setq sel (entsel (strcat "\nClick Pipe/Manhole text to rename ID to [" currStr "] (or press Enter to exit): ")))
    (if sel
      (progn
        (setq ent (car sel) entType (cdr (assoc 0 (entget ent))))
        
        ;; Ensure they clicked a valid text object (Now includes MULTILEADER for Manholes)
        (if (member entType '("TEXT" "MTEXT" "MULTILEADER"))
          (progn
            ;; Extract text safely via ActiveX to perfectly handle Multileaders
            (setq obj (vlax-ename->vla-object ent))
            (setq oldTxt (vla-get-TextString obj))
            
            ;; Execute the smart replacement
            (setq newTxt (replace-id oldTxt currStr))
            
            ;; Inject the updated text back into the entity
            (vla-put-TextString obj newTxt)
            
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
          (princ "\nError: Selected object is not TEXT, MTEXT, or MULTILEADER.")
        )
      )
      (setq loop nil) ;; Exits loop when the user presses Enter
    )
  )
  (princ)
)