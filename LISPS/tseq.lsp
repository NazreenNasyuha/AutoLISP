;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tseq.lsp
;;; COMMAND     : TSEQ
;;; DESCRIPTION : Rapid Smart-Rename Sequencer with AutoCAD MText Boundary
;;;               Protection & Alpha/Numeric Auto-Incrementation
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In civil drainage and pipe network drafting, multi-line MTEXT labels embed
;;; crucial hydraulic data formatted with width factors and line breaks:
;;;   e.g. "{\W0.5;D01; \P600%%c\P15m\P1:100}"
;;; Standard CAD text editors or find/replace tools often corrupt or strip these
;;; internal formatting codes when drafters renumber pipes.
;;;
;;; TSEQ provides an intelligent in-place sequencer that preserves formatting:
;;;   1. Prompts the drafter for an initial starting sequence (e.g. "D01" or "P01").
;;;   2. Analyzes the sequence structure:
;;;        - Detects alphanumeric prefix (e.g. "D")
;;;        - Extracts initial numeric value (e.g. 1)
;;;        - Preserves zero-padding formatting (e.g. "01", "001")
;;;        - Supports alphabetic incrementation (A -> B -> C ...)
;;;   3. Enters a rapid, continuous click-to-rename loop.
;;;   4. Uses a smart semicolon parser (`replace-id`) to isolate and replace ONLY
;;;      the pipe ID token while leaving the MText width factor (`{\W0.5;`) and
;;;      all subsequent pipe size, length, and gradient paragraphs completely intact.
;;;   5. Auto-increments the sequence counter after each click, allowing dozens
;;;      of consecutive pipes along a mainline to be renumbered in seconds.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type TSEQ in the AutoCAD / GstarCAD command line.
;;;  2. Enter the starting sequence identifier (e.g. "D01" or press Enter for default).
;;;  3. Click pipe label texts in downstream sequence.
;;;  4. The pipe ID updates dynamically to [D01], [D02], [D03] without disturbing
;;;     pipe diameter, length, or slope formatting.
;;;  5. Press Enter or Spacebar to exit.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - defSeq : Default starting identifier sequence (default: "D01").
;;; ==========================================================================

(defun c:tseq ( / *error* defSeq userSeq currStr parsed seqType pre val pad
                  loop sel ent elist entType oldTxt newTxt oldCmd
                  replace-id split-str pad-num )
  (vl-load-com)
  (setq oldCmd (getvar "CMDECHO"))
  
  ;; =========================================================================
  ;; 1. ERROR HANDLING & SYSTEM SETUP
  ;; =========================================================================
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )
  
  ;; =========================================================================
  ;; 2. MTEXT BOUNDARY PARSER & INCREMENTATION HELPERS
  ;; =========================================================================
  ;; Detects the semicolon boundary to protect pipe details and MTEXT formatting
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
  ;; 3. SEQUENCE INITIALIZATION PROMPT
  ;; =========================================================================
  (setq defSeq "D01") 
  (setq userSeq (getstring (strcat "\nEnter starting ID sequence <" defSeq ">: ")))
  (setq currStr (if (= userSeq "") defSeq userSeq))
  
  ;; Parse the sequence to configure the counting engine
  (setq parsed (split-str currStr)
        seqType (nth 0 parsed) pre (nth 1 parsed) val (nth 2 parsed) pad (nth 3 parsed))
  
  ;; =========================================================================
  ;; 4. CONTINUOUS CLICK-TO-REPLACE EXECUTION LOOP
  ;; =========================================================================
  (setq loop T)
  (while loop
    (setq sel (entsel (strcat "\nClick pipe text to rename ID to [" currStr "] (or press Enter to exit): ")))
    (if sel
      (progn
        (setq ent (car sel) elist (entget ent) entType (cdr (assoc 0 elist)))
        
        ;; Verify that the selected entity is TEXT or MTEXT
        (if (or (= entType "TEXT") (= entType "MTEXT"))
          (progn
            (setq oldTxt (cdr (assoc 1 elist)))
            
            ;; Execute the smart replacement
            (setq newTxt (replace-id oldTxt currStr))
            
            ;; Update the entity in drawing database
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