;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tseq.lsp
;;; COMMAND     : TSEQ
;;; DESCRIPTION : Discipline-Aware Smart ID Sequencer (Drainage & Sewerage)
;;;               Auto-detects Drainage Pipes vs. Sewerage Manholes and
;;;               updates ONLY the ID while locking all engineering details.
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:tseq ( / *error* defSeq userSeq currStr parsed seqType pre val pad
                  loop sel ent elist entType oldTxt newTxt oldCmd history lastItem
                  get-text put-text detect-type replace-id split-str pad-num)

  (setq oldCmd (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[TSEQ Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; 1. AUTOCAD & GSTARCAD SAFE TEXT READER / WRITER
  ;;    (Skips "LEADER_LINE{" in MULTILEADERs & handles MTEXT chunks)
  ;; -------------------------------------------------------------------------
  (defun get-text (ent / elist typ obj res item)
    (setq elist (entget ent) typ (cdr (assoc 0 elist)) res nil)

    ;; Try ActiveX property first
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
    )

    ;; Pure DXF Fallback
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (cond
        ((= typ "MULTILEADER")
         (setq res "")
         (foreach item elist
           (if (and (= (car item) 304)
                    (/= (strcase (cdr item)) "LEADER_LINE{")
                    (= res ""))
             (setq res (cdr item))
           )
         ))
        ((= typ "MTEXT")
         (setq res "")
         (foreach item elist
           (if (or (= (car item) 3) (= (car item) 1))
             (setq res (strcat res (cdr item)))
           )
         ))
        (T
         (setq res (cdr (assoc 1 elist))))
      )
    )
    (if (and res (= (type res) 'STR)) res "")
  )

  (defun put-text (ent newTxt / elist typ newElist replaced obj)
    (setq elist (entget ent) typ (cdr (assoc 0 elist)))
    (cond
      ((= typ "MULTILEADER")
       (setq newElist nil replaced nil)
       (foreach item elist
         (if (and (not replaced)
                  (= (car item) 304)
                  (/= (strcase (cdr item)) "LEADER_LINE{"))
           (progn
             (setq newElist (cons (cons 304 newTxt) newElist))
             (setq replaced T)
           )
           (setq newElist (cons item newElist))
         )
       )
       (entmod (reverse newElist)))

      ((= typ "MTEXT")
       (setq newElist nil)
       (foreach item elist
         (if (/= (car item) 3)
           (if (= (car item) 1)
             (setq newElist (cons (cons 1 newTxt) newElist))
             (setq newElist (cons item newElist))
           )
         )
       )
       (entmod (reverse newElist)))

      (T
       (if (assoc 1 elist)
         (entmod (subst (cons 1 newTxt) (assoc 1 elist) elist))
       ))
    )

    ;; Sync via ActiveX
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (progn
        (vl-catch-all-apply 'vlax-put-property (list obj 'TextString newTxt))
        (vl-catch-all-apply 'vla-update (list obj))
      )
    )
    (entupd ent)
  )

  ;; -------------------------------------------------------------------------
  ;; 2. SMART DISCIPLINE DETECTOR & ID REPLACER
  ;;    Distinguishes:
  ;;      - "SEWER_MH"   : Manhole callout (MH...\PGL=...\PIL=...\PD=...)
  ;;      - "DRAIN_PIPE" : Drainage label (A01; 600%%c... or A29g: 000%%c...)
  ;;      - "SEWER_PIPE" : Un-ID'd Sewer pipe (225mm%%c VCP...) -> Protected!
  ;;      - "GENERIC"    : Plain single-token ID placeholder (e.g. D01, MH-01)
  ;; -------------------------------------------------------------------------
  (defun replace-id (txt newID / len i loopP ch sec semi stylePrefix remTxt uRem
                                 pSemi pColon pSlashP pSlashLow pCR pLF pBrace
                                 pDia endPos suffix headStr)
    (setq len (strlen txt) i 1 loopP T)

    ;; Step A: Walk past leading MTEXT formatting codes ({\W0.5;, {\f...;, \A1;, etc.)
    (while (and loopP (<= i len))
      (setq ch  (substr txt i 1))
      (setq sec (if (< i len) (substr txt (1+ i) 1) ""))
      (cond
        ((= ch "{")
         (setq i (1+ i)))
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi
           (setq i (+ semi 2))
           (setq loopP nil)
         ))
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )

    (setq stylePrefix (if (> i 1) (substr txt 1 (1- i)) ""))
    (setq remTxt      (substr txt i))
    (setq uRem        (strcase remTxt))

    ;; Locate key structural markers in remTxt
    (setq pSemi     (vl-string-search ";" remTxt)
          pColon    (vl-string-search ":" remTxt)
          pSlashP   (vl-string-search "\\P" uRem)
          pCR       (vl-string-search "\r" remTxt)
          pLF       (vl-string-search "\n" remTxt)
          pBrace    (vl-string-search "}" remTxt)
          pDia      (cond
                      ((vl-string-search "%%C" uRem))
                      ((vl-string-search "MM" uRem))
                      ((vl-string-search "\U+00F8" uRem))
                      ((vl-string-search "Ø" uRem))
                      (T nil)))

    (cond
      ;; ---------------------------------------------------------------------
      ;; CASE 1: SEWERAGE MANHOLE (Contains GL=, IL=, D=, or DEPTH)
      ;; Replaces ONLY Line 1 (the MH ID) and keeps \PGL=...\PIL=...\PD=...
      ;; ---------------------------------------------------------------------
      ((or (wcmatch uRem "*GL=*,*IL=*,*GL:*,*IL:*,*DEPTH*")
           (and (wcmatch uRem "MH*") (or pSlashP pCR pLF)))
       (setq endPos (strlen remTxt))
       (foreach p (list pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq suffix (substr remTxt (1+ endPos)))
       (list "SEWER_MH" (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 2: SEWERAGE PIPE WITHOUT ID (Starts directly with 225mm%%c VCP...)
      ;; Do NOT overwrite pipe size/material/length if there is no ID prefix!
      ;; ---------------------------------------------------------------------
      ((and (null pSemi)
            (or (wcmatch uRem "*MM%%C*,*VCP*,*RCP*,*HDPE*")
                (and pDia (< pDia 5))))
       (list "SEWER_PIPE_NO_ID" txt)
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 3: DRAINAGE PIPE WITH ";" DELIMITER (e.g. A01; 600%%c 28m 1:000)
      ;; Replaces ONLY the ID before ";" (or before \P if multi-line without ;)
      ;; ---------------------------------------------------------------------
      (pSemi
       (setq endPos pSemi)
       (if (and pSlashP (< pSlashP endPos)) (setq endPos pSlashP))
       (setq suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 4: DRAINAGE PIPE WITH ":" DELIMITER BEFORE DIAMETER
      ;; (e.g. A29g: 000%%c 20m 1:000 -> catches ":" after A29g, NOT in 1:000)
      ;; ---------------------------------------------------------------------
      ((and pColon (or (and pDia (< pColon pDia)) (< pColon 8)))
       (setq endPos pColon)
       (if (and pSlashP (< pSlashP endPos)) (setq endPos pSlashP))
       (setq suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 5: MULTI-LINE LABEL WITHOUT ";" (Replaces first line before \P)
      ;; ---------------------------------------------------------------------
      ((or pSlashP pCR pLF)
       (setq endPos (strlen remTxt))
       (foreach p (list pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq suffix (substr remTxt (1+ endPos)))
       (list "MULTILINE" (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 6: SINGLE-TOKEN PLACEHOLDER (e.g. "P1" or "{\W0.5;D01}")
      ;; ---------------------------------------------------------------------
      (T
       (setq endPos (if pBrace pBrace (strlen remTxt)))
       (setq suffix (substr remTxt (1+ endPos)))
       (list "GENERIC" (strcat stylePrefix newID suffix))
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 3. SEQUENCE PARSER & INCREMENTER (Supports 01->02 and 53A->53B)
  ;; -------------------------------------------------------------------------
  (defun split-str (str / len i numStr lst)
    (setq len (strlen str))
    (if (> len 0)
      (progn
        (setq lst (ascii (substr str len 1)))
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
    (setq s (itoa num))
    (while (< (strlen s) padLen) (setq s (strcat "0" s)))
    s
  )

  (princ "\nCommand: TSEQ v4.0 (Smart Drainage & Sewerage ID Sequencer)")

  ;; =========================================================================
  ;; 4. PROMPT FOR STARTING ID
  ;; =========================================================================
  (setq defSeq "D01")
  (setq userSeq (getstring T (strcat "\nEnter starting ID sequence (e.g. A01, 53A, MH-01) <" defSeq ">: ")))
  (setq currStr (if (= (vl-string-trim " \t" userSeq) "") defSeq (vl-string-trim " \t" userSeq)))

  (setq parsed  (split-str currStr)
        seqType (nth 0 parsed)
        pre     (nth 1 parsed)
        val     (nth 2 parsed)
        pad     (nth 3 parsed)
        history nil
        loop    T)

  ;; =========================================================================
  ;; 5. CLICK-TO-RENAME LOOP (WITH UNDO & DISCIPLINE PROTECTION)
  ;; =========================================================================
  (while loop
    (initget "Undo")
    (setq sel (entsel (strcat "\nClick Drainage/Manhole text to rename ID to [" currStr "] [Undo] <Exit>: ")))
    (cond
      ((null sel)
       (setq loop nil))

      ((= sel "Undo")
       (if history
         (progn
           (setq lastItem (car history)
                 history  (cdr history))
           (put-text (nth 0 lastItem) (nth 1 lastItem))
           (setq currStr (nth 2 lastItem)
                 val     (nth 3 lastItem))
           (princ (strcat "\n>> Reverted last text. Next ID reset to [" currStr "]."))
         )
         (princ "\n>> Nothing to undo.")
       ))

      (T
       (setq ent (car sel) elist (entget ent) entType (cdr (assoc 0 elist)))
       (if (member entType '("TEXT" "MTEXT" "MULTILEADER"))
         (progn
           (setq oldTxt (get-text ent))
           (setq res    (replace-id oldTxt currStr)
                 mode   (nth 0 res)
                 newTxt (nth 1 res))

           (if (= mode "SEWER_PIPE_NO_ID")
             (princ "\n>> Skipped: This Sewer Pipe label has no ID prefix (starts with pipe diameter). Click a Manhole or Drain ID!")
             (progn
               ;; Save state for Undo before modifying
               (setq history (cons (list ent oldTxt currStr val) history))

               (put-text ent newTxt)
               (princ (strcat "\n>> [" mode "] Updated ID to " currStr " (Details preserved)."))

               ;; Increment sequence for next click
               (cond
                 ((= seqType "NUM")
                  (setq val (1+ val) currStr (strcat pre (pad-num val pad))))
                 ((= seqType "ALPHA")
                  (setq val (1+ val))
                  (if (= val 91) (setq val 65))
                  (if (= val 123) (setq val 97))
                  (setq currStr (strcat pre (chr val))))
               )
             )
           )
         )
         (princ "\nError: Selected object is not TEXT, MTEXT, or MULTILEADER.")
       ))
    )
  )
  (princ)
)