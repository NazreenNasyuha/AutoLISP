(defun c:tseq ( / *error* defSeq userSeq currStr parsed seqType pre val pad
                  loop sel ent elist entType oldTxt newTxt oldCmd history lastItem
                  get-text put-text replace-id split-str pad-num next-seq)
  (vl-load-com)
  (setq oldCmd (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[TSEQ Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; 1. UNIVERSAL ENTITY TEXT READER & WRITER (TEXT, MTEXT, MULTILEADER)
  ;; -------------------------------------------------------------------------
  (defun get-text (ent / elist typ obj res)
    (setq elist (entget ent) typ (cdr (assoc 0 elist)) res nil)
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (setq res (vl-catch-all-apply 'vla-get-TextString (list obj)))
    )
    (if (or (null res) (vl-catch-all-error-p res) (= res ""))
      (setq res (if (= typ "MULTILEADER") (cdr (assoc 304 elist)) (cdr (assoc 1 elist))))
    )
    (if res res "")
  )

  (defun put-text (ent newTxt / elist typ obj)
    (setq elist (entget ent) typ (cdr (assoc 0 elist)))
    (if (= typ "MULTILEADER")
      (if (assoc 304 elist) (entmod (subst (cons 304 newTxt) (assoc 304 elist) elist)))
      (if (assoc 1 elist)   (entmod (subst (cons 1 newTxt)   (assoc 1 elist)   elist)))
    )
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and obj (not (vl-catch-all-error-p obj)))
      (progn
        (vl-catch-all-apply 'vla-put-TextString (list obj newTxt))
        (vl-catch-all-apply 'vla-update (list obj))
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 2. SMART PARSER: Supports ";", ":" (A29g: 000%%c), and "\P" (Manholes)
  ;; -------------------------------------------------------------------------
  (defun replace-id (txt newID / len i loopP ch sec semi stylePrefix remTxt endPos p suffix)
    (setq len (strlen txt) i 1 loopP T)

    ;; Step A: Walk past leading MTEXT formatting codes ({\W0.5;, {\f...;, \A1;, \L, etc.)
    (while (and loopP (<= i len))
      (setq ch  (substr txt i 1))
      (setq sec (if (< i len) (substr txt (1+ i) 1) ""))
      (cond
        ((= ch "{")
         (setq i (1+ i)))
        ;; Codes that end with a semicolon (;): \f \F \W \w \H \h \C \c \T \t \Q \q \A \a \p
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi
           (setq i (+ semi 2))
           (setq loopP nil)
         ))
        ;; Single-letter toggle codes without a semicolon: \L \l \O \o \K \k
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )

    (setq stylePrefix (if (> i 1) (substr txt 1 (1- i)) ""))
    (setq remTxt (substr txt i))

    ;; Step B: Find the earliest delimiter ending the ID (";", ":", "\P", "\p", "\r", "\n", "}")
    (setq endPos (strlen remTxt))
    (foreach del '(";" ":" "\\P" "\\p" "\r" "\n" "}")
      (if (setq p (vl-string-search del remTxt))
        (if (< p endPos) (setq endPos p))
      )
    )

    ;; Step C: Keep the delimiter and everything after it untouched
    (setq suffix (substr remTxt (1+ endPos)))

    (strcat stylePrefix newID suffix)
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

  (princ "\nCommand: TSEQ (Rapid Smart-Rename Sequencer)")

  ;; =========================================================================
  ;; 4. PROMPT FOR STARTING ID
  ;; =========================================================================
  (setq defSeq "D01")
  (setq userSeq (getstring (strcat "\nEnter starting ID sequence <" defSeq ">: ")))
  (setq currStr (if (= userSeq "") defSeq userSeq))

  (setq parsed  (split-str currStr)
        seqType (nth 0 parsed)
        pre     (nth 1 parsed)
        val     (nth 2 parsed)
        pad     (nth 3 parsed)
        history nil
        loop    T)

  ;; =========================================================================
  ;; 5. CLICK-TO-RENAME LOOP (WITH UNDO)
  ;; =========================================================================
  (while loop
    (initget "Undo")
    (setq sel (entsel (strcat "\nClick Pipe/Manhole text to rename ID to [" currStr "] [Undo] <Exit>: ")))
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
           (setq newTxt (replace-id oldTxt currStr))

           ;; Save state for Undo before modifying
           (setq history (cons (list ent oldTxt currStr val) history))

           (put-text ent newTxt)

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
         (princ "\nError: Selected object is not TEXT, MTEXT, or MULTILEADER.")
       ))
    )
  )
  (princ)
)