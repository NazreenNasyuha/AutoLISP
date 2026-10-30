;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tseq.lsp
;;; COMMAND     : TSEQ
;;; DESCRIPTION : Universal Smart ID Sequencer (AutoCAD 2024+ / LT 2024+ /
;;;               GstarCAD Proof). Auto-detects the ID in ANY Text, MText,
;;;               Multileader, or Block Attribute and updates ONLY the ID
;;;               while locking all formatting and engineering details.
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:tseq ( / *error* oldCmd defSeq userSeq currStr parsed seqType pre val pad
                  loop sel ent subEnt pt ssPick elist entType oldTxt res mode oldID newTxt
                  history lastItem
                  ts:trim get-text put-text
                  replace-id split-str pad-num )

  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[TSEQ Error]: " msg))
    )
    (princ)
  )

  ;; Pure AutoLISP string trimmer
  (defun ts:trim (s / len i1 i2)
    (if (or (null s) (/= (type s) 'STR))
      ""
      (progn
        (setq len (strlen s) i1 1 i2 len)
        (while (and (<= i1 len) (member (substr s i1 1) '(" " "\t" "\r" "\n")))
          (setq i1 (1+ i1))
        )
        (while (and (>= i2 i1) (member (substr s i2 1) '(" " "\t" "\r" "\n")))
          (setq i2 (1- i2))
        )
        (if (> i1 i2) "" (substr s i1 (1+ (- i2 i1))))
      )
    )
  )

  ;; =========================================================================
  ;; 1. 3-LAYER UNIVERSAL TEXT READER & WRITER (AutoCAD 2024+ / LT 2024+ Proof)
  ;;    Supports TEXT, MTEXT, MULTILEADER, ATTRIB, and ATTDEF
  ;; =========================================================================
  (defun get-text (ent / elist typ res obj item)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          res   nil)

    ;; Layer 1: Native AutoCAD 2024+ / LT 2024+ getpropertyvalue for MULTILEADER
    (if (and (= typ "MULTILEADER") (boundp 'getpropertyvalue))
      (setq res (vl-catch-all-apply 'getpropertyvalue (list ent "MText/Contents")))
    )

    ;; Layer 2: ActiveX TextString property
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (progn
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (and obj (not (vl-catch-all-error-p obj)))
          (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
        )
      )
    )

    ;; Layer 3: Pure DXF fallback (skips "LEADER_LINE{" in MULTILEADER & joins MTEXT chunks)
    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (cond
        ((= typ "MULTILEADER")
         (setq res "")
         (foreach item elist
           (if (and (= (car item) 304)
                    (= (type (cdr item)) 'STR)
                    (/= (strcase (cdr item)) "LEADER_LINE{")
                    (= res ""))
             (setq res (cdr item))
           )
         ))
        ((= typ "MTEXT")
         (setq res "")
         (foreach item elist
           (if (and (or (= (car item) 3) (= (car item) 1))
                    (= (type (cdr item)) 'STR))
             (setq res (strcat res (cdr item)))
           )
         ))
        (T
         (if (and (assoc 1 elist) (= (type (cdr (assoc 1 elist))) 'STR))
           (setq res (cdr (assoc 1 elist)))
           (setq res "")
         ))
      )
    )
    (if (and res (= (type res) 'STR)) res "")
  )

  (defun put-text (ent newTxt / elist typ newElist replaced item obj ok)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          ok    nil)

    (cond
      ;; A. MULTILEADER: Native AutoCAD 2024+ setpropertyvalue + ActiveX + DXF 304
      ((= typ "MULTILEADER")
       (if (boundp 'setpropertyvalue)
         (if (not (vl-catch-all-error-p
                    (vl-catch-all-apply 'setpropertyvalue (list ent "MText/Contents" newTxt))))
           (setq ok T)
         )
       )
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
       (if (and obj (not (vl-catch-all-error-p obj)))
         (progn
           (if (not (vl-catch-all-error-p
                      (vl-catch-all-apply 'vlax-put-property (list obj 'TextString newTxt))))
             (setq ok T)
           )
           (vl-catch-all-apply 'vla-update (list obj))
         )
       )
       (if (not ok)
         (progn
           (setq newElist nil replaced nil)
           (foreach item elist
             (if (and (not replaced)
                      (= (car item) 304)
                      (= (type (cdr item)) 'STR)
                      (/= (strcase (cdr item)) "LEADER_LINE{"))
               (progn
                 (setq newElist (cons (cons 304 newTxt) newElist))
                 (setq replaced T)
               )
               (setq newElist (cons item newElist))
             )
           )
           (entmod (reverse newElist))
         )
       ))

      ;; B. MTEXT: Clear overflow DXF 3 chunks, update DXF 1, and sync property
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
       (entmod (reverse newElist))
       (if (boundp 'setpropertyvalue)
         (vl-catch-all-apply 'setpropertyvalue (list ent "Contents" newTxt))
       )
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
       (if (and obj (not (vl-catch-all-error-p obj)))
         (progn
           (vl-catch-all-apply 'vlax-put-property (list obj 'TextString newTxt))
           (vl-catch-all-apply 'vla-update (list obj))
         )
       ))

      ;; C. Standard TEXT / ATTRIB / ATTDEF
      (T
       (if (assoc 1 elist)
         (entmod (subst (cons 1 newTxt) (assoc 1 elist) elist))
       )
       (if (boundp 'setpropertyvalue)
         (vl-catch-all-apply 'setpropertyvalue (list ent "TextString" newTxt))
       ))
    )
    (entupd ent)
  )

  ;; =========================================================================
  ;; 2. UNIVERSAL MTEXT INLINE PREFIX STRIPPER & SMART ID DETECTOR
  ;;    Handles ANY MText / Text / MLeader format in ANY drawing:
  ;;    - "{\W0.5;A01; 600%%c 28m 1:000}"    -> Replaces "A01"
  ;;    - "{\fArial|b1;{\W0.8;A01}; 600%%c}" -> Replaces "A01"
  ;;    - "A29g: 600%%c 20m 1:000"           -> Replaces "A29g"
  ;;    - "A01 600%%c 20m 1:500"             -> Replaces "A01"
  ;;    - "MH-01;\PGL: 34.50m\PIL: 32.10m"   -> Replaces "MH-01"
  ;;    - "MH 01\PGL=13.40\PIL=10.40"        -> Replaces "MH 01"
  ;;    - "{\W0.5;D01}" or "MH-01"           -> Replaces "D01" / "MH-01"
  ;; =========================================================================
  (defun replace-id (txt newID / len i loopP ch sec semi stylePrefix remTxt uRem
                                 pSemi pColon pSlashP pCR pLF pBrace pSpace
                                 pDia endPos p suffix oldID)
    (setq len (strlen txt) i 1 loopP T)

    ;; Step A: Walk past ALL leading MTEXT formatting codes ({\W0.5;, {\f...;, \A1;, \pxqc;, etc.)
    (while (and loopP (<= i len))
      (setq ch  (substr txt i 1)
            sec (if (< i len) (substr txt (1+ i) 1) ""))
      (cond
        ((or (= ch "{") (= ch " "))
         (setq i (1+ i)))
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi
           (setq i (+ semi 2))
           (setq loopP nil)
         ))
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k" "~")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )

    (setq stylePrefix (if (> i 1) (substr txt 1 (1- i)) ""))
    (setq remTxt      (substr txt i))
    (setq uRem        (strcase remTxt))

    ;; Step B: Locate structural delimiters in the visible text
    (setq pSemi   (vl-string-search ";" remTxt)
          pColon  (vl-string-search ":" remTxt)
          pSlashP (vl-string-search "\\P" uRem)
          pCR     (vl-string-search "\r" remTxt)
          pLF     (vl-string-search "\n" remTxt)
          pBrace  (vl-string-search "}" remTxt)
          pSpace  (vl-string-search " " remTxt)
          pDia    (cond
                    ((vl-string-search "%%C" uRem))
                    ((vl-string-search "MM" uRem))
                    ((vl-string-search "\\U+00F8" uRem))
                    ((vl-string-search "Ø" uRem))
                    ((vl-string-search "DIA" uRem))
                    (T nil)))

    (cond
      ;; ---------------------------------------------------------------------
      ;; CASE 1: SEWERAGE MANHOLE (Contains GL=, IL=, GL:, IL:, D=, or DEPTH)
      ;; Replaces ONLY the Manhole ID before ";", "\P", or "}"
      ;; ---------------------------------------------------------------------
      ((or (wcmatch uRem "*GL=*,*IL=*,*GL:*,*IL:*,*DEPTH*,*\\PD=*")
           (and (wcmatch uRem "MH*,*MANHOLE*") (or pSlashP pCR pLF)))
       (setq endPos (strlen remTxt))
       (foreach p (list pSemi pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "SEWER_MH" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 2: UN-ID'D SEWER PIPE (Starts directly with pipe diameter like "225mm%%c VCP")
      ;; Protects pipe specifications if there is no ID prefix
      ;; ---------------------------------------------------------------------
      ((and (null pSemi)
            (or (wcmatch uRem "###MM*,####MM*,###%%C*,####%%C*,###Ø*,####Ø*,### VCP*,### RCP*,### HDPE*")
                (and pDia (<= pDia 4) (>= (ascii (substr uRem 1 1)) 48) (<= (ascii (substr uRem 1 1)) 57))))
       (list "SEWER_PIPE_NO_ID" "" txt)
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 3: DRAINAGE / PIPE LABEL WITH ";" DELIMITER
      ;; (e.g. "A01; 600%%c 28m 1:000" or "{\fArial;A01}; 600%%c")
      ;; ---------------------------------------------------------------------
      (pSemi
       (setq endPos pSemi)
       (foreach p (list pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 4: DRAINAGE / PIPE LABEL WITH ":" DELIMITER BEFORE DIAMETER
      ;; (e.g. "A29g: 600%%c 20m 1:000" -> catches ":" after A29g, NOT in 1:000)
      ;; ---------------------------------------------------------------------
      ((and pColon
            (not (wcmatch uRem "1:#*"))
            (or (and pDia (< pColon pDia)) (< pColon 10)))
       (setq endPos pColon)
       (foreach p (list pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 5: SPACE-SEPARATED PIPE LABEL (No ";" or ":", e.g. "A01 600%%c 20m 1:500")
      ;; Replaces the ID token before the first space while keeping " 600%%c..."
      ;; ---------------------------------------------------------------------
      ((and pSpace pDia (< pSpace pDia) (<= pSpace 12))
       (setq endPos pSpace)
       (foreach p (list pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 6: MULTI-LINE LABEL WITHOUT ";" (Replaces Line 1 ID before \P)
      ;; ---------------------------------------------------------------------
      ((or pSlashP pCR pLF)
       (setq endPos (strlen remTxt))
       (foreach p (list pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "MULTILINE_ID" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 7: SINGLE-TOKEN ID (e.g. "D01", "{\W0.5;A01}", "MH-01", "1A")
      ;; ---------------------------------------------------------------------
      (T
       (setq endPos (if pBrace pBrace (strlen remTxt)))
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "SINGLE_ID" oldID (strcat stylePrefix newID suffix))
      )
    )
  )

  ;; =========================================================================
  ;; 3. SEQUENCE PARSER & INCREMENTER
  ;;    Supports:
  ;;    - Numeric suffix : "A01" -> "A02", "MH-01" -> "MH-02", "1" -> "2"
  ;;    - Alpha suffix   : "1A"  -> "1B",  "53a"   -> "53b"
  ;; =========================================================================
  (defun split-str (str / len i numStr lst)
    (setq str (ts:trim str)
          len (strlen str))
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

  (princ "\nCommand: TSEQ v5.0 (AutoCAD 2024+ Universal ID Detector & Sequencer)")

  ;; =========================================================================
  ;; 4. STARTING ID SETUP (Type Starting ID or Press Enter for Default)
  ;; =========================================================================
  (setq defSeq "A01")
  (setq userSeq (getstring T (strcat "\nEnter starting ID (e.g. A01, 1A, MH-01) or press Enter for <" defSeq ">: ")))
  (setq currStr (if (= (ts:trim userSeq) "") defSeq (ts:trim userSeq)))

  (setq parsed  (split-str currStr)
        seqType (nth 0 parsed)
        pre     (nth 1 parsed)
        val     (nth 2 parsed)
        pad     (nth 3 parsed)
        history nil
        loop    T)

  ;; =========================================================================
  ;; 5. CLICK-TO-RENAME LOOP (With Click-Through, Pick-Sample & Undo)
  ;; =========================================================================
  (while loop
    (initget "Set Pick Undo")
    (setq sel (entsel (strcat "\nClick text to set ID to [" currStr "] [Set/Pick/Undo] <Exit>: ")))
    (cond
      ((null sel)
       (setq loop nil))

      ;; Option "Set": Type a new current ID sequence on the fly
      ((= sel "Set")
       (setq userSeq (getstring T (strcat "\nEnter new starting ID <" currStr ">: ")))
       (if (/= (ts:trim userSeq) "")
         (setq currStr (ts:trim userSeq)
               parsed  (split-str currStr)
               seqType (nth 0 parsed)
               pre     (nth 1 parsed)
               val     (nth 2 parsed)
               pad     (nth 3 parsed))
       )
       (princ (strcat "\n>> Active ID sequence set to [" currStr "].")))

      ;; Option "Pick": Click an existing label in the drawing to adopt its ID + 1
      ((= sel "Pick")
       (if (setq subEnt (entsel "\nClick existing text to copy its ID & increment: "))
         (progn
           (setq oldTxt (get-text (car subEnt))
                 res    (replace-id oldTxt "TEMP")
                 oldID  (ts:trim (nth 1 res)))
           (if (/= oldID "")
             (progn
               (setq parsed  (split-str oldID)
                     seqType (nth 0 parsed)
                     pre     (nth 1 parsed)
                     val     (nth 2 parsed)
                     pad     (nth 3 parsed))
               (cond
                 ((= seqType "NUM")
                  (setq val (1+ val) currStr (strcat pre (pad-num val pad))))
                 ((= seqType "ALPHA")
                  (setq val (1+ val))
                  (if (= val 91) (setq val 65))
                  (if (= val 123) (setq val 97))
                  (setq currStr (strcat pre (chr val))))
                 (T (setq currStr oldID))
               )
               (princ (strcat "\n>> Detected ID \"" oldID "\". Next ID set to [" currStr "]."))
             )
             (princ "\n>> Could not detect an ID in that object.")
           )
         )
       ))

      ;; Option "Undo": Revert the last renamed object and step sequence back
      ((= sel "Undo")
       (if history
         (progn
           (setq lastItem (car history)
                 history  (cdr history))
           (put-text (nth 0 lastItem) (nth 1 lastItem))
           (setq currStr (nth 2 lastItem)
                 parsed  (split-str currStr)
                 seqType (nth 0 parsed)
                 pre     (nth 1 parsed)
                 val     (nth 2 parsed)
                 pad     (nth 3 parsed))
           (princ (strcat "\n>> Reverted last text. Next ID reset to [" currStr "]."))
         )
         (princ "\n>> Nothing to undo.")
       ))

      ;; Clicked an object
      ((listp sel)
       (setq ent     (car sel)
             pt      (cadr sel)
             elist   (entget ent)
             entType (cdr (assoc 0 elist)))

       ;; If user clicked a Block (INSERT) with Attributes, check sub-entity via nentselp
       (if (= entType "INSERT")
         (if (setq subEnt (nentselp pt))
           (if (member (cdr (assoc 0 (entget (car subEnt)))) '("ATTRIB" "TEXT" "MTEXT"))
             (setq ent     (car subEnt)
                   elist   (entget ent)
                   entType (cdr (assoc 0 elist)))
           )
         )
       )

       ;; Click-Through Helper: If a pipe line or hatch overlaps the text, grab the text underneath!
       (if (not (member entType '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB" "ATTDEF")))
         (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT,MULTILEADER"))))
           (setq ent     (ssname ssPick 0)
                 elist   (entget ent)
                 entType (cdr (assoc 0 elist)))
         )
       )

       (if (member entType '("TEXT" "MTEXT" "MULTILEADER" "ATTRIB" "ATTDEF"))
         (progn
           (setq oldTxt (get-text ent)
                 res    (replace-id oldTxt currStr)
                 mode   (nth 0 res)
                 oldID  (nth 1 res)
                 newTxt (nth 2 res))

           (if (= mode "SEWER_PIPE_NO_ID")
             (princ "\n>> Skipped: This Sewer Pipe label has no ID prefix (starts directly with diameter). Click a Manhole or Drain ID!")
             (progn
               ;; Save state for Undo before modifying
               (setq history (cons (list ent oldTxt currStr) history))

               (put-text ent newTxt)
               (if (= entType "ATTRIB")
                 (entupd (cdr (assoc 330 elist)))
               )
               (princ (strcat "\n>> [" mode "] Changed ID \"" oldID "\" -> \"" currStr "\" (Details preserved)."))

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
         (princ "\n>> Error: Selected object is not TEXT, MTEXT, MULTILEADER, or ATTRIB.")
       ))
    )
  )

  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)

(princ "\nTSEQ v5.0 loaded. Type TSEQ to run.")
(princ)