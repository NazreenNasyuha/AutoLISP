;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tseq.lsp
;;; COMMAND     : TSEQ
;;; DESCRIPTION : Universal Smart ID Sequencer (v7.0).
;;;               - Preserves parenthetical tags like "(260 PE)", "(EXTG)", "(STP)"
;;;               - Allows spaces when typing IDs (e.g. "MH 8") by temporarily
;;;                 disabling DYNMODE so Spacebar never prematurely submits
;;;               - Supports TEXT, MTEXT, MULTILEADER, and ATTRIB (LT 2024+ Safe)
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:tseq ( / *error* oldCmd oldDyn defSeq userSeq currStr parsed seqType pre val pad
                  loop sel ent subEnt pt ssPick elist entType oldTxt res mode oldID newTxt
                  history lastItem
                  ts:trim ts:digit-p ts:lead-digits ts:starts-with-3digits-p
                  ts:ask-id-string ts:toggle-space
                  get-text put-text replace-id split-str pad-num )

  (setq oldCmd (getvar "CMDECHO")
        oldDyn (vl-catch-all-apply 'getvar (list "DYNMODE")))
  (if (vl-catch-all-error-p oldDyn) (setq oldDyn nil))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldDyn (vl-catch-all-apply 'setvar (list "DYNMODE" oldDyn)))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[TSEQ Error]: " msg))
    )
    (princ)
  )

  ;; =========================================================================
  ;; 1. PURE AUTOLISP STRING, INPUT & TOKEN HELPERS
  ;; =========================================================================
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

  (defun ts:digit-p (ch / a)
    (if (= ch "")
      nil
      (progn
        (setq a (ascii ch))
        (and (>= a 48) (<= a 57))
      )
    )
  )

  (defun ts:lead-digits (s / len i)
    (setq len (strlen s) i 1)
    (while (and (<= i len) (ts:digit-p (substr s i 1)))
      (setq i (1+ i))
    )
    (1- i)
  )

  (defun ts:starts-with-3digits-p (s / trimmed nDig)
    (setq trimmed (ts:trim s)
          nDig    (ts:lead-digits trimmed))
    (and (>= nDig 3) (<= nDig 4))
  )

  ;; Temporarily disables DYNMODE during getstring so pressing Spacebar types a
  ;; real space (e.g. "MH 8") instead of submitting the Dynamic Input tooltip box.
  (defun ts:ask-id-string (promptStr / curDyn ans)
    (setq curDyn (vl-catch-all-apply 'getvar (list "DYNMODE")))
    (if (and curDyn (not (vl-catch-all-error-p curDyn)) (/= curDyn 0))
      (vl-catch-all-apply 'setvar (list "DYNMODE" 0))
    )
    (setq ans (getstring T promptStr))
    (if (and curDyn (not (vl-catch-all-error-p curDyn)) (/= curDyn 0))
      (vl-catch-all-apply 'setvar (list "DYNMODE" curDyn))
    )
    (if ans (ts:trim ans) "")
  )

  ;; Toggles a single space between alphabetic prefix and trailing number ("MH8" <-> "MH 8")
  (defun ts:toggle-space (str / p st pfx numStr)
    (setq p  (split-str str)
          st (nth 0 p))
    (if (= st "NUM")
      (progn
        (setq pfx    (nth 1 p)
              numStr (pad-num (nth 2 p) (nth 3 p)))
        (if (and (> (strlen pfx) 0) (= (substr pfx (strlen pfx) 1) " "))
          (strcat (ts:trim pfx) numStr)
          (if (> (strlen pfx) 0)
            (strcat pfx " " numStr)
            str
          )
        )
      )
      str
    )
  )

  ;; =========================================================================
  ;; 2. 3-LAYER UNIVERSAL TEXT READER & WRITER (AutoCAD 2024+ / LT 2024+ Proof)
  ;;    Supports TEXT, MTEXT, MULTILEADER, ATTRIB, and ATTDEF
  ;; =========================================================================
  (defun get-text (ent / elist typ res obj item)
    (setq elist (entget ent)
          typ   (cdr (assoc 0 elist))
          res   nil)

    (if (and (= typ "MULTILEADER") (boundp 'getpropertyvalue))
      (setq res (vl-catch-all-apply 'getpropertyvalue (list ent "MText/Contents")))
    )

    (if (or (null res) (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
      (progn
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (and obj (not (vl-catch-all-error-p obj)))
          (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
        )
      )
    )

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
  ;; 3. VERSATILE PIPE / DRAIN / MANHOLE ID DETECTOR & REPLACER
  ;;    - Stops BEFORE parenthetical notes like " (260 PE)", "(EXTG)", "(STP)"
  ;;      so "MH 8 (260 PE)\PGL=6.40..." -> "MH 9 (260 PE)\PGL=6.40..."
  ;;    - Handles ",", ";", ":", " - ", "/", space, and "\P" delimiters
  ;; =========================================================================
  (defun replace-id (txt newID / len i loopP ch sec semi stylePrefix remTxt uRem
                                 pSemi pComma pColon pSlash pDashSp pSlashP pCR pLF pBrace pSpace
                                 pSpParen pParen parenBound
                                 diaMarkers m pos pDia nLead afterLead startsWithBareDia
                                 punctCands firstPunct endPos p suffix oldID)
    (setq len (strlen txt) i 1 loopP T)

    ;; Step A: Walk past leading MTEXT formatting codes ({\W0.5;, {\f...;, \A1;, \pxqc;, etc.)
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

    ;; Step B: Locate all structural delimiters in the visible text
    (setq pSemi    (vl-string-search ";" remTxt)
          pComma   (vl-string-search "," remTxt)
          pColon   (vl-string-search ":" remTxt)
          pSlash   (vl-string-search "/" remTxt)
          pDashSp  (vl-string-search " - " remTxt)
          pSlashP  (vl-string-search "\\P" uRem)
          pCR      (vl-string-search "\r" remTxt)
          pLF      (vl-string-search "\n" remTxt)
          pBrace   (vl-string-search "}" remTxt)
          pSpace   (vl-string-search " " remTxt)
          pSpParen (vl-string-search " (" remTxt)
          pParen   (vl-string-search "(" remTxt))

    ;; Detect parenthetical notes such as " (260 PE)", "(260 PE)", " (EXTG)", " (STP)"
    (setq parenBound nil)
    (cond
      ((and pSpParen (> pSpParen 0))
       (setq parenBound pSpParen))
      ((and pParen (> pParen 0))
       (setq parenBound pParen))
    )

    ;; Find earliest diameter / pipe specification marker
    (setq diaMarkers '("%%C" "MM" "\\U+00F8" "\\U+00D8" "\\U+2205" "\\U+2300" "Ø" "∅" "⌀" "DIA" "VCP" "RCP" "HDPE" "UPVC")
          pDia       nil)
    (foreach m diaMarkers
      (setq pos (vl-string-search m uRem))
      (if (and pos (or (null pDia) (< pos pDia)))
        (setq pDia pos)
      )
    )

    ;; Check if label starts DIRECTLY with a 3-or-4 digit pipe diameter (e.g. "600%%c 17m" or "225mm%%c VCP")
    (setq nLead             (ts:lead-digits uRem)
          afterLead         (ts:trim (substr uRem (1+ nLead)))
          startsWithBareDia (and (>= nLead 3)
                                 (<= nLead 4)
                                 (or (wcmatch afterLead "MM*,%%C*,\\U+*,Ø*,∅*,⌀*,DIA*,VCP*,RCP*,HDPE*,UPVC*,X#*")
                                     (and pDia (<= pDia 5)))))

    ;; Gather earliest punctuation delimiter (",", ";", ":", " - ", "/")
    (setq punctCands nil)
    (if pSemi
      (setq punctCands (cons pSemi punctCands))
    )
    (if (and pComma (or (null pDia) (< pComma pDia) (<= pComma 15)))
      (setq punctCands (cons pComma punctCands))
    )
    (if (and pDashSp (or (null pDia) (< pDashSp pDia) (<= pDashSp 15)))
      (setq punctCands (cons pDashSp punctCands))
    )
    (if (and pSlash
             (or (null pSlashP) (< pSlash pSlashP))
             (or (null pDia) (< pSlash pDia))
             (<= pSlash 12))
      (setq punctCands (cons pSlash punctCands))
    )
    (if (and pColon
             (not (wcmatch uRem "1:#*"))
             (or (and pDia (< pColon pDia)) (< pColon 10)))
      (setq punctCands (cons pColon punctCands))
    )

    (cond
      ;; ---------------------------------------------------------------------
      ;; CASE 1: SEWERAGE MANHOLE (Contains GL=, IL=, GL:, IL:, D=, or DEPTH)
      ;; Stops before " (260 PE)", ";", ",", "\P", or "}" so PE & levels stay intact
      ;; ---------------------------------------------------------------------
      ((or (wcmatch uRem "*GL=*,*IL=*,*GL:*,*IL:*,*DEPTH*,*\\PD=*")
           (and (wcmatch uRem "MH*,*MANHOLE*") (or pSlashP pCR pLF)))
       (setq endPos (strlen remTxt))
       (foreach p (list parenBound pSemi pComma pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "SEWER_MH" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 2: UN-ID'D PIPE (Starts directly with 3-4 digit pipe diameter)
      ;; ---------------------------------------------------------------------
      (startsWithBareDia
       (list "SEWER_PIPE_NO_ID" "" txt)
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 3: PUNCTUATION-SEPARATED PIPE DETAIL (",", ";", ":", " - ", "/")
      ;; ---------------------------------------------------------------------
      (punctCands
       (setq firstPunct (apply 'min punctCands)
             endPos     firstPunct)
       (foreach p (list parenBound pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 4: SPACE-SEPARATED PIPE DETAIL ("A01 600%%c 20m", "5c 600x600")
      ;; ---------------------------------------------------------------------
      ((and pSpace
            (<= pSpace 15)
            (or (and pDia (< pSpace pDia))
                (ts:starts-with-3digits-p (substr uRem (+ pSpace 2)))))
       (setq endPos pSpace)
       (foreach p (list parenBound pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "DRAIN_PIPE" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 5: MULTI-LINE LABEL WITHOUT PUNCTUATION (Replaces Line 1 ID before \P or "(")
      ;; ---------------------------------------------------------------------
      ((or pSlashP pCR pLF)
       (setq endPos (strlen remTxt))
       (foreach p (list parenBound pSlashP pCR pLF pBrace)
         (if (and p (< p endPos)) (setq endPos p))
       )
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "MULTILINE_ID" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 6: SINGLE-LINE ID WITH PARENTHETICAL TAG (e.g. "MH 8 (260 PE)")
      ;; ---------------------------------------------------------------------
      (parenBound
       (setq endPos parenBound)
       (if (and pBrace (< pBrace endPos)) (setq endPos pBrace))
       (setq oldID  (substr remTxt 1 endPos)
             suffix (substr remTxt (1+ endPos)))
       (list "TAGGED_ID" oldID (strcat stylePrefix newID suffix))
      )

      ;; ---------------------------------------------------------------------
      ;; CASE 7: SINGLE-TOKEN ID (e.g. "D01", "5c", "MH 8", "MH-01")
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
  ;; 4. SEQUENCE PARSER & INCREMENTER
  ;;    Supports spaces in prefix: "MH 8" -> pre="MH ", val=8, pad=1 -> "MH 9"
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

  (princ "\nCommand: TSEQ v7.0 (Universal Pipe & Manhole ID Sequencer)")

  ;; =========================================================================
  ;; 5. STARTING ID SETUP (Spaces Allowed - e.g. "MH 8", "MH-01", "A01", "5c")
  ;; =========================================================================
  (setq defSeq "MH 1")
  (setq userSeq (ts:ask-id-string (strcat "\nEnter starting ID (spaces allowed, e.g. MH 8, MH-01, A01, 5c) <" defSeq ">: ")))
  (setq currStr (if (= userSeq "") defSeq userSeq))

  (setq parsed  (split-str currStr)
        seqType (nth 0 parsed)
        pre     (nth 1 parsed)
        val     (nth 2 parsed)
        pad     (nth 3 parsed)
        history nil
        loop    T)

  ;; =========================================================================
  ;; 6. CLICK-TO-RENAME LOOP (With Set, Pick, Space Toggle, Click-Through & Undo)
  ;; =========================================================================
  (while loop
    (initget "Set Pick Space Undo")
    (setq sel (entsel (strcat "\nClick text to set ID to [" currStr "] [Set/Pick/Space/Undo] <Exit>: ")))
    (cond
      ((null sel)
       (setq loop nil))

      ;; Option "Set": Type a new current ID sequence on the fly (spaces allowed)
      ((= sel "Set")
       (setq userSeq (ts:ask-id-string (strcat "\nEnter new starting ID (spaces allowed, e.g. MH 8) <" currStr ">: ")))
       (if (/= userSeq "")
         (setq currStr userSeq
               parsed  (split-str currStr)
               seqType (nth 0 parsed)
               pre     (nth 1 parsed)
               val     (nth 2 parsed)
               pad     (nth 3 parsed))
       )
       (princ (strcat "\n>> Active ID sequence set to [" currStr "].")))

      ;; Option "Space": Toggle space between prefix and number ("MH8" <-> "MH 8")
      ((= sel "Space")
       (setq currStr (ts:toggle-space currStr)
             parsed  (split-str currStr)
             seqType (nth 0 parsed)
             pre     (nth 1 parsed)
             val     (nth 2 parsed)
             pad     (nth 3 parsed))
       (princ (strcat "\n>> Active ID spacing toggled to [" currStr "].")))

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

       (if (= entType "INSERT")
         (if (setq subEnt (nentselp pt))
           (if (member (cdr (assoc 0 (entget (car subEnt)))) '("ATTRIB" "TEXT" "MTEXT"))
             (setq ent     (car subEnt)
                   elist   (entget ent)
                   entType (cdr (assoc 0 elist)))
           )
         )
       )

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
             (princ "\n>> Skipped: This Pipe label has no ID prefix (starts directly with diameter). Click a label with an ID!")
             (progn
               (setq history (cons (list ent oldTxt currStr) history))

               (put-text ent newTxt)
               (if (= entType "ATTRIB")
                 (entupd (cdr (assoc 330 elist)))
               )
               (princ (strcat "\n>> [" mode "] Changed ID \"" oldID "\" -> \"" currStr "\" (Details & PE preserved)."))

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

  (if oldDyn (vl-catch-all-apply 'setvar (list "DYNMODE" oldDyn)))
  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)

(princ "\nTSEQ v7.0 loaded. Type TSEQ to run.")
(princ)