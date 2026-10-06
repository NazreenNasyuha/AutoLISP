;;; ==========================================================================
;;; TSEQ - Sequential text copy (A01 -> A02 -> A03 ..., 1 -> 2 -> 3 ...)
;;; Universal build: copies via entget/entmake (no COPY command, no COM).
;;; ==========================================================================
(defun c:tseq ( / *error* sel ent ed typ currStr parsed seqType pre val pad
                  pt ptW delta base loop
                  split-str pad-num next-str shift-pt place-copy)

  (defun *error* (msg)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  ;; Split a string into (type prefix value padWidth). Types: NUM / ALPHA / NONE
  (defun split-str (str / len i numStr lastCh)
    (setq len (strlen str))
    (if (> len 0)
      (progn
        (setq lastCh (ascii (substr str len 1)))
        (cond
          ((and (>= lastCh 48) (<= lastCh 57))                 ;; ends in digit
           (setq i len numStr "")
           (while (and (> i 0)
                       (>= (ascii (substr str i 1)) 48)
                       (<= (ascii (substr str i 1)) 57))
             (setq numStr (strcat (substr str i 1) numStr) i (1- i)))
           (list "NUM" (substr str 1 i) (atoi numStr) (strlen numStr)))
          ((or (and (>= lastCh 65) (<= lastCh 90))             ;; ends in letter
               (and (>= lastCh 97) (<= lastCh 122)))
           (list "ALPHA" (substr str 1 (1- len)) lastCh 0))
          (T (list "NONE" str 0 0))))
      (list "NONE" str 0 0)))

  ;; Left-pad a number with zeros
  (defun pad-num (num padLen / s)
    (setq s (itoa num))
    (while (< (strlen s) padLen) (setq s (strcat "0" s)))
    s)

  ;; Advance the sequence by one step (updates val and currStr)
  (defun next-str ()
    (cond
      ((= seqType "NUM")
       (setq val (1+ val) currStr (strcat pre (pad-num val pad))))
      ((= seqType "ALPHA")
       (setq val (1+ val))
       (if (= val 91)  (setq val 65))    ;; Z -> A
       (if (= val 123) (setq val 97))    ;; z -> a
       (setq currStr (strcat pre (chr val))))))

  ;; Translate a stored point by delta (TEXT stores points in OCS, MTEXT in WCS)
  (defun shift-pt (p)
    (if (= typ "TEXT")
      (trans (mapcar '+ (trans p ent 0) delta) 0 ent)
      (mapcar '+ p delta)))

  ;; Clone the source entity with new text at the new location
  (defun place-copy (str / out d)
    (setq out nil)
    (foreach d ed
      (cond
        ((member (car d) '(-1 5 330 102 360)) nil)                      ;; handles / owners / reactors
        ((= (car d) 1)  (setq out (cons (cons 1 str) out)))             ;; new string
        ((= (car d) 3)  nil)                                            ;; MTEXT overflow chunks
        ((= (car d) 10) (setq out (cons (cons 10 (shift-pt (cdr d))) out)))
        ((and (= (car d) 11) (= typ "TEXT"))                            ;; TEXT alignment point
         (setq out (cons (cons 11 (shift-pt (cdr d))) out)))
        (T (setq out (cons d out)))))
    (entmake (reverse out)))

  (setq sel (entsel "\nSelect source text to copy (TEXT or MTEXT): "))
  (cond
    ((null sel) (princ "\nNothing selected."))
    (T
     (setq ent (car sel) ed (entget ent) typ (cdr (assoc 0 ed)))
     (cond
       ((not (member typ '("TEXT" "MTEXT")))
        (princ "\nSelected object must be TEXT or MTEXT."))
       ((assoc 3 ed)
        (princ "\nMTEXT is too long (multi-chunk) to sequence safely."))
       (T
        (setq currStr (cdr (assoc 1 ed))
              parsed  (split-str currStr)
              seqType (nth 0 parsed) pre (nth 1 parsed)
              val     (nth 2 parsed) pad (nth 3 parsed)
              base    (if (= typ "TEXT") (trans (cdr (assoc 10 ed)) ent 0) (cdr (assoc 10 ed))))

        (next-str)   ;; first copy already gets the NEXT number (not a duplicate of the source)

        (setq loop T)
        (while loop
          (setq pt (getpoint (strcat "\nClick insertion point for [" currStr "] or press Enter to exit: ")))
          (if pt
            (progn
              (setq ptW   (trans pt 1 0)
                    delta (mapcar '- ptW base))
              (if (place-copy currStr)
                (next-str)
                (princ "\nCopy failed.")))
            (setq loop nil)))))))
  (princ)
)
