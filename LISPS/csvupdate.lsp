(defun c:csvupdate ( / *error* oldCmd oldDimZin esc-wildcard split-csv normalize-id split-mtext
                       get-text put-text is-sewer-mh-p
                       filename file line cols id searchId val1 val2 val3 category csvMode
                       dict refSel refEnt refLayer ss count i ent elist entType
                       oldTxt parsed stylePrefix displayId closeBraces cleanId match
                       updated missing skipped dataList glStr ilStr dStr
                       szStr lenStr gradStr lenNum newTxt pt hgt gap p1 p2 p3 p4)
  (vl-load-com)
  
  ;; Save system variables
  (setq oldCmd (getvar "CMDECHO"))
  (setq oldDimZin (getvar "DIMZIN"))
  
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if file (progn (close file) (setq file nil)))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[CSVUPDATE Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; 1. ESCAPE WILDCARDS (Critical for layers starting with "#JRK")
  ;; -------------------------------------------------------------------------
  (defun esc-wildcard (s / k ch out)
    (setq k 1 out "")
    (while (<= k (strlen s))
      (setq ch (substr s k 1))
      (if (member ch '("`" "#" "*" "?" "@" "." "~" "[" "]" ","))
        (setq out (strcat out "`" ch))
        (setq out (strcat out ch))
      )
      (setq k (1+ k))
    )
    out
  )

  ;; -------------------------------------------------------------------------
  ;; 2. SAFE CSV LINE SPLITTER (Never crashes on blank lines)
  ;; -------------------------------------------------------------------------
  (defun split-csv (str / lst pos)
    (setq lst nil)
    (while (setq pos (vl-string-search "," str))
      (setq lst (cons (vl-string-trim " \"\t\r\n" (substr str 1 pos)) lst))
      (setq str (substr str (+ pos 2)))
    )
    (setq lst (cons (vl-string-trim " \"\t\r\n" str) lst))
    (reverse lst)
  )

  ;; -------------------------------------------------------------------------
  ;; 3. ULTRA-FUZZY ID NORMALIZER ("MH 58", "MH-58", "058", "58" -> "58")
  ;; -------------------------------------------------------------------------
  (defun normalize-id (s / k ch asciiVal out)
    (setq k 1 out "")
    (while (<= k (strlen s))
      (setq ch (substr s k 1) asciiVal (ascii ch))
      (if (or (and (>= asciiVal 48) (<= asciiVal 57))
              (and (>= asciiVal 65) (<= asciiVal 90))
              (and (>= asciiVal 97) (<= asciiVal 122)))
        (setq out (strcat out (strcase ch)))
      )
      (setq k (1+ k))
    )
    (if (and (>= (strlen out) 2) (= (substr out 1 2) "MH"))
      (setq out (substr out 3))
    )
    (while (and (> (strlen out) 1) (= (substr out 1 1) "0"))
      (setq out (substr out 2))
    )
    out
  )

  ;; -------------------------------------------------------------------------
  ;; 4. SMART MTEXT PARSER (Preserves {\fSwis721...; and {\W0.5; prefixes)
  ;; -------------------------------------------------------------------------
  (defun split-mtext (txt / len i braceCnt loopP ch two semi stylePrefix remTxt endPos p displayId closeBraces)
    (setq len (strlen txt) i 1 braceCnt 0 loopP T)
    (while (and loopP (<= i len))
      (setq ch (substr txt i 1))
      (setq two (strcase (substr txt i 2)))
      (cond
        ((= ch "{") (setq braceCnt (1+ braceCnt) i (1+ i)))
        ((and (= ch "\\") (/= two "\\P") (/= two "\\N"))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi (setq i (+ semi 2)) (setq loopP nil)))
        (T (setq loopP nil))
      )
    )
    (setq stylePrefix (if (> i 1) (substr txt 1 (1- i)) ""))
    (setq remTxt (substr txt i))
    
    (setq endPos (strlen remTxt))
    (foreach del '(";" "\\P" "\\p" "\r" "\n" "}")
      (if (setq p (vl-string-search del remTxt))
        (if (< p endPos) (setq endPos p))
      )
    )
    
    (setq displayId (vl-string-trim " \t" (substr remTxt 1 endPos)))
    (setq closeBraces "")
    (repeat braceCnt (setq closeBraces (strcat closeBraces "}")))
    (list stylePrefix displayId closeBraces)
  )

  ;; -------------------------------------------------------------------------
  ;; 5. UNIVERSAL ENTITY TEXT READER & WRITER
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

  (defun is-sewer-mh-p (txt dispId / uTxt uId)
    (setq uTxt (strcase txt) uId (strcase dispId))
    (or (wcmatch uTxt "*GL=*,*IL=*,*GL:*,*IL:*")
        (wcmatch uId "MH*"))
  )

  (princ "\nCommand: CSVUPDATE (Smart Data Injection Engine)")
  (setvar "CMDECHO" 0)

  ;; =========================================================================
  ;; STEP 1: READ CSV FILE
  ;; =========================================================================
  (setq filename (getfiled "Select Engineer's CSV File" "" "csv" 0))
  (if (not filename) (progn (princ "\nNo file selected.") (exit)))

  (setq file (open filename "r") dict nil csvMode "DRAINAGE")
  (while (setq line (read-line file))
    (if (/= (vl-string-trim " \t\r\n" line) "")
      (progn
        (setq cols (split-csv line))
        (if (>= (length cols) 5)
          (progn
            (setq id       (nth 0 cols)
                  val1     (nth 1 cols)
                  val2     (nth 2 cols)
                  val3     (nth 3 cols)
                  category (strcase (nth 4 cols)))
            (if (= category "SEWERAGE") (setq csvMode "SEWERAGE"))
            (setq searchId (normalize-id id))
            (if (/= searchId "")
              (setq dict (cons (cons searchId (list val1 val2 val3 category)) dict))
            )
          )
        )
      )
    )
  )
  (close file)
  (setq file nil)

  (if (not dict)
    (progn (alert "Error: No valid 5-column data found in CSV.") (exit))
  )

  ;; =========================================================================
  ;; STEP 2: SELECT REFERENCE TEXT & TARGET LAYER
  ;; =========================================================================
  (princ "\nSelect ONE placeholder text to target its layer: ")
  (setq refSel (ssget ":S" '((0 . "TEXT,MTEXT,MULTILEADER"))))
  (if (not refSel) (progn (princ "\nNo reference text selected.") (exit)))

  (setq refEnt (ssname refSel 0)
        refLayer (cdr (assoc 8 (entget refEnt))))

  (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER")
                             (cons 8 (esc-wildcard refLayer))
                             (cons 410 (getvar "CTAB")))))

  ;; =========================================================================
  ;; STEP 3: INJECT DATA WHILE PRESERVING FORMATTING
  ;; =========================================================================
  (if ss
    (progn
      (setq count (sslength ss) i 0 updated 0 missing 0 skipped 0)
      
      ;; Force DIMZIN to 0 to strictly enforce trailing zeros on 2 decimal formatting
      (setvar "DIMZIN" 0) 
      
      (while (< i count)
        (setq ent (ssname ss i)
              elist (entget ent)
              entType (cdr (assoc 0 elist))
              oldTxt (get-text ent))

        (setq parsed      (split-mtext oldTxt)
              stylePrefix (nth 0 parsed)
              displayId   (nth 1 parsed)
              closeBraces (nth 2 parsed)
              cleanId     (normalize-id displayId))

        (if (and (= csvMode "SEWERAGE") (not (is-sewer-mh-p oldTxt displayId)))
          (setq skipped (1+ skipped))
          (progn
            (setq match (assoc cleanId dict))
            (if match
              (progn
                (setq dataList (cdr match)
                      val1     (nth 0 dataList)
                      val2     (nth 1 dataList)
                      val3     (nth 2 dataList)
                      category (nth 3 dataList))

                (if (= category "SEWERAGE")
                  (progn
                    ;; Formats numbers to exactly 2 decimals (DIMZIN guarantees the trailing zeros)
                    (setq glStr (rtos (atof val1) 2 2)
                          ilStr (rtos (atof val2) 2 2)
                          dStr  (rtos (atof val3) 2 2))
                    (setq newTxt (strcat stylePrefix displayId "\\PGL=" glStr "\\PIL=" ilStr "\\PD=" dStr closeBraces))
                  )
                  (progn
                    (setq szStr   (itoa (fix (+ (atof val1) 0.5)))
                          lenNum  (atof val2)
                          lenStr  (itoa (fix (+ lenNum 0.5)))
                          gradStr (itoa (fix (+ (atof val3) 0.5))))
                    (cond
                      ((<= lenNum 6.0)
                       (setq newTxt (strcat stylePrefix displayId ";\\P" szStr "%%c\\P" lenStr "m\\P1:" gradStr closeBraces)))
                      ((<= lenNum 14.0)
                       (setq newTxt (strcat stylePrefix displayId "; " szStr "%%c\\P" lenStr "m 1:" gradStr closeBraces)))
                      (T
                       (setq newTxt (strcat stylePrefix displayId "; " szStr "%%c " lenStr "m 1:" gradStr closeBraces)))
                    )
                  )
                )

                (put-text ent newTxt)
                (setq updated (1+ updated))
              )
              (progn
                ;; Red Box for missing IDs
                (setq pt (cdr (assoc 10 elist)))
                (setq hgt (cdr (assoc 40 elist)))
                (setq gap (if (and hgt (> hgt 0.0)) (* hgt 1.2) 1500.0))
                (setq p1 (list (- (car pt) gap) (- (cadr pt) gap) 0.0)
                      p2 (list (+ (car pt) (* gap 3.5)) (- (cadr pt) gap) 0.0)
                      p3 (list (+ (car pt) (* gap 3.5)) (+ (cadr pt) gap) 0.0)
                      p4 (list (- (car pt) gap) (+ (cadr pt) gap) 0.0))
                (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                               (cons 8 refLayer) (cons 62 1) '(90 . 4) '(70 . 1) (cons 43 50.0)
                               (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2))
                               (list 10 (car p3) (cadr p3)) (list 10 (car p4) (cadr p4))))
                (setq missing (1+ missing))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      (alert (strcat "CSV DATA INJECTION COMPLETE\n\n"
                     "Target Layer: " refLayer "\n"
                     "Mode: " csvMode "\n\n"
                     "Texts Updated: " (itoa updated) "\n"
                     "Missing from CSV (Red Boxed): " (itoa missing)
                     (if (> skipped 0) (strcat "\nNon-Manhole Texts Skipped: " (itoa skipped)) "")))
    )
    (alert (strcat "No text objects found on layer: " refLayer))
  )

  ;; Restore system variables
  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)