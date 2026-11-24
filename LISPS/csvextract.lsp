;; ============================================================================
;; CSVEXTRACT.LSP — Round-Trip CAD-to-Bridge Schedule Extractor
;; ============================================================================
;; Command: CSVEXTRACT
;; Scans selected TEXT, MTEXT, and MULTILEADER callouts in AutoCAD (or an entire
;; layer via sample pick), strips inline MTEXT formatting codes, parses drainage
;; run annotations (ID, Size, Length, Gradient) and sewerage/drainage manhole
;; callouts (ID, GL, IL, Depth), and exports a clean schedule CSV ready to drop
;; directly into the Web Bridge or open in Microsoft Excel.
;;
;; Fully compliant with autolisp_rules_standards.md (Rules #1 through #9).
;; ============================================================================

(vl-load-com)

(defun c:CSVEXTRACT ( / *error* old_cmdecho old_dimzin
                        esc-filter get-text strip-mtext trim-str csv-cell
                        extract-num-after parse-callout
                        modeAns sampleEnt refLayer ss i ent raw clean
                        parsed rows outPath f rowCnt drainCnt mhCnt)

  ;; --------------------------------------------------------------------------
  ;; Local Error Handler
  ;; --------------------------------------------------------------------------
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_dimzin  (setvar "DIMZIN"  old_dimzin))
    (if (and f (= (type f) 'FILE)) (close f))
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n[CSVEXTRACT Error]: " msg))
    )
    (princ)
  )

  (setq old_cmdecho (getvar "CMDECHO")
        old_dimzin  (getvar "DIMZIN")
  )
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  ;; --------------------------------------------------------------------------
  ;; Rule #1: Wildcard Escaping for ssget Filters
  ;; --------------------------------------------------------------------------
  (defun esc-filter (str / idx ch out)
    (if (fboundp 'jrk:esc-filter)
      (jrk:esc-filter str)
      (progn
        (setq out "" idx 1)
        (if str
          (while (<= idx (strlen str))
            (setq ch (substr str idx 1))
            (if (wcmatch ch "[#*?@.~`[\\],`]")
              (setq out (strcat out "`" ch))
              (setq out (strcat out ch))
            )
            (setq idx (1+ idx))
          )
        )
        out
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; Rule #2: 3-Layer Entity Text Extraction
  ;; --------------------------------------------------------------------------
  (defun get-text (e / ed typ str vla res rest)
    (if (fboundp 'jrk:get-text)
      (jrk:get-text e)
      (if (and e (setq ed (entget e)))
        (progn
          (setq typ (cdr (assoc 0 ed))
                str nil
          )
          ;; Layer 1: getpropertyvalue
          (if (and (null str) (fboundp 'getpropertyvalue))
            (progn
              (setq res
                (cond
                  ((= typ "MULTILEADER") (vl-catch-all-apply 'getpropertyvalue (list e "MText/Contents")))
                  ((= typ "MTEXT")       (vl-catch-all-apply 'getpropertyvalue (list e "Contents")))
                  ((= typ "TEXT")        (vl-catch-all-apply 'getpropertyvalue (list e "TextString")))
                  (t nil)
                )
              )
              (if (and res (not (vl-catch-all-error-p res)) (= (type res) 'STR) (/= res ""))
                (setq str res)
              )
            )
          )
          ;; Layer 2: ActiveX VLA COM
          (if (null str)
            (progn
              (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
              (if (and vla (not (vl-catch-all-error-p vla)))
                (progn
                  (setq res (vl-catch-all-apply 'vlax-get-property (list vla 'TextString)))
                  (if (and res (not (vl-catch-all-error-p res)) (= (type res) 'STR))
                    (setq str res)
                  )
                )
              )
            )
          )
          ;; Layer 3: Pure DXF Fallback
          (if (null str)
            (cond
              ((= typ "MULTILEADER")
               (setq rest ed)
               (while rest
                 (if (and (= (caar rest) 304) (/= (cdar rest) "LEADER_LINE{"))
                   (setq str (cdar rest) rest nil)
                   (setq rest (cdr rest))
                 )
               )
              )
              ((or (= typ "TEXT") (= typ "MTEXT"))
               (setq str "")
               (foreach pair ed
                 (if (or (= (car pair) 3) (= (car pair) 1))
                   (setq str (strcat str (cdr pair)))
                 )
               )
              )
            )
          )
          str
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; Strip MTEXT formatting codes (\P -> newline, {\W0.5;...} -> content)
  ;; --------------------------------------------------------------------------
  (defun trim-str (s)
    (if s
      (vl-string-trim " \t\r\n" s)
      ""
    )
  )

  (defun strip-mtext (s / len idx ch nextCh out semiPos)
    (setq out "" idx 1 len (if s (strlen s) 0))
    (while (<= idx len)
      (setq ch (substr s idx 1))
      (cond
        ;; Strip braces
        ((or (= ch "{") (= ch "}"))
         (setq idx (1+ idx))
        )
        ;; Handle MTEXT control sequences starting with backslash
        ((and (= ch "\\") (< idx len))
         (setq nextCh (substr s (1+ idx) 1))
         (cond
           ;; Paragraph break \P or \N -> space/pipe delimiter
           ((or (= nextCh "P") (= nextCh "p") (= nextCh "N") (= nextCh "n"))
            (setq out (strcat out " | ")
                  idx (+ idx 2)
            )
           )
           ;; Non-breaking space \~
           ((= nextCh "~")
            (setq out (strcat out " ")
                  idx (+ idx 2)
            )
           )
           ;; Formatting code terminated by semicolon: \W0.5; \H1.2x; \C1; \fArial; \A1;
           ((wcmatch nextCh "[ACFHQTWfpcLlOoKk]")
            (if (wcmatch nextCh "[LlOoKk]")
              (setq idx (+ idx 2))
              (progn
                (setq semiPos (vl-string-search ";" s idx))
                (if semiPos
                  (setq idx (+ semiPos 2))
                  (setq idx (+ idx 2))
                )
              )
            )
           )
           (t
            (setq out (strcat out ch)
                  idx (1+ idx)
            )
           )
         )
        )
        (t
         (setq out (strcat out ch)
               idx (1+ idx)
         )
        )
      )
    )
    (trim-str out)
  )

  ;; --------------------------------------------------------------------------
  ;; CSV Cell Sanitizer (Prevents Formula Injection & quotes commas)
  ;; --------------------------------------------------------------------------
  (defun csv-cell (val / s firstCh)
    (setq s (if val (vl-princ-to-string val) ""))
    (if (> (strlen s) 0)
      (progn
        (setq firstCh (substr s 1 1))
        (if (and (wcmatch firstCh "[=+@-]")
                 (not (distof s)))
          (setq s (strcat "'" s))
        )
      )
    )
    (if (or (vl-string-search "," s)
            (vl-string-search "\"" s)
            (vl-string-search "\n" s))
      (progn
        ;; Escape double quotes
        (while (vl-string-search "\"" s)
          (setq s (vl-string-subst "'\"'" "\"" s))
        )
        (strcat "\"" s "\"")
      )
      s
    )
  )

  ;; --------------------------------------------------------------------------
  ;; Extract numeric token immediately following a keyword in uppercase text
  ;; --------------------------------------------------------------------------
  (defun extract-num-after (uStr kw / pos sub idx len ch numStr started done)
    (setq pos (vl-string-search (strcase kw) uStr))
    (if pos
      (progn
        (setq sub (substr uStr (+ pos (strlen kw) 1))
              len (strlen sub)
              idx 1
              numStr ""
              started nil
              done nil
        )
        (while (and (<= idx len) (not done))
          (setq ch (substr sub idx 1))
          (cond
            ((wcmatch ch "[0-9.]")
             (setq numStr (strcat numStr ch)
                   started T
             )
            )
            ((and (not started) (wcmatch ch "[-+]"))
             (setq numStr (strcat numStr ch)
                   started T
             )
            )
            (started
             (setq done T)
            )
          )
          (setq idx (1+ idx))
        )
        (if (and (/= numStr "") (distof numStr))
          numStr
          ""
        )
      )
      ""
    )
  )

  ;; --------------------------------------------------------------------------
  ;; Parse a stripped CAD Callout into (ID Size Length Gradient GL IL Depth Category RawText)
  ;; --------------------------------------------------------------------------
  (defun parse-callout (txt / uTxt idTok rest len idx ch tokStarted
                              glVal ilVal depthVal lenVal gradVal sizeVal
                              posm posmm subBefore k kCh cat)
    (setq uTxt (strcase txt)
          idTok ""
          len (strlen txt)
          idx 1
    )
    ;; Extract leading ID token (up to first space or pipe delimiter)
    (while (and (<= idx len)
                (not (wcmatch (substr txt idx 1) "[ |\t\r\n]")))
      (setq idTok (strcat idTok (substr txt idx 1))
            idx   (1+ idx)
      )
    )
    (setq idTok (vl-string-trim " :;|," idTok))

    ;; Extract Manhole Levels (GL, IL, D/DEPTH)
    (setq glVal    (extract-num-after uTxt "GL"))
    (setq ilVal    (extract-num-after uTxt "IL"))
    (setq depthVal (extract-num-after uTxt "DEPTH"))
    (if (= depthVal "")
      (setq depthVal (extract-num-after uTxt "D="))
    )
    (if (and (= depthVal "") (/= glVal "") (/= ilVal ""))
      (setq depthVal (rtos (- (atof glVal) (atof ilVal)) 2 2))
    )

    ;; Extract Gradient ("1:200" or "1 IN 200")
    (setq gradVal (extract-num-after uTxt "1:"))
    (if (= gradVal "")
      (setq gradVal (extract-num-after uTxt "1 IN "))
    )
    (if (/= gradVal "")
      (setq gradVal (strcat "1:" gradVal))
    )

    ;; Extract Pipe Size ("300MM" or "225 DIA")
    (setq sizeVal "")
    (setq posmm (vl-string-search "MM" uTxt))
    (if posmm
      (progn
        (setq k posmm)
        (while (and (> k 0) (= (substr uTxt k 1) " "))
          (setq k (1- k))
        )
        (while (and (> k 0) (wcmatch (substr uTxt k 1) "[0-9xX*]"))
          (setq sizeVal (strcat (substr txt k 1) sizeVal)
                k       (1- k)
          )
        )
        (if (/= sizeVal "")
          (setq sizeVal (strcat sizeVal "mm"))
        )
      )
    )

    ;; Extract Length ("25.50M " or "25.5M|")
    (setq lenVal "")
    (setq idx 1)
    (while (<= idx len)
      (if (and (= (substr uTxt idx 1) "M")
               (or (= idx len) (not (wcmatch (substr uTxt (1+ idx) 1) "[A-Z0-9]"))))
        (progn
          (setq k (1- idx)
                subBefore ""
          )
          (while (and (> k 0) (wcmatch (substr uTxt k 1) "[0-9.]"))
            (setq subBefore (strcat (substr uTxt k 1) subBefore)
                  k         (1- k)
            )
          )
          (if (and (/= subBefore "")
                   (distof subBefore)
                   (or (= k 0) (wcmatch (substr uTxt k 1) "[ |(\t-]")))
            (setq lenVal (strcat subBefore "m"))
          )
        )
      )
      (setq idx (1+ idx))
    )

    ;; Classify Category
    (setq cat
      (cond
        ((or (/= glVal "") (/= ilVal "")) "MANHOLE")
        ((or (/= sizeVal "") (/= lenVal "") (/= gradVal "")) "DRAIN_OR_PIPE")
        (t "GENERAL_TEXT")
      )
    )

    (list idTok sizeVal lenVal gradVal glVal ilVal depthVal cat txt)
  )

  ;; --------------------------------------------------------------------------
  ;; Main Interactive Flow
  ;; --------------------------------------------------------------------------
  (princ "\n========================================================")
  (princ "\n  CSVEXTRACT — Round-Trip CAD-to-Bridge Schedule Export ")
  (princ "\n========================================================")

  (initget "Selection Layer")
  (setq modeAns (getkword "\nSelect extraction mode [Selection/Layer] <Selection>: "))
  (if (null modeAns) (setq modeAns "Selection"))

  (cond
    ((= modeAns "Layer")
     (setq sampleEnt (car (entsel "\nPick a sample TEXT/MTEXT/MULTILEADER on the target layer: ")))
     (if sampleEnt
       (progn
         (setq refLayer (cdr (assoc 8 (entget sampleEnt))))
         (princ (strcat "\nScanning all text entities on layer [" refLayer "]..."))
         (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER")
                                    (cons 8 (esc-filter refLayer)))))
       )
     )
    )
    (t
     (princ "\nSelect TEXT, MTEXT, or MULTILEADER callouts to extract: ")
     (setq ss (ssget '((0 . "TEXT,MTEXT,MULTILEADER"))))
    )
  )

  (if (null ss)
    (princ "\nNo valid text entities selected. Command cancelled.")
    (progn
      (setq rows     nil
            rowCnt   0
            drainCnt 0
            mhCnt    0
            i        0
      )
      (while (< i (sslength ss))
        (setq ent   (ssname ss i)
              raw   (get-text ent)
              clean (strip-mtext raw)
        )
        (if (and clean (/= clean ""))
          (progn
            (setq parsed (parse-callout clean))
            (setq rows   (cons parsed rows)
                  rowCnt (1+ rowCnt)
            )
            (cond
              ((= (nth 7 parsed) "MANHOLE")       (setq mhCnt (1+ mhCnt)))
              ((= (nth 7 parsed) "DRAIN_OR_PIPE") (setq drainCnt (1+ drainCnt)))
            )
          )
        )
        (setq i (1+ i))
      )
      ;; Sort rows alphabetically by ID for clean schedule order
      (setq rows (vl-sort (reverse rows)
                          '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))

      (setq outPath (getfiled "Save Extracted CAD Schedule CSV"
                              "AutoCAD_Extracted_Schedule.csv"
                              "csv"
                              1))
      (if (and outPath (setq f (open outPath "w")))
        (progn
          (write-line "ID,Size,Length,Gradient,GL,IL,Depth,Category,RawCallout" f)
          (foreach r rows
            (write-line
              (strcat
                (csv-cell (nth 0 r)) ","
                (csv-cell (nth 1 r)) ","
                (csv-cell (nth 2 r)) ","
                (csv-cell (nth 3 r)) ","
                (csv-cell (nth 4 r)) ","
                (csv-cell (nth 5 r)) ","
                (csv-cell (nth 6 r)) ","
                (csv-cell (nth 7 r)) ","
                (csv-cell (nth 8 r))
              )
              f
            )
          )
          (close f)
          (setq f nil)
          (princ (strcat "\n[CSVEXTRACT] Successfully exported " (itoa rowCnt) " callouts ("
                         (itoa drainCnt) " pipe/drain runs, "
                         (itoa mhCnt) " manholes/sumps) to:\n  " outPath))
          (princ "\nTip: Drop this CSV directly into the Web Bridge for hydraulic QA/QC or batch updates!")
        )
        (princ "\nExport cancelled.")
      )
    )
  )

  (setvar "CMDECHO" old_cmdecho)
  (setvar "DIMZIN"  old_dimzin)
  (princ)
)

(princ "\nCSVEXTRACT loaded. Type CSVEXTRACT to extract CAD callouts to a Web Bridge CSV.")
(princ)
