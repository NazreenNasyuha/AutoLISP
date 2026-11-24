(vl-load-com)

(defun c:csvupdate ( / *error* oldCmd oldDimZin undoStarted
                       errBoxSize errBoxWidth errBoxColor dupBoxColor zoomHeight
                       esc-filter fmt-2dec split-csv normalize-id split-mtext join-str-list
                       get-text put-text is-sewer-mh-p pick-target-text get-box-center draw-qa-box
                       filename file line cols id searchId val1 val2 val3 category csvMode
                       dict csvDupIds totalCsvRows csvIdList matchedCsvIds unmatchedCsvIds csvEntry
                       refEnt refLayer layerFlags ss count i ent elist entType entLayer
                       oldTxt parsed stylePrefix displayId delimChar closeBraces cleanId match
                       cadIdCounts existingCnt isCadDup isCsvDup dupCount dupIdNames cadDupIdNames
                       updated missing skipped dataList glStr ilStr dStr
                       szStr lenStr gradStr lenNum newTxt pt hgt boxRad errorPt dupPt zoomTarget)

  ;; =========================================================================
  ;; USER SETTINGS (QA/QC RED & BLUE BOX APPEARANCE)
  ;; =========================================================================
  (setq errBoxSize  10000.0) ;; Half-width of QA/QC box (10000 = 20m x 20m box)
  (setq errBoxWidth 400.0)   ;; Polyline thickness of QA/QC boxes
  (setq errBoxColor 1)       ;; 1 = Red (Missing ID from CSV)
  (setq dupBoxColor 5)       ;; 5 = Blue (Duplicate ID in CAD or CSV)
  (setq zoomHeight  35000.0) ;; Camera zoom view height when jumping to an error
  ;; =========================================================================

  (setq oldCmd (getvar "CMDECHO"))
  (setq oldDimZin (getvar "DIMZIN"))

  (defun *error* (msg)
    (if undoStarted (progn (command "_.UNDO" "_End") (setq undoStarted nil)))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if file (progn (close file) (setq file nil)))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[CSVUPDATE Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; 0. STANDARDS-COMPLIANT WILDCARD ESCAPER (Rule #1) & DECIMAL FORMATTER (Rule #4)
  ;; -------------------------------------------------------------------------
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

  (defun fmt-2dec (val / s dotPos fracLen)
    (if (fboundp 'jrk:fmt-2dec)
      (jrk:fmt-2dec val)
      (progn
        (setq s (rtos val 2 2)
              dotPos (vl-string-search "." s)
        )
        (if (null dotPos)
          (strcat s ".00")
          (progn
            (setq fracLen (- (strlen s) dotPos 1))
            (while (< fracLen 2)
              (setq s (strcat s "0")
                    fracLen (1+ fracLen)
              )
            )
            s
          )
        )
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 1. SAFE CSV LINE SPLITTER
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
  ;; 2. ULTRA-FUZZY ID NORMALIZER ("MH 58", "MH-058", "58" -> "58")
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
  ;; 3. SMART MTEXT PARSER (Supports ";", ":", and "\P")
  ;; -------------------------------------------------------------------------
  (defun split-mtext (txt / len i braceCnt loopP ch sec semi stylePrefix remTxt endPos p displayId delimChar closeBraces)
    (setq len (strlen txt) i 1 braceCnt 0 loopP T)

    (while (and loopP (<= i len))
      (setq ch  (substr txt i 1))
      (setq sec (if (< i len) (substr txt (1+ i) 1) ""))
      (cond
        ((= ch "{")
         (setq braceCnt (1+ braceCnt) i (1+ i)))
        ((and (= ch "\\") (member sec '("F" "f" "W" "w" "H" "h" "C" "c" "T" "t" "Q" "q" "A" "a" "p")))
         (setq semi (vl-string-search ";" txt (1- i)))
         (if semi (setq i (+ semi 2)) (setq loopP nil)))
        ((and (= ch "\\") (member sec '("L" "l" "O" "o" "K" "k")))
         (setq i (+ i 2)))
        (T (setq loopP nil))
      )
    )

    (setq stylePrefix (if (> i 1) (substr txt 1 (1- i)) ""))
    (setq remTxt (substr txt i))

    (setq endPos (strlen remTxt) delimChar ";")
    (foreach del '(";" ":" "\\P" "\\p" "\r" "\n" "}")
      (if (setq p (vl-string-search del remTxt))
        (if (< p endPos)
          (progn
            (setq endPos p)
            (if (or (= del ";") (= del ":")) (setq delimChar del))
          )
        )
      )
    )

    (setq displayId (if (> endPos 0) (vl-string-trim " \t" (substr remTxt 1 endPos)) ""))
    (setq closeBraces "")
    (repeat braceCnt (setq closeBraces (strcat closeBraces "}")))
    (list stylePrefix displayId delimChar closeBraces)
  )

  ;; -------------------------------------------------------------------------
  ;; 4. AUTOCAD-SAFE 3-LAYER TEXT READER & WRITER (Rule #2)
  ;; -------------------------------------------------------------------------
  (defun get-text (ent / elist typ obj res item)
    (if (fboundp 'jrk:get-text)
      (jrk:get-text ent)
      (progn
        (setq elist (entget ent) typ (cdr (assoc 0 elist)) res nil)
        ;; Layer 1: Modern Property Engine
        (if (and (null res) (fboundp 'getpropertyvalue))
          (progn
            (setq res
              (cond
                ((= typ "MULTILEADER") (vl-catch-all-apply 'getpropertyvalue (list ent "MText/Contents")))
                ((= typ "MTEXT")       (vl-catch-all-apply 'getpropertyvalue (list ent "Contents")))
                ((= typ "TEXT")        (vl-catch-all-apply 'getpropertyvalue (list ent "TextString")))
                (t nil)
              )
            )
            (if (or (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
              (setq res nil)
            )
          )
        )
        ;; Layer 2: ActiveX VLA COM Fallback
        (if (null res)
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (and obj (not (vl-catch-all-error-p obj)))
              (setq res (vl-catch-all-apply 'vlax-get-property (list obj 'TextString)))
            )
            (if (or (vl-catch-all-error-p res) (/= (type res) 'STR) (= res ""))
              (setq res nil)
            )
          )
        )
        ;; Layer 3: Pure DXF Fallback
        (if (null res)
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
    )
  )

  (defun put-text (ent newTxt / elist typ ok res newElist replaced obj)
    (if (fboundp 'jrk:put-text)
      (jrk:put-text ent newTxt)
      (progn
        (setq elist (entget ent) typ (cdr (assoc 0 elist)) ok nil)
        ;; Layer 1: Modern Property Engine
        (if (and (not ok) (fboundp 'setpropertyvalue))
          (progn
            (setq res
              (cond
                ((= typ "MULTILEADER") (vl-catch-all-apply 'setpropertyvalue (list ent "MText/Contents" newTxt)))
                ((= typ "MTEXT")       (vl-catch-all-apply 'setpropertyvalue (list ent "Contents" newTxt)))
                ((= typ "TEXT")        (vl-catch-all-apply 'setpropertyvalue (list ent "TextString" newTxt)))
                (t nil)
              )
            )
            (if (and (not (vl-catch-all-error-p res)) (= (get-text ent) newTxt))
              (setq ok T)
            )
          )
        )
        ;; Layer 2: ActiveX VLA COM Fallback
        (if (not ok)
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (and obj (not (vl-catch-all-error-p obj)))
              (progn
                (setq res (vl-catch-all-apply 'vlax-put-property (list obj 'TextString newTxt)))
                (if (not (vl-catch-all-error-p res))
                  (progn
                    (vl-catch-all-apply 'vla-update (list obj))
                    (if (= (get-text ent) newTxt)
                      (setq ok T)
                    )
                  )
                )
              )
            )
          )
        )
        ;; Layer 3: Pure DXF Fallback
        (if (not ok)
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
             (entmod (reverse newElist))
             (entupd ent))

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
             (entupd ent))

            (T
             (if (assoc 1 elist)
               (progn
                 (entmod (subst (cons 1 newTxt) (assoc 1 elist) elist))
                 (entupd ent)
               )
             ))
          )
        )
        (entupd ent)
      )
    )
  )

  (defun is-sewer-mh-p (txt dispId / uTxt uId)
    (setq uTxt (strcase txt) uId (strcase dispId))
    (or (wcmatch uTxt "*GL=*,*IL=*,*GL:*,*IL:*")
        (wcmatch uId "MH*"))
  )

  ;; -------------------------------------------------------------------------
  ;; 5. QA/QC BOX GENERATOR (Red for Missing, Blue for Duplicate — Rule #5 WCS)
  ;; -------------------------------------------------------------------------
  (defun get-box-center (ent elist entType / vla minPt maxPt pMin pMax p)
    (setq p nil)
    (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (and vla (not (vl-catch-all-error-p vla))
             (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list vla 'minPt 'maxPt)))))
      (progn
        (setq pMin (vlax-safearray->list minPt)
              pMax (vlax-safearray->list maxPt)
              p    (list (/ (+ (car pMin) (car pMax)) 2.0)
                         (/ (+ (cadr pMin) (cadr pMax)) 2.0)
                         0.0)
        )
      )
    )
    (if (null p)
      (progn
        (if (= entType "MULTILEADER")
          (setq p (cdr (assoc 12 elist)))
        )
        (if (null p)
          (setq p (cdr (assoc 10 elist)))
        )
        (if p
          (setq p (trans p ent 0))
          (setq p '(0.0 0.0 0.0))
        )
        (setq p (list (+ (car p) (* errBoxSize 0.25))
                      (- (cadr p) (* errBoxSize 0.25))
                      0.0))
      )
    )
    p
  )

  (defun draw-qa-box (pt rad col lay / p1 p2 p3 p4)
    (setq p1 (list (- (car pt) rad) (- (cadr pt) rad) 0.0)
          p2 (list (+ (car pt) rad) (- (cadr pt) rad) 0.0)
          p3 (list (+ (car pt) rad) (+ (cadr pt) rad) 0.0)
          p4 (list (- (car pt) rad) (+ (cadr pt) rad) 0.0))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                   (cons 8 lay) (cons 62 col) '(90 . 4) '(70 . 1) (cons 43 errBoxWidth)
                   (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2))
                   (list 10 (car p3) (cadr p3)) (list 10 (car p4) (cadr p4))))
  )

  ;; -------------------------------------------------------------------------
  ;; 6. CLICK-THROUGH TARGET PICKER
  ;; -------------------------------------------------------------------------
  (defun pick-target-text ( / loop sel e pt ssPick picked)
    (setq loop T picked nil)
    (while loop
      (setq sel (entsel "\nSelect ONE placeholder text to target its layer (or press Enter to exit): "))
      (if sel
        (progn
          (setq e  (car sel)
                pt (cadr sel))
          (if (member (cdr (assoc 0 (entget e))) '("TEXT" "MTEXT" "MULTILEADER"))
            (setq picked e loop nil)
            (if (setq ssPick (ssget pt '((0 . "TEXT,MTEXT,MULTILEADER"))))
              (setq picked (ssname ssPick 0) loop nil)
              (princ (strcat "\nClicked object is a " (cdr (assoc 0 (entget e))) ". Please click directly on the text."))
            )
          )
        )
        (setq loop nil)
      )
    )
    picked
  )

  (defun join-str-list (lst sep / out)
    (if (null lst)
      ""
      (progn
        (setq out (car lst))
        (foreach item (cdr lst)
          (setq out (strcat out sep item))
        )
        out
      )
    )
  )

  (princ "\nCommand: CSVUPDATE v3.3 (QA/QC Audit Report + Red/Blue Box Edition)")
  (setvar "CMDECHO" 0)

  ;; =========================================================================
  ;; STEP 1: READ CSV FILE & CHECK FOR CSV DUPLICATES
  ;; =========================================================================
  (setq filename (getfiled "Select Engineer's CSV File" "" "csv" 0))
  (if (not filename) (progn (princ "\nNo file selected.") (exit)))

  (setq file (open filename "r"))
  (if (not file)
    (progn
      (alert "Error: Cannot open CSV file.\nPlease close the file in Excel and try again.")
      (exit)
    )
  )

  (setq dict nil csvDupIds nil csvIdList nil totalCsvRows 0 csvMode "DRAINAGE")
  (while (setq line (read-line file))
    (if (/= (vl-string-trim " \t\r\n" line) "")
      (progn
        (setq cols (split-csv line))
        (cond
          ((>= (length cols) 5)
           (setq id       (nth 0 cols)
                 val1     (nth 1 cols)
                 val2     (nth 2 cols)
                 val3     (nth 3 cols)
                 category (strcase (nth 4 cols)))
           (if (= category "SEWERAGE") (setq csvMode "SEWERAGE"))
           (setq searchId (normalize-id id))
           (if (and (/= searchId "") (/= (strcase id) "ID"))
             (progn
               (setq totalCsvRows (1+ totalCsvRows))
               ;; Check if this ID already exists in the CSV file
               (if (assoc searchId dict)
                 (if (not (member searchId csvDupIds))
                   (setq csvDupIds (cons searchId csvDupIds))
                 )
                 (setq csvIdList (cons (cons searchId (strcase id)) csvIdList))
               )
               (setq dict (cons (cons searchId (list val1 val2 val3 category)) dict))
             )
           ))
          ((>= (length cols) 2)
           (setq id   (nth 0 cols)
                 val1 (nth 1 cols))
           (setq searchId (normalize-id id))
           (if (and (/= searchId "") (/= (strcase id) "ID"))
             (progn
               (setq totalCsvRows (1+ totalCsvRows))
               (if (assoc searchId dict)
                 (if (not (member searchId csvDupIds))
                   (setq csvDupIds (cons searchId csvDupIds))
                 )
                 (setq csvIdList (cons (cons searchId (strcase id)) csvIdList))
               )
               (setq dict (cons (cons searchId (list val1 "" "" "RAW2COL")) dict))
             )
           ))
        )
      )
    )
  )
  (close file)
  (setq file nil)
  (setq csvIdList (reverse csvIdList))

  (if (not dict)
    (progn (alert "Error: No valid data found in CSV.") (exit))
  )

  ;; =========================================================================
  ;; STEP 2: SELECT REFERENCE TEXT & CHECK FOR LOCKED LAYER
  ;; =========================================================================
  (setq refEnt (pick-target-text))
  (if (not refEnt) (progn (princ "\nCommand cancelled.") (exit)))

  (setq refLayer (cdr (assoc 8 (entget refEnt))))

  ;; Guard against locked layers in AutoCAD (DXF 70 bit 4 = Locked)
  (setq layerFlags (cdr (assoc 70 (tblsearch "LAYER" refLayer))))
  (if (and layerFlags (= (logand layerFlags 4) 4))
    (progn
      (alert (strcat "ERROR: Layer \"" refLayer "\" is currently LOCKED!\n\nPlease unlock the layer in AutoCAD and run CSVUPDATE again."))
      (exit)
    )
  )

  (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER")
                             (cons 8 (esc-filter refLayer)))))

  ;; =========================================================================
  ;; STEP 3: PRE-SCAN FOR DUPLICATE IDs IN CAD & INJECT DATA
  ;; =========================================================================
  (if ss
    (progn
      (command "_.UNDO" "_Begin")
      (setq undoStarted T)
      (setq count (sslength ss) i 0 cadIdCounts nil)

      ;; PASS 1: Count how many times each ID appears on the target layer in CAD
      (while (< i count)
        (setq ent      (ssname ss i)
              elist    (entget ent)
              entLayer (cdr (assoc 8 elist)))
        (if (= (strcase entLayer) (strcase refLayer))
          (progn
            (setq oldTxt    (get-text ent)
                  parsed    (split-mtext oldTxt)
                  displayId (nth 1 parsed)
                  cleanId   (normalize-id displayId))
            (if (not (and (= csvMode "SEWERAGE") (not (is-sewer-mh-p oldTxt displayId))))
              (if (/= cleanId "")
                (if (setq existingCnt (assoc cleanId cadIdCounts))
                  (setq cadIdCounts (subst (cons cleanId (1+ (cdr existingCnt))) existingCnt cadIdCounts))
                  (setq cadIdCounts (cons (cons cleanId 1) cadIdCounts))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; PASS 2: Inject CSV Data & Draw Red (Missing) / Blue (Duplicate) Boxes
      (setq i 0 updated 0 missing 0 dupCount 0 skipped 0
            dupIdNames nil cadDupIdNames nil matchedCsvIds nil unmatchedCsvIds nil
            errorPt nil dupPt nil)
      (setvar "DIMZIN" 0)

      (while (< i count)
        (setq ent      (ssname ss i)
              elist    (entget ent)
              entType  (cdr (assoc 0 elist))
              entLayer (cdr (assoc 8 elist)))

        (if (= (strcase entLayer) (strcase refLayer))
          (progn
            (setq oldTxt      (get-text ent)
                  parsed      (split-mtext oldTxt)
                  stylePrefix (nth 0 parsed)
                  displayId   (nth 1 parsed)
                  delimChar   (nth 2 parsed)
                  closeBraces (nth 3 parsed)
                  cleanId     (normalize-id displayId)
                  match       (assoc cleanId dict))

            (if (and (= csvMode "SEWERAGE") (not (is-sewer-mh-p oldTxt displayId)) (null match))
              (setq skipped (1+ skipped))
              (progn
                (setq pt (get-box-center ent elist entType))
                (setq hgt (if (= entType "MULTILEADER") (cdr (assoc 140 elist)) (cdr (assoc 40 elist))))
                (setq boxRad (if (and hgt (> (* hgt 4.0) errBoxSize)) (* hgt 4.0) errBoxSize))

                ;; Check if this ID is duplicated in the CAD drawing or in the CSV
                (setq isCadDup (and (/= cleanId "") (> (cdr (assoc cleanId cadIdCounts)) 1)))
                (setq isCsvDup (and (/= cleanId "") (member cleanId csvDupIds)))

                (if isCadDup
                  (if (not (member (strcase displayId) cadDupIdNames))
                    (setq cadDupIdNames (cons (strcase displayId) cadDupIdNames))
                  )
                )

                (if (or isCadDup isCsvDup)
                  (progn
                    ;; Draw Thick BLUE Box (slightly larger radius so it never overlaps a Red box)
                    (draw-qa-box pt (* boxRad 1.15) dupBoxColor refLayer)
                    (setq dupCount (1+ dupCount))
                    (if (not (member (strcase displayId) dupIdNames))
                      (setq dupIdNames (cons (strcase displayId) dupIdNames))
                    )
                    (if (null dupPt) (setq dupPt pt))
                  )
                )

                ;; Check if ID exists in CSV
                (if match
                  (progn
                    (if (not (member cleanId matchedCsvIds))
                      (setq matchedCsvIds (cons cleanId matchedCsvIds))
                    )
                    (setq dataList (cdr match)
                          val1     (nth 0 dataList)
                          val2     (nth 1 dataList)
                          val3     (nth 2 dataList)
                          category (nth 3 dataList))

                    (cond
                      ((= category "RAW2COL")
                       (setq newTxt val1))

                      ((= category "SEWERAGE")
                       (setq glStr (fmt-2dec (atof val1))
                             ilStr (fmt-2dec (atof val2))
                             dStr  (fmt-2dec (atof val3)))
                       (setq newTxt (strcat stylePrefix displayId "\\PGL=" glStr "\\PIL=" ilStr "\\PD=" dStr closeBraces)))

                      (T
                       (setq szStr   (itoa (fix (+ (atof val1) 0.5)))
                             lenNum  (atof val2)
                             lenStr  (itoa (fix (+ lenNum 0.5)))
                             gradStr (itoa (fix (+ (atof val3) 0.5))))
                       (cond
                         ((<= lenNum 6.0)
                          (setq newTxt (strcat stylePrefix displayId delimChar "\\P" szStr "%%c\\P" lenStr "m\\P1:" gradStr closeBraces)))
                         ((<= lenNum 14.0)
                          (setq newTxt (strcat stylePrefix displayId delimChar " " szStr "%%c\\P" lenStr "m 1:" gradStr closeBraces)))
                         (T
                          (setq newTxt (strcat stylePrefix displayId delimChar " " szStr "%%c " lenStr "m 1:" gradStr closeBraces)))
                       ))
                    )

                    (put-text ent newTxt)
                    (setq updated (1+ updated))
                  )
                  (progn
                    ;; Missing from CSV -> Draw Thick RED Box
                    (draw-qa-box pt boxRad errBoxColor refLayer)
                    (setq missing (1+ missing))
                    (if (null errorPt) (setq errorPt pt))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; Compare CSV ID list against matched CAD IDs to find CSV IDs missing in CAD
      (foreach csvEntry csvIdList
        (if (not (member (car csvEntry) matchedCsvIds))
          (setq unmatchedCsvIds (cons (cdr csvEntry) unmatchedCsvIds))
        )
      )
      (setq unmatchedCsvIds (reverse unmatchedCsvIds)
            cadDupIdNames   (reverse cadDupIdNames)
            dupIdNames      (reverse dupIdNames))

      ;; Print Full QA/QC Audit Report in AutoCAD Command Line
      (princ "\n================================================================")
      (princ "\n          CSVUPDATE v3.3 — QA/QC ENGINEERING AUDIT REPORT       ")
      (princ "\n================================================================")
      (princ (strcat "\n  Target CAD Layer            : " refLayer))
      (princ (strcat "\n  Discipline Mode             : " csvMode))
      (princ "\n----------------------------------------------------------------")
      (princ "\n  1. TOTAL CSV ROWS VS. TOTAL MATCHED IN CAD")
      (princ (strcat "\n     - Total CSV Rows Loaded  : " (itoa totalCsvRows) " (" (itoa (length csvIdList)) " unique CSV IDs)"))
      (princ (strcat "\n     - Total Matched in CAD   : " (itoa (length matchedCsvIds)) " unique CSV IDs (" (itoa updated) " CAD texts updated)"))
      (princ (strcat "\n     - CSV IDs Not in CAD     : " (itoa (length unmatchedCsvIds))))
      (princ (strcat "\n     - CAD Texts Not in CSV   : " (itoa missing) " [Flagged with RED Box]"))
      (princ (strcat "\n     - Duplicate CAD Texts    : " (itoa dupCount) " [Flagged with BLUE Box]"))
      (princ "\n----------------------------------------------------------------")
      (princ "\n  2. CSV IDs NOT FOUND ANYWHERE IN CAD SELECTION:")
      (if unmatchedCsvIds
        (princ (strcat "\n     " (join-str-list unmatchedCsvIds ", ")))
        (princ "\n     None (All CSV IDs were found in CAD)")
      )
      (princ "\n----------------------------------------------------------------")
      (princ "\n  3. DUPLICATE IDs FOUND IN CAD:")
      (if cadDupIdNames
        (princ (strcat "\n     " (join-str-list cadDupIdNames ", ")))
        (if dupIdNames
          (princ (strcat "\n     " (join-str-list dupIdNames ", ")))
          (princ "\n     None (No duplicate IDs found in CAD)")
        )
      )
      (princ "\n================================================================\n")

      ;; Choose zoom target (prioritize Missing Red Box, then Duplicate Blue Box)
      (setq zoomTarget (if errorPt errorPt dupPt))

      (alert (strcat "CSV DATA INJECTION COMPLETE (v3.3)\n\n"
                     "Target Layer: " refLayer "\n"
                     "Mode: " csvMode "\n\n"
                     "1. Total CSV Rows vs. Matched in CAD:\n"
                     "   - CSV Rows Loaded: " (itoa totalCsvRows) " (" (itoa (length csvIdList)) " unique IDs)\n"
                     "   - Matched in CAD: " (itoa (length matchedCsvIds)) " unique IDs (" (itoa updated) " texts updated)\n"
                     "   - Missing from CSV (RED Box): " (itoa missing) "\n"
                     "   - Duplicate IDs Found (BLUE Box): " (itoa dupCount) "\n\n"
                     "2. CSV IDs NOT Found in CAD (" (itoa (length unmatchedCsvIds)) "):\n   "
                     (if unmatchedCsvIds (join-str-list unmatchedCsvIds ", ") "None") "\n\n"
                     "3. Duplicate IDs Found in CAD:\n   "
                     (if cadDupIdNames
                       (join-str-list cadDupIdNames ", ")
                       (if dupIdNames (join-str-list dupIdNames ", ") "None"))
                     (if (> skipped 0) (strcat "\n\nNon-Manhole Texts Skipped: " (itoa skipped)) "")
                     "\n\n(Full QA/QC Audit Report printed to Command Line - Press F2 to view)"
                     (if zoomTarget "\nClick OK to jump directly to the first flagged box." "")))

      (if undoStarted (progn (command "_.UNDO" "_End") (setq undoStarted nil)))
      (if zoomTarget
        (command "_.ZOOM" "_C" (trans zoomTarget 0 1) zoomHeight)
      )
    )
    (alert (strcat "No text objects found on layer: " refLayer))
  )

  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (if oldCmd (setvar "CMDECHO" oldCmd))
  (princ)
)