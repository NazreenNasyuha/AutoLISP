;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : epanode.lsp
;;; COMMAND     : EPANODE
;;; DESCRIPTION : EPANET Hydraulic Model Junction/Node Results Table Generator
;;;               (Auto-Updates Existing Blocks In-Place or Places New Tables)
;;; ==========================================================================

(defun c:epanode ( / *error* oldCmd oldDimZin
                     defTxt tblLayer tblColor txtColor txtStyle
                     filename fh line data inNodes nodeList nodeData
                     nodeID qVal headVal rpVal hslVal txtHgt lastAng count total
                     pt1 pt2 wAng blockName rowH col1W totW totH
                     ssExist existingMap i ent elist bName info exNodeID exHgt
                     mode updated placed
                     parse-line clean-name draw-text make-block ensure-layer
                     get-block-node-info replace-block-instance )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq defTxt   1000.0)           ;; Default text height for all tables (mm)
  (setq tblLayer "water text")     ;; Target layer for the table and text
  (setq tblColor 7)                ;; 7 = White/Black (Table border lines)
  (setq txtColor 7)                ;; 7 = White/Black (Text values)
  (setq txtStyle "HAZA")           ;; Text style name
  ;; =========================================================================

  (setq oldCmd    (getvar "CMDECHO"))
  (setq oldDimZin (getvar "DIMZIN"))

  ;; =========================================================================
  ;; 2. ERROR HANDLING & CLEANUP
  ;; =========================================================================
  (defun *error* (msg)
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if fh (progn (close fh) (setq fh nil)))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EPANODE] Error: " msg)))
    (princ "\nBatch placement ended.")
    (princ))

  ;; =========================================================================
  ;; 3. PARSING & BLOCK UTILITIES
  ;; =========================================================================
  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")))))

  (defun parse-line (str / lst word k ch)
    (setq lst nil word "" k 1)
    (while (<= k (strlen str))
      (setq ch (substr str k 1))
      (if (or (= ch " ") (= ch "\t"))
        (if (/= word "") (setq lst (cons word lst) word ""))
        (setq word (strcat word ch)))
      (setq k (1+ k)))
    (if (/= word "") (setq lst (cons word lst)))
    (reverse lst))

  (defun clean-name (s / k ch c out)
    (setq k 1 out "")
    (while (<= k (strlen s))
      (setq ch (substr s k 1) c (ascii ch))
      (setq out (strcat out
                        (if (or (and (>= c 48) (<= c 57)) (and (>= c 65) (<= c 90))
                                (and (>= c 97) (<= c 122)) (member ch '("_" "-")))
                          ch "_")))
      (setq k (1+ k)))
    out)

  (defun draw-text (str x y hgt)
    (entmake (list '(0 . "TEXT")
                   (cons 8 tblLayer)
                   (cons 62 txtColor)
                   (list 10 x y 0.0)
                   (cons 40 hgt)
                   (cons 1 str)
                   '(72 . 0)
                   (list 11 x y 0.0)
                   '(73 . 2)
                   (cons 7 txtStyle))))

  (defun make-block (bName id hsl q rp hgt / rH c1W tW tH)
    (setq rH  (* hgt 2.0)
          c1W (* hgt 4.5)
          tW  (+ c1W (* hgt 9.5))
          tH  (* rH 4.0))
    (if (entmake (list '(0 . "BLOCK") (cons 2 bName) '(70 . 0) '(10 0.0 0.0 0.0)))
      (progn
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                       (cons 8 tblLayer) (cons 62 tblColor)
                       '(90 . 4) '(70 . 1)
                       (list 10 0.0 0.0)
                       (list 10 tW 0.0)
                       (list 10 tW (- tH))
                       (list 10 0.0 (- tH))))
        (entmake (list '(0 . "LINE") (cons 8 tblLayer) (cons 62 tblColor) (list 10 0.0 (- rH) 0.0)      (list 11 tW (- rH) 0.0)))
        (entmake (list '(0 . "LINE") (cons 8 tblLayer) (cons 62 tblColor) (list 10 0.0 (* rH -2.0) 0.0) (list 11 tW (* rH -2.0) 0.0)))
        (entmake (list '(0 . "LINE") (cons 8 tblLayer) (cons 62 tblColor) (list 10 0.0 (* rH -3.0) 0.0) (list 11 tW (* rH -3.0) 0.0)))
        (entmake (list '(0 . "LINE") (cons 8 tblLayer) (cons 62 tblColor) (list 10 c1W (- rH) 0.0)      (list 11 c1W (- tH) 0.0)))

        (draw-text id    (* hgt 0.8) (/ rH -2.0)   hgt)
        (draw-text "HSL" (* hgt 0.8) (* rH -1.5)   hgt)
        (draw-text "Q"   (* hgt 0.8) (* rH -2.5)   hgt)
        (draw-text "RP"  (* hgt 0.8) (* rH -3.5)   hgt)

        (draw-text (strcat hsl "m")  (+ c1W (* hgt 0.8)) (* rH -1.5) hgt)
        (draw-text (strcat q "lps")  (+ c1W (* hgt 0.8)) (* rH -2.5) hgt)
        (draw-text (strcat rp "m")   (+ c1W (* hgt 0.8)) (* rH -3.5) hgt)
        (entmake '((0 . "ENDBLK"))))))

  ;; Inspect an existing EPANODE block definition to get its (nodeID . txtHgt)
  (defun get-block-node-info (bName / bRec sub ed typ foundId foundHgt)
    (if (wcmatch (strcase bName) "EPATBL_*,EPANODE_*,EPATABLE_NODE_*")
      (progn
        (setq bRec (tblsearch "BLOCK" bName)
              sub  (if bRec (cdr (assoc -2 bRec)) nil))
        ;; The very first TEXT inside an EPANODE block is always the Node ID
        (while (and sub (null foundId))
          (setq ed  (entget sub)
                typ (cdr (assoc 0 ed)))
          (if (or (= typ "TEXT") (= typ "MTEXT"))
            (setq foundId  (cdr (assoc 1 ed))
                  foundHgt (cdr (assoc 40 ed)))
          )
          (setq sub (entnext sub))
        )
        (if foundId (cons (strcase (vl-string-trim " \t" foundId)) (if foundHgt foundHgt defTxt)) nil)
      )
      nil
    )
  )

  ;; Swap an existing INSERT entity to point to the new block definition
  (defun replace-block-instance (ent newBName / ed)
    (setq ed (entget ent))
    (if (not (entmod (subst (cons 2 newBName) (assoc 2 ed) ed)))
      ;; Universal fallback if entmod on group 2 is restricted
      (progn
        (entmake (list '(0 . "INSERT")
                       (cons 8 (cdr (assoc 8 ed)))
                       (cons 2 newBName)
                       (assoc 10 ed)
                       (if (assoc 41 ed) (assoc 41 ed) '(41 . 1.0))
                       (if (assoc 42 ed) (assoc 42 ed) '(42 . 1.0))
                       (if (assoc 43 ed) (assoc 43 ed) '(43 . 1.0))
                       (if (assoc 50 ed) (assoc 50 ed) '(50 . 0.0))))
        (entdel ent)
      )
      (entupd ent)
    )
  )

  ;; =========================================================================
  ;; 4. INGEST REPORT FILE
  ;; =========================================================================
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)
  (ensure-layer tblLayer tblColor)

  (if (not (tblsearch "STYLE" txtStyle))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                   (cons 2 txtStyle) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0)
                   '(71 . 0) '(3 . "romans.shx") '(4 . ""))))

  (setq filename (getfiled "Select EPANET Report (.rpt)" "" "rpt;txt" 0))
  (cond
    ((null filename) (princ "\nNo file selected. Command cancelled."))
    ((null (setq fh (open filename "r"))) (princ "\nCannot open the selected file."))
    (T
     (setq inNodes nil nodeList nil)
     (while (setq line (read-line fh))
       (cond
         ((wcmatch line "*Node Results*") (setq inNodes T))
         ((wcmatch line "*Link Results*") (setq inNodes nil))
         (inNodes
          (setq data (parse-line line))
          (if (and (>= (length data) 4)
                   (distof (nth 1 data)) (distof (nth 2 data)) (distof (nth 3 data))
                   (not (assoc (nth 0 data) nodeList)))
            (progn
              ;; HSL = Head (nth 2) - Pressure (nth 3)
              (setq headVal (atof (nth 2 data)))
              (setq rpVal   (atof (nth 3 data)))
              (setq hslVal  (rtos (- headVal rpVal) 2 3))
              (setq nodeList (cons (list (nth 0 data) (nth 1 data) hslVal (nth 3 data)) nodeList)))))))
     (close fh)
     (setq fh nil nodeList (reverse nodeList))

     ;; =====================================================================
     ;; 5. SCAN FOR EXISTING TABLES IN DRAWING & UPDATE OR PLACE NEW
     ;; =====================================================================
     (if (null nodeList)
       (princ "\nNo nodes found in the Node Results section.")
       (progn
         ;; Scan drawing for any existing EPANODE blocks
         (setq ssExist (ssget "_X" '((0 . "INSERT")))
               existingMap nil)
         (if ssExist
           (progn
             (setq i 0)
             (while (< i (sslength ssExist))
               (setq ent   (ssname ssExist i)
                     elist (entget ent)
                     bName (cdr (assoc 2 elist))
                     info  (get-block-node-info bName))
               (if info
                 (setq existingMap (cons (list (car info) ent (cdr info)) existingMap))
               )
               (setq i (1+ i))
             )
           )
         )

         (setq mode "New")
         (if existingMap
           (progn
             (initget "Update New")
             (setq mode (getkword (strcat "\nFound " (itoa (length existingMap))
                                          " existing Node tables in drawing. [Update existing/New placement] <Update>: ")))
             (if (null mode) (setq mode "Update"))
           )
         )

         (setq updated 0 placed 0)

         ;; MODE A: AUTO-UPDATE EXISTING BLOCKS IN-PLACE
         (if (= mode "Update")
           (progn
             (foreach nodeData nodeList
               (setq nodeID (nth 0 nodeData)
                     qVal   (nth 1 nodeData)
                     hslVal (nth 2 nodeData)
                     rpVal  (nth 3 nodeData))
               ;; Update every matching instance of this Node ID in the drawing
               (foreach item existingMap
                 (if (= (car item) (strcase nodeID))
                   (progn
                     (setq ent       (nth 1 item)
                           exHgt     (nth 2 item)
                           blockName (clean-name (strcat "EpaTbl_" nodeID "_H" (rtos exHgt 2 2)
                                                         "_" hslVal "_" qVal "_" rpVal)))
                     (if (not (tblsearch "BLOCK" blockName))
                       (make-block blockName nodeID hslVal qVal rpVal exHgt)
                     )
                     (replace-block-instance ent blockName)
                     (setq updated (1+ updated))
                   )
                 )
               )
             )
             (alert (strcat "EPANODE AUTO-UPDATE COMPLETE\n\n"
                            "Existing Node Tables Updated In-Place: " (itoa updated)))
           )

           ;; MODE B: MANUAL PLACEMENT FOR NEW TABLES
           (progn
             (setq txtHgt (getdist (strcat "\nSpecify Text Height for ALL tables <" (rtos defTxt 2 2) ">: ")))
             (if (not txtHgt) (setq txtHgt defTxt))

             (setq lastAng 0.0
                   count   1
                   total   (length nodeList))

             (foreach nodeData nodeList
               (setq nodeID (nth 0 nodeData)
                     qVal   (nth 1 nodeData)
                     hslVal (nth 2 nodeData)
                     rpVal  (nth 3 nodeData))

               (setq pt1 (getpoint (strcat "\n[" (itoa count) "/" (itoa total)
                                           "] Click insertion point (Top-Left) for Node " nodeID
                                           " (or Enter to skip): ")))
               (if pt1
                 (progn
                   (setq pt2 (getpoint pt1 (strcat "\nClick 2nd point to set rotation <"
                                                   (rtos (* 180.0 (/ lastAng pi)) 2 0) " deg>: ")))
                   (if pt2 (setq lastAng (angle pt1 pt2)))

                   (setq wAng (angle '(0.0 0.0 0.0) (trans (list (cos lastAng) (sin lastAng) 0.0) 1 0 T)))

                   (setq blockName (clean-name (strcat "EpaTbl_" nodeID "_H" (rtos txtHgt 2 2)
                                                       "_" hslVal "_" qVal "_" rpVal)))
                   (if (not (tblsearch "BLOCK" blockName))
                     (make-block blockName nodeID hslVal qVal rpVal txtHgt))

                   (entmake (list '(0 . "INSERT")
                                  (cons 8 tblLayer)
                                  (cons 2 blockName)
                                  (list 10 (car (trans pt1 1 0)) (cadr (trans pt1 1 0)) (caddr (trans pt1 1 0)))
                                  '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                                  (cons 50 wAng)))
                   (setq placed (1+ placed))))
               (setq count (1+ count)))
             (princ (strcat "\n>> Placed " (itoa placed) " node tables successfully."))
           )
         )
       )
     )
    )
  )

  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)