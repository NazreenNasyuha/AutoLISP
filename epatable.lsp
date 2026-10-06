;;; ==========================================================================
;;; EPATABLE - Read an EPANET .rpt and place one result table per node
;;; Universal build: entmake blocks + INSERT (no commands, no OSMODE changes).
;;; Tables are keyed by node ID AND values, so re-running with new results
;;; never re-uses a stale block.
;;; ==========================================================================
(defun c:epatable ( / *error* defTxt filename fh line data inNodes nodeList nodeData
                      nodeID qVal hslVal rpVal txtHgt lastAng count total
                      pt1 pt2 wAng blockName rowH col1W totW totH
                      parse-line clean-name draw-text make-block)

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq defTxt 1000.0)   ;; default text height for all tables
  ;; =========================================================================

  (defun *error* (msg)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg)))
    (if fh (progn (close fh) (setq fh nil)))   ;; never leave the report file locked
    (princ "\nBatch placement ended.")
    (princ))

  ;; Split a line on spaces / tabs
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

  ;; Keep only characters that are legal in block names
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

  (defun draw-text (str x y)
    (entmake (list '(0 . "TEXT") '(8 . "0") (list 10 x y 0.0) (cons 40 txtHgt) (cons 1 str)
                   '(72 . 0) (list 11 x y 0.0) '(73 . 2))))

  ;; Build the table block (origin = top-left corner)
  (defun make-block (bName id hsl q rp)
    (if (entmake (list '(0 . "BLOCK") (cons 2 bName) '(70 . 0) '(10 0.0 0.0 0.0)))
      (progn
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbPolyline")
                       '(90 . 4) '(70 . 1)
                       (list 10 0.0 0.0) (list 10 totW 0.0) (list 10 totW (- totH)) (list 10 0.0 (- totH))))
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 0.0 (- rowH) 0.0)    (list 11 totW (- rowH) 0.0)))
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 0.0 (* rowH -2.0) 0.0) (list 11 totW (* rowH -2.0) 0.0)))
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 0.0 (* rowH -3.0) 0.0) (list 11 totW (* rowH -3.0) 0.0)))
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 col1W (- rowH) 0.0)  (list 11 col1W (- totH) 0.0)))
        (draw-text id    (* txtHgt 0.8) (/ rowH -2.0))
        (draw-text "HSL" (* txtHgt 0.8) (* rowH -1.5))
        (draw-text "Q"   (* txtHgt 0.8) (* rowH -2.5))
        (draw-text "RP"  (* txtHgt 0.8) (* rowH -3.5))
        (draw-text (strcat hsl "m")  (+ col1W (* txtHgt 0.8)) (* rowH -1.5))
        (draw-text (strcat q "lps")  (+ col1W (* txtHgt 0.8)) (* rowH -2.5))
        (draw-text (strcat rp "m")   (+ col1W (* txtHgt 0.8)) (* rowH -3.5))
        (entmake '((0 . "ENDBLK"))))))

  ;; ---- read the report ----------------------------------------------------
  (setq filename (getfiled "Select EPANET Report (.rpt)" "" "rpt;txt" 0))
  (cond
    ((null filename) (princ "\nNo file selected."))
    ((null (setq fh (open filename "r"))) (princ "\nCannot open the selected file."))
    (T
     (setq inNodes nil nodeList nil)
     (while (setq line (read-line fh))
       (cond
         ((wcmatch line "*Node Results*") (setq inNodes T))
         ((wcmatch line "*Link Results*") (setq inNodes nil))
         (inNodes
          (setq data (parse-line line))
          ;; real data rows: ID + three numeric columns (Demand, Head, Pressure).
          ;; Only the first time-step per node is kept.
          (if (and (>= (length data) 4)
                   (distof (nth 1 data)) (distof (nth 2 data)) (distof (nth 3 data))
                   (not (assoc (nth 0 data) nodeList)))
            (setq nodeList (cons (list (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data)) nodeList))))))
     (close fh)
     (setq fh nil nodeList (reverse nodeList))

     (if (null nodeList)
       (princ "\nNo nodes found in the Node Results section.")
       (progn
         (setq txtHgt (getdist (strcat "\nSpecify Text Height for ALL tables <" (rtos defTxt 2 2) ">: ")))
         (if (not txtHgt) (setq txtHgt defTxt))

         (setq rowH  (* txtHgt 2.0)  col1W (* txtHgt 4.5)
               totW  (+ col1W (* txtHgt 9.5))  totH (* rowH 4.0)
               lastAng 0.0 count 1 total (length nodeList))

         (foreach nodeData nodeList
           (setq nodeID (nth 0 nodeData) qVal (nth 1 nodeData)
                 hslVal (nth 2 nodeData) rpVal (nth 3 nodeData))

           (setq pt1 (getpoint (strcat "\n[" (itoa count) "/" (itoa total)
                                       "] Click insertion point (Top-Left) for Node " nodeID
                                       " (or Enter to skip): ")))
           (if pt1
             (progn
               ;; rotation: click a 2nd point, or Enter to keep the last angle (UCS radians)
               (setq pt2 (getpoint pt1 (strcat "\nClick 2nd point to set rotation <"
                                               (rtos (* 180.0 (/ lastAng pi)) 2 0) " deg>: ")))
               (if pt2 (setq lastAng (angle pt1 pt2)))
               ;; UCS angle -> WCS angle
               (setq wAng (angle '(0.0 0.0 0.0) (trans (list (cos lastAng) (sin lastAng) 0.0) 1 0 T)))

               (setq blockName (clean-name (strcat "EpaTbl_" nodeID "_H" (rtos txtHgt 2 2)
                                                   "_" hslVal "_" qVal "_" rpVal)))
               (if (not (tblsearch "BLOCK" blockName))
                 (make-block blockName nodeID hslVal qVal rpVal))

               (entmake (list '(0 . "INSERT") (cons 2 blockName) (list 10 (car (trans pt1 1 0)) (cadr (trans pt1 1 0)) (caddr (trans pt1 1 0)))
                              '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 wAng)))))
           (setq count (1+ count)))
         (princ "\nAll node tables processed successfully.")))))
  (princ)
)
