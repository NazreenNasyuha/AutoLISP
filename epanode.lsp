;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : epanode.lsp
;;; COMMAND     : EPANODE
;;; DESCRIPTION : EPANET Hydraulic Model Junction/Node Results Table Generator
;;;               (Parses .rpt file & Inserts Head, Demand, Pressure Tables via Native Entmake)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In municipal water supply submissions, engineers must submit water reticulation
;;; key plans showing junction pressures and hydraulic gradelines under peak and
;;; fire flow conditions.
;;;
;;; EPANODE automates the extraction and placement of these tables:
;;;   1. Prompts for an EPANET simulation report file (*.rpt / *.txt).
;;;   2. Parses the "Node Results:" section, extracting Node ID, Demand (lps),
;;;      Hydraulic Head / HSL (m), and Residual Pressure / RP (m).
;;;   3. Prompts for a uniform text height (default 1000.0 mm).
;;;   4. Iteratively prompts the user to place a table next to each network node.
;;;   5. Uses pure entmake for both block definitions and block insertions:
;;;      - ZERO command line calls during insertion.
;;;      - ZERO OSMODE disruption or snap jumping.
;;;   6. STALE BLOCK PREVENTATIVE CACHING: Block names are hashed using both the
;;;      Node ID and the exact hydraulic values. Re-running EPANODE with updated
;;;      simulation runs immediately reflects the new pressures without caching
;;;      stale definitions.
;;;   7. Full UCS/WCS translation safety for angular orientation.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type EPANODE in the command line.
;;;  2. Select your EPANET .rpt simulation report.
;;;  3. Specify text height (press Enter to accept default 1000).
;;;  4. For each node in the network:
;;;     - Click insertion point (Top-Left corner) near the node symbol (or Enter to skip).
;;;     - Click a 2nd point to orient the table (or Enter to reuse the previous angle).
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - defTxt : Default text height for table cells (mm).
;;; ==========================================================================

(defun c:epanode ( / *error* defTxt filename fh line data inNodes nodeList nodeData
                      nodeID qVal hslVal rpVal txtHgt lastAng count total
                      pt1 pt2 wAng blockName rowH col1W totW totH
                      parse-line clean-name draw-text make-block )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq defTxt 1000.0)   ;; Default text height for all tables (mm)
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & CLEANUP
  ;; =========================================================================
  (defun *error* (msg)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EPANODE] Error: " msg)))
    (if fh (progn (close fh) (setq fh nil)))   ;; Ensure file handle is released
    (princ "\nBatch placement ended.")
    (princ))

  ;; =========================================================================
  ;; 3. PARSING & STRING UTILITIES
  ;; =========================================================================
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

  ;; Keep only alphanumeric characters safe for block definitions
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

  ;; Draw cell text via native entmake
  (defun draw-text (str x y)
    (entmake (list '(0 . "TEXT") '(8 . "0")
                   (list 10 x y 0.0) (cons 40 txtHgt) (cons 1 str)
                   '(72 . 0) (list 11 x y 0.0) '(73 . 2))))

  ;; Build the table block definition (origin = top-left corner)
  (defun make-block (bName id hsl q rp)
    (if (entmake (list '(0 . "BLOCK") (cons 2 bName) '(70 . 0) '(10 0.0 0.0 0.0)))
      (progn
        ;; Outer border
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbPolyline")
                       '(90 . 4) '(70 . 1)
                       (list 10 0.0 0.0)
                       (list 10 totW 0.0)
                       (list 10 totW (- totH))
                       (list 10 0.0 (- totH))))
        ;; Horizontal lines
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 0.0 (- rowH) 0.0)      (list 11 totW (- rowH) 0.0)))
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 0.0 (* rowH -2.0) 0.0) (list 11 totW (* rowH -2.0) 0.0)))
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 0.0 (* rowH -3.0) 0.0) (list 11 totW (* rowH -3.0) 0.0)))
        ;; Vertical column separator
        (entmake (list '(0 . "LINE") '(8 . "0") (list 10 col1W (- rowH) 0.0)    (list 11 col1W (- totH) 0.0)))

        ;; Labels
        (draw-text id    (* txtHgt 0.8) (/ rowH -2.0))
        (draw-text "HSL" (* txtHgt 0.8) (* rowH -1.5))
        (draw-text "Q"   (* txtHgt 0.8) (* rowH -2.5))
        (draw-text "RP"  (* txtHgt 0.8) (* rowH -3.5))

        ;; Values
        (draw-text (strcat hsl "m")  (+ col1W (* txtHgt 0.8)) (* rowH -1.5))
        (draw-text (strcat q "lps")  (+ col1W (* txtHgt 0.8)) (* rowH -2.5))
        (draw-text (strcat rp "m")   (+ col1W (* txtHgt 0.8)) (* rowH -3.5))
        (entmake '((0 . "ENDBLK"))))))

  ;; =========================================================================
  ;; 4. INGEST REPORT FILE
  ;; =========================================================================
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
          ;; Real data rows: ID + three numeric columns (Demand, Head, Pressure).
          ;; Only the first time-step per node is retained.
          (if (and (>= (length data) 4)
                   (distof (nth 1 data)) (distof (nth 2 data)) (distof (nth 3 data))
                   (not (assoc (nth 0 data) nodeList)))
            (setq nodeList (cons (list (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data)) nodeList))))))
     (close fh)
     (setq fh nil nodeList (reverse nodeList))

     ;; =====================================================================
     ;; 5. BATCH TABLE PLACEMENT LOOP
     ;; =====================================================================
     (if (null nodeList)
       (princ "\nNo nodes found in the Node Results section.")
       (progn
         (setq txtHgt (getdist (strcat "\nSpecify Text Height for ALL tables <" (rtos defTxt 2 2) ">: ")))
         (if (not txtHgt) (setq txtHgt defTxt))

         (setq rowH    (* txtHgt 2.0)
               col1W   (* txtHgt 4.5)
               totW    (+ col1W (* txtHgt 9.5))
               totH    (* rowH 4.0)
               lastAng 0.0
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
               ;; Rotation prompt: pick a 2nd point, or press Enter to keep previous angle
               (setq pt2 (getpoint pt1 (strcat "\nClick 2nd point to set rotation <"
                                               (rtos (* 180.0 (/ lastAng pi)) 2 0) " deg>: ")))
               (if pt2 (setq lastAng (angle pt1 pt2)))

               ;; Translate UCS angle -> WCS angle
               (setq wAng (angle '(0.0 0.0 0.0) (trans (list (cos lastAng) (sin lastAng) 0.0) 1 0 T)))

               ;; Unique block name includes hydraulic values to prevent caching stale data
               (setq blockName (clean-name (strcat "EpaTbl_" nodeID "_H" (rtos txtHgt 2 2)
                                                   "_" hslVal "_" qVal "_" rpVal)))
               (if (not (tblsearch "BLOCK" blockName))
                 (make-block blockName nodeID hslVal qVal rpVal))

               ;; Pure native entmake INSERT
               (entmake (list '(0 . "INSERT")
                              (cons 2 blockName)
                              (list 10 (car (trans pt1 1 0)) (cadr (trans pt1 1 0)) (caddr (trans pt1 1 0)))
                              '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                              (cons 50 wAng)))))
           (setq count (1+ count)))
         (princ "\n>> All node tables processed successfully.")))))
  (princ)
)
