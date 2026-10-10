;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : epalink.lsp
;;; COMMAND     : EPALINK
;;; DESCRIPTION : EPANET Hydraulic Model Link/Pipe Results Table Generator
;;;               (Parses .rpt file & Inserts Formatted Flow, Velocity, Headloss Tables)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In water reticulation design and hydraulic modeling, engineers use EPANET to
;;; simulate network distribution pipelines. Transferring pipe hydraulic outputs
;;; (Flow, Velocity, and Unit Headloss) onto CAD key plans has historically been
;;; a tedious copy-paste task.
;;;
;;; EPALINK automates this bridge:
;;;   1. Prompts the user to select an EPANET simulation report file (*.rpt / *.txt).
;;;   2. Parses the "Link Results:" section, extracting Link ID, Flow (lps),
;;;      Velocity (m/s), and Headloss (m/km).
;;;   3. Prompts for a uniform text height (default 1000.0 mm).
;;;   4. Iterates through every link record in the model, prompting the drafter
;;;      to place each table adjacent to its respective pipe in the drawing.
;;;   5. Uses fast, memory-safe entmake BLOCK definitions containing structured
;;;      gridlines and middle-aligned text cells.
;;;   6. Remembers rotation angles between clicks so drafters can align tables
;;;      along parallel pipe corridors with a single tap.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type EPALINK in the command line.
;;;  2. Select your EPANET .rpt file.
;;;  3. Confirm or adjust the text height (press Enter to accept default 1000).
;;;  4. For each pipe in the report:
;;;     - Click the insertion point (Top-Left corner) on your drawing.
;;;     - Click or enter a rotation angle (press Enter to repeat the previous angle).
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - defTxt : Default text height for all generated table cells (mm).
;;; ==========================================================================

(defun c:epalink ( / *error* defTxt oldOsnap filename file line data inLinkSection 
                      linkList linkData linkID flowVal velVal hlVal 
                      txtHgt lastAng count total 
                      pt1 ang angDeg blockName 
                      rowH col1W col2W totW totH parse-epanet-line draw-text )
  
  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq defTxt 1000.0) ;; Default text height for all table cells (mm)
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM SAFEGUARDS
  ;; =========================================================================
  (setq oldOsnap (getvar "OSMODE"))
  (defun *error* (msg)
    (if oldOsnap (setvar "OSMODE" oldOsnap))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EPALINK] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. PARSING HELPER ROUTINE
  ;; =========================================================================
  ;; Split whitespace-delimited columns in EPANET report line
  (defun parse-epanet-line (str / lst word i char)
    (setq lst nil word "" i 1)
    (while (<= i (strlen str))
      (setq char (substr str i 1))
      (if (or (= char " ") (= char "\t"))
        (if (/= word "") (progn (setq lst (cons word lst)) (setq word "")))
        (setq word (strcat word char)))
      (setq i (1+ i))
    )
    (if (/= word "") (setq lst (cons word lst)))
    (reverse lst)
  )

  ;; =========================================================================
  ;; 4. REPORT INGESTION
  ;; =========================================================================
  (setq filename (getfiled "Select EPANET Report (.rpt)" "" "rpt;txt" 0))
  (if (not filename) (progn (princ "\nNo file selected. Command cancelled.") (exit)))

  (setq file (open filename "r") inLinkSection nil linkList nil)
  (while (setq line (read-line file))
    (if (wcmatch line "*Link Results:*") (setq inLinkSection T))
    ;; Stop reading links if it reaches another report section (e.g. Node Results)
    (if (and inLinkSection (wcmatch line "*Node Results:*")) (setq inLinkSection nil))
    
    (if inLinkSection
      (progn
        (setq data (parse-epanet-line line))
        (if (and (>= (length data) 4)
                 (not (wcmatch (nth 0 data) "*---*"))
                 (/= (nth 0 data) "Link")
                 (/= (nth 0 data) "ID")
                 (/= (nth 0 data) "LPS"))
          (setq linkList (cons (list (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data)) linkList))
        )
      )
    )
  )
  (close file)
  (setq linkList (reverse linkList))

  ;; =========================================================================
  ;; 5. BATCH TABLE PLACEMENT
  ;; =========================================================================
  (if linkList
    (progn
      (setq txtHgt (getdist (strcat "\nSpecify Text Height <" (rtos defTxt 2 2) ">: ")))
      (if (not txtHgt) (setq txtHgt defTxt))

      (setq rowH  (* txtHgt 2.0)
            col1W (* txtHgt 4.5)
            col2W (* txtHgt 9.5)
            totW  (+ col1W col2W)
            totH  (* rowH 4.0))
      (setq lastAng 0.0 count 1 total (length linkList))

      (foreach linkData linkList
        (setq linkID  (nth 0 linkData)
              flowVal (nth 1 linkData)
              velVal  (nth 2 linkData)
              hlVal   (nth 3 linkData))

        (setvar "OSMODE" oldOsnap)
        (setq pt1 (getpoint (strcat "\n[" (itoa count) "/" (itoa total) "] Click Top-Left for Link " linkID ": ")))
        
        (if pt1
          (progn
            (setq ang (getangle pt1 (strcat "\nClick rotation <" (rtos (* 180.0 (/ lastAng pi)) 2 0) "°>: ")))
            (if ang (setq lastAng ang))
            (setq angDeg (* 180.0 (/ lastAng pi)))
            (setq blockName (strcat "EpaLink_" linkID "_Hgt" (vl-string-translate "." "-" (rtos txtHgt 2 2))))

            ;; Build Block Definition if not already defined
            (if (not (tblsearch "BLOCK" blockName))
              (progn
                (entmake (list '(0 . "BLOCK") (cons 2 blockName) '(70 . 0) '(10 0.0 0.0 0.0)))
                ;; Table Outer Border
                (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                               '(90 . 4) '(70 . 1)
                               (list 10 0.0 0.0)
                               (list 10 totW 0.0)
                               (list 10 totW (- totH))
                               (list 10 0.0 (- totH))))
                ;; Horizontal Separators
                (entmake (list '(0 . "LINE") (list 10 0.0 (- rowH) 0.0) (list 11 totW (- rowH) 0.0)))
                (entmake (list '(0 . "LINE") (list 10 0.0 (* rowH -2.0) 0.0) (list 11 totW (* rowH -2.0) 0.0)))
                (entmake (list '(0 . "LINE") (list 10 0.0 (* rowH -3.0) 0.0) (list 11 totW (* rowH -3.0) 0.0)))
                ;; Vertical Separator
                (entmake (list '(0 . "LINE") (list 10 col1W (- rowH) 0.0) (list 11 col1W (- totH) 0.0)))

                ;; Subroutine to draw text inside cell
                (defun draw-text (str pX pY)
                  (entmake (list '(0 . "TEXT") (cons 1 str)
                                 (list 10 pX pY 0.0) (list 11 pX pY 0.0)
                                 (cons 40 txtHgt) '(72 . 0) '(73 . 2))))

                ;; Labels
                (draw-text linkID (* txtHgt 0.8) (/ rowH -2.0))
                (draw-text "Flow" (* txtHgt 0.8) (* rowH -1.5))
                (draw-text "Vel"  (* txtHgt 0.8) (* rowH -2.5))
                (draw-text "HL"   (* txtHgt 0.8) (* rowH -3.5))
                
                ;; Hydraulic Values
                (draw-text (strcat flowVal "lps") (+ col1W (* txtHgt 0.8)) (* rowH -1.5))
                (draw-text (strcat velVal "m/s")  (+ col1W (* txtHgt 0.8)) (* rowH -2.5))
                (draw-text (strcat hlVal "m/km")  (+ col1W (* txtHgt 0.8)) (* rowH -3.5))
                (entmake '((0 . "ENDBLK")))
              )
            )
            ;; Insert the Block
            (setvar "OSMODE" 0)
            (command "_.INSERT" blockName "_NON" pt1 1 1 angDeg)
            (setvar "OSMODE" oldOsnap)
          )
        )
        (setq count (1+ count))
      )
      (princ "\n>> All link tables placed successfully.")
    )
    (princ "\nNo link data found in report.")
  )

  (if oldOsnap (setvar "OSMODE" oldOsnap))
  (princ)
)