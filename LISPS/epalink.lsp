;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : epalink.lsp
;;; COMMAND     : EPALINK
;;; DESCRIPTION : EPANET Hydraulic Pipe / Link Simulation Results Annotator
;;;               (Parametric Hexagon Link ID & Hydraulic Summary Inserter)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In water reticulation and municipal pressure network design, hydraulic
;;; engineers simulate networks in EPANET and must transfer the results
;;; (flow rate Q, velocity V, headloss HL, and length L) back onto CAD keyplans.
;;;
;;; EPALINK automates this bridge:
;;;   1. Reads native EPANET simulation report files (.rpt or .txt).
;;;   2. Uses a double-pass parser to cross-reference the Link-Node Table (pipe
;;;      lengths) with the Link Results Table (flow, velocity, unit headloss).
;;;   3. For each pipe, interactively prompts the user to place the annotation
;;;      and define the pipe alignment angle.
;;;   4. Automatically generates a dynamic block containing:
;;;        - A Magenta hexagon circumscribing the Link ID number.
;;;        - Red text showing hydraulic metrics:
;;;          `L={length}m, Q={flow}lps, Vs={velocity}m/s, HL={headloss}m/km`.
;;;   5. Places all geometry on layer `#JRK - RD Drain Text` with correct UCS
;;;      rotation and OSNAP management.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `EPALINK` in the CAD command line.
;;;   2. Select the EPANET `.rpt` or `.txt` output report file.
;;;   3. Specify the Text Height (default: 1000 drawing units).
;;;   4. For each pipe link:
;;;        - Click insertion point on pipe.
;;;        - Click second point to align with pipe direction (or press Enter).
;;;   5. Block with Hexagon ID and hydraulic data is inserted automatically.
;;; ==========================================================================

(defun c:epalink ( / *error* defTxt txtLayer txtColor hexColor oldOsnap filename file line data 
                      inLinkNodeSection inLinkResultSection lenDict
                      linkList linkData linkID lenVal flowVal velVal hlVal 
                      txtHgt lastAng count total pt1 ang angDeg
                      blockName hexRad pts i p entData outStr textOffsetX)
                      
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq defTxt 1000.0) 
  (setq txtLayer "#JRK - RD Drain Text") 
  (setq txtColor 1) ;; 1 = Red for the pipe info
  (setq hexColor 6) ;; 6 = Magenta for the Hexagon and ID number
  ;; =========================================================================

  (setq oldOsnap (getvar "OSMODE"))
  (defun *error* (msg) (if oldOsnap (setvar "OSMODE" oldOsnap)) (princ))

  ;; Force layer creation if it doesn't exist
  (setvar "CMDECHO" 0)
  (if (not (tblsearch "LAYER" txtLayer)) 
    (command "_.LAYER" "_M" txtLayer "_C" txtColor "" "")
    (command "_.LAYER" "_C" txtColor txtLayer "")
  )

  (defun parse-epanet-line (str / lst word i char)
    (setq lst nil word "" i 1)
    (while (<= i (strlen str))
      (setq char (substr str i 1))
      (if (or (= char " ") (= char "\t")) 
        (if (/= word "") (progn (setq lst (cons word lst)) (setq word ""))) 
        (setq word (strcat word char))
      )
      (setq i (1+ i))
    )
    (if (/= word "") (setq lst (cons word lst)))
    (reverse lst)
  )

  (setq filename (getfiled "Select EPANET Report (.rpt)" "" "rpt;txt" 0))
  (if (not filename) (progn (princ "\nNo file selected.") (exit)))

  (setq file (open filename "r") 
        inLinkNodeSection nil 
        inLinkResultSection nil 
        lenDict nil 
        linkList nil)
        
  ;; =========================================================================
  ;; SMART FILE PARSER (Double-Pass to grab Length and Results)
  ;; =========================================================================
  (while (setq line (read-line file))
    
    (if (wcmatch line "*Link - Node Table:*") (progn (setq inLinkNodeSection T inLinkResultSection nil)))
    (if (wcmatch line "*Node Results:*") (progn (setq inLinkNodeSection nil inLinkResultSection nil)))
    (if (wcmatch line "*Link Results:*") (progn (setq inLinkResultSection T inLinkNodeSection nil)))
    
    (if inLinkNodeSection
      (progn
        (setq data (parse-epanet-line line))
        (if (and (>= (length data) 4) (not (wcmatch (nth 0 data) "*---*")) (/= (nth 0 data) "Link") (/= (nth 0 data) "ID"))
          (setq lenDict (cons (cons (nth 0 data) (nth 3 data)) lenDict)) 
        )
      )
    )
    
    (if inLinkResultSection
      (progn
        (setq data (parse-epanet-line line))
        (if (and (>= (length data) 4) (not (wcmatch (nth 0 data) "*---*")) (/= (nth 0 data) "Link") (/= (nth 0 data) "ID") (/= (nth 0 data) "LPS"))
          (setq linkList (cons (list (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data)) linkList))
        )
      )
    )
  )
  (close file)
  (setq linkList (reverse linkList))

  ;; =========================================================================
  ;; GENERATE GEOMETRY IN AUTOCAD
  ;; =========================================================================
  (if linkList
    (progn
      (setq txtHgt (getdist (strcat "\nSpecify Text Height <" (rtos defTxt 2 2) ">: ")))
      (if (not txtHgt) (setq txtHgt defTxt))

      (setq lastAng 0.0 count 1 total (length linkList))

      (foreach linkData linkList
        (setq linkID (nth 0 linkData) flowVal (nth 1 linkData) velVal (nth 2 linkData) hlVal (nth 3 linkData))
        
        (setq lenVal (cdr (assoc linkID lenDict)))
        (if (not lenVal) (setq lenVal "??")) 
        
        (setvar "OSMODE" oldOsnap)
        (setq pt1 (getpoint (strcat "\n[" (itoa count) "/" (itoa total) "] Click insertion point for Link <" linkID ">: ")))
        
        (if pt1
          (progn
            (setq ang (getangle pt1 (strcat "\nClick rotation <" (rtos (* 180.0 (/ lastAng pi)) 2 0) "°>: ")))
            (if ang (setq lastAng ang))
            
            ;; Convert radians to degrees for the _.INSERT command
            (setq angDeg (* 180.0 (/ lastAng pi))) 
            
            ;; Create a unique block name based on the data so it updates if the .rpt changes
            (setq blockName (strcat "EpaHex_" linkID "_" (vl-string-translate "." "" flowVal) "_" (vl-string-translate "." "" lenVal)))

            ;; Build the block perfectly flat at (0,0) to avoid UCS coordinate math errors
            (if (not (tblsearch "BLOCK" blockName))
              (progn
                (entmake (list '(0 . "BLOCK") (cons 2 blockName) '(70 . 0) '(10 0.0 0.0 0.0)))
                
                ;; 1. DRAW THE MAGENTA HEXAGON (Centered at 0,0)
                (setq hexRad (* txtHgt 1.35) pts nil i 0)
                (while (< i 6)
                  (setq pts (append pts (list (polar '(0.0 0.0 0.0) (* i (/ pi 3.0)) hexRad))))
                  (setq i (1+ i))
                )
                (setq entData (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                    (cons 8 txtLayer) (cons 62 hexColor) '(90 . 6) '(70 . 1)))
                (foreach p pts (setq entData (append entData (list (list 10 (car p) (cadr p))))))
                (entmake entData)

                ;; 2. DRAW THE MAGENTA ID NUMBER (Centered at 0,0)
                (entmake (list '(0 . "TEXT") (cons 8 txtLayer) (cons 62 hexColor) (cons 1 linkID) 
                               '(10 0.0 0.0 0.0) '(11 0.0 0.0 0.0) 
                               (cons 40 txtHgt) '(72 . 1) '(73 . 2)))

                ;; 3. DRAW THE RED PIPE RESULTS TEXT (Offset to the right)
                (setq outStr (strcat "L=" lenVal "m, Q=" flowVal "lps, Vs=" velVal "m/s, HL=" hlVal "m/km"))
                (setq textOffsetX (* txtHgt 1.8))
                
                (entmake (list '(0 . "TEXT") (cons 8 txtLayer) (cons 62 txtColor) (cons 1 outStr) 
                               (list 10 textOffsetX 0.0 0.0) (list 11 textOffsetX 0.0 0.0) 
                               (cons 40 txtHgt) '(72 . 0) '(73 . 2)))
                               
                (entmake '((0 . "ENDBLK")))
              )
            )

            ;; Let native AutoCAD handle the complex UCS translation and rotation
            (setvar "OSMODE" 0)
            (command "_.INSERT" blockName "_NON" pt1 1 1 angDeg) 
            (setvar "OSMODE" oldOsnap)
          )
        )
        (setq count (1+ count))
      )
    )
    (princ "\nNo link data found.")
  )
  (if oldOsnap (setvar "OSMODE" oldOsnap))
  (princ)
)