;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : epalink.lsp
;;; COMMAND     : EPALINK
;;; DESCRIPTION : EPANET Hydraulic Model Link/Pipe Results Table Generator
;;;               (Parses .rpt file & Inserts Hexagon + Flow, Vel, HL MText)
;;; ==========================================================================

(defun c:epalink ( / *error* defTxt txtLayer txtColor hexColor txtStyle oldOsnap filename file line data 
                      inLinkNodeSection inLinkResultSection lenDict
                      linkList linkData linkID lenVal flowVal velVal hlVal 
                      txtHgt lastAng count total pt1 pt2 wAng blockName hexRad pts i p entData outStr textOffsetX clean-name)
                      
  ;; =========================================================================
  ;; USER SETTINGS (Matched to Screenshot Properties)
  ;; =========================================================================
  (setq defTxt 1200.0)           ;; Text height
  (setq txtLayer "Water Text")   ;; Layer name
  (setq txtColor 1)              ;; 1 = Red for the pipe info
  (setq hexColor 6)              ;; 6 = Magenta for the Hexagon and ID number
  (setq txtStyle "HAZA")         ;; Text style name
  ;; =========================================================================

  (setq oldOsnap (getvar "OSMODE"))
  (defun *error* (msg) 
    (if oldOsnap (setvar "OSMODE" oldOsnap)) 
    (if file (progn (close file) (setq file nil)))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EPALINK] Error: " msg)))
    (princ)
  )

  ;; Force layer creation if it doesn't exist
  (setvar "CMDECHO" 0)
  (if (not (tblsearch "LAYER" txtLayer)) 
    (command "_.LAYER" "_M" txtLayer "_C" txtColor "" "")
    (command "_.LAYER" "_C" txtColor txtLayer "")
  )

  ;; Force text style creation if it doesn't exist
  (if (not (tblsearch "STYLE" txtStyle))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                   (cons 2 txtStyle) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0)
                   '(71 . 0) '(3 . "romans.shx") '(4 . ""))))

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

  ;; Safely strip invalid characters for block definitions
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

  (setq filename (getfiled "Select EPANET Report (.rpt)" "" "rpt;txt" 0))
  (if (not filename) (progn (princ "\nNo file selected.") (exit)))

  (setq file (open filename "r") 
        inLinkNodeSection nil 
        inLinkResultSection nil 
        lenDict nil 
        linkList nil)
        
  ;; =========================================================================
  ;; SMART FILE PARSER (Double-Pass)
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
  (setq file nil)
  (setq linkList (reverse linkList))

  ;; =========================================================================
  ;; GENERATE GEOMETRY (Using epanode's native WCS Orientation)
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
            ;; Rotation prompt like epanode
            (setq pt2 (getpoint pt1 (strcat "\nClick 2nd point to set rotation <" (rtos (* 180.0 (/ lastAng pi)) 2 0) " deg>: ")))
            (if pt2 (setq lastAng (angle pt1 pt2)))
            
            ;; -----------------------------------------------------------------
            ;; WCS TRANSLATION SAFETY (From epanode)
            ;; -----------------------------------------------------------------
            (setq wAng (angle '(0.0 0.0 0.0) (trans (list (cos lastAng) (sin lastAng) 0.0) 1 0 T)))
            
            (setq blockName (clean-name (strcat "EpaHex_" linkID "_H" (rtos txtHgt 2 2) "_" flowVal "_" lenVal)))

            ;; Build the block perfectly flat at (0,0)
            (if (not (tblsearch "BLOCK" blockName))
              (progn
                (entmake (list '(0 . "BLOCK") (cons 2 blockName) '(70 . 0) '(10 0.0 0.0 0.0)))
                
                ;; 1. MAGENTA HEXAGON (Centered at 0,0)
                (setq hexRad (* txtHgt 1.35) pts nil i 0)
                (while (< i 6)
                  (setq pts (append pts (list (polar '(0.0 0.0 0.0) (* i (/ pi 3.0)) hexRad))))
                  (setq i (1+ i))
                )
                (setq entData (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                    (cons 8 txtLayer) (cons 62 hexColor) '(90 . 6) '(70 . 1)))
                (foreach p pts (setq entData (append entData (list (list 10 (car p) (cadr p))))))
                (entmake entData)

                ;; 2. MAGENTA ID NUMBER (MTEXT: Middle Center)
                (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") 
                               (cons 8 txtLayer) (cons 62 hexColor) 
                               '(100 . "AcDbMText")
                               '(10 0.0 0.0 0.0)
                               (cons 40 txtHgt) 
                               '(41 . 0.0)
                               '(71 . 5) ;; Middle Center Attachment
                               '(72 . 1)
                               (cons 1 linkID) 
                               (cons 7 txtStyle)
                               '(73 . 1) '(44 . 1.0)))

                ;; 3. RED PIPE RESULTS TEXT (MTEXT: Middle Left Offset)
                (setq outStr (strcat "L=" lenVal "m, Q=" flowVal "lps, Vs=" velVal "m/s, HL=" hlVal "m/km"))
                (setq textOffsetX (* txtHgt 1.8))
                
                (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") 
                               (cons 8 txtLayer) (cons 62 txtColor) 
                               '(100 . "AcDbMText")
                               (list 10 textOffsetX 0.0 0.0)
                               (cons 40 txtHgt) 
                               '(41 . 0.0)
                               '(71 . 4) ;; Middle Left Attachment (Grows outward)
                               '(72 . 1)
                               (cons 1 outStr) 
                               (cons 7 txtStyle)
                               '(73 . 1) '(44 . 1.0)))
                               
                (entmake '((0 . "ENDBLK")))
              )
            )

            ;; -----------------------------------------------------------------
            ;; PURE NATIVE ENTMAKE INSERTION (Bypasses active UCS bugs)
            ;; -----------------------------------------------------------------
            (setvar "OSMODE" 0)
            (entmake (list '(0 . "INSERT")
                           (cons 2 blockName)
                           (list 10 (car (trans pt1 1 0)) (cadr (trans pt1 1 0)) (caddr (trans pt1 1 0)))
                           '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                           (cons 50 wAng)))
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