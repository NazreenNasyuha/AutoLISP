;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : epalink.lsp
;;; COMMAND     : EPALINK
;;; DESCRIPTION : EPANET Hydraulic Model Link/Pipe Results Table Generator
;;;               (Parses .rpt file & Auto-Updates or Inserts Hexagon + MText)
;;; ==========================================================================

(defun c:epalink ( / *error* oldCmd oldDimZin oldOsnap
                     defTxt txtLayer txtColor hexColor txtStyle
                     filename file line data
                     inLinkNodeSection inLinkResultSection lenDict
                     linkList linkData linkID cleanID lenVal flowVal velVal hlVal
                     txtHgt lastAng count total pt1 pt2 wAng blockName
                     ssExist existingMap i ent elist bName info exHgt insPt rotAng
                     mode updated placed matched
                     parse-epanet-line clean-name ensure-layer
                     make-link-block get-block-link-info )

  ;; =========================================================================
  ;; 1. USER SETTINGS
  ;; =========================================================================
  (setq defTxt   1200.0)           ;; Text height
  (setq txtLayer "Water Text")     ;; Layer name
  (setq txtColor 1)                ;; 1 = Red for the pipe info
  (setq hexColor 6)                ;; 6 = Magenta for the Hexagon and ID number
  (setq txtStyle "HAZA")           ;; Text style name
  ;; =========================================================================

  (setq oldOsnap  (getvar "OSMODE"))
  (setq oldCmd    (getvar "CMDECHO"))
  (setq oldDimZin (getvar "DIMZIN"))

  ;; =========================================================================
  ;; 2. ERROR HANDLING & CLEANUP
  ;; =========================================================================
  (defun *error* (msg)
    (if oldOsnap  (setvar "OSMODE" oldOsnap))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if oldDimZin (setvar "DIMZIN" oldDimZin))
    (if file (progn (close file) (setq file nil)))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[EPALINK] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. HELPER FUNCTIONS
  ;; =========================================================================
  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")))))

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

  ;; Build the Hexagon + MText Block flat at (0,0)
  (defun make-link-block (bName id lenV flowV velV hlV hgt / hexRad pts k p entData outStr textOffsetX)
    (if (entmake (list '(0 . "BLOCK") (cons 2 bName) '(70 . 0) '(10 0.0 0.0 0.0)))
      (progn
        ;; 1. MAGENTA HEXAGON (Centered at 0,0)
        (setq hexRad (* hgt 1.35) pts nil k 0)
        (while (< k 6)
          (setq pts (append pts (list (polar '(0.0 0.0 0.0) (* k (/ pi 3.0)) hexRad))))
          (setq k (1+ k))
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
                       (cons 40 hgt)
                       '(41 . 0.0)
                       '(71 . 5) ;; Middle Center Attachment
                       '(72 . 1)
                       (cons 1 id)
                       (cons 7 txtStyle)
                       '(73 . 1) '(44 . 1.0)))

        ;; 3. RED PIPE RESULTS TEXT (MTEXT: Middle Left Offset)
        (setq outStr (strcat "L=" lenV "m, Q=" flowV "lps, Vs=" velV "m/s, HL=" hlV "m/km"))
        (setq textOffsetX (* hgt 1.8))

        (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity")
                       (cons 8 txtLayer) (cons 62 txtColor)
                       '(100 . "AcDbMText")
                       (list 10 textOffsetX 0.0 0.0)
                       (cons 40 hgt)
                       '(41 . 0.0)
                       '(71 . 4) ;; Middle Left Attachment (Grows outward)
                       '(72 . 1)
                       (cons 1 outStr)
                       (cons 7 txtStyle)
                       '(73 . 1) '(44 . 1.0)))

        (entmake '((0 . "ENDBLK")))
      )
    )
  )

  ;; Read Link ID and Text Height from inside an existing EpaHex_* block definition
  (defun get-block-link-info (bName / bRec sub ed typ foundId foundHgt)
    (if (wcmatch (strcase bName) "EPAHEX_*")
      (progn
        (setq bRec (tblsearch "BLOCK" bName)
              sub  (if bRec (cdr (assoc -2 bRec)) nil))
        ;; The first MTEXT/TEXT inside EpaHex_* is always the Link ID inside the hexagon
        (while (and sub (null foundId))
          (setq ed  (entget sub)
                typ (cdr (assoc 0 ed)))
          (if (or (= typ "MTEXT") (= typ "TEXT"))
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

  ;; =========================================================================
  ;; 4. SETUP LAYER, STYLE & INGEST REPORT FILE (Double-Pass)
  ;; =========================================================================
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)
  (ensure-layer txtLayer txtColor)

  (if (not (tblsearch "STYLE" txtStyle))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                   (cons 2 txtStyle) '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0)
                   '(71 . 0) '(3 . "romans.shx") '(4 . ""))))

  (setq filename (getfiled "Select EPANET Report (.rpt)" "" "rpt;txt" 0))
  (if (not filename) (progn (princ "\nNo file selected.") (exit)))

  (setq file (open filename "r")
        inLinkNodeSection nil
        inLinkResultSection nil
        lenDict nil
        linkList nil)

  (if (not file) (progn (princ "\nCannot open the selected file.") (exit)))

  (while (setq line (read-line file))
    (if (wcmatch line "*Link - Node Table*") (setq inLinkNodeSection T inLinkResultSection nil))
    (if (wcmatch line "*Node Results*")      (setq inLinkNodeSection nil inLinkResultSection nil))
    (if (wcmatch line "*Link Results*")      (setq inLinkResultSection T inLinkNodeSection nil))

    ;; Pass 1: Extract Pipe Length from Link - Node Table
    (if inLinkNodeSection
      (progn
        (setq data (parse-epanet-line line))
        (if (and (>= (length data) 4)
                 (not (wcmatch (nth 0 data) "*---*"))
                 (/= (nth 0 data) "Link")
                 (/= (nth 0 data) "ID")
                 (distof (nth 3 data))
                 (not (assoc (nth 0 data) lenDict)))
          (setq lenDict (cons (cons (nth 0 data) (nth 3 data)) lenDict))
        )
      )
    )

    ;; Pass 2: Extract Flow, Velocity, and Headloss from Link Results
    (if inLinkResultSection
      (progn
        (setq data (parse-epanet-line line))
        (if (and (>= (length data) 4)
                 (not (wcmatch (nth 0 data) "*---*"))
                 (/= (nth 0 data) "Link")
                 (/= (nth 0 data) "ID")
                 (/= (nth 0 data) "LPS")
                 (distof (nth 1 data))
                 (distof (nth 2 data))
                 (distof (nth 3 data))
                 (not (assoc (nth 0 data) linkList)))
          (setq linkList (cons (list (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data)) linkList))
        )
      )
    )
  )
  (close file)
  (setq file nil)
  (setq linkList (reverse linkList))

  ;; =========================================================================
  ;; 5. AUTO-UPDATE EXISTING LINK BLOCKS OR PLACE NEW ONES
  ;; =========================================================================
  (if linkList
    (progn
      ;; Scan drawing for any existing EpaHex_* blocks
      (setq ssExist (ssget "_X" '((0 . "INSERT") (2 . "EpaHex_*,EPAHEX_*")))
            existingMap nil)
      (if ssExist
        (progn
          (setq i 0)
          (while (< i (sslength ssExist))
            (setq ent   (ssname ssExist i)
                  elist (entget ent)
                  bName (cdr (assoc 2 elist))
                  info  (get-block-link-info bName))
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
                                       " existing Link labels in drawing. [Update existing/New placement] <Update>: ")))
          (if (null mode) (setq mode "Update"))
        )
      )

      ;; Ask for text height if placing new blocks (or if some links aren't in the drawing yet)
      (if (= mode "New")
        (progn
          (setq txtHgt (getdist (strcat "\nSpecify Text Height <" (rtos defTxt 2 2) ">: ")))
          (if (not txtHgt) (setq txtHgt defTxt))
        )
        (setq txtHgt defTxt)
      )

      (setq lastAng 0.0 count 1 total (length linkList) updated 0 placed 0)

      (foreach linkData linkList
        (setq linkID  (nth 0 linkData)
              cleanID (clean-name linkID)
              flowVal (nth 1 linkData)
              velVal  (nth 2 linkData)
              hlVal   (nth 3 linkData)
              lenVal  (cdr (assoc linkID lenDict)))
        (if (not lenVal) (setq lenVal "??"))

        (setq matched nil)

        ;; If in Update mode, replace all matching existing link blocks in-place
        (if (= mode "Update")
          (foreach item existingMap
            (if (= (car item) (strcase linkID))
              (progn
                (setq ent       (nth 1 item)
                      exHgt     (nth 2 item)
                      elist     (entget ent)
                      insPt     (cdr (assoc 10 elist))
                      rotAng    (if (cdr (assoc 50 elist)) (cdr (assoc 50 elist)) 0.0)
                      blockName (clean-name (strcat "EpaHex_" cleanID "_H" (rtos exHgt 2 2)
                                                    "_" flowVal "_" velVal "_" hlVal "_" lenVal)))
                (if (not (tblsearch "BLOCK" blockName))
                  (make-link-block blockName linkID lenVal flowVal velVal hlVal exHgt)
                )
                ;; Insert updated block at the exact same position and rotation
                (entmake (list '(0 . "INSERT")
                               (cons 8 txtLayer)
                               (cons 2 blockName)
                               (cons 10 insPt)
                               (if (assoc 41 elist) (assoc 41 elist) '(41 . 1.0))
                               (if (assoc 42 elist) (assoc 42 elist) '(42 . 1.0))
                               (if (assoc 43 elist) (assoc 43 elist) '(43 . 1.0))
                               (cons 50 rotAng)))
                (entdel ent)
                (setq updated (1+ updated)
                      matched T)
              )
            )
          )
        )

        ;; If placing New (or if this Link ID was not found in the drawing yet), prompt user to place it
        (if (not matched)
          (progn
            (setvar "OSMODE" oldOsnap)
            (setq pt1 (getpoint (strcat "\n[" (itoa count) "/" (itoa total)
                                        "] Click insertion point for Link <" linkID "> (or Enter to skip): ")))
            (if pt1
              (progn
                (setq pt2 (getpoint pt1 (strcat "\nClick 2nd point to set rotation <"
                                                (rtos (* 180.0 (/ lastAng pi)) 2 0) " deg>: ")))
                (if pt2 (setq lastAng (angle pt1 pt2)))

                ;; WCS Translation Safety (from epanode)
                (setq wAng (angle '(0.0 0.0 0.0) (trans (list (cos lastAng) (sin lastAng) 0.0) 1 0 T)))

                (setq blockName (clean-name (strcat "EpaHex_" cleanID "_H" (rtos txtHgt 2 2)
                                                    "_" flowVal "_" velVal "_" hlVal "_" lenVal)))
                (if (not (tblsearch "BLOCK" blockName))
                  (make-link-block blockName linkID lenVal flowVal velVal hlVal txtHgt)
                )

                ;; Pure Native Entmake Insertion
                (setvar "OSMODE" 0)
                (entmake (list '(0 . "INSERT")
                               (cons 8 txtLayer)
                               (cons 2 blockName)
                               (list 10 (car (trans pt1 1 0)) (cadr (trans pt1 1 0)) (caddr (trans pt1 1 0)))
                               '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                               (cons 50 wAng)))
                (setvar "OSMODE" oldOsnap)
                (setq placed (1+ placed))
              )
            )
          )
        )
        (setq count (1+ count))
      )

      (princ (strcat "\n>> EPALINK Complete! Auto-updated " (itoa updated)
                     " existing link(s) in-place, and placed " (itoa placed) " new link(s)."))
    )
    (princ "\nNo link data found.")
  )

  (if oldOsnap  (setvar "OSMODE" oldOsnap))
  (if oldDimZin (setvar "DIMZIN" oldDimZin))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)