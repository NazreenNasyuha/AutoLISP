(defun c:epatable ( / *error* defTxt oldOsnap filename file line data inNodeSection 
                      nodeList nodeData nodeID qVal hslVal rpVal 
                      txtHgt lastAng count total 
                      pt1 ang angDeg blockName 
                      rowH col1W col2W totW totH)
                      
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq defTxt 1000.0) 
  ;; =========================================================================

  (setq oldOsnap (getvar "OSMODE"))
  (defun *error* (msg)
    (if oldOsnap (setvar "OSMODE" oldOsnap))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ "\nBatch placement ended.")
    (princ)
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

  (setq file (open filename "r") inNodeSection nil nodeList nil)
  
  (while (setq line (read-line file))
    (if (wcmatch line "*Node Results:*") (setq inNodeSection T))
    (if (wcmatch line "*Link Results:*") (setq inNodeSection nil))
    (if inNodeSection
      (progn
        (setq data (parse-epanet-line line))
        (if (and (>= (length data) 4) (not (wcmatch (nth 0 data) "*---*"))
                 (/= (nth 0 data) "Node") (/= (nth 0 data) "ID") (/= (nth 0 data) "LPS"))
          (setq nodeList (cons (list (nth 0 data) (nth 1 data) (nth 2 data) (nth 3 data)) nodeList))
        )
      )
    )
  )
  (close file)
  (setq nodeList (reverse nodeList))

  (if (not nodeList)
    (princ "\nNo nodes found in the Node Results section.")
    (progn
      (setq txtHgt (getdist (strcat "\nSpecify Text Height for ALL tables <" (rtos defTxt 2 2) ">: ")))
      (if (not txtHgt) (setq txtHgt defTxt))

      (setq rowH (* txtHgt 2.0) col1W (* txtHgt 4.5) col2W (* txtHgt 9.5) totW (+ col1W col2W) totH (* rowH 4.0))
      (setq lastAng 0.0 count 1 total (length nodeList))

      (foreach nodeData nodeList
        (setq nodeID (nth 0 nodeData) qVal (nth 1 nodeData) hslVal (nth 2 nodeData) rpVal (nth 3 nodeData))
        (setvar "OSMODE" oldOsnap)
        (setq pt1 (getpoint (strcat "\n[" (itoa count) "/" (itoa total) "] Click insertion point (Top-Left) for Node " nodeID " (or Enter to skip): ")))
        
        (if pt1
          (progn
            (setq ang (getangle pt1 (strcat "\nClick 2nd point to set rotation (direction of top edge) <" (rtos (* 180.0 (/ lastAng pi)) 2 0) "°>: ")))
            (if ang (setq lastAng ang))
            (setq angDeg (* 180.0 (/ lastAng pi)))
            (setq blockName (strcat "EpaTable_Node_" nodeID "_Hgt" (vl-string-translate "." "-" (rtos txtHgt 2 2))))

            (if (not (tblsearch "BLOCK" blockName))
              (progn
                (entmake (list '(0 . "BLOCK") (cons 2 blockName) '(70 . 0) '(10 0.0 0.0 0.0)))
                (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                               '(90 . 4) '(70 . 1) (list 10 0.0 0.0) (list 10 totW 0.0) (list 10 totW (- totH)) (list 10 0.0 (- totH))))
                (entmake (list '(0 . "LINE") (list 10 0.0 (- rowH) 0.0) (list 11 totW (- rowH) 0.0)))
                (entmake (list '(0 . "LINE") (list 10 0.0 (* rowH -2.0) 0.0) (list 11 totW (* rowH -2.0) 0.0)))
                (entmake (list '(0 . "LINE") (list 10 0.0 (* rowH -3.0) 0.0) (list 11 totW (* rowH -3.0) 0.0)))
                (entmake (list '(0 . "LINE") (list 10 col1W (- rowH) 0.0) (list 11 col1W (- totH) 0.0)))

                (defun draw-text (str pX pY)
                  (entmake (list '(0 . "TEXT") (cons 1 str) (list 10 pX pY 0.0) (list 11 pX pY 0.0) 
                                 (cons 40 txtHgt) '(72 . 0) '(73 . 2))))

                (draw-text nodeID (* txtHgt 0.8) (/ rowH -2.0))
                (draw-text "HSL" (* txtHgt 0.8) (* rowH -1.5))
                (draw-text "Q" (* txtHgt 0.8) (* rowH -2.5))
                (draw-text "RP" (* txtHgt 0.8) (* rowH -3.5))
                (draw-text (strcat hslVal "m") (+ col1W (* txtHgt 0.8)) (* rowH -1.5))
                (draw-text (strcat qVal "lps") (+ col1W (* txtHgt 0.8)) (* rowH -2.5))
                (draw-text (strcat rpVal "m") (+ col1W (* txtHgt 0.8)) (* rowH -3.5))
                (entmake '((0 . "ENDBLK")))
              )
            )
            (setvar "OSMODE" 0)
            (command "_.INSERT" blockName "_NON" pt1 1 1 angDeg)
            (setvar "OSMODE" oldOsnap)
          )
        )
        (setq count (1+ count))
      )
      (princ "\nAll node tables processed successfully.")
    )
  )
  (if oldOsnap (setvar "OSMODE" oldOsnap))
  (princ)
)
