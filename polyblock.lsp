;;; ==========================================================================
;;; POLYBLOCK - Draw a Rectangle and instantly convert to a Dimensioned Block
;;; Universal build: vanilla AutoLISP, AutoCAD / LT / GstarCAD.
;;; ==========================================================================
(defun c:polyblock ( / *error* oldCmd oldOsnap oldColor lastEnt ent elist pts 
                       d1 d2 minX maxX minY maxY L W ftL ftW 
                       textStr cenX cenY cenPt baseName blkName n txtEnt
                       mmPerFoot textHgt drawLabel polyColor textColor )

  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq mmPerFoot 304.8)    ;; Conversion factor: 1 Foot = 304.8 mm
  (setq polyColor "4")      ;; 4 = Cyan for the footprint boundary line
  (setq textHgt   1500.0)   ;; Height of the text label inside the block
  (setq textColor 3)        ;; 3 = Green text for dimensions
  (setq drawLabel T)        ;; Set to nil if you do not want the text inside the block
  ;; =========================================================================

  (setq oldCmd   (getvar "CMDECHO") 
        oldOsnap (getvar "OSMODE")
        oldColor (getvar "CECOLOR"))
  
  (defun *error* (msg)
    (setvar "CMDECHO" oldCmd)
    (setvar "OSMODE" oldOsnap)
    (setvar "CECOLOR" oldColor)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (princ "\nCommand: POLYBLOCK (Draw footprint to create Dimension Block)")
  
  ;; 1. Track the last entity before we start drawing
  (setq lastEnt (entlast))
  
  ;; 2. Force the active color to match the User Settings
  (setvar "CECOLOR" polyColor)
  
  ;; 3. Keep CMDECHO on so the user can see the native Rectangle prompts
  (setvar "CMDECHO" 1)
  (princ "\nSpecify first corner of the pad footprint: ")
  (command "_.RECTANG")
  
  ;; Wait for the user to finish drawing the rectangle
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
  
  ;; Turn off CMDECHO for the background blocking process and restore user's original color
  (setvar "CMDECHO" 0)
  (setvar "CECOLOR" oldColor)
  
  (setq ent (entlast))
  
  ;; 4. Check if a new entity was successfully created
  (if (not (equal ent lastEnt))
    (progn
      (setq elist (entget ent))
      
      ;; Validate that it is a Closed Polyline (Rectangle)
      (if (and (= (cdr (assoc 0 elist)) "LWPOLYLINE")
               (= (logand (cdr (assoc 70 elist)) 1) 1))
        (progn
          
          ;; Extract all vertices (X, Y)
          (setq pts nil)
          (foreach x elist 
            (if (= (car x) 10) (setq pts (cons (list (cadr x) (caddr x) 0.0) pts)))
          )
          (setq pts (reverse pts))
          
          ;; Calculate Exact Dimensions
          (if (or (= (length pts) 4) (= (length pts) 5))
            (progn
              (setq d1 (distance (nth 0 pts) (nth 1 pts)))
              (setq d2 (distance (nth 1 pts) (nth 2 pts)))
              (setq L (max d1 d2) W (min d1 d2))
            )
            ;; Fallback for complex shapes (Bounding Box)
            (progn
              (setq minX (apply 'min (mapcar 'car pts)))
              (setq maxX (apply 'max (mapcar 'car pts)))
              (setq minY (apply 'min (mapcar 'cadr pts)))
              (setq maxY (apply 'max (mapcar 'cadr pts)))
              (setq L (- maxX minX) W (- maxY minY))
              (if (< L W) (setq d1 L L W W d1)) ;; Swap so L is always longest
            )
          )
          
          ;; Convert to Feet and round to the nearest whole number (e.g. 80' x 40')
          (setq ftL (rtos (/ L mmPerFoot) 2 0))
          (setq ftW (rtos (/ W mmPerFoot) 2 0))
          (setq textStr (strcat ftL "' x " ftW "'"))
          
          ;; Calculate Center Point of the shape
          (setq cenX (/ (apply '+ (mapcar 'car pts)) (length pts)))
          (setq cenY (/ (apply '+ (mapcar 'cadr pts)) (length pts)))
          (setq cenPt (list cenX cenY 0.0))
          
          ;; Generate Unique Block Name (e.g., PAD_80x40, PAD_80x40-1)
          (setq baseName (strcat "PAD_" ftL "x" ftW))
          (setq blkName baseName n 1)
          (while (tblsearch "BLOCK" blkName)
            (setq blkName (strcat baseName "-" (itoa n)))
            (setq n (1+ n))
          )
          
          (setvar "OSMODE" 0) ;; Turn off osnaps to prevent shifting during insertion
          
          ;; Create text and convert into a Block
          (if drawLabel
            (progn
              ;; Generate perfectly centered MTEXT
              (entmake (list '(0 . "MTEXT")
                             '(100 . "AcDbEntity")
                             '(100 . "AcDbMText")
                             (cons 10 cenPt)
                             (cons 40 textHgt)
                             '(71 . 5) ;; 5 = Middle Center
                             (cons 1 textStr)
                             (cons 62 textColor)
                             (assoc 8 elist) ;; Inherit polyline layer
                       ))
              (setq txtEnt (entlast))
              
              ;; Group polyline and text into the new Block definition
              (command "_.BLOCK" blkName "_NON" cenPt ent txtEnt "")
            )
            (progn
              ;; Just block the polyline if text is disabled
              (command "_.BLOCK" blkName "_NON" cenPt ent "")
            )
          )
          
          ;; Insert the block precisely where it was drawn
          (command "_.INSERT" blkName "_NON" cenPt 1.0 1.0 0.0)
          
          (princ (strcat "\nSuccess! Shape stamped as Block: " blkName))
        )
        (princ "\nError: Entity is not a closed polyline.")
      )
    )
    (princ "\nDrawing cancelled.")
  )
  
  (setvar "CMDECHO" oldCmd)
  (setvar "OSMODE" oldOsnap)
  (setvar "CECOLOR" oldColor)
  (princ)
)

(princ "\n[POLYBLOCK] Loaded. Type POLYBLOCK to draw a pad and block it.")
(princ)