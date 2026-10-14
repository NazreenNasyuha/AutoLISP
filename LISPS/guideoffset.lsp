;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : guideoffset.lsp
;;; COMMAND     : GUIDEOFFSET
;;; DESCRIPTION : Continuous Click-to-Trace Closed Parcel Guide Offset Generator
;;;               (Auto-Closing Polyline Tracer with Directional Offset)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In road corridor and residential drain design, drafters must frequently
;;; trace parcel perimeters, kerb lines, or building set-backs at a fixed offset
;;; (e.g., 1500mm from fence/boundary) to establish drain alignment guides.
;;; Tracing, joining, closing, offsetting, changing layer, and deleting the
;;; temporary trace manually is repetitive and tedious.
;;;
;;; GUIDEOFFSET streamlines this entire sequence:
;;;   1. Prompts for the offset clearance distance (default: 1500 drawing units).
;;;   2. Creates or verifies a dedicated guide layer: `DRN-GUIDE-{distance}` (Color 8).
;;;   3. Allows continuous point picking to trace parcel corners.
;;;   4. Automatically seals and closes the polyline boundary (`_C`).
;;;   5. Prompts the drafter to click inside or outside for the offset direction.
;;;   6. Generates the offset boundary on the dedicated guide layer and deletes
;;;      the temporary trace line automatically.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;   1. Type `GUIDEOFFSET` in the CAD command line.
;;;   2. Enter offset distance (e.g., 1500).
;;;   3. Click boundary corners sequentially (Enter when finished).
;;;   4. Click inside or outside to specify offset direction.
;;;   5. The offset guide polyline is drawn on its dedicated layer.
;;; ==========================================================================

(defun c:guideoffset ( / *error* defOff userOff offsetDist layerName 
                       pt ptList oldOs oldCmd plEnt ptOut guideEnt)
  
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq defOff 1500.0) ;; Default offset distance
  ;; =========================================================================

  (setq oldCmd (getvar "CMDECHO") oldOs (getvar "OSMODE"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if oldOs (setvar "OSMODE" oldOs))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nAction Cancelled: " msg)))
    (princ)
  )

  (princ "\nCommand: GUIDEOFFSET (Continuous Pick Offset - Closed Shape)")
  
  ;; 1. Type distance directly
  (setq userOff (getreal (strcat "\nSpecify offset distance <" (rtos defOff 2 0) ">: ")))
  (setq offsetDist (if (not userOff) defOff userOff))
  (setq layerName (strcat "DRN-GUIDE-" (rtos offsetDist 2 0)))
  
  (if (not (tblsearch "LAYER" layerName))
    (command "_.LAYER" "_M" layerName "_C" 8 "" "")
    (command "_.LAYER" "_C" 8 layerName "")
  )
  
  ;; 2. Infinite loop to collect your trace points
  (setq ptList nil)
  (while (setq pt (getpoint (if ptList "\nClick next point (or Enter to finish): " "\nClick 1st point: ")))
    (setq ptList (append ptList (list pt)))
  )
  
  (if (and ptList (> (length ptList) 2)) ;; Requires at least 3 points to close a shape
    (progn
      (setvar "OSMODE" 0) (setvar "CMDECHO" 0)
      
      ;; 3. Draw the polyline and FORCE it to Close
      (command "_.PLINE")
      (foreach p ptList (command "_NON" p))
      (command "_C") ;; "_C" triggers the "Close" command, sealing the gap
      (setq plEnt (entlast))
      
      (setvar "OSMODE" oldOs) ;; Turn OSNAP back on so user can comfortably click
      
      ;; 4. Wait for user to manually pick the offset direction
      (setq ptOut (getpoint "\nClick to specify offset direction (Inside or Outside): "))
      
      (if ptOut
        (progn
          (setvar "OSMODE" 0) 
          (command "_.OFFSET" offsetDist plEnt "_NON" ptOut "")
          
          (if (not (equal plEnt (entlast)))
            (progn
              (setq guideEnt (entlast))
              (command "_.CHPROP" guideEnt "" "_LA" layerName "")
              (entdel plEnt) ;; Delete the original trace line
              (princ "\nGuide drawn successfully.")
            )
            (progn (entdel plEnt) (princ "\nError: Invalid shape geometry or offset distance too large."))
          )
        )
        (progn (entdel plEnt) (princ "\nAction cancelled. Shape deleted.")) ;; Cleanup if user presses Esc
      )
      
      (setvar "OSMODE" oldOs) (setvar "CMDECHO" oldCmd)
    )
    (princ "\nYou need to click at least 3 points to create a closed shape.")
  )
  (princ)
)