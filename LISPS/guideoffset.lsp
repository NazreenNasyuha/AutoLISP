;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : guideoffset.lsp
;;; COMMAND     : GUIDEOFFSET (Alias: GOFF)
;;; DESCRIPTION : Continuous Boundary Tracer & Closed Offset Guide Generator
;;;               (Live Trace Preview, Point-by-Point Undo & Inside/Outside Pick)
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:guideoffset ( / *error* oldCmd oldOs
                       ;; --- settings ---
                       defOff layerPrefix guideColor
                       ;; --- working variables ---
                       userOff offsetDist layerName ptList loop pt lastPt
                       tempEnts plEnt ptOut guideEnt ed
                       ;; --- local helpers ---
                       ensure-layer clear-temp redraw-temp )

  ;; =========================================================================
  ;; 1. USER SETTINGS
  ;; =========================================================================
  (setq defOff      1500.0)       ;; Default offset distance (mm)
  (setq layerPrefix "DRN-GUIDE-") ;; Layer prefix (creates e.g. "DRN-GUIDE-1500")
  (setq guideColor  8)            ;; 8 = Grey (AutoCAD Color Index)
  ;; =========================================================================

  (setq oldCmd   (getvar "CMDECHO")
        oldOs    (getvar "OSMODE")
        tempEnts nil)

  ;; -------------------------------------------------------------------------
  ;; 2. ERROR HANDLING & CLEANUP
  ;; -------------------------------------------------------------------------
  (defun clear-temp ( / e)
    (foreach e tempEnts
      (if (and e (entget e)) (entdel e))
    )
    (setq tempEnts nil)
  )

  (defun *error* (msg)
    (clear-temp)
    (if (and plEnt (entget plEnt)) (entdel plEnt))
    (if oldOs  (setvar "OSMODE" oldOs))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[GUIDEOFFSET] Action Cancelled: " msg))
    )
    (princ)
  )

  ;; Create layer without changing the user's active CLAYER
  (defun ensure-layer (nm col)
    (if (not (tblsearch "LAYER" nm))
      (progn
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 nm) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
        (if (not (tblsearch "LAYER" nm))
          (command "_.-LAYER" "_N" nm "_C" col nm "")
        )
      )
    )
  )

  ;; Redraw temporary yellow trace segments while picking points
  (defun redraw-temp ( / i pA pB)
    (clear-temp)
    (if (> (length ptList) 1)
      (progn
        (setq i 0)
        (while (< i (1- (length ptList)))
          (setq pA (trans (nth i ptList) 1 0)
                pB (trans (nth (1+ i) ptList) 1 0))
          (if (entmake (list '(0 . "LINE") (cons 8 layerName) '(62 . 2) (cons 10 pA) (cons 11 pB)))
            (setq tempEnts (cons (entlast) tempEnts))
          )
          (setq i (1+ i))
        )
      )
    )
  )

  ;; =========================================================================
  ;; 3. MAIN WORKFLOW
  ;; =========================================================================
  (setvar "CMDECHO" 0)
  (princ "\nCommand: GUIDEOFFSET (Continuous Pick Offset - Closed Shape)")

  ;; Step 1: Prompt for offset distance
  (setq userOff (getreal (strcat "\nSpecify offset distance <" (rtos defOff 2 0) ">: ")))
  (setq offsetDist (if (null userOff) defOff userOff))
  (setq layerName  (strcat layerPrefix (rtos offsetDist 2 0)))
  (ensure-layer layerName guideColor)

  ;; Step 2: Interactive point collection with Undo support
  (setq ptList nil loop T)
  (while loop
    (setq lastPt (if ptList (last ptList) nil))
    (if ptList
      (progn
        (initget "Undo")
        (setq pt (getpoint lastPt (strcat "\nClick next corner (" (itoa (length ptList)) " picked) [Undo] <Finish>: ")))
      )
      (setq pt (getpoint "\nClick 1st corner: "))
    )

    (cond
      ((null pt)
       (setq loop nil))

      ((= pt "Undo")
       (if ptList
         (progn
           (setq ptList (reverse (cdr (reverse ptList))))
           (redraw-temp)
           (princ "\n>> Last corner undone.")
         )
         (princ "\n>> No points to undo.")
       ))

      ((listp pt)
       (setq ptList (append ptList (list pt)))
       (redraw-temp))
    )
  )

  (clear-temp)

  ;; Step 3: Build closed trace polyline and offset Inside or Outside
  (if (>= (length ptList) 3)
    (progn
      (setvar "OSMODE" 0)
      (command "_.PLINE")
      (foreach p ptList (command "_NON" p))
      (command "_C")
      (setq plEnt (entlast))

      (setvar "OSMODE" oldOs)
      (setq ptOut (getpoint "\nClick to specify offset direction (Inside or Outside): "))

      (if ptOut
        (progn
          (setvar "OSMODE" 0)
          (command "_.OFFSET" offsetDist plEnt "_NON" ptOut "")

          (if (not (equal plEnt (entlast)))
            (progn
              (setq guideEnt (entlast)
                    ed       (entget guideEnt))
              ;; Assign guide layer & ByLayer color via entmod
              (setq ed (subst (cons 8 layerName) (assoc 8 ed) ed))
              (if (assoc 62 ed)
                (setq ed (subst '(62 . 256) (assoc 62 ed) ed))
              )
              (entmod ed)
              (entupd guideEnt)
              (entdel plEnt)
              (setq plEnt nil)
              (princ (strcat "\n>> Success! Closed guide offset (" (rtos offsetDist 2 0) ") drawn on layer " layerName "."))
            )
            (progn
              (entdel plEnt)
              (setq plEnt nil)
              (princ "\n>> Error: Invalid shape geometry or offset distance too large.")
            )
          )
        )
        (progn
          (entdel plEnt)
          (setq plEnt nil)
          (princ "\n>> Action cancelled. Temporary trace deleted.")
        )
      )
    )
    (princ "\n>> You need to click at least 3 corners to create a closed shape.")
  )

  (setvar "OSMODE" oldOs)
  (setvar "CMDECHO" oldCmd)
  (princ)
)

;; Quick Command Alias
(defun c:goff () (c:guideoffset))