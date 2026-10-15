;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : load-all.lsp
;;; COMMAND     : LOADALL
;;; DESCRIPTION : Master Automated Suite Loader & Interactive Command Directory
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================

(defun c:loadall ( / *error* oldCmd dir userProfile path1 path2 p files loaded failed f fullPath )
  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[AutoLISP Suite] Load error: " msg)))
    (princ))

  ;; =========================================================================
  ;; TEAM-PROOF DIRECTORY RESOLUTION (Matches your exact LISPS folder layout)
  ;; =========================================================================
  (setq dir "")
  (setq userProfile (getenv "USERPROFILE")) 
  
  ;; Check the exact Desktop extraction paths (Looking for "LISPS" folder)
  (setq path1 (strcat userProfile "\\Desktop\\AutoLISP 1.0\\AutoLISP 1.0\\LISPS\\"))
  (setq path2 (strcat userProfile "\\Desktop\\AutoLISP 1.0\\LISPS\\"))

  (cond
    ;; 1. Check if the files are on the user's Desktop in the nested folder
    ((findfile (strcat path1 "pipec.lsp"))
     (setq dir path1))
    ;; 2. Check if the files are on the user's Desktop in a single folder
    ((findfile (strcat path2 "pipec.lsp"))
     (setq dir path2))
    ;; 3. Fallback: Check if CAD knows where load-all.lsp is, and look INSIDE the LISPS subfolder
    ((findfile "load-all.lsp")
     (setq p (findfile "load-all.lsp"))
     (if (vl-filename-directory p)
       (setq dir (strcat (vl-filename-directory p) "\\LISPS\\"))))
    ;; 4. Fallback: Check the folder of the currently open drawing
    ((/= (getvar "DWGPREFIX") "")
     (setq dir (getvar "DWGPREFIX")))
  )

  ;; =========================================================================
  ;; MODULE REGISTRATION (15 files)
  ;; =========================================================================
  (setq files '(
    "catchmentarea.lsp"
    "csvupdate.lsp"
    "epalink.lsp"
    "epanode.lsp"
    "getallaream.lsp"
    "getarea.lsp"
    "getareaacre.lsp"
    "getareaha.lsp"
    "getaream.lsp"
    "getlength.lsp"
    "guideoffset.lsp"
    "pipec.lsp"
    "r180.lsp"
    "sewpipec.lsp"
    "tseq.lsp"
  ))

  ;; =========================================================================
  ;; BATCH LOADING ENGINE
  ;; =========================================================================
  (setq loaded 0 failed 0)
  (foreach f files
    (setq fullPath (if (/= dir "") (strcat dir f) f))
    (if (not (findfile fullPath))
      (setq fullPath (findfile f)))
    (if (and fullPath (findfile fullPath))
      (if (vl-catch-all-error-p (vl-catch-all-apply 'load (list fullPath)))
        (progn
          (princ (strcat "\n  [FAILED] " f))
          (setq failed (1+ failed)))
        (progn
          (setq loaded (1+ loaded))))
      (progn
        (princ (strcat "\n  [NOT FOUND] " f))
        (setq failed (1+ failed)))))

  (setvar "CMDECHO" (if oldCmd oldCmd 1))

  ;; =========================================================================
  ;; SUMMARY BANNER & COMMAND DIRECTORY
  ;; =========================================================================
  (princ "\n================================================================")
  (princ "\n   CIVIL & INFRASTRUCTURE AUTOLISP AUTOMATION SUITE LOADED      ")
  (princ "\n================================================================")
  (princ (strcat "\n  Status: " (itoa loaded) " scripts loaded successfully"))
  (if (> failed 0) (princ (strcat " (" (itoa failed) " failed)")))
  (princ "\n----------------------------------------------------------------")
  (princ "\n  COMMAND        DESCRIPTION")
  (princ "\n  ------------   -----------------------------------------------")
  (princ "\n  CATCHMENTAREA  4-corner house/road catchment divider with ridge")
  (princ "\n  CSVUPDATE      Batch update CAD text placeholders from Excel CSV")
  (princ "\n  EPALINK        Auto-label EPANET pipes (Diam, Vel, Headloss)")
  (princ "\n  EPANODE        Auto-label EPANET junction nodes (Elevation, Head, Pressure)")
  (princ "\n  GETALLAREAM    Batch sum multiple polylines into total m2 + label")
  (princ "\n  GETAREA        Extract polyline area (mm2 / m2 / ha / acres)")
  (princ "\n  GETAREAACRE    Extract polyline area in acres (ac) + centroid text")
  (princ "\n  GETAREAHA      Extract polyline area in hectares (ha) + centroid text")
  (princ "\n  GETAREAM       Extract polyline area in square meters (m2) + text")
  (princ "\n  GETLENGTH      Cumulative path distance measurement to clipboard")
  (princ "\n  GUIDEOFFSET    Trace boundary and generate offset guide line")
  (princ "\n  PIPEC          Road drain pipe generator with sumps & SIL labels")
  (princ "\n  R180           In-place 180-degree entity flip on click")
  (princ "\n  SEWPIPEC       Sewer pipe network generator with manholes & IL")
  (princ "\n  TSEQ           Sequential increment text/mtext copier (A01, A02...)")
  (princ "\n================================================================")
  (princ "\n  Type any command above to run. Run LOADALL to reload suite.")
  (princ "\n================================================================\n")
  (princ)
)

;; Auto-run on file load
(c:loadall)