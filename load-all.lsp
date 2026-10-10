;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : load-all.lsp
;;; COMMAND     : LOADALL
;;; DESCRIPTION : Master Automated Suite Loader & Interactive Command Directory
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In multi-user civil engineering drawing production, drafters require an
;;; effortless, one-command mechanism to load the complete infrastructure
;;; automation library without manual APPLOAD dialog picking or menu compilation.
;;;
;;; LOADALL provides automated library initialization:
;;;   1. Dynamically detects its host directory using `findfile` and `DWGPREFIX`.
;;;   2. Systematically iterates through all 16 specialized civil engineering
;;;      LISP modules using error-trapped `vl-catch-all-apply 'load`.
;;;   3. Accurately reports loading status and any missing dependencies.
;;;   4. Prints a formatted ASCII command directory to the CAD command line,
;;;      serving as an instant quick-reference cheat sheet for drafters.
;;;   5. Automatically executes on initial file load `(c:loadall)` so adding
;;;      `load-all.lsp` to CAD Startup Suite initializes everything instantly.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Drag and drop `load-all.lsp` into any open AutoCAD / GstarCAD window,
;;;     or type `LOADALL` in the command line.
;;;  2. All 16 automation modules are instantly compiled into memory.
;;;  3. The interactive command directory is displayed in the command history.
;;; ==========================================================================

(defun c:loadall ( / *error* oldCmd dir p files loaded failed f fullPath )
  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; =========================================================================
  ;; 1. ERROR HANDLING & SYSTEM SETUP
  ;; =========================================================================
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[AutoLISP Suite] Load error: " msg)))
    (princ))

  ;; =========================================================================
  ;; 2. DIRECTORY RESOLUTION & MODULE REGISTRATION
  ;; =========================================================================
  ;; Determine current script directory
  (setq dir "")
  (cond
    ((findfile "load-all.lsp")
     (setq p (findfile "load-all.lsp"))
     (if (vl-filename-directory p)
       (setq dir (strcat (vl-filename-directory p) "\\"))))
    ((/= (getvar "DWGPREFIX") "")
     (setq dir (getvar "DWGPREFIX"))))

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
    "repsim.lsp"
    "sewpipec.lsp"
    "tseq.lsp"
  ))

  ;; =========================================================================
  ;; 3. BATCH LOADING ENGINE
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
  ;; 4. SUMMARY BANNER & COMMAND DIRECTORY
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
  (princ "\n  REPSIM         Global identical text replacement with layer filter")
  (princ "\n  SEWPIPEC       Sewer pipe network generator with manholes & IL")
  (princ "\n  TSEQ           Sequential increment text/mtext copier (A01, A02...)")
  (princ "\n================================================================")
  (princ "\n  Type any command above to run. Run LOADALL to reload suite.")
  (princ "\n================================================================\n")
  (princ)
)

;; Auto-run on file load
(c:loadall)
