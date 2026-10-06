;;; ==========================================================================
;;; LOAD-ALL.LSP - Master Loader for Civil & Infrastructure AutoLISP Suite
;;; Universal Loader for AutoCAD, AutoCAD LT (2024+), GstarCAD, ZWCAD & BricsCAD
;;; ==========================================================================
(defun c:loadall ( / *error* files loaded failed p f fullPath dir)
  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[AutoLISP Suite] Load error: " msg)))
    (princ))

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
    "pipec.lsp"
    "guideoffset.lsp"
    "tseq.lsp"
    "r180.lsp"
    "getlength.lsp"
    "catchmentarea.lsp"
    "epatable.lsp"
    "getarea.lsp"
    "getaream.lsp"
    "getareaha.lsp"
    "getareaacre.lsp"
    "repsim.lsp"
  ))

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

  ;; Display summary banner
  (princ "\n================================================================")
  (princ "\n   CIVIL & INFRASTRUCTURE AUTOLISP AUTOMATION SUITE LOADED      ")
  (princ "\n================================================================")
  (princ (strcat "\n  Status: " (itoa loaded) " scripts loaded successfully"))
  (if (> failed 0) (princ (strcat " (" (itoa failed) " failed)")))
  (princ "\n----------------------------------------------------------------")
  (princ "\n  COMMAND        DESCRIPTION")
  (princ "\n  ------------   -----------------------------------------------")
  (princ "\n  PIPEC          Road drain pipe generator with sumps & SIL labels")
  (princ "\n  GUIDEOFFSET    Trace boundary and generate offset guide line")
  (princ "\n  CATCHMENTAREA  4-corner house/road catchment divider with ridge")
  (princ "\n  EPATABLE       Parse EPANET .rpt report into dynamic CAD tables")
  (princ "\n  GETAREA        Extract polyline area (mm2 / m2 / ha / acres)")
  (princ "\n  GETAREAM       Extract polyline area in square meters (m2)")
  (princ "\n  GETAREAHA      Extract polyline area in hectares (ha)")
  (princ "\n  GETAREAACRE    Extract polyline area in acres (ac)")
  (princ "\n  GETLENGTH      Cumulative path distance measurement to clipboard")
  (princ "\n  TSEQ           Sequential increment text/mtext copier (A01, A02...)")
  (princ "\n  R180           In-place 180-degree entity flip on click")
  (princ "\n  REPSIM         Global identical text replacement with layer filter")
  (princ "\n================================================================")
  (princ "\n  Type any command above to run. Run LOADALL to reload suite.")
  (princ "\n================================================================\n")
  (princ)
)

;; Auto-run on file load
(c:loadall)
