;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : load-all.lsp
;;; COMMAND     : LOADALL
;;; DESCRIPTION : Dynamic Folder-Scanning Master Loader & Command Directory
;;;               (Auto-loads ALL *.lsp files in folder + 19-Module Fallback)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD, BricsCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:loadall ( / *error* oldCmd dir userProfile candidateDirs d p
                     rawFiles files loaded failed f fullPath )
  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[AutoLISP Suite] Load error: " msg)))
    (princ))

  ;; =========================================================================
  ;; 1. TEAM-PROOF DIRECTORY RESOLUTION
  ;; =========================================================================
  (setq dir "")
  (setq userProfile (getenv "USERPROFILE"))

  ;; Check all standard Desktop & local folder structures automatically
  (setq candidateDirs
    (append
      (if userProfile
        (list
          (strcat userProfile "\\Desktop\\AutoLISP 1.0\\AutoLISP 1.0\\LISPS\\")
          (strcat userProfile "\\Desktop\\AutoLISP 1.0\\LISPS\\")
          (strcat userProfile "\\Desktop\\AutoLISP 1.0\\AutoLISP 1.0\\LISP\\")
          (strcat userProfile "\\Desktop\\AutoLISP 1.0\\LISP\\")
          (strcat userProfile "\\Desktop\\AutoLISP 1.0\\AutoLISP 1.0\\")
          (strcat userProfile "\\Desktop\\AutoLISP 1.0\\")
        )
        nil
      )
      (if (setq p (findfile "load-all.lsp"))
        (if (vl-filename-directory p)
          (list
            (strcat (vl-filename-directory p) "\\LISPS\\")
            (strcat (vl-filename-directory p) "\\LISP\\")
            (strcat (vl-filename-directory p) "\\")
          )
          nil
        )
        nil
      )
      (if (/= (getvar "DWGPREFIX") "")
        (list
          (strcat (getvar "DWGPREFIX") "LISPS\\")
          (strcat (getvar "DWGPREFIX") "LISP\\")
          (getvar "DWGPREFIX")
        )
        nil
      )
    )
  )

  ;; Pick the first directory that contains pipec.lsp (or any .lsp scripts)
  (foreach d candidateDirs
    (if (and (= dir "")
             (or (findfile (strcat d "pipec.lsp"))
                 (> (length (vl-remove-if
                              '(lambda (x) (= (strcase x) "LOAD-ALL.LSP"))
                              (vl-directory-files d "*.lsp" 1)))
                    0)))
      (setq dir d)
    )
  )

  ;; =========================================================================
  ;; 2. DYNAMIC FOLDER SCANNER (Loads EVERY *.lsp file in the folder!)
  ;; =========================================================================
  (setq rawFiles (if (/= dir "") (vl-directory-files dir "*.lsp" 1) nil))

  ;; Exclude load-all.lsp itself so it never loops recursively
  (if rawFiles
    (setq files
      (vl-sort
        (vl-remove-if '(lambda (x) (= (strcase x) "LOAD-ALL.LSP")) rawFiles)
        '<
      )
    )
  )

  ;; Fallback list (All 19 scripts from your folder) if CAD Support Path is used without dir
  (if (null files)
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
      "guidemiddle.lsp"
      "guideoffset.lsp"
      "pipec.lsp"
      "pipechouse.lsp"
      "r180.lsp"
      "repsim.lsp"
      "sewpipec.lsp"
      "torient.lsp"
      "tseq.lsp"
    ))
  )

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
  ;; 4. SUMMARY BANNER & COMPLETE 19-COMMAND DIRECTORY
  ;; =========================================================================
  (princ "\n================================================================")
  (princ "\n   CIVIL & INFRASTRUCTURE AUTOLISP AUTOMATION SUITE LOADED      ")
  (princ "\n================================================================")
  (if (/= dir "")
    (princ (strcat "\n  Folder: " dir))
  )
  (princ (strcat "\n  Status: " (itoa loaded) " scripts loaded automatically"))
  (if (> failed 0) (princ (strcat " (" (itoa failed) " failed)")))
  (princ "\n----------------------------------------------------------------")
  (princ "\n  COMMAND        DESCRIPTION")
  (princ "\n  ------------   -----------------------------------------------")
  (princ "\n  CATCHMENTAREA  4-corner house/road catchment divider with ridge")
  (princ "\n  CSVUPDATE      Smart Excel CSV injector (Red Missing / Blue Dup)")
  (princ "\n  EPALINK        Auto-label & update EPANET pipes (Hex + L/Q/Vs/HL)")
  (princ "\n  EPANODE        Auto-label & update EPANET nodes (HSL, Q, RP)")
  (princ "\n  GETALLAREAM    Batch sum multiple polylines into total m2 + label")
  (princ "\n  GETAREA        Extract polyline area (mm2 / m2 / ha / acres)")
  (princ "\n  GETAREAACRE    Extract polyline area in acres (ac) + centroid text")
  (princ "\n  GETAREAHA      Extract polyline area in hectares (ha) + centroid text")
  (princ "\n  GETAREAM       Extract polyline area in square meters (m2) + text")
  (princ "\n  GETLENGTH      Cumulative path distance measurement to clipboard")
  (princ "\n  GUIDEMIDDLE    4-corner road centerline generator with extension")
  (princ "\n  GUIDEOFFSET    Trace closed boundary and generate offset guide")
  (princ "\n  PIPEC          Road drain generator with sumps & SIL00.00 MLeader")
  (princ "\n  PIPECHOUSE     Residential drain generator with live 30m ruler")
  (princ "\n  R180           Universal 180-deg true-middle flipper (1-click/multi)")
  (princ "\n  REPSIM         Replace similar text + Canva/Word Style Painter")
  (princ "\n  SEWPIPEC       Sewer network generator with manholes & MH MLeader")
  (princ "\n  TORIENT        Universal orientation tool (Blocks/MText/Text/Lines)")
  (princ "\n  TSEQ           Smart discipline-aware ID sequencer (Drains & MHs)")
  (princ "\n================================================================")
  (princ "\n  Type any command above to run. Run LOADALL to reload suite.")
  (princ "\n================================================================\n")
  (princ)
)

;; Auto-run on file load
(c:loadall)