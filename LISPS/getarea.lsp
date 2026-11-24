;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getarea.lsp
;;; COMMAND     : GETAREA
;;; DESCRIPTION : Multi-Unit Polyline Area Extractor with Instant Clipboard Copy
;;;               (Formats mm², m², Hectares, and Acres Simultaneously)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
;;; ==========================================================================
;;;
;;; OVERVIEW & TECHNICAL SPECIFICATIONS:
;;; --------------------------------------------------------------------------
;;; In infrastructure and land development submissions, area quantities must be
;;; reported across various documentation standards:
;;;   - Building footprint & drainage sumps  -> Square Metres (m²)
;;;   - Macro catchments & agricultural lots -> Hectares (ha)
;;;   - Land title grants & master plans     -> Acres (ac)
;;;
;;; GETAREA solves the conversion friction:
;;;   1. Prompts the drafter to pick a polyline boundary with a single click.
;;;   2. Queries the drawing's geometric area.
;;;   3. Simultaneously computes the area in:
;;;        - Raw Drawing Units (mm² or du²)
;;;        - Square Metres (m²)
;;;        - Hectares (ha) [1 ha = 10,000 m²]
;;;        - Acres (ac)    [1 acre = 4,046.856 m²]
;;;   4. Concatenates all 4 conversions into a clean summary string:
;;;      e.g. "1250000000.00 mm2 / 1250.000 m2 / 0.125 ha / 0.309 ac"
;;;   5. Automatically pipes the formatted string directly into the Windows
;;;      Clipboard using Windows CLIP.EXE via an isolated temp file.
;;;
;;; DRAFTING WORKFLOW:
;;; --------------------------------------------------------------------------
;;;  1. Type GETAREA in the command line.
;;;  2. Click any closed or open polyline boundary.
;;;  3. The conversion summary is printed to the command prompt and immediately
;;;     ready to Paste (Ctrl+V) into Excel, Word, or an email report.
;;;
;;; USER SETTINGS:
;;; --------------------------------------------------------------------------
;;;  - decimals  : Number of decimal places for formatted metrics (default 3).
;;;  - unitsPerM : Drawing units per metre (default 1000.0 = drawing in mm).
;;; ==========================================================================

(vl-load-com)

(defun c:getarea ( / *error* sysVars sysVals decimals unitsPerM ss ent
                     rawArea sqm haArea acreArea outText rawLabel fmt-dec copy-clip )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq decimals  3)       ;; Number of decimal places for converted values
  (setq unitsPerM 1000.0)  ;; Drawing units per linear metre (1000 = mm)
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM VARIABLE RESTORATION
  ;; =========================================================================
  (setq sysVars '("CMDECHO" "DIMZIN")
        sysVals (mapcar 'getvar sysVars))
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETAREA] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. ZERO-PADDED FORMATTER (Rule #4) & HARDENED CLIPBOARD INTEGRATION
  ;; =========================================================================
  (defun fmt-dec (val dec / s dotPos fracLen)
    (if (fboundp 'jrk:fmt-dec)
      (jrk:fmt-dec val dec)
      (progn
        (setq s (rtos val 2 dec))
        (if (> dec 0)
          (progn
            (setq dotPos (vl-string-search "." s))
            (if (null dotPos)
              (progn
                (setq s (strcat s "."))
                (repeat dec (setq s (strcat s "0")))
              )
              (progn
                (setq fracLen (- (strlen s) dotPos 1))
                (while (< fracLen dec)
                  (setq s (strcat s "0") fracLen (1+ fracLen))
                )
              )
            )
          )
        )
        s
      )
    )
  )

  (defun copy-clip (txt / html pw clip ok tmp fh wsh cmd)
    (if (fboundp 'jrk:copy-clip)
      (jrk:copy-clip txt)
      (progn
        (setq ok nil)
        (setq html (vl-catch-all-apply 'vlax-create-object (list "htmlfile")))
        (if (and html (not (vl-catch-all-error-p html)))
          (progn
            (setq pw (vl-catch-all-apply 'vlax-get (list html 'ParentWindow)))
            (if (and pw (not (vl-catch-all-error-p pw)))
              (progn
                (setq clip (vl-catch-all-apply 'vlax-get (list pw 'ClipboardData)))
                (if (and clip (not (vl-catch-all-error-p clip)))
                  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke (list clip 'setData "Text" txt))))
                    (setq ok T)
                  )
                )
              )
            )
            (vl-catch-all-apply 'vlax-release-object (list html))
          )
        )
        (if (not ok)
          (progn
            (setq tmp (vl-filename-mktemp "jrk_clip_" (getenv "TEMP") ".txt"))
            (if (and tmp (setq fh (open tmp "w")))
              (progn
                (princ txt fh)
                (close fh)
                (setq cmd (strcat "cmd.exe /c clip < \"" tmp "\""))
                (setq wsh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
                (if (and wsh (not (vl-catch-all-error-p wsh)))
                  (progn
                    (vl-catch-all-apply 'vlax-invoke (list wsh 'Run cmd 0 :vlax-true))
                    (vl-catch-all-apply 'vlax-release-object (list wsh))
                    (setq ok T)
                  )
                )
                (vl-catch-all-apply 'vl-file-delete (list tmp))
              )
            )
          )
        )
        ok
      )
    )
  )

  ;; =========================================================================
  ;; 4. AREA EXTRACTION & MULTI-UNIT CONVERSION
  ;; =========================================================================
  (princ "\nCommand: GETAREA (Multi-Unit Area Extractor to Clipboard)")

  (if (setq ss (ssget "_:S" '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      (command "_.AREA" "_O" ent)
      (setq rawArea (getvar "AREA"))

      ;; Perform hydraulic and land surveying conversions
      (setq sqm      (/ rawArea (* unitsPerM unitsPerM))  ;; Square metres
            haArea   (/ sqm 10000.0)                      ;; Hectares
            acreArea (/ sqm 4046.8564224)                 ;; International acres
            rawLabel (if (equal unitsPerM 1000.0 1e-9) " mm2 / " " du2 / "))

      ;; Build clean formatted output string
      (setq outText (strcat (fmt-dec rawArea 2) rawLabel
                            (fmt-dec sqm decimals) " m2 / "
                            (fmt-dec haArea decimals) " ha / "
                            (fmt-dec acreArea decimals) " ac"))

      ;; Copy to Windows clipboard and display to user
      (if (copy-clip outText)
        (princ (strcat "\n>> Copied to Clipboard: " outText))
        (princ (strcat "\n>> " outText " (clipboard unavailable)")))
    )
    (princ "\nNo valid polyline selected.")
  )

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)
