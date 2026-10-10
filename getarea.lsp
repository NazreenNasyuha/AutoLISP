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

(defun c:getarea ( / *error* sysVars sysVals decimals unitsPerM ss ent
                     rawArea sqm haArea acreArea outText rawLabel copy-clip )

  ;; =========================================================================
  ;; 1. USER SETTINGS & CONFIGURATION
  ;; =========================================================================
  (setq decimals  3)       ;; Number of decimal places for converted values
  (setq unitsPerM 1000.0)  ;; Drawing units per linear metre (1000 = mm)
  ;; =========================================================================

  ;; =========================================================================
  ;; 2. ERROR HANDLING & SYSTEM VARIABLE RESTORATION
  ;; =========================================================================
  (setq sysVars '("CMDECHO")
        sysVals (mapcar 'getvar sysVars))

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETAREA] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. WINDOWS CLIPBOARD INTEGRATION
  ;; =========================================================================
  ;; Copies string to the OS clipboard via temporary text file (100% LT / GstarCAD safe)
  (defun copy-clip (txt / path fh)
    (setq path (strcat (cond ((getenv "TEMP")) ((getenv "TMP")) ("C:\\Temp")) "\\acad_clip.txt"))
    (if (and (boundp 'startapp) (setq fh (open path "w")))
      (progn
        (princ txt fh)
        (close fh)
        (startapp (strcat "cmd.exe /c clip < \"" path "\""))
        T)
      nil)
  )

  ;; =========================================================================
  ;; 4. AREA EXTRACTION & MULTI-UNIT CONVERSION
  ;; =========================================================================
  (princ "\nCommand: GETAREA (Multi-Unit Area Extractor to Clipboard)")

  (if (setq ss (ssget "_:S" '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq ent (ssname ss 0))
      (setvar "CMDECHO" 0)
      (command "_.AREA" "_O" ent)
      (setq rawArea (getvar "AREA"))

      ;; Perform hydraulic and land surveying conversions
      (setq sqm      (/ rawArea (* unitsPerM unitsPerM))  ;; Square metres
            haArea   (/ sqm 10000.0)                      ;; Hectares
            acreArea (/ sqm 4046.8564224)                 ;; International acres
            rawLabel (if (equal unitsPerM 1000.0 1e-9) " mm2 / " " du2 / "))

      ;; Build clean formatted output string
      (setq outText (strcat (rtos rawArea 2 2) rawLabel
                            (rtos sqm 2 decimals) " m2 / "
                            (rtos haArea 2 decimals) " ha / "
                            (rtos acreArea 2 decimals) " ac"))

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
