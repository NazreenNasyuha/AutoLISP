;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : getarea.lsp
;;; COMMAND     : GETAREA
;;; DESCRIPTION : Multi-Unit Polyline Area Extractor with Instant Clipboard Copy
;;;               (Formats mm², m², Hectares, and Acres Simultaneously)
;;; AUTHOR      : Professional Infrastructure CAD Automation
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD)
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
    (if (and (boundp 'command-s) (> (getvar "CMDACTIVE") 0))
      (vl-catch-all-apply 'command-s (list))
    )
    (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[GETAREA] Error: " msg)))
    (princ)
  )

  ;; =========================================================================
  ;; 3. ZERO-PADDED FORMATTER & 4-LAYER CLIPBOARD INTEGRATION
  ;; =========================================================================
  (defun fmt-dec (val dec / s dotPos fracLen)
    (if (boundp 'jrk:fmt-dec)
      (jrk:fmt-dec val dec)
      (progn
        (setvar "DIMZIN" 0)
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

  (defun copy-clip (txt / html pw clip ok tmpDir tmp fh wsh cmd pid safeTxt i ch tmpEnt)
    (if (boundp 'jrk:copy-clip)
      (jrk:copy-clip txt)
      (progn
        (setq ok nil)

        ;; Layer 1: htmlfile COM ClipboardData
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

        ;; Layer 2: WScript.Shell synchronous clip.exe
        (if (not ok)
          (progn
            (setq tmpDir (cond ((getenv "TEMP")) ((getenv "TMP")) ((getvar "TEMPPREFIX")) (".")))
            (setq tmp (if (boundp 'vl-filename-mktemp)
                        (vl-filename-mktemp "jrk_clip_" tmpDir ".txt")
                        (strcat tmpDir "\\jrk_clip.txt")))
            (if (and tmp (setq fh (open tmp "w")))
              (progn
                (princ txt fh)
                (close fh)
                (setq cmd (strcat "cmd.exe /c clip < \"" tmp "\""))
                (setq wsh (vl-catch-all-apply 'vlax-create-object (list "WScript.Shell")))
                (if (and wsh (not (vl-catch-all-error-p wsh)))
                  (progn
                    (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke (list wsh 'Run cmd 0 :vlax-true))))
                      (setq ok T)
                    )
                    (vl-catch-all-apply 'vlax-release-object (list wsh))
                  )
                )
                (if (boundp 'vl-file-delete) (vl-file-delete tmp))
              )
            )
          )
        )

        ;; Layer 3: AutoCAD LT 2024+ & Windows 11 Safe startapp + clip.exe
        (if (and (not ok) (boundp 'startapp))
          (progn
            (setq safeTxt "" i 1)
            (while (<= i (strlen txt))
              (setq ch (substr txt i 1))
              (if (not (member ch '("\"" "&" "|" "<" ">" "^" "%")))
                (setq safeTxt (strcat safeTxt ch))
              )
              (setq i (1+ i))
            )
            (setq pid (vl-catch-all-apply 'startapp
                        (list "cmd.exe" (strcat "/c <nul set /p=\"" safeTxt "\" | clip"))))
            (if (and pid (not (vl-catch-all-error-p pid)) (numberp pid) (> pid 0))
              (setq ok T)
            )
          )
        )

        ;; Layer 4: Pure CAD Native _.COPYCLIP Fallback
        (if (not ok)
          (if (entmake (list '(0 . "TEXT")
                             '(10 0.0 0.0 0.0)
                             '(40 . 1.0)
                             (cons 1 txt)))
            (progn
              (setq tmpEnt (entlast))
              (vl-catch-all-apply 'command (list "_.COPYCLIP" tmpEnt ""))
              (entdel tmpEnt)
              (setq ok T)
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

      (setq sqm      (/ rawArea (* unitsPerM unitsPerM))
            haArea   (/ sqm 10000.0)
            acreArea (/ sqm 4046.8564224)
            rawLabel (if (equal unitsPerM 1000.0 1e-9) " mm2 / " " du2 / "))

      (setq outText (strcat (fmt-dec rawArea 2) rawLabel
                            (fmt-dec sqm decimals) " m2 / "
                            (fmt-dec haArea decimals) " ha / "
                            (fmt-dec acreArea decimals) " ac"))

      (if (copy-clip outText)
        (princ (strcat "\n>> Copied to Clipboard: " outText))
        (princ (strcat "\n>> " outText " (clipboard unavailable)")))
    )
    (princ "\nNo valid polyline selected.")
  )

  (mapcar '(lambda (v x) (if x (setvar v x))) sysVars sysVals)
  (princ)
)