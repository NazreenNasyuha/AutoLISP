;;; ==========================================================================
;;; SYSTEM      : Civil & Infrastructure CAD Automation Suite
;;; MODULE      : tbox.lsp
;;; COMMAND     : TBOX (Aliases: HOUSECOUNT, FTAREA)
;;; DESCRIPTION : House / Lot Polyline Counter, Block Creator & Sq.Ft Area Tag
;;;               - Pick a house/lot polyline
;;;               - Automatically converts area to Square Feet (sq.ft)
;;;                 (supports Millimeter [1m=1000] and Meter [1m=1] drawings)
;;;               - Stamps a numbered copy tag (e.g. #01, #02) and area label
;;;                 (e.g. 1,250.00 sq.ft) on each copied polyline
;;;               - Optionally creates a reusable CAD Block from the polyline
;;;                 and/or copies the area / count to Windows Clipboard
;;;               - Follows your cursor on each copy/placement until Enter/Esc
;;; COMPATIBILITY: Universal (AutoCAD, AutoCAD LT 2024+, GstarCAD, ZWCAD)
;;; ==========================================================================

(vl-load-com)

(defun c:tbox ( / *error* oldCmd oldOs oldDimzin undoStarted
                  ;; --- settings ---
                  unitsMode unitsPerM tagPrefix startNum curNum padLen
                  makeBlock blkNamePrefix placeMode copyToClip showAreaTag
                  txtHgt txtLay
                  ;; --- helper functions ---
                  tb:trim tb:fmt-num tb:fmt-int-pad tb:copy-clip
                  tb:get-poly-pts-wcs tb:poly-area-wcs tb:poly-centroid-wcs
                  tb:make-shifted-pts tb:draw-poly-wcs tb:draw-text-wcs
                  tb:create-house-block
                  ;; --- state variables ---
                  sel ent ed srcPts srcAreaRaw srcAreaM2 srcAreaFt2
                  ans nVal dispPt txtStr areaStr newPolyEnt blkDefName
                  grState grPt grPts i stepEnts totalCopied
                  prevPt curPt dx dy shiftPts )

  ;; =========================================================================
  ;; 1. DEFAULT SETTINGS (Adjustable Live on Command Line)
  ;; =========================================================================
  (setq unitsMode     "AUTO"     ;; "AUTO" detects mm vs m from polyline size; or "MM" / "M"
        unitsPerM     1000.0     ;; 1000.0 = Millimeter drawing (1m = 1000 units), 1.0 = Meter drawing
        tagPrefix     "#"        ;; Prefix before counter number (e.g. "#", "H-", "LOT ")
        startNum      1          ;; Starting count number
        curNum        1          ;; Active counter number (increments each copy)
        padLen        2          ;; Zero-padding width (2 -> 01, 02...; 1 -> 1, 2...)
        makeBlock     "Yes"      ;; "Yes" = Create/update a CAD Block definition too; "No" = Polyline + Tag only
        blkNamePrefix "HOUSE_"   ;; Generated block name prefix (e.g. HOUSE_01)
        placeMode     "COPY"     ;; "COPY" = Copy polyline + number + area; "BLOCK" = Insert numbered block
        copyToClip    "Area"     ;; "Area" = Copy sq.ft value, "Count" = Copy count, "Off" = Disabled
        showAreaTag   "Yes"      ;; "Yes" = Stamp sq.ft area text alongside count number
        txtHgt        nil        ;; Auto-scaled if nil
        txtLay        nil        ;; Uses source polyline layer
        undoStarted   nil)

  (setq oldCmd    (getvar "CMDECHO")
        oldOs     (getvar "OSMODE")
        oldDimzin (getvar "DIMZIN"))
  (setvar "CMDECHO" 0)
  (setvar "DIMZIN" 0)

  (defun *error* (msg)
    (redraw)
    (if undoStarted
      (progn
        (if (boundp 'command-s)
          (vl-catch-all-apply '(lambda () (command-s "_.UNDO" "_End")))
          (vl-catch-all-apply '(lambda () (command "_.UNDO" "_End")))
        )
        (setq undoStarted nil)
      )
    )
    (if oldDimzin (setvar "DIMZIN" oldDimzin))
    (if oldOs     (setvar "OSMODE" oldOs))
    (if oldCmd    (setvar "CMDECHO" oldCmd))
    (if (and msg (not (member (strcase msg) '("CONSOLE BREAK" "FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\n[TBOX Error]: " msg))
    )
    (princ)
  )

  ;; =========================================================================
  ;; 2. PURE AUTOLISP STRING, NUMBER & 4-LAYER CLIPBOARD HELPERS
  ;; =========================================================================
  (defun tb:trim (s / len i1 i2)
    (if (or (null s) (/= (type s) 'STR))
      ""
      (progn
        (setq len (strlen s) i1 1 i2 len)
        (while (and (<= i1 len) (member (substr s i1 1) '(" " "\t" "\r" "\n")))
          (setq i1 (1+ i1))
        )
        (while (and (>= i2 i1) (member (substr s i2 1) '(" " "\t" "\r" "\n")))
          (setq i2 (1- i2))
        )
        (if (> i1 i2) "" (substr s i1 (1+ (- i2 i1))))
      )
    )
  )

  (defun tb:fmt-int-pad (n pad / s)
    (setq s (itoa n))
    (while (< (strlen s) pad)
      (setq s (strcat "0" s))
    )
    s
  )

  ;; Formats decimal number cleanly with no DIMZIN trailing-zero loss
  (defun tb:fmt-num (val dec / s dotPos fracLen)
    (setvar "DIMZIN" 0)
    (setq s (rtos (if (numberp val) val 0.0) 2 dec))
    (if (= (substr s 1 1) ".")
      (setq s (strcat "0" s))
    )
    (if (= (substr s 1 2) "-.")
      (setq s (strcat "-0." (substr s 3)))
    )
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
              (setq s       (strcat s "0")
                    fracLen (1+ fracLen))
            )
          )
        )
      )
    )
    s
  )

  (defun tb:copy-clip (txt / html pw clip ok tmpDir tmp fh wsh cmd pid safeTxt i ch tmpEnt)
    (if (boundp 'jrk:copy-clip)
      (jrk:copy-clip txt)
      (progn
        (setq ok nil)
        ;; Layer 1: htmlfile COM
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
        ;; Layer 2: WScript.Shell + clip.exe
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
        ;; Layer 3: startapp + clip.exe (AutoCAD LT 2024+ / Windows 11 safe)
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
        ;; Layer 4: Native CAD _.COPYCLIP fallback
        (if (not ok)
          (if (entmake (list '(0 . "TEXT") '(10 0.0 0.0 0.0) '(40 . 1.0) (cons 1 txt)))
            (progn
              (setq tmpEnt (entlast))
              (vl-catch-all-apply '(lambda () (command "_.COPYCLIP" tmpEnt "")))
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
  ;; 3. PURE DXF POLYLINE GEOMETRY, SHOELACE AREA & CENTROID IN WCS
  ;; =========================================================================
  (defun tb:get-poly-pts-wcs (ent / ed typ pts wcsPt sub subEd)
    (setq ed  (entget ent)
          typ (cdr (assoc 0 ed))
          pts nil)
    (cond
      ((= typ "LWPOLYLINE")
       (foreach item ed
         (if (= (car item) 10)
           (progn
             (setq wcsPt (trans (list (cadr item) (caddr item) 0.0) ent 0))
             (setq pts (append pts (list (list (car wcsPt) (cadr wcsPt) 0.0))))
           )
         )
       ))
      ((= typ "POLYLINE")
       (setq sub (entnext ent))
       (while (and sub (/= (cdr (assoc 0 (setq subEd (entget sub)))) "SEQEND"))
         (if (= (cdr (assoc 0 subEd)) "VERTEX")
           (progn
             (setq wcsPt (trans (cdr (assoc 10 subEd)) sub 0))
             (setq pts (append pts (list (list (car wcsPt) (cadr wcsPt) 0.0))))
           )
         )
         (setq sub (entnext sub))
       ))
    )
    ;; Remove duplicate consecutive closing point if present
    (if (and (>= (length pts) 2)
             (< (distance (car pts) (last pts)) 1.0e-6))
      (setq pts (reverse (cdr (reverse pts))))
    )
    pts
  )

  ;; Shoelace area in drawing units^2
  (defun tb:poly-area-wcs (pts / n k p1 p2 sum)
    (setq n   (length pts)
          sum 0.0
          k   0)
    (if (>= n 3)
      (progn
        (while (< k n)
          (setq p1  (nth k pts)
                p2  (nth (rem (1+ k) n) pts)
                sum (+ sum (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1))))
                k   (1+ k))
        )
        (/ (abs sum) 2.0)
      )
      0.0
    )
  )

  ;; True geometric centroid (or vertex average fallback) in 2D WCS
  (defun tb:poly-centroid-wcs (pts / n k p1 p2 cross areaSum cxSum cySum cx cy)
    (setq n (length pts))
    (cond
      ((>= n 3)
       (setq areaSum 0.0 cxSum 0.0 cySum 0.0 k 0)
       (while (< k n)
         (setq p1      (nth k pts)
               p2      (nth (rem (1+ k) n) pts)
               cross   (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
               areaSum (+ areaSum cross)
               cxSum   (+ cxSum (* (+ (car p1) (car p2)) cross))
               cySum   (+ cySum (* (+ (cadr p1) (cadr p2)) cross))
               k       (1+ k))
       )
       (if (> (abs areaSum) 1.0e-8)
         (setq cx (/ cxSum (* 3.0 areaSum))
               cy (/ cySum (* 3.0 areaSum)))
         (setq cx (/ (apply '+ (mapcar 'car pts)) (float n))
               cy (/ (apply '+ (mapcar 'cadr pts)) (float n)))
       )
       (list cx cy 0.0))
      ((> n 0)
       (list (caar pts) (cadar pts) 0.0))
      (T '(0.0 0.0 0.0))
    )
  )

  (defun tb:make-shifted-pts (pts dx dy)
    (mapcar '(lambda (p) (list (+ (car p) dx) (+ (cadr p) dy) 0.0)) pts)
  )

  (defun tb:draw-poly-wcs (pts lay isClosed / dxfList)
    (setq dxfList
      (append
        (list '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
              (cons 8 lay)
              '(100 . "AcDbPolyline")
              (cons 90 (length pts))
              (cons 70 (if isClosed 1 0))
              '(43 . 0.0))
        (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)
      )
    )
    (entmake dxfList)
    (entlast)
  )

  (defun tb:draw-text-wcs (str pt hgt lay)
    (entmake (list '(0 . "TEXT")
                   '(100 . "AcDbEntity")
                   (cons 8 lay)
                   '(100 . "AcDbText")
                   (list 10 (car pt) (cadr pt) 0.0)
                   (list 11 (car pt) (cadr pt) 0.0)
                   (cons 40 hgt)
                   (cons 1 str)
                   '(50 . 0.0)
                   '(41 . 0.85)
                   '(72 . 1)
                   '(73 . 2)))
    (entlast)
  )

  ;; Creates a CAD Block definition from `pts` (origin at centroid) when makeBlock = "Yes"
  (defun tb:create-house-block (blkName pts lay / cen relPts blkRec blkEnt vEnt)
    (if (or (null blkName) (= blkName "") (< (length pts) 3))
      nil
      (progn
        (setq cen    (tb:poly-centroid-wcs pts)
              relPts (mapcar '(lambda (p) (list (- (car p) (car cen)) (- (cadr p) (cadr cen)) 0.0)) pts))
        (if (tblsearch "BLOCK" blkName)
          (progn
            (setq blkEnt (tb:draw-poly-wcs relPts lay T))
            (if blkEnt
              (vl-catch-all-apply
                '(lambda ()
                   (command "_.BLOCK" blkName "_Y" '(0.0 0.0 0.0) blkEnt "")
                 )
              )
            )
            (if (tblsearch "BLOCK" blkName) blkName nil)
          )
          (progn
            (setq blkRec
              (entmake (list '(0 . "BLOCK")
                             '(100 . "AcDbEntity")
                             (cons 8 lay)
                             '(100 . "AcDbBlock")
                             (cons 2 blkName)
                             '(70 . 0)
                             '(10 0.0 0.0 0.0))))
            (if blkRec
              (progn
                (entmake
                  (append
                    (list '(0 . "LWPOLYLINE")
                          '(100 . "AcDbEntity")
                          (cons 8 lay)
                          '(100 . "AcDbPolyline")
                          (cons 90 (length relPts))
                          '(70 . 1)
                          '(43 . 0.0))
                    (mapcar '(lambda (p) (list 10 (car p) (cadr p))) relPts)
                  )
                )
                (setq vEnt (entmake '((0 . "ENDBLK") (100 . "AcDbBlockEnd"))))
                (if vEnt blkName nil)
              )
              nil
            )
          )
        )
      )
    )
  )

  ;; =========================================================================
  ;; 4. MAIN INTERACTIVE WORKFLOW
  ;; =========================================================================
  (princ "\n==================================================================")
  (princ "\n  TBOX : House / Lot Polyline Counter, Block Builder & Sq.Ft Area")
  (princ "\n  - Click House/Lot Polyline -> Converts Area to sq.ft")
  (princ "\n  - Click destinations to stamp numbered copy + sq.ft area")
  (princ "\n==================================================================")

  (setq srcPts nil srcAreaRaw 0.0 srcAreaM2 0.0 srcAreaFt2 0.0 totalCopied 0)

  ;; Step 1: Pick House/Lot Polyline (with live configuration options)
  (while (null srcPts)
    (initget "Start Prefix Mode Block Units Clip Exit")
    (setq sel (entsel
      (strcat "\nSelect House/Lot Polyline [Start/Prefix/Mode/Block/Units/Clip/Exit] "
              "(Next:" tagPrefix (tb:fmt-int-pad curNum padLen)
              ", Mode:" placeMode
              ", Block:" makeBlock
              ", Units:" unitsMode "): ")))

    (cond
      ((or (= sel "Exit") (null sel))
       (setq srcPts 'CANCEL))

      ((= sel "Start")
       (initget 6)
       (setq nVal (getint (strcat "\nEnter starting number <" (itoa curNum) ">: ")))
       (if nVal (setq startNum nVal curNum nVal)))

      ((= sel "Prefix")
       (setq ans (getstring T (strcat "\nEnter number prefix (e.g. #, H-, LOT ) <" tagPrefix ">: ")))
       (if ans (setq tagPrefix ans)))

      ((= sel "Mode")
       (initget "Copy Block")
       (setq ans (getkword "\nStamp mode [Copy polyline+tags / Block insert] <Copy>: "))
       (setq placeMode (if (= ans "Block") "BLOCK" "COPY")))

      ((= sel "Block")
       (setq makeBlock (if (= makeBlock "Yes") "No" "Yes"))
       (princ (strcat "\n>> Create CAD Block from house polyline: " makeBlock)))

      ((= sel "Units")
       (initget "Auto Millimeters Meters")
       (setq ans (getkword "\nDrawing units [Auto / Millimeters (1m=1000) / Meters (1m=1)] <Auto>: "))
       (cond
         ((= ans "Meters")      (setq unitsMode "M"    unitsPerM 1.0))
         ((= ans "Millimeters") (setq unitsMode "MM"   unitsPerM 1000.0))
         (T                     (setq unitsMode "AUTO" unitsPerM 1000.0))
       ))

      ((= sel "Clip")
       (initget "Area Count Off")
       (setq ans (getkword "\nCopy to Windows Clipboard [Area (sq.ft) / Count / Off] <Area>: "))
       (setq copyToClip (cond ((= ans "Count") "Count") ((= ans "Off") "Off") (T "Area"))))

      ((listp sel)
       (setq ent (car sel)
             ed  (entget ent))
       (if (member (cdr (assoc 0 ed)) '("LWPOLYLINE" "POLYLINE"))
         (progn
           (setq srcPts     (tb:get-poly-pts-wcs ent)
                 srcAreaRaw (tb:poly-area-wcs srcPts)
                 txtLay     (cdr (assoc 8 ed)))
           (if (< (length srcPts) 3)
             (progn
               (princ "\n>> Polyline needs at least 3 vertices to compute area. Pick another polyline.")
               (setq srcPts nil)
             )
             (progn
               (if (= unitsMode "AUTO")
                 (if (> srcAreaRaw 50000.0)
                   (setq unitsPerM 1000.0)
                   (setq unitsPerM 1.0)
                 )
               )
               ;; 1 m^2 = 10.76391041673611 sq.ft
               (setq srcAreaM2  (/ srcAreaRaw (* unitsPerM unitsPerM))
                     srcAreaFt2 (* srcAreaM2 10.76391041673611))
               (if (null txtHgt)
                 (setq txtHgt (* 2.5 unitsPerM))
               )
               (princ (strcat "\n>> House Area: " (tb:fmt-num srcAreaFt2 2) " sq.ft"
                              " (" (tb:fmt-num srcAreaM2 2) " m2)"
                              " | Units: " (if (= unitsPerM 1000.0) "mm" "m")))
             )
           )
         )
         (princ "\n>> Please select a LWPOLYLINE or POLYLINE.")
       ))
    )
  )

  ;; Step 2: Interactive Cursor-Following Placement & Counting Loop
  (if (and (listp srcPts) (>= (length srcPts) 3))
    (progn
      (command "_.UNDO" "_Begin")
      (setq undoStarted T)

      (if (= makeBlock "Yes")
        (setq blkDefName (tb:create-house-block
                           (strcat blkNamePrefix (tb:fmt-int-pad curNum padLen))
                           srcPts
                           txtLay))
        (setq blkDefName nil)
      )

      (setq prevPt (tb:poly-centroid-wcs srcPts)
            grState T)

      (while grState
        (setq txtStr  (strcat tagPrefix (tb:fmt-int-pad curNum padLen))
              areaStr (strcat (tb:fmt-num srcAreaFt2 2) " sq.ft"))

        (princ (strcat "\r[" txtStr " | " areaStr "] Click destination point (or Enter to finish): "))
        (setq grPt (grread T 15 0))

        (cond
          ;; Mouse movement -> draw live rubber-band polyline preview following cursor
          ((= (car grPt) 5)
           (setq curPt (cadr grPt)
                 dx    (- (car curPt) (car prevPt))
                 dy    (- (cadr curPt) (cadr prevPt))
                 grPts (tb:make-shifted-pts srcPts dx dy))
           (redraw)
           (if (> (length grPts) 1)
             (progn
               (setq i 0)
               (while (< i (1- (length grPts)))
                 (grdraw (trans (nth i grPts) 0 1) (trans (nth (1+ i) grPts) 0 1) 3 0)
                 (setq i (1+ i))
               )
               (grdraw (trans (last grPts) 0 1) (trans (car grPts) 0 1) 3 0)
             )
           ))

          ;; Left click (3) -> stamp numbered copy + sq.ft area
          ((= (car grPt) 3)
           (setq curPt    (cadr grPt)
                 dx       (- (car curPt) (car prevPt))
                 dy       (- (cadr curPt) (cadr prevPt))
                 shiftPts (tb:make-shifted-pts srcPts dx dy)
                 dispPt   (tb:poly-centroid-wcs shiftPts)
                 stepEnts nil)

           (if (= placeMode "BLOCK")
             (progn
               (if (= makeBlock "Yes")
                 (setq blkDefName (tb:create-house-block
                                    (strcat blkNamePrefix (tb:fmt-int-pad curNum padLen))
                                    shiftPts
                                    txtLay))
               )
               (if (and blkDefName (tblsearch "BLOCK" blkDefName))
                 (progn
                   (entmake (list '(0 . "INSERT")
                                  '(100 . "AcDbEntity")
                                  (cons 8 txtLay)
                                  '(100 . "AcDbBlockReference")
                                  (cons 2 blkDefName)
                                  (list 10 (car dispPt) (cadr dispPt) 0.0)
                                  '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))
                   (setq stepEnts (cons (entlast) stepEnts))
                 )
                 (setq stepEnts (cons (tb:draw-poly-wcs shiftPts txtLay T) stepEnts))
               )
             )
             (progn
               (setq newPolyEnt (tb:draw-poly-wcs shiftPts txtLay T))
               (if newPolyEnt (setq stepEnts (cons newPolyEnt stepEnts)))
               (if (= makeBlock "Yes")
                 (tb:create-house-block
                   (strcat blkNamePrefix (tb:fmt-int-pad curNum padLen))
                   shiftPts
                   txtLay)
               )
             )
           )

           ;; Stamp Count Number + sq.ft Area text
           (if (= showAreaTag "Yes")
             (progn
               (setq stepEnts (cons (tb:draw-text-wcs
                                      txtStr
                                      (list (car dispPt) (+ (cadr dispPt) (* txtHgt 0.7)) 0.0)
                                      txtHgt txtLay) stepEnts))
               (setq stepEnts (cons (tb:draw-text-wcs
                                      areaStr
                                      (list (car dispPt) (- (cadr dispPt) (* txtHgt 0.7)) 0.0)
                                      (* txtHgt 0.85) txtLay) stepEnts))
             )
             (setq stepEnts (cons (tb:draw-text-wcs txtStr dispPt txtHgt txtLay) stepEnts))
           )

           (setq totalCopied (1+ totalCopied)
                 curNum      (1+ curNum))

           (cond
             ((= copyToClip "Area")  (tb:copy-clip (tb:fmt-num srcAreaFt2 2)))
             ((= copyToClip "Count") (tb:copy-clip (itoa totalCopied)))
           )

           (princ (strcat "\n>> [" txtStr "] Placed (" areaStr ") | Total Copied: " (itoa totalCopied))))

          ;; Enter (car=2 and cadr=13) or Right-click/Cancel (car=2 and cadr=27) -> exit loop
          ((and (= (car grPt) 2) (member (cadr grPt) '(13 27)))
           (setq grState nil))
        )
      )

      (redraw)
      (command "_.UNDO" "_End")
      (setq undoStarted nil)
      (princ (strcat "\n>> TBOX Finished! Total copied: " (itoa totalCopied)
                     " | Area per unit: " (tb:fmt-num srcAreaFt2 2) " sq.ft."))
    )
  )

  (if oldDimzin (setvar "DIMZIN" oldDimzin))
  (if oldOs     (setvar "OSMODE" oldOs))
  (if oldCmd    (setvar "CMDECHO" oldCmd))
  (princ)
)

;; Quick Command Aliases
(defun c:housecount () (c:tbox))
(defun c:ftarea ()     (c:tbox))
(princ "\nTBOX loaded. Type TBOX, HOUSECOUNT, or FTAREA to run.")
(princ)