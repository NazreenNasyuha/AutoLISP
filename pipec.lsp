(defun c:pipec ( / *error* askInfo pt1 pt2 ang dist dist_m_float numSeg segDist 
                   i sumpPt pA pB segMid segDistM linePt1 linePt2 arrowSize 
                   arrowP1 arrowP2 txtAngRad pText code size grad str mtextStr
                   drainLayer drainColor drainWidth arrowLayer arrowColor arrowWidth 
                   textLayer textColor txtHgt txtStyle txtWidth 
                   sumpLayer sumpColor sumpRad
                   defCode defSize defGrad mlLayer mlText
                   sump-exists-p sil-exists-p is-text-near textPt0 newMl oldCmd
                   distAbove distBelow pTextAbove pTextBelow blockedAbove blockedBelow
                   global-history stepEnts loop sumpPt_UCS textPt0_UCS)
                   
  ;; =========================================================================
  ;; USER SETTINGS
  ;; =========================================================================
  (setq drainLayer "#JRK - RD Drain Line")
  (setq drainColor 4)          
  (setq drainWidth 250.0)      
  
  (setq arrowLayer "#JRK - RD Drain FLOW")
  (setq arrowColor 4)          
  (setq arrowWidth 0.0)        
  
  (setq sumpLayer "#JRK - RD Drain Manhole_Sump")
  (setq sumpColor 4)           
  (setq sumpRad 600.0)         
  
  (setq textLayer "#JRK - RD Drain Text")               
  (setq textColor 7)           
  (setq txtHgt 2000.0)         
  (setq txtStyle "1000-T2")    
  (setq txtWidth 0.5)          
  
  (setq mlLayer "#JRK - RD Drain Text IL") 
  (setq mlText "{\\W0.5;SIL00.00}") 
  
  (setq defCode "A01")
  (setq defSize "600")
  (setq defGrad "1:000")
  ;; =========================================================================

  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (not (wcmatch (strcat msg "") "*Cancel*,*QUIT*")) (princ (strcat "\nAction Cancelled: " msg)))
    (princ)
  )

  (setvar "CMDECHO" 0)
  (if (not (tblsearch "LAYER" drainLayer)) (command "_.LAYER" "_M" drainLayer "_C" drainColor "" "") (command "_.LAYER" "_C" drainColor drainLayer ""))
  (if (not (tblsearch "LAYER" arrowLayer)) (command "_.LAYER" "_M" arrowLayer "_C" arrowColor "" "") (command "_.LAYER" "_C" arrowColor arrowLayer ""))
  (if (not (tblsearch "LAYER" sumpLayer)) (command "_.LAYER" "_M" sumpLayer "_C" sumpColor "" "") (command "_.LAYER" "_C" sumpColor sumpLayer ""))
  (if (not (tblsearch "LAYER" textLayer)) (command "_.LAYER" "_M" textLayer "_C" textColor "" "") (command "_.LAYER" "_C" textColor textLayer ""))
  (if (not (tblsearch "LAYER" mlLayer)) (command "_.LAYER" "_M" mlLayer "_C" textColor "" "") (command "_.LAYER" "_C" textColor mlLayer ""))
  (setvar "CMDECHO" oldCmd)
  
  (if (not (tblsearch "STYLE" txtStyle))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                   (cons 2 txtStyle) '(70 . 0) '(40 . 0.0) (cons 41 txtWidth) '(50 . 0.0) '(71 . 0) '(3 . "romans.shx") '(4 . "")))
  )

  (defun sump-exists-p (pt / ss)
    (setq ss (ssget "X" (list '(-4 . ">=,>=,*") (list 10 (- (car pt) 100.0) (- (cadr pt) 100.0) 0.0)
                              '(-4 . "<=,<=,*") (list 10 (+ (car pt) 100.0) (+ (cadr pt) 100.0) 0.0)
                              '(0 . "CIRCLE") (cons 8 sumpLayer))))
    (if ss T nil)
  )

  (defun sil-exists-p (pt / ss n ent txt found)
    (setq ss (ssget "X" (list '(-4 . ">=,>=,*") (list 10 (- (car pt) 1500.0) (- (cadr pt) 1500.0) 0.0)
                              '(-4 . "<=,<=,*") (list 10 (+ (car pt) 1500.0) (+ (cadr pt) 1500.0) 0.0)
                              '(0 . "TEXT,MTEXT,MULTILEADER"))))
    (if ss
      (progn (setq n 0 found nil)
        (while (< n (sslength ss))
          (setq ent (entget (ssname ss n)))
          (setq txt (if (= (cdr (assoc 0 ent)) "MULTILEADER") (cdr (assoc 304 ent)) (cdr (assoc 1 ent))))
          (if (and txt (wcmatch (strcase txt) "*SIL*")) (setq found T))
          (setq n (1+ n)))
        found)
      nil)
  )

  (defun is-text-near (pt / boxP1 boxP2 ss)
    (setq boxP1 (trans (list (- (car pt) (* txtHgt 2.0)) (- (cadr pt) (* txtHgt 2.0)) 0.0) 0 1))
    (setq boxP2 (trans (list (+ (car pt) (* txtHgt 2.0)) (+ (cadr pt) (* txtHgt 2.0)) 0.0) 0 1))
    (setq ss (ssget "C" boxP1 boxP2 '((0 . "TEXT,MTEXT"))))
    (if ss T nil)
  )

  (initget "Yes No")
  (setq askInfo (getkword "\nEnter custom drain details? [Yes/No] <No>: "))
  (setq code defCode size defSize grad defGrad)
  (if (= askInfo "Yes")
    (progn (setq code (getstring (strcat "\nCode <" defCode ">: "))) (if (= code "") (setq code defCode))
           (setq size (getstring (strcat "\nSize <" defSize ">: "))) (if (= size "") (setq size defSize))
           (setq grad (getstring (strcat "\nGradient <" defGrad ">: "))) (if (= grad "") (setq grad defGrad))))

  (setq global-history nil)
  (setq loop T)
  
  (while loop
    (initget "Undo")
    (setq pt1 (getpoint "\nClick Start Point of Drain [Undo] <Exit>: "))
    
    (cond
      ((not pt1) (setq loop nil)) 
      
      ((= pt1 "Undo")
       (if global-history
         (progn
           (foreach e (car global-history) (if (entget e) (vl-catch-all-apply 'entdel (list e))))
           (setq global-history (cdr global-history))
           (princ "\n>> Last drain segment undone.")
         )
         (princ "\n>> Nothing to undo.")
       )
      )
      
      (t
       (initget "Undo")
       (setq pt2 (getpoint pt1 "\nClick End Point of Drain [Undo] <Cancel>: "))
       
       (cond
         ((not pt2) (princ "\n>> Cancelled point."))
         ((= pt2 "Undo") (princ "\n>> Cancelled point."))
         (t
           (setq stepEnts nil) 
           
           (setq pt1 (trans pt1 1 0) pt2 (trans pt2 1 0)
                 dist (distance pt1 pt2) dist_m_float (/ dist 1000.0)
                 ang (angle pt1 pt2) arrowSize (* txtHgt 0.8)
                 txtAngRad (if (and (> (* 180.0 (/ ang pi)) 90.0) (<= (* 180.0 (/ ang pi)) 270.0)) (- ang pi) ang)
                 numSeg (fix (+ (/ dist_m_float 30.0) 0.9999))
                 segDist (/ dist numSeg) i 0)

           (while (<= i numSeg)
             (setq sumpPt (polar pt1 ang (* i segDist)))
             
             (if (not (sump-exists-p sumpPt))
               (progn
                 (entmake (list '(0 . "CIRCLE") (cons 8 sumpLayer) (list 10 (car sumpPt) (cadr sumpPt) 0.0) (cons 40 sumpRad)))
                 (setq stepEnts (cons (entlast) stepEnts)) 
               )
             )
             
             (if (not (sil-exists-p sumpPt))
               (progn
                 (setvar "CMDECHO" 0)
                 (vl-catch-all-apply 'setvar (list "CMLEADERSTYLE" "1000-T2"))
                 
                 (command "_.UCS" "_W")
                 (command "_.UCS" "_Z" (* 180.0 (/ txtAngRad pi)))
                 (setq sumpPt_UCS (trans sumpPt 0 1))
                 (setq textPt0_UCS (list (+ (car sumpPt_UCS) (* txtHgt 0.8)) (+ (cadr sumpPt_UCS) (* txtHgt 1.5)) 0.0))
                 (command "_.MLEADER" "_NON" sumpPt_UCS "_NON" textPt0_UCS mlText)
                 (command "_.UCS" "_P") 
                 (command "_.UCS" "_P") 
                 
                 (if (entlast) 
                   (progn
                     (setq newMl (entlast))
                     (setq stepEnts (cons newMl stepEnts)) 
                     (entmod (subst (cons 8 mlLayer) (assoc 8 (entget newMl)) (entget newMl)))
                   )
                 )
                 (setvar "CMDECHO" oldCmd)
               )
             )
             (setq i (1+ i)))

           (setq i 0)
           (while (< i numSeg)
             (setq pA (polar pt1 ang (* i segDist)) pB (polar pt1 ang (* (1+ i) segDist))
                   segMid (list (/ (+ (car pA) (car pB)) 2.0) (/ (+ (cadr pA) (cadr pB)) 2.0) 0.0)
                   segDistM (fix (+ (/ (distance pA pB) 1000.0) 0.5))
                   linePt1 (polar pA ang sumpRad) linePt2 (polar pB (+ ang pi) sumpRad))

             (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                            (cons 8 drainLayer) '(90 . 2) '(70 . 0) (cons 43 drainWidth)
                            (list 10 (car linePt1) (cadr linePt1)) (list 10 (car linePt2) (cadr linePt2))))
             (setq stepEnts (cons (entlast) stepEnts))

             (setq arrowP1 (polar segMid (+ ang (* pi 0.85)) arrowSize) arrowP2 (polar segMid (- ang (* pi 0.85)) arrowSize))
             (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                            (cons 8 arrowLayer) '(90 . 3) '(70 . 0) (cons 43 arrowWidth)
                            (list 10 (car arrowP1) (cadr arrowP1)) (list 10 (car segMid) (cadr segMid)) (list 10 (car arrowP2) (cadr arrowP2))))
             (setq stepEnts (cons (entlast) stepEnts))

             (cond
               ((<= segDistM 6)
                (setq str (strcat code ";\\P" size "%%c\\P" (itoa segDistM) "m\\P" grad))
                (setq distAbove (* txtHgt 0.25)) 
                (setq distBelow (* txtHgt 3.5)))
               ((<= segDistM 14)
                (setq str (strcat code "; " size "%%c\\P" (itoa segDistM) "m " grad))
                (setq distAbove (* txtHgt 0.25))
                (setq distBelow (* txtHgt 1.8)))
               (T
                (setq str (strcat code "; " size "%%c " (itoa segDistM) "m " grad))
                (setq distAbove (* txtHgt 0.25))
                (setq distBelow (* txtHgt 1.25)))
             )

             (setq pTextAbove (polar segMid (+ txtAngRad (/ pi 2.0)) distAbove))
             (setq pTextBelow (polar segMid (- txtAngRad (/ pi 2.0)) distBelow))
             
             (setq blockedAbove (is-text-near pTextAbove))
             (setq blockedBelow (is-text-near pTextBelow))
             
             (if (and blockedAbove (not blockedBelow))
               (setq pText pTextBelow)
               (setq pText pTextAbove)
             )

             (setq mtextStr (strcat "{\\W" (rtos txtWidth 2 4) ";" str "}"))
             (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                            (cons 8 textLayer) (cons 7 txtStyle) 
                            (list 10 (car pText) (cadr pText) 0.0)
                            (cons 40 txtHgt) (cons 41 0.0) (cons 44 0.8) (cons 73 1)      
                            (cons 50 txtAngRad) (list 11 (cos txtAngRad) (sin txtAngRad) 0.0) 
                            '(71 . 8) (cons 1 mtextStr)))
             (setq stepEnts (cons (entlast) stepEnts))
                            
             (setq i (1+ i)))
             
           (setq global-history (cons stepEnts global-history))
             
           (if (> numSeg 1)
             (princ (strcat "\nSuccess! Auto-split into " (itoa numSeg) " segments."))
             (princ "\nSuccess! Pipe generated.")
           )
         )
       )
      )
    )
  )
  (princ)
)
