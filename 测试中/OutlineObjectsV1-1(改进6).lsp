;;-----------------------=={ Outline Objects  }==-----------------------;;
;;                                                                      ;;
;;  This program enables the user to generate one or more closed        ;;
;;  polylines or regions outlining all objects in a selection.          ;;
;;                                                                      ;;
;;  Following a valid selection, the program calculates the overall     ;;
;;  rectangular extents of all selected objects and constructs a        ;;
;;  temporary rectangular polyline offset outside of such extents.      ;;
;;                                                                      ;;
;;  Using a point located within the offset margin between the extents  ;;
;;  of the selection and temporary rectangular frame, the program then  ;;
;;  leverages the standard AutoCAD BOUNDARY command to construct        ;;
;;  polylines and/or regions surrounding all 'islands' within the       ;;
;;  temporary bounding frame.                                           ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-11-30                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-01-23                                      ;;
;;                                                                      ;;
;;  - Added option to erase original objects.                           ;;
;;----------------------------------------------------------------------;;

(defun c:outline ( / *error* idx sel )
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    (if	(setq sel (ssget))
      (progn
	(setq #t0# (getvar "cdate"))
	(LM:startundo (LM:acdoc))
	(LM:outline sel)
	(princ
	  (strcat "\nºÄÊ±£º"
		  (rtos (* 1000000 (- (getvar "cdate") #t0#) 2 3))
		  "Ãë"
	  )
	)	
	(initget "Yes No")
	(if (/=	"No"
		(getkword
		  "\nErase original objects? [Yes/No] <Yes>: "
		)
	    )
	  (repeat (setq idx (sslength sel))
	    (entdel (ssname sel (setq idx (1- idx))))
	  )
	)
	(LM:endundo (LM:acdoc))
      )
    )
    (princ)
)

;; Outline Objects  -  Lee Mac
;; Attempts to generate a polyline outlining the selected objects.
;; sel - [sel] Selection Set to outline
;; Returns: [sel] A selection set of all objects created

(defun LM:outline (sel / a app are box cmd dis el en enl enlbox	enlst ent lst n	o obj olss pl rtn ss ssbox sumlst
		       tmp x y   ENO )
    (if (setq box (LM:ssboundingbox sel))
        (progn
	    
            (setq app (vlax-get-acad-object)
                  dis (/ (apply 'distance box) 20.0)
                  lst (mapcar '(lambda ( a o ) (mapcar o a (list dis dis))) box '(- +))
                  are (apply '* (apply 'mapcar (cons '- (reverse lst))))
                  dis (* dis 1.5)
                  ent
                (entmakex
                    (append
                       '(   (000 . "LWPOLYLINE")
                            (100 . "AcDbEntity")
                            (100 . "AcDbPolyline")
                            (090 . 4)
                            (070 . 1)
                        )
                        (mapcar '(lambda ( x ) (cons 10 (mapcar '(lambda ( y ) ((eval y) lst)) x)))
                           '(   (caar   cadar)
                                (caadr  cadar)
                                (caadr cadadr)
                                (caar  cadadr)
                            )
                        )
                    )
                )
            )
            (apply 'vlax-invoke
                (vl-list* app 'zoomwindow
                    (mapcar '(lambda ( a o ) (mapcar o a (list dis dis 0.0))) box '(- +))
                )
            )
	    (setq ssbox (ssget "c" (car box)(cadr box)))
            (setq cmd (getvar 'cmdecho)
                  enl (entlast)
                  rtn (ssadd)
            )
            (while (setq tmp (entnext enl)) (setq enl tmp))
            (setvar 'cmdecho 0)
            (command
                "_.-boundary" "_a" "_b" "_n" sel ent "" "_i" "_y" "_o" "_p" "" "_non"
                (trans (mapcar '- (car box) (list (/ dis 3.0) (/ dis 3.0))) 0 1) ""
            )
            (while (< 0 (getvar 'cmdactive)) (command ""))
            (entdel ent)
            (while (setq enl (entnext enl))
                (if (and (vlax-property-available-p (setq obj (vlax-ename->vla-object enl)) 'area)
                         (equal (vla-get-area obj) are 1e-4)
                    )
                    (entdel enl)
                    (ssadd  enl rtn)
                )
            )
	    (progn
	      (setq enlbox (yj-ss2lst ssbox))
	      (foreach x enlbox (redraw x 2)) ;Òþ²ØÔ­¶ÔÏó
	      (setq olss rtn)
	      (setq el (yj-ss2lst olss))
	      (setq el (mapcar
			 '(lambda (x)
			    (if	(= (yj-dxf 0 x) "REGION")
			      (yj-region2pline x)
			      x
			    )
			  )
			 el
		       )
	      ) ;ÃæÓò×ª¶à¶ÎÏß
	      (setq sumlst '())
	      (setq n 0)
	      (repeat (length el)
		(setq en (nth n el))
		(setq eno (car (yj-offset en "W" 0.01 "n")))
		(setq pl (gxl-GetSamplet eno 0.5)) ;ÏÒ¸ßÉèÖÃ
		(entdel eno)
		(if (setq ss (ssget "wp"
				    pl
			     )
		    )
		  (progn
		    (setq enlst (yj-ss2lst ss))
		    (setq enlst (vl-remove en enlst))
		    (setq sumlst (append enlst sumlst))
		  )
		)
		(setq n (1+ n))
	      )
	      (if (> (length sumlst) 0)
		(progn
		  (setq sumlst (yj_un-repeat-list sumlst))
		  (foreach n sumlst (vla-delete (vlax-ename->vla-object n)))
		  (setq el (yj-lst-subtract el sumlst))
		)
	      )
	      (foreach x enlbox (redraw x 1)) ;ÏÔÊ¾Ô­¶ÔÏó
	    )
            (vla-zoomprevious app)
            (setvar 'cmdecho cmd)
            el
        )
    )
)

;| Selection Set Bounding Box  -  Lee Mac
 Returns a list of the lower-left and upper-right WCS coordinates of a
 rectangular frame bounding all objects in a supplied selection set.
 s - [sel] Selection set for which to return bounding box|;

(defun LM:ssboundingbox ( s / a b i m n o )
    (repeat (setq i (sslength s))
        (if
            (and
                (setq o (vlax-ename->vla-object (ssname s (setq i (1- i)))))
                (vlax-method-applicable-p o 'getboundingbox)
                (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list o 'a 'b))))
            )
            (setq m (cons (vlax-safearray->list a) m)
                  n (cons (vlax-safearray->list b) n)
            )
        )
    )
    (if (and m n)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list m n))
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com) (princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;(gxl-GetSamplet CURVE d) °´ÏÒ¸ß·µ»ØÇúÏßÄâºÏÑù±¾µã
;;²ÎÊý curve = ÇúÏßÍ¼ÔªÃû/¶ÔÏóÃû
;;     d = ÏÒ¸ßÏÞ²îÖµ
;;   By Gu_xl Ã÷¾­Í¨µÀ£¬2015.04.01ÐÞÕýÁËSPLINEµÄÒ»¸öBug
(defun gxl-GetSamplet (CURVE D / PerDistToLine GETPOINTS NAME DXF PL I)
  (defun PerDistToLine (pt p1 p2 / norm)
    (setq norm (mapcar '- p2 p1)
	  p1   (trans p1 0 norm)
	  pt   (trans pt 0 norm)
    )
    (abs (- (car pt) (car p1)))
  )
  (defun getpoints (curve stPar enPar d / ps pe pm)
    (setq ps (vlax-curve-getPointAtParam curve stPar)
	  pe (vlax-curve-getPointAtParam curve enPar)
	  pm (vlax-curve-getPointAtParam curve (* 0.5 (+ stPar enPar)))
    )
   (if pm
    (if	(<= (PerDistToLine pm ps pe) d)
      (list pe)
      (append (getpoints curve stpar (* 0.5 (+ stPar enPar)) d)
	      (getpoints curve (* 0.5 (+ stPar enPar)) enPar d)
      )
    )
  (list pe)
  )
  )
  (if (= 'vla-object (type curve))
    (setq curve (vlax-vla-object->ename curve))
  )
  (cond
    ((=	"LINE"
	(setq name (cdr (assoc 0 (setq dxf (entget curve)))))
     )
     (list (vlax-curve-getStartPoint curve)
	   (vlax-curve-getEndPoint curve)
     )
    )
    ((= "ARC" name)
     (cons (vlax-curve-getstartpoint curve)
	   (getpoints curve
		      (vlax-curve-getStartParam curve)
		      (vlax-curve-getEndParam curve)
		      d
	   )
     )
    )
    ((= "CIRCLE" name)
     (cons (vlax-curve-getstartpoint curve)
	   (append
	     (getpoints	curve
			0
			pi
			d
	     )
	     (getpoints	curve
			pi
			(* 2 pi)
			d
	     )
	   )
     )
    )
    ((= "ELLIPSE" name)
     (if (vlax-curve-isClosed curve)
       (cons (vlax-curve-getstartpoint curve)
	     (append
	       (getpoints curve
			  0
			  pi
			  d
	       )
	       (getpoints curve
			  pi
			  (* 2 pi)
			  d
	       )
	     )
       )
       (cons (vlax-curve-getstartpoint curve)
	     (getpoints	curve
			(vlax-curve-getStartParam curve)
			(vlax-curve-getEndParam curve)
			d
	     )
       )
     )
    )
    ((= "SPLINE" name)
     (setq
       pl (mapcar 'cdr
		  (vl-remove-if-not '(lambda (x) (= 11 (car x))) dxf)
	  )
     )
     (if (not pl)
       (setq pl
	      (mapcar
		'(lambda (x) (vlax-curve-getclosestpointto curve (cdr x)))
		(vl-remove-if-not '(lambda (x) (= 10 (car x))) dxf)
	      )
       )
     )
     (setq pl (mapcar '(lambda (x)
			 (vlax-curve-getParamAtPoint
			   curve
			   x
			 )
		       )
		      pl
	      )
     )
     (if (equal (car pl) (last pl) 1e-6)
        (setq
	 pl (reverse
	      (cons (vlax-curve-getEndParam curve) (cdr (reverse pl)))
	    )
       )
     )
     (setq pl (mapcar 'list pl (cdr pl))
     )
     (setq pl
	    (apply 'append
		   (mapcar
		     '(lambda (x)
			(list
			  (list (car x) (* 0.5 (apply '+ x)))
			  (list (* 0.5 (apply '+ x)) (cadr x))
			)
		      )
		     pl
		   )
	    )
     )
     (cons
       (vlax-curve-getStartPoint curve)
       (apply
	 'append
	 (mapcar '(lambda (x)
		    (apply 'GETPOINTS (append (cons curve x) (list d)))
		  )
		 pl
	 )
       )
     )
    )
    ((WCMATCH name "*POLYLINE")
     (setq pl nil
	   i  -1
     )
     (while (< i (vlax-curve-getEndParam curve))
       (setq pl (cons (setq i (1+ i)) pl))
     )
     (setq pl (reverse pl)
	   pl (mapcar 'list pl (cdr pl))
     )
     (cons
       (vlax-curve-getStartPoint curve)
       (apply
	 'append
	 (mapcar '(lambda (x)
		    (apply 'GETPOINTS (append (cons curve x) (list d)))
		  )
		 pl
	 )
       )
     )
    )
  )
)
;|============================================================;;
;;;ÃæÓò×ª¶à¶ÎÏß-------------------------yjtdkj.2021.06
²ÎÊý: en       - Òª´¦ÀíµÄÑ¡Ôñ¼¯»òÍ¼ÔªÃû
·µ»Ø: Éú³ÉµÄ¶à¶ÎÏßµÄÍ¼ÔªÃû
|;
(defun yj-region2pline (en /  e0 ln lnn name odlst pl pt1 pt2 sel x)
  (progn
    (defun lst2ss (lst / i ss)
      (setq ss (ssadd))
      (while lst
	(setq ss  (ssadd (car lst) ss)
	      lst (cdr lst)
	)
      )
      ss
    )
    (defun explodex (ents)
      (vl-cmdf "qaflags" 1 ".explode" ents "" "qaflags" 0)
      (while (setq ents (ssget "P" '((0 . "*POLYLINE"))))
	(explodex ents)
      )
    )
    (defun new_list (e / lst)
      (while (setq e (entnext e))
	(if (not (member (cdr (assoc 0 (entget e)))
			 '("ATTRIB" "VERTEX" "SEQEND")
		 )
	    )
	  (setq lst (cons e lst))
	)
      )
      lst
    )
    (setq odlst	(mapcar	'getvar
			'("cmdecho" "osmode" "peditaccept")
		)
    )
    (mapcar 'setvar
	    '("cmdecho" "osmode" "peditaccept")
	    '(0 0 1)
    )
  )
  (setq e0 (entlast))
  (explodex en)
  (setq ln (new_list e0))
  (setq	ln (mapcar '(lambda (x)
		      (setq name (cdr (assoc 0 (entget x))))
		      (setq pl (GXL-GETSAMPLET x 0.5)) ;ÏÒ¸ßÉèÖÃ
		      (if (/= name "ARC")
			(progn
			  (setq	lnn (mapcar '(lambda (pt1 pt2)
					       (entmake
						 (list '(0 . "LINE") (cons 10 pt1) (cons 11 pt2))
					       )
					       (entlast)
					     )
					    pl
					    (cdr pl)
				    )
			  )
			  (entdel x)
			)
			(setq lnn (list x))
		      )
		      lnn
		    )
		   ln
	   )
  )
  (setq ln (apply 'append ln))
  (setq sel (lst2ss ln))
  (command "_.pedit" "_m" sel "" "_j" "" "")
  (mapcar 'setvar
	  '("cmdecho" "osmode" "peditaccept")
	  odlst
  )
  (entlast)
)
;|******************************************************************************************;;
;;;ÔÚÁÐ±íÖÐ¹ýÂËµôÖØ¸´ÏîÄ¿µÄÐÂÁÐ±í-------------------------ÁõÖÇ.2009.03
²ÎÊý: lst= Ô­ÁÐ±í
·µ»Ø:²»ÖØ¸´ÏîÄ¿µÄÐÂÁÐ±í.
|;
(defun yj_un-repeat-list (lst / a lst2)
  (while (setq a    (car lst)
	       lst2 (cons a lst2)
	       lst  (vl-remove a lst)
	 )
  )
  (reverse lst2)
)
;;===============================================;;   
(defun yj-ss2lst (ss / i l)
      (if ss
	(repeat	(setq i (sslength ss))
	  (setq l (cons (ssname ss (setq i (1- i))) l))
	)
      )
    )
;;;°´Ö¸×éÂëÕÒ³öÈ«Í¼ÔªµÄ×éÂëÖµ
    (defun yj-dxf (key ename) (cdr (assoc key (entget ename))))
;;;±íµÄ²î¼¯
(defun yj-lst-subtract (lst1 lst2 / lst)
  (setq lst (append lst1 lst2))
  (vl-remove-if '(lambda (x) (member x (cdr (member x lst)))) lst)
)
;;;========================================================================================;;;
    ;;ÅúÁ¿Æ«ÒÆ By Gu_xl 2013.04.01 --yjtdkj 2021.07¸Ä½ø
    ;;²ÎÊý£ºss      - Ñ¡Ôñ¼¯»òÍ¼Ôª
    ;;²ÎÊý£ºkd0     - Æ«ÒÆ·½ÏòW£­ÏòÍâ£¬N£­ÏòÄÚ£¬ÆäËü£­Ë«Ïò
    ;;²ÎÊý£ºoffset  - Æ«ÒÆÁ¿
    ;;²ÎÊý£ºkd      - ÊÇ·ñÉ¾³ýÔ­Í¼Ôª£¬YÊÇ
    ;;·µ»Ø£ºÍ¼ÔªÁÐ±í
    ;;Àý×Ó£º(yj-offset en "W" 500 "Y")
    (defun yj-offset	(ss kd0 offset kd / n en tmp new enlst lw oldlay)
      (defun CLOCKWISEP	(en / lw minp MaxP lst)
	(setq lw (vlax-ename->vla-object en))
	(vla-GetBoundingBox lw 'MinP 'MaxP)
	(setq
	  minp (vlax-safearray->list minp)
	  MaxP (vlax-safearray->list MaxP)
	  lst  (mapcar
		 (function
		   (lambda (x)
		     (vlax-curve-getParamAtPoint
		       lw
		       (vlax-curve-getClosestPointTo lw x)
		     )
		   )
		 )
		 (list minp
		       (list (car minp) (cadr MaxP))
		       MaxP
		       (list (car MaxP) (cadr minp))
		 )
	       )
	)
	(if (or
	      (<= (car lst) (cadr lst) (caddr lst) (cadddr lst))
	      (<= (cadr lst) (caddr lst) (cadddr lst) (car lst))
	      (<= (caddr lst) (cadddr lst) (car lst) (cadr lst))
	      (<= (cadddr lst) (car lst) (cadr lst) (caddr lst))
	    )
	  t
	)
      )
      (setq oldLAY (getvar "CLAYER")) ;±£´æ¾ÉµÄµ±Ç°Í¼²ã
      (if (= (type ss) 'ename)
	(progn
	  (setq tmp (ssadd))
	  (ssadd ss tmp)
	  (setq ss tmp)
	)
      )
      (setq enlst '())
      (repeat (setq n (sslength ss))
	(setq en (ssname ss (setq n (1- n))))
	(cond
	  ((or (= "ARC" (cdr (assoc 0 (entget en))))
	       (= "CIRCLE" (cdr (assoc 0 (entget en))))
	   )
	   (cond ((= kd0 "W")
		  (vla-offset (vlax-ename->vla-object en) offset)
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		 )
		 ((= kd0 "N")
		  (vla-offset (vlax-ename->vla-object en) (- offset))
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		 )
		 (t
		  (vla-offset (vlax-ename->vla-object en) offset)
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		  (vla-offset (vlax-ename->vla-object en) (- offset))
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		 )
	   )
	  )
	  (t
	   (cond ((= kd0 "W")
		  (if (CLOCKWISEP en)
		    (vla-offset (vlax-ename->vla-object en) (- offset))
		    (vla-offset (vlax-ename->vla-object en) offset)
		  )
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		 )
		 ((= kd0 "N")
		  (if (CLOCKWISEP en)
		    (vla-offset (vlax-ename->vla-object en) offset)
		    (vla-offset (vlax-ename->vla-object en) (- offset))
		  )
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		 )
		 (t
		  (vla-offset (vlax-ename->vla-object en) offset)
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		  (vla-offset (vlax-ename->vla-object en) (- offset))
		  (vla-put-layer (vlax-ename->vla-object (setq new (entlast))) oldLAY)
		  (setq enlst (cons new enlst))
		 )
	   )
	  )
	)
	(if (= kd "Y")
	  (entdel en)
	)
      )
      enlst
    )
