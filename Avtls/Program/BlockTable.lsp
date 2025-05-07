
;;图块统计表

(defun c:blockcount (/ *error* e ename ename1 height jd ksl1 ksl2 kuandu0 kuandu01 kuandu1 kuandu2 kuandu3 lenflat lenupright lst_ename lst_ksl lst_kslcdr lst_l_xy max0 max01 max1 max2 max3 max4 max5 midp minscx minscy myline1 myline2 n name newjd newpl nn obj oldlayer oldorth oldosmode oldtextstyle pl_lst pt1 pt11 pt2 pt22 pt3 pt4 sc ss1 ssnew theight tkuandu tkuandu0 tkuandu1 tkuandu2 tkuandu3 tuceng txt vla_e)
	(defun *error* (str)
		(setvar "osmode" oldosmode)
		(setvar "orthomode" oldorth)
		(setvar "textstyle" oldtextstyle)
		(and oldlayer (setvar "clayer" oldlayer));返回原图层
		(vla-endundomark *doc*)
		(setvar "cmdecho" 1)
		(princ)
	)
	;开始准备工作
	(vla-startundomark *doc*)
	(setq oldosmode (getvar "osmode"))
	(setq oldorth (getvar "orthomode"))
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	;设置当前文字样式
	(entmake (list
						 '(0 . "STYLE")
						 '(100 . "AcDbSymbolTableRecord")
						 '(100 . "AcDbTextStyleTableRecord")
						 (cons 2 "hztxt")
						 '(70 . 0)
						 '(3 . "tssdeng.shx")
						 '(4 . "hztxt.shx")
					 )
	)
	(setq oldtextstyle (getvar "textstyle"))
	(setvar "textstyle" "hztxt")
	;设置当前图层
	(setq oldlayer (getvar "clayer"));设置旧图层，以备返回
	(setq name "lb_算量辅助");设置标注当前图层及文字样式
	(av:setactivelayer name 7);新建并设置当前层
	;开始吧，兄弟
	(setq ss1 (ssget ":A:L"
							'(
								 (0 . "insert")
								 ;(100 . "acdbblockreference")
							 )
						)
	)
	(if (and ss1 (> (sslength ss1) 0))
		(progn
			(setq n 0)
			(while (and (< n (sslength ss1)))
				(setq e (ssname ss1 n))
				(setq ename (cdr (assoc 2 (entget e))))
				(setq vla_e (vlax-ename->vla-object e))
				(av:bwk vla_e)
				(setq lst_l_xy (append lst_l_xy (list (list ename l_x l_y e))))
				(setq tuceng (cdr (assoc 8 (entget e))))
				(setq lst_ename (append lst_ename (list (list ename tuceng))))
				(setq n (1+ n))
			)
			(while (and lst_ename)
				(setq ksl1 (length lst_ename))
				(setq ename1 (nth 0 lst_ename))
				(setq lst_ename (vl-remove ename1 lst_ename))
				(setq ksl2 (length lst_ename))
				(setq lst_ksl (append lst_ksl (list (list ename1 (- ksl1 ksl2)))))
			)
			(setq lst_ksl (append (list (list (list "图块名称" "所在图层") "图块数量")) lst_ksl))
			;--------------------------------------------------------
			;(initget 128 "draw excel")
			;(setq key (getkword "\n请选择输出方式[图纸中(draw)/输出到(excel)]：<默认图纸中>"))
			;(if (not key)(progn(setq key "draw")))
			;(cond
			;	((= key "draw")
			;----------------------------------------------------------
			(setvar "osmode" 7223)
			(setq pt1 (getpoint "\n指定表格位置:"))
			(setq pt2 (getcorner pt1 "\n指定对角点位:"))
			(setvar "osmode" 0)
			(setq
				pt11 (list (min(car pt1)(car pt2)) (max(cadr pt1)(cadr pt2)))
				pt22 (list (max(car pt1)(car pt2)) (min(cadr pt1)(cadr pt2)))
				pt1 pt11
				pt2 pt22
			)
			(setq lenflat (distance pt1 (setq  pt3 (list (car pt2) (cadr pt1)))))
			(setq lenupright (distance pt1 (setq  pt4 (list (car pt1) (cadr pt2)))))
			(setq pl_lst (list
										 (list (car pt1) (cadr pt1))
										 pt3
										 (list (car pt2) (cadr pt2))
										 pt4
										 (list (car pt1) (cadr pt1))
									 )
			)
			(setq pl_lst (apply 'append pl_lst))
			(setq newpl
				(vla-addlightweightpolyline *ms*
					(vlax-make-variant (vlax-safearray-fill (vlax-make-safearray 5 (cons 0 (1- (length pl_lst)))) pl_lst))
				)
			)
			(setq max0 6)
			(setq max1 (eval (cons 'max (mapcar 'strlen (mapcar 'caar lst_ksl)))))
			(setq max2 (eval (cons 'max (mapcar 'strlen (mapcar 'cadar lst_ksl)))))
			(setq max3 6)
			(setq max01 (cadr(vl-sort(list max0 max1 max2 max3) '(lambda(x y)(< x y)))))
			(setq max4 (length lst_ksl))
			(setq max5 (+ max0 max01 max1 max2 max3))
			(setq height (/ lenupright max4))
			(setq kuandu0 (* (/ lenflat max5) max0))
			(setq tkuandu0 (/ kuandu0 max0))
			(setq kuandu01 (* (/ lenflat max5) max01))
			(setq kuandu1 (* (/ lenflat max5) max1))
			(setq tkuandu1 (/ kuandu1 max1))
			(setq kuandu2 (* (/ lenflat max5) max2))
			(setq tkuandu2 (/ kuandu2 max2))
			(setq kuandu3 (* (/ lenflat max5) max3))
			(setq tkuandu3 (/ kuandu3 max3))
			(setq tkuandu (* (eval (cons 'min (list tkuandu0 tkuandu1 tkuandu2 tkuandu3))) 0.8))
			(vla-put-constantwidth newpl (* (/ lenupright max4) 0.05))
			(setq n 1)
			(setq myline1 (vla-addline *ms* (vlax-3d-point pt1) (vlax-3d-point pt3)))
			(repeat max4 (vla-offset myline1 (* n -1 height)) (setq n (1+ n)))
			(vla-delete myline1)
			(setq myline2 (vla-addline *ms* (vlax-3d-point pt1) (vlax-3d-point pt4)))
			(vla-offset myline2 kuandu0)
			(vla-offset myline2 (+ kuandu0 kuandu01))
			(vla-offset myline2 (+ kuandu0 kuandu01 kuandu1))
			(vla-offset myline2 (+ kuandu0 kuandu01 kuandu1 kuandu2))
			(vla-delete myline2)
			(setq lst_ksl (cdr lst_ksl))
			(setq lst_ksl (append (list (list (list "图块名称" "所在图层") "图块数量")) lst_ksl))
			(setq n 0)
			(setq ssnew (ssadd))
			(setq theight (* 0.5 height))
			(while (and (> theight tkuandu)) (setq theight (* theight 0.9)))
			(setq lst_kslcdr (vl-sort (cdr lst_ksl) '(lambda(x y) (< (cadar x) (cadar y)))))
			(setq lst_kslcdr (vl-sort lst_kslcdr
												 '(lambda(x y) (if (= (cadar x) (cadar y)) (progn (< (cadr x) (cadr y)))))
											 )
			)
			(setq lst_ksl (append (list (car lst_ksl)) lst_kslcdr))
			;------------------------------------------------------------
			;(setq theight (* 2 theight)) ;设定字高比例
			;------------------------------------------------------------
			(repeat max4
				(setq txt (nth n lst_ksl))
				(entmakex
					(list
						'(0 . "TEXT")
						(cons 11
							(list
								(+ (car pt1) (* kuandu0 0.5))
								(- (cadr pt1) (- (* (1+ n) height) (* 0.5 height)))
							)
						) ;插入点
						(cons 40 theight) ;字高
						(cons 50 0) ;旋转弧度
						(cons 1 (if(= n 0) (progn "序号") (progn(itoa n)))) ;文字内容
						(cons 10 '(0 0 0)) ;控制是否显示
						(cons 72 4) ;控制点位置
					)
				)
				(if (= n 0)
					(progn
						(entmakex
							(list
								'(0 . "TEXT")
								(cons 11
									(list
										(+ (car pt1) (+ kuandu0 (* kuandu01 0.5)))
										(- (cadr pt1) (- (* (1+ n) height) (* 0.5 height)))
									)
								) ;插入点
								(cons 40 theight) ;字高
								(cons 50 0) ;旋转弧度
								(cons 1 (if (= n 0) (progn "图块图形"))) ;文字内容
								(cons 10 '(0 0 0)) ;控制是否显示
								(cons 72 4) ;控制点位置
							)
						)
					)
					(progn
						
						;(princ "\nLST_XY: ") (PRINC lst_l_xy)
						;(princ "\nLST_KSL: ") (PRINC (nth n lst_ksl))
						
						
						(setq minscx (/ (* kuandu01 0.7) (cadr (assoc (caar (nth n lst_ksl)) lst_l_xy))))
						(setq minscy (/ (* height 0.7) (caddr (assoc (caar (nth n lst_ksl)) lst_l_xy))))
						(setq e (last (assoc (caar (nth n lst_ksl)) lst_l_xy)))
						(setq jd (cdr (assoc 10 (entget e))))
						(setq lst_l_xy (vl-remove (assoc (caar (nth n lst_ksl)) lst_l_xy) lst_l_xy))
						(setq sc (eval (cons 'min (list minscx minscy))))
						(setq newjd (list
													(+ (car pt1) (+ kuandu0 (* kuandu01 0.5)))
													(- (cadr pt1) (- (* (1+ n) height) (* 0.5 height)))
													0.0
												)
						)
						(vla-move
							(setq obj (vla-copy (vlax-ename->vla-object e)))
							(vlax-3D-point jd)
							(vlax-3D-point newjd)
						)
						(if (= (cdr (assoc 0 (entget e))) "text")
							(progn (vla-delete (vlax-ename->vla-object e)))
						)
						(ssadd (entlast) ssnew)
						(vla-ScaleEntity obj (vlax-3D-point newjd) sc)
						(ssadd (entlast) ssnew)
						(setq vla_e (vlax-ename->vla-object (entlast)))
						(av:bwk vla_e)
						(setq midp (polar minp (angle minp maxp) (/ l_djx 2)))
						(if (not (equal midp newjd 0.001))
							(vla-move
								(vlax-ename->vla-object (entlast))
								(vlax-3d-point midp)
								(vlax-3d-point newjd)
							)
						)
						(setq nn 1)
						(while (and (or (> l_y (* height 0.7)) (< l_y (* height 0.5))) (< nn 20))
							(cond
								((> l_y (* height 0.7))
									(vla-ScaleEntity (vlax-ename->vla-object (entlast)) (vlax-3D-point newjd) 0.9)
								)
								((< l_y (* height 0.5))
									(vla-ScaleEntity (vlax-ename->vla-object (entlast)) (vlax-3D-point newjd) 1.1)
								)
							)
							(setq vla_e (vlax-ename->vla-object (entlast)))
							(av:bwk vla_e)
							(setq nn (1+ nn))
						)
						(setq nn 1)
						(while (and (and (> l_x (* kuandu01 0.9)) (< n 100)))
							(vla-ScaleEntity (vlax-ename->vla-object (entlast)) (vlax-3D-point newjd) 0.9)
							(setq vla_e (vlax-ename->vla-object (entlast)))
							(vla-getboundingbox vla_e 'minp 'maxp)
							(av:bwk vla_e)
							(setq nn (1+ nn))
						)
						(if (= (cdr (assoc 0 (entget e))) "text")
							(progn
								(entmakex
									(list
										'(0 . "TEXT")
										(cons 11 (list 0 0)) ;插入点
										(cons 40 theight) ;字高
										(cons 50 0) ;旋转弧度
										(cons 1 "外部参照块!") ;文字内容
										(cons 10 '(0 0 0)) ;控制是否显示
										(cons 72 4) ;控制点位置
									)
								)
								(vla-delete (vlax-ename->vla-object (entlast)))
							)
						)
					)
				)
				(entmakex
					(list
						'(0 . "TEXT")
						(cons 72 4) ;控制点位置
						(cons 11
							(list
								(+ (car pt1) (+ kuandu0 kuandu01 (* kuandu1 0.5)))
								(- (cadr pt1) (- (* (1+ n) height) (* 0.5 height)))
							)
						) ;插入点
						(cons 40 theight) ;字高
						(cons 50 0) ;旋转弧度
						(cons 1 (caar (nth n lst_ksl))) ;文字内容
						(cons 10 '(0 0 0)) ;控制是否显示
					)
				)
				(entmakex
					(list
						'(0 . "TEXT")
						(cons 72 4) ;控制点位置
						(cons 11
							(list
								(+ (car pt1) (+ kuandu0 kuandu01 kuandu1 (* kuandu2 0.5)))
								(- (cadr pt1) (- (* (1+ n) height) (* 0.5 height)))
							)
						) ;插入点
						(cons 40 theight) ;字高
						(cons 50 0) ;旋转弧度
						(cons 1 (cadar (nth n lst_ksl))) ;文字内容
						(cons 10 '(0 0 0)) ;控制是否显示
					)
				)
				(entmakex
					(list
						'(0 . "TEXT")
						(cons 72 4) ;控制点位置
						(cons 11
							(list
								(+ (car pt1) (+ kuandu0 kuandu01 kuandu1 kuandu2 (* kuandu3 0.5)))
								(- (cadr pt1) (- (* (1+ n) height) (* 0.5 height)))
							)
						) ;插入点
						(cons 40 theight) ;字高
						(cons 50 0) ;旋转弧度
						(cons 1
							(if (> n 0)
								(progn (itoa (last (nth n lst_ksl))))
								(progn   (last (nth n lst_ksl)))
							)
						) ;文字内容
						(cons 10 '(0 0 0)) ;控制是否显示
					)
				)
				(setq n (1+ n))
			)
			;----------------------------------------------------------
			;	)
			;	((= key "excel")
			;		(setq ff1 (vl-filename-mktemp nil nil ".csv"))
			;		(setq ff2 (open ff1 "w"))
			;		(setq i 0)
			;		(repeat (length lst_ksl)
			;			(setq lst_bzmp (nth i lst_ksl))
			;			(setq lst_bzmp (list (caar lst_bzmp)(cadar lst_bzmp)(cadr lst_bzmp)))
			;			(setq txt (vl-string-translate "()" ",,," (vl-princ-to-string lst_bzmp)))
			;			(setq txt (vl-string-subst "" "," txt))
			;			(write-line txt ff2)
			;			(setq i (1+ i))
			;		)
			;		(close ff2)
			;		(command-s "_ai_editcustfile" ff1)
			;		(print)
			;	)
			;)
			;----------------------------------------------------------
		)
		(princ "\n提示：请选择图块!")
	)
	(*error* str)
)


(defun av:bwk(vla_e / e e_lst);其它函数为全局函数
	(if (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list vla_e 'minp 'maxp)))
		(progn
			(princ "\n你选中的有外部参照块或其他未知情况，程序将无法正确显示图形!")
			(if (not (member ename e_lst))
				(progn
					(setq e_lst (append e_lst (list ename)))
					(vla-copy vla_e)
					(entmake
						(list
							'(0 . "TEXT")
							(cons 11 (list 0 0)) ;插入点
							(cons 40 200) ;字高
							(cons 50 0) ;旋转弧度
							(cons 1 "外部参照块!") ;文字内容
							(cons 10 '(0 0 0)) ;控制是否显示
							(cons 72 4) ;控制点位置
						)
					)
					(vla-delete vla_e)
					(setq e (entlast))
					(setq vla_e (vlax-ename->vla-object (entlast)))
					(vla-getboundingbox vla_e 'minp 'maxp)
					(setq minp (vlax-safearray->list minp))
					(setq maxp (vlax-safearray->list maxp))
				)
			)
		)
		(progn
			(setq minp (vlax-safearray->list minp))
			(setq maxp (vlax-safearray->list maxp))
		)
	)
	(setq minp (list (car minp) (cadr minp)))
	(setq maxp (list (car maxp) (cadr maxp)))
	(setq l_x (- (car maxp) (car minp)))
	(setq l_y (- (cadr maxp) (cadr minp)))
	(setq l_djx (distance minp maxp))
)


(princ)
