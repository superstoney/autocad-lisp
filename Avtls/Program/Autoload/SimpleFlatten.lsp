(vl-load-com)
(defun c:SimpleFlatten(/ *error* 3dblk1 3dblk2 all flg h h/2 k midpt pix pt1 pt2 sc ss ssblk tch view1 w w/2 x0 x1 x2 y0 y1 y2)
	(defun *error*(msg)
		(vla-Regen *doc* acActiveViewport)
		(if (and k (/= 1 k))
			(progn
				(viewto3d 1)
				;(command-s "plan" "c")
				(command-s "zoom" "w" pt1 pt2);可以避免返回错位
			)
		)
    (vla-EndUndoMark *doc*)
		(setvar "CMDECHO" 1)
    (princ)
  )
	(setvar "CMDECHO" 0)
	(vla-StartUndoMark *doc*)
	(cond
		((= 1 (vlax-get *doc* 'ActiveSpace)))
		(t (vlax-put *doc* 'ActiveSpace 1))
	)
	(cond
		((= 1 (getvar "blockeditor"))
			(command-s "bclose" "s")
		)
		((/= "" (getvar "refeditname"))
			(command-s "refclose" "s")
		)
		(t nil)
	)
	
	(if (equal (setq view1 (getvar "VIEWDIR")) (list 0.0 0.0 1.0))
		(progn
			(setq
				pix (getvar "screensize") ;当前视口宽高像素值
				sc (/ (car pix) (cadr pix)) ;当前视口宽高比
				h (getvar "viewsize") ;视口高度
				h/2 (/ h 2) ;半高
				w (* h sc) ;视口宽度
				w/2 (/ w 2) ;半宽
				midpt (getvar "viewctr") ;视口中心点
				x0 (car midpt) ;中心点X轴
				y0 (cadr midpt) ;中心点Y轴
				x1 (- x0 w/2) ;X轴左坐标
				x2 (+ x0 w/2) ;X轴右坐标
				y1 (- y0 h/2) ;y轴下坐标
				y2 (+ y0 h/2) ;y轴上坐标
				pt1 (list x1 y1 0)
				pt2 (list x2 y2 0)
			)
			(setq k (viewto3d 5))
		)
	)
	(setvar "qaflags" 1);炸开选择集所有实体
	(setq flg t)
	(while (progn
					 (princ "\n锁定和冻结图层不压缩，Esc退出，Enter全选")
					 (and flg
						 (setq ss (cond
												((ssget))
												((setq all (ssget "A" '(
																								 ;(2 . "~`**")
																							 ))) (setq flg nil) all)
												(t nil)
											)
						 )
					 )
				 )
		(while (setq tch (ssget "P" (list (cons 0 "TCH_*,3D*"))))
			(command-s "explode" tch "")
		)
		(flatten-3dto2d ss)
		(setq 3dblk1 (flatten-get3dblk-list ss))
		(setvar "qaflags" 0);防止cmd出错
		(while 3dblk1
			(setq 3dblk2 (list))
			(foreach l 3dblk1
				(command-s "bedit" l)
				(flatten-3dto2d (ssget "A"))
				(setq ssblk (ssget "A" '((0 . "INSERT") (2 . "~`**"))))
				(setq 3dblk2 (append (flatten-get3dblk-list ssblk) 3dblk2))
				(command-s "bclose" "s")
			)
			(setq 3dblk1 (av:delstsame 3dblk2))
		)
	)
	(*error* nil)
)
;;压缩全部图元三维信息
(defun flatten-3dto2d(ssblk / *sbar ismod lst m ssnlst)
	;(setq ssblk (ssget "A"))
	(setq ssnlst (vl-catch-all-apply 'av:ss->ssnlst (list ssblk)))
	(if (vl-catch-all-error-p ssnlst) (setq ssnlst nil))
	(foreach l ssnlst
		(princ (strcat "\r正在进行三维压缩..." (setq *sbar (Spinbar *sbar))))
		(setq ismod nil)
		(setq lst (mapcar (function (lambda(x / k z)
																	(setq k (car x))
																	(cond
																		((and
																			 ;key=10 11 12 13 210；210容易导出图形丢失
																			 (member k (list 10 11 12 13))
																			 (setq z (cadddr x))
																			 (/= 0 z)
																		 )
																			(setq ismod t)
																			(list k (cadr x) (caddr x) 0)
																		)
																		((and
																			 (member k (list 38 39))
																			 (/= 0 (cdr x))
																		 )
																			(setq ismod t)
																			(cons k 0)
																		)
																		(t x)
																	)
																)
											)
								(entget l)
							)
		)
		(if ismod (entmod lst))
	)
)
;;得到3维块名列表（屏蔽文字影响）
;;返回三维图块列表
(defun flatten-get3dblk-list(ssblk / 2dlst 3dslst blklst box lst obj ptz ssnlst)
	;(setq ssblk (ssget "A" '((0 . "INSERT")(2 . "~`**"))))
	(setq ssnlst (vl-catch-all-apply 'av:ss->ssnlst (list ssblk)))
	(if (vl-catch-all-error-p ssnlst) (setq ssnlst nil))
	;消除引用相同的块
	(setq lst (list))
	(foreach l ssnlst
		(setq lst (cons (cons l (cdr (assoc 2 (entget l)))) lst))
	)
	(setq ssnlst (list) blklst (list))
	(foreach l lst
		(cond
			((member (cdr l) blklst))
			(t
				(setq ssnlst (cons (car l) ssnlst))
				(setq blklst (cons (cdr l) blklst))
			)
		)
	)
	;取得三维图元
	(foreach l ssnlst
		(setq obj (vlax-ename->vla-object l))
		(cond
			((progn
				 (setq box (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur)))
				 (vl-catch-all-error-p box)
			 )
				(setq ptz (mapcar 'caddr (lm-get-blkboundingbox obj)))
			)
			(t
				(setq ptz (mapcar '(lambda(x) (caddr (vlax-safearray->list x))) (list ll ur)))
			)
		)
		(and
			ptz
			(or (< 5 (abs (car ptz))) (< 5 (abs (cadr ptz))))
			(setq 3dslst (cons l 3dslst))
		)
	)
	;取得三维块名
	(foreach l 3dslst
		(setq 2dlst (cons (cdr (assoc 2 (entget l))) 2dlst))
	)
	(setq 2dlst (av:delstsame 2dlst))
)
;;说明:视图定位与三维旋转
;;参数:n:1-10，直接定位相应视图；nil，视图选项
;;返回:无
;;例如:(viewto3d nil) (viewto3d 1) (viewto3d 8)
(defun viewto3d(n / i k pt pt11 pt22 s str viewlst)
	(setq
		pt (av:getscr4pt)
		pt11 (vlax-3d-point (car pt))
		pt22 (vlax-3d-point (caddr pt))
	)
	(setq viewlst (list
									(list 1 "top" "俯视")
									(list 2 "bottom" "仰视")
									(list 3 "left" "左视")
									(list 4 "right" "右视")
									(list 5 "front" "前视")
									(list 6 "back" "后视")
									(list 7 "sw" "西南")
									(list 8 "se" "东南")
									(list 9 "ne" "东北")
									(list 10 "nw" "西北")
								)
	)
	(setq str "")
	(foreach l viewlst (setq str (strcat str (caddr l) "(" (itoa (car l)) ")/")))
	(setq str (strcat "\n请选视角[" str "]<默认俯视>"))
	(setq s t)
	(while s
		(cond
			((progn
				 (or n (setq i (vl-catch-all-apply 'getint (list str))))
				 (vl-catch-all-error-p i)
			 )
				(setq k "top" i 1 s nil)
			)
			(t
				(if n (progn (setq i n s nil)))
				(setq i (cond ((and (< 0 i) (< i 11)) i) (t (setq s nil) 1)))
				(setq k (cadr (assoc i viewlst)))
			)
		)
		(command-s "view" k)
		(vla-zoomwindow *acad* pt11 pt22)
		(if (/= i 1) (command-s "3dorbit"))
	)
	i
)
