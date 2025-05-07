
;;;;;===================================================================

;;说明:测量长度与面积
(defun c:t()
	(princ "-->查询长度与面积")
	(av:sscalandtips nil)
)
;;说明:标注长度与面积
(defun c:tt()
	(princ "-->标注长度与面积")
	(av:sscalandtips T)
)
;;绘制多段线
(defun c:fg()
	(princ "-->多段线 ")
	(command "pline")
	(sssentlast)
)

;;说明:计算线条长度与面积
;;参数:dim:控制是否回显每个线条的计算信息，T和NIL
;;返回:无
(defun av:sscalandtips(dim / en ess i l lst oldcl s ss strl strs suml sums)
	(av:startips t)
	(cond
		((setq ss (ssget '((0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE,ARC,LINE"))))
			(setq oldcl (getvar "clayer"))
			(setq i 0 suml 0 sums 0 ess (ssadd))
			(repeat (sslength ss)
				(setq en (ssname ss i))
				(setq i (1+ i))
				(setq lst (av:calent en))
				(setq l (car lst))
				(setq s (cadr lst))
				(av:calentips i l s)
				(setq suml (+ l suml))
				(setq sums (+ s sums))
				;准备标注
				(setq strl (strcat "L=" (ValToStr l 1 3) "m"))
				(cond
					((and s (> s 0))
						(setq strs (strcat "S=" (ValToStr s 2 3) "m2"))
					)
					(t (setq strs nil))
				)
				(and dim
					(setq ess (av:dimentls en strl strs ess))
				)
			)
			(setvar "clayer" oldcl)
			(sssetfirst nil ess)
			(av:endtips (1+ i) suml sums)
		)
		(t (av:setdrawscale))
	)
	(princ)
)
;;说明:单个图形计算
;;参数:en:图元名，选择集中的单个对像ename
;;参数:i:图元序号
;;返回:列表，长度和面积数值
(defun av:calent(en / l obj pt2 s)
	(setq obj (vlax-ename->vla-object en))
	(setq pt2 (vlax-curve-getEndParam obj))
	(setq l (vlax-curve-getDistAtParam obj pt2))
	(setq s (vl-catch-all-apply 'vlax-curve-getArea (list obj)))
	(setq l (* (float l) (expt *key:drawscale* 1)))
	(setq s (* (float s) (expt *key:drawscale* 2)))
	(list l s)
)
;;说明:单个图元长度与面积提示
;;参数:i:序号
;;参数:l:长度
;;参数:s:面积
;;返回:长度与面积文本
(defun av:calentips(i l s)
	(princ (strcat "\n第" (itoa i) "个图元, 长度=" (rtos l 2 0)  "mm"))
	(princ (cond ((zerop s) "，无面积信息" ) (t (strcat ", 面积=" (rtos s 2 0) "mm2"))))
)
;;说明:开始测算时确认当前比例
;;参数:s:T或nil，用来控制是否提示当前测量比例和信息提示
;;返回:当前绘图比例，文本格式
(defun av:startips(s / dsc2str msg)
	;(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0))
	(cond
		((member (type *key:drawscale*) (list 'INT 'REAL)))
		(T (setq *key:drawscale* 1.0))
	)
	(setq dsc2str (rtos *key:drawscale* 2 3))
	(setq msg (strcat "\n当前测量比例1:" dsc2str "，以mm为单位，直接回车可重置测量比例"))
	(and s (princ msg))
	dsc2str
)
(av:startips nil)
;;说明:结束测算时信息反馈
;;参数:m:测量个数
;;参数:suml:累计总长度
;;参数:sums:累计总面积
;;返回:文本信息提示
(defun av:endtips(m suml sums)
	(if (/= 1 (getvar "nomutt"))(setvar "nomutt" 1))
	(princ (strcat "\n注意：按比例【1:" (av:startips nil) "】测量统计，以mm为单位。"))
	(princ (strcat "\n>>>共测量" (itoa (1- m)) "段"))
	(cond
		((zerop suml) (princ "，没有距离信息，请重新测量!"))
		(t (princ (strcat "，累计总长度= " (ValToStr suml 1 3) "m")))
	)
	(and sums (> sums 0)
		(princ (strcat "，累计总面积= " (ValToStr sums 2 3) "m2"))
	)
	(terpri)
)
;;说明:标注单个图元长度和面积
;;参数:en:图元
;;参数:str1:长度信息
;;参数:strs:面积信息
;;参数:ess:选择集，可为nil
;;返回:ess如为空，则只返回当前当次生成标注
(defun av:dimentls(en str1 strs ess / box cl co ent h midp minl)
	(setq ent (entget en))
	(setq cl (cdr (assoc 8 ent)))
	(setq co (cdr (assoc 70 ent)))
	(if av:setactivelayer (av:setactivelayer cl co))
	(or ess (setq ess (ssadd)))
	(cond
		(strs
			(setq box (getbox-midp-minl en))
			(setq midp (car box))
			(setq minl (cadr box))
			(setq h (getscalen minl 15 1000))
			(entmaketext strl midp 0 h 0.2)
			(setq ess (ssadd (entlast) ess))
			(entmaketext strs midp 0 h -1.2)
			(setq ess (ssadd (entlast) ess))
		)
		(t (member (cdr (assoc 0 (entget en))) (list "LINE" "LWPOLYLINE"))
			(av:dimentl en strl)
			(setq ess (ssadd (entlast) ess))
		)
	)
	ess
)
;;说明:标注线长
;;参数:en:直线
;;返回:标注文本属性
(defun av:dimentl(en strl / ang h l lst midp pt1 pt2)
	(setq pt1 (vlax-curve-getStartPoint en))
	(setq pt2 (vlax-curve-getEndPoint en))
	(setq midp (mapcar '(lambda(x y)(/ (+ x y) 2)) pt1 pt2))
	;确认字高
	(setq l (distance pt1 pt2))
	(setq h (getscalen l 20 150))
	;坐标排序，文字标注于线上
	(setq lst (vl-sort (list pt1 pt2) '(lambda(x y) (< (cadr x) (cadr y)))))
	(setq lst (vl-sort lst '(lambda(x y) (< (car x) (car y)))))
	(setq ang (angle (car lst) (cadr lst)))
	;开始标注文字
	(cond ((zerop l))
		(t (entmaketext strl midp ang h 0.2))
	)
)

;;;===========================================================

;自由矩形
(defun c:rec(/ ang geta4pt2 isa4 pt1 pt2 ss)
	(defun GETA4PT2(w h / x1 x2 y1 y2)
		(setq
			x1 (car pt1)
			y1 (cadr pt1)
			x2 (+ x1 w)
			y2 (+ y1 h)
		)
		(list x2 y2 0)
	)
	(princ "-->自由矩形 ")
	(if (= 1 (getvar "nomutt"))(setvar "nomutt" 0))
	(sssetfirst nil nil)
	(setq pt1 (getpoint "\n指定矩形第一个角点："))
	(initget "A B")
	(cond
		((and
			 (= (type pt1) 'list)
			 (setq pt2 (getcorner pt1 "\n[/竖向A4(A)/横向A4(B)]指定对角点：<回车任意角>"))
			 (= (type pt2) 'list)
		 )
			(setq ang 0)
		)
		((and
			 (null pt2)
			 (setq ang (getangle pt1 "\n指定旋转角："))
		 )
			(setq ang (/ (* ang 180.0) pi))
			(princ ang)
			(setq pt2 pause)
		)
		((wcmatch pt2 "A")
			(setq pt2 (GETA4PT2 210 297))
			(setq ang 0)
			(setq isa4 t)
		)
		((wcmatch pt2 "B")
			(setq pt2 (GETA4PT2 297 210))
			(setq ang 0)
			(setq isa4 t)
		)
		(t nil)
	)
	(setvar "cmdecho" 0)
	(command-s "rectang" pt1 "r" ang pt2)
	(command-s "rectang" pt1 "r" 0 pt1)
	(entdel (entlast))
	(cond
		(isa4
			(setq ss (ssadd) ss (ssadd (entlast) ss))
			(command-s "scale" ss "" pt1 "R" pt1 pt2)
		)
	)
	(setvar "cmdecho" 1)
	(sssentlast);命令在计算函数中
)

;矩形云线
(defun c:reccloud(/ al dis1 dis2 dis plst pp1 pp2 pp3 pp4)
	(c:rec);绘制矩形
	;设定比例弧长
  (setq plst (av:getptn (entget (entlast))))
	(setq
		pp1 (car plst)
		pp2 (cadr plst)
		pp3 (caddr plst)
		pp4 (cadddr plst)
	)
	(setq
		dis1 (distance pp1 pp2)
		dis2 (distance pp2 pp3)
		dis (min dis1 dis2)
	)
	(setq al (getscalen dis 8 dis))
	(setvar "cmdecho" 0)
	(command "revcloud" "a" al al "s" "c" "o" "l" "")
	(setvar "cmdecho" 1)
	(princ)
)


;;;===========================================

;;说明:鼠标连续测距，生成临时虚线 
(defun c:pt-pt-dis(/ *error* +dist dist1 dist2 endcmf m pt1 pt1s pt2)
	(defun *error*(msg)(endcmf +dist))
	(defun endcmf (+dist)
		(setq +dist (vl-string-subst "" "+" +dist))
		(cond
			(av:putcliptext
				(av:putcliptext +dist)
				(princ (strcat "\n已复制：" +dist))
			)
			(t nil)
		)
		(and pt2 (av:endtips m dist1 nil))
		(terpri)
		(princ)
	)
	;主程序开始
	(princ "-->连续测距")
	(av:startips t)
	(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0))
	(setq pt1 (getpoint "\n请指定开始点: "))
	(and pt1 (setq pt2 (getpoint pt1 ">>>请指定下一点:")))
	(cond
		(pt2
			(grdraw pt1 pt2 1 1)
			(princ " >>>重新开始[F]")
			(setq dist1 (distance pt2 pt1))
			(setq dist1 (* dist1 *key:drawscale*))
			(setq +dist "")
			(setq +dist (strcat +dist "+" (ValToStr dist1 1 3)))
			(setq m 1)
			(princ (strcat "\n第" (itoa m) "段长度=" (rtos dist1 2 0) "mm"))
			(setq pt1s (cons pt2 (list pt1)))
			(setq dist1 (distance (car pt1s) (last pt1s)))
			(setq dist1 (* dist1 *key:drawscale*))
			(princ (strcat "，累计长度=" (rtos dist1 2 0) "mm。"))
			(setq m (1+ m))
			(while
				(if pt1 (progn(initget 128 "f")(setq pt1 (getpoint pt2))))
				(if (= pt1 "f")
					(progn 
						(setq pt2 (getpoint "\n请指定重新开始点: ")) 
						(setq pt1 (getpoint pt2 " 请指定下一点: "))
						(grdraw pt1 pt2 1 1)
						(if pt2 (progn (setq pt1s (cons pt2 (list pt1)))))
					)
				)
				(setq pt1s (cons pt1 pt1s))
				(if (>= (length pt1s) 2) (grdraw pt1 pt2 1 1))
				(setq dist2 (distance pt2 pt1))
				(setq dist2 (* dist2 *key:drawscale*))
				(setq +dist (strcat +dist "+" (ValToStr dist2 1 3)))
				(princ (strcat "\n第" (itoa m) "段长度=" (rtos dist2 2 0) "mm"))
				(setq dist1 (+ dist2 dist1))
				(princ (strcat "，累计长度=" (rtos dist1 2 0) "mm。"))
				(setq pt2 pt1)
				(setq m (1+ m))
			)
			(endcmf +dist)
		)
		(pt1 (princ ">>>加油，奥利给！"))
		(t (av:setdrawscale));设置测量比例
	)
)

;;;===========================================================

;;说明:绘线标注长度，封闭时有面积
(defun c:pline-dim(/ *error* +dist data dist distsum endcmf fent h h1 n oldcl p1 p2 ss ssl startpt str)
	(defun *error*(msg)((endcmf +dist)))
	(defun endcmf (+dist / ent ess obj s strl strs)
		;复制到粘贴板
		(setq +dist (vl-string-subst "" "+" +dist))
		(cond
			(av:putcliptext
				(av:putcliptext +dist)
				(princ (strcat "\n已复制：" +dist))
			)
			(t nil)
		)
		(cond
			;单个线段时无需总量
			((< n 3)(entdel fent))
			(T (setvar "peditaccept" 1)
				(command "pedit" "m" ssl "" "J" 1 "")
			)
		)	
		;封闭区域时标注面积和周长
		(if (equal startPT p1 1)
			(progn
				(setq ent (entlast))
				(setq obj (vlax-ename->vla-object ent))
				(setq s (vla-get-area obj))
				(setq s (* s (expt *key:drawscale* 2)))
				(setq strl (cdr (assoc 1 data)))
				(setq strl (vl-string-subst "" "总" strl))
				(setq strs (strcat "S=" (ValToStr s 2 2) "m2"))
				(setq ess (av:dimentls (entlast) strl strs nil))
				(entdel fent)
			)
		)
		(av:endtips n distsum s)
		(setvar "CLAYER" oldcl)
		;(sssetfirst nil ess)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(setq oldcl (getvar "CLAYER"))
	(if av:setactivelayer (av:setactivelayer "LB_绘线标注" 7))
	;单位提示与设置
	(av:startips T)
	(while (/= (type (setq p1 (getpoint "\n起始点:"))) 'list)
		(av:setdrawscale)
	)	
	(setq startPT p1 n 1 distsum 0)
	(setq ssl (ssadd) +dist "")
	;继续下一点
	(princ " >>>下一点")
	(while (setq p2 (getpoint p1))
		;两点画线
		(entmake (list '(0 . "LINE") (cons 10 p1) (cons 11 p2)))
		;标注每个线长
		(setq ss (entlast))
		(setq ssl (ssadd (entlast) ssl))
		(setq dist (distance p1 p2))
		(setq dist (* dist *key:drawscale*))
		(setq str (strcat "l=" (ValToStr dist 1 3) "m"))
		(av:dimentl ss str)
		;;标注总长度
		(setq distsum (+ dist distsum))
		(setq str (strcat "L总=" (ValToStr distsum 1 3) "m"))
		(if (= 1 n)
			(progn ;首次总长线标注
				(setq h (getscalen dist 20 500))
				(and (= 1 n) (setq h1 h))
				(entmaketext str startPT 0 h1 0.4)
				(setq fent (entlast) data (entget fent))
			)
			(progn ;替换总长线标注
				(setq data (subst (cons 1 str) (assoc 1 data) data))
				(entmod data)
			)
		)
		;;提示测量结果
		(princ (strcat "\n第" (rtos n 2 0) "段长=" (rtos dist 2 0)))
		(princ (strcat "，累计长=" (rtos distsum 2 0)))
		(setq +dist (strcat +dist "+" (ValToStr dist 1 3)))
		;准备循环
		(setq p1 p2)
		(setq n (1+ n))
	)	
	(endcmf +dist)
)

;;;===========================================================

;;测量面积
(defun c:manuarea(/ *error* ent obj oldc oldt vlr)
	(princ "-->区域面积测量")
	(defun *error*(msg / l lst s strl strs)
		(cond
			(oldc
				(if vlr (vlr-remove vlr))
				(if hatchent (progn (vla-delete hatchent) (setq hatchent nil)))
				(vla-put-Closed obj 1)
				(setvar "transparencydisplay" oldt)
				(setvar "cetransparency" oldc)
			)
			(t (vla-endundomark *doc*))
		)
		;标注长度与面积
		(setq lst (av:calent ent) l (car lst) s (cadr lst))
		(setq strl (strcat "L=" (ValToStr l 1 3) "m"))
		(setq strs (strcat "S=" (ValToStr s 2 3) "m2"))
		(av:dimentls ent strl strs nil)
		(setvar "CMDECHO" 1)
		(princ)
	)
	(setvar "CMDECHO" 0)
	(cond
		((setq oldc (getvar "cetransparency"))
			(vl-cmdf "pline")
			(command pause)
			;控制指定给单个对象或 ByLayer 的透明度特性是可见还是被禁用。
			(and (setq oldt (getvar "transparencydisplay")) (setvar "transparencydisplay" 1))
			;确认对象
			(setq ent (entlast))
			(setq obj (vlax-ename->vla-object ent))
			;临时浅色显示区域填充
			(setq vlr (vlr-object-reactor (list obj) nil '((:vlr-modified . xgm:vlrmodht))))
			(while (= 1 (getvar "cmdactive")) (command pause))
		)
		(T
			(vla-startundomark *doc*)
			(command "wipeout")
			(while (= 1 (getvar "cmdactive")) (command pause))
			(command "explode" (entlast) "")
			(setvar "peditaccept" 1)
			(command "pedit" "m" (ssget "p") "" "J" 1 "")
			(setq ent (entlast))
		)
	)
	(*error* nil)
)
;反应器执行填充命令
(defun xgm:vlrmodht(obj vlr d / dxf ent)
	(setq ent (vlax-vla-object->ename obj))
	(setq dxf (entget ent))
	(if hatchent (vla-Delete hatchent))
	;设定新对象的透明度级别。
	(setvar "cetransparency" 80)
	(dxf:makehatch dxf)
	(setq hatchent (vlax-ename->vla-object (entlast)))
)
;生成填充
(defun dxf:makehatch(dxf / ptlst)
	(setq ptlst (av:getptn dxf))
	(entmake (append
						 (list
							 '(0 . "HATCH")
							 '(100 . "AcDbEntity")
							 '(67 . 0) ;值为空或 0 时即指对象在模型空间，如果为 1 指在图形空间
							 '(410 . "Model")
							 (cons 8 (getvar "CLAYER")) ;图层名
							 '(62 . 100) ;颜色
							 '(100 . "AcDbHatch") ;子类标记
							 '(10 0.0 0.0 0.0) ;顶点坐标
							 '(210 0.0 0.0 1.0) ;挤出方向（选择性，默认=0，0，1）
							 '(2 . "SOLID") ;填充图案名
							 '(70 . 1) ;实体填充标志（0 = 图案填充；1 = 实体填充）
							 '(71 . 0) ;关联性标志（0 = 无关联；1 = 关联）
							 '(91 . 1) ;边界路径（环）数
							 '(92 . 3) ;边界路径类型标志
							 '(72 . 0) ;“有凸度”标志
							 '(73 . 1) ;“关闭”标志
						 )
						 (list (cons 93 (length ptlst)) ) ;多段线顶点数
						 (mapcar '(lambda (x) (cons 10 x)) ptlst) ;顶点坐标
						 (list
							 '(97 . 0) ;源边界对象数
							 '(75 . 0) ;孤岛检测样式：0 = 普通，1 = 外部，2 = 忽略
							 '(76 . 1) ;填充图案类型：0 = 用户定义, 1 = 预定义, 2 = 自定义
							 '(98 . 0) ;基点数，填0就好，避免计算。
							 '(93 . 3) ;多段线顶点数
						 )
					 )
	)
)
;;==================================================================
;;说明:自动计数
(defun c:MarkPileNum(/ *error* dxf ent ess fgetbox filter flst fristna getbox h key l lst m midp mn msg1 msg2 n oldcl pl pre pt1 pt2 ptlst s ss ssl str t1 t2 t3 v)
	(princ "\n说明：自动计数并标注,可应用于咬合桩、柱、图块等编号")
	;程序返回信息
	(defun *error*(msg / out t2)
		(setvar "clayer" oldcl)
		;终止时返回信息
		(cond
			((member msg '("函数被取消" "函数已取消" ";错误:quit / exit abort"))
				(setq out "计算被强行终止，目前")
			)
			(T (setq out ""))
		)
		;结束计时，统计消耗时间
		(setq t2 (rtos (/ (- (getvar "millisecs") t1) 1000.000) 2 3))
		;汇报信息
		(cond
			(m (princ (strcat "\n>>>" out "共标注" (itoa m) "个对象，编号至" str "，总耗时" t2 "秒。")))
			(t (princ "计算被强行终止，尚未来得及标注"))
		)
		(princ)
	)
	;提醒多选禁忌
	(if (= 1 (getvar "nomutt"))(setvar "nomutt" 0))
	;开始运行
	(setq oldcl (getvar "clayer"))
	;关闭反应器
	(if (and
				av:totalreader_pickfirst_reactor
				(setq v (vlr-added-p av:totalreader_pickfirst_reactor))
			)
		(vlr-remove av:totalreader_pickfirst_reactor)
	)
	;选择对象
	(cond
		((and
			 dxf:assoc8
			 dxf:assoc62
			 (setq ent (entsel "\n请点选起点图元并参照:<回车忽略特性>"))
		 )
			(setq fristna (car ent))
			(setq dxf (entget fristna))
			(setq filter
				(list
					(cons 0 (cdr (assoc 0 dxf))) ;类型
					(dxf:assoc8 dxf) ;图层
					(dxf:assoc62 dxf) ;颜色
				)
			)
			(princ ">>>请框选范围")
		)
		(t (princ "\n提示：先点击选择起始位置，再框选范围"))
	)
	(setq ss (ssget filter))
	(sssetfirst nil ss)
	(setq ssl (sslength ss))
	(setq msg1 (if ss (strcat "已选择" (itoa ssl) "个对象") ""))
	(if (> ssl (setq mn 1500)) ;建议图元数量
		(setq msg1 (strcat msg1 "，计算时间可能过久，建议分类操作，或单次少于" (itoa mn) "个！"))
	)
	(prompt msg1)
	;确认前缀及起始编号
	(if (null *num*) (setq *num* 0))
	(if (null *pre*) (setq *pre* ""))
	(setq msg2 (strcat
							 "\n请输入前缀或起始编号[<A>/<B>/<C>/无前缀<NULL>/重新编号<1>]:"
							 "<" (cond ((wcmatch *pre* "") "无前缀") (t (strcat "前缀" *pre*)))
							 ",起始" (itoa (1+ *num*)) ">"
						 )
	)
	(setq key (getstring msg2))
	(cond
		((null key))
		((wcmatch key ""))
		((> (atoi key) 0) (setq *num* (1- (atoi key))))
		((wcmatch key "NULL") (setq pre ""))
		((wcmatch key "0")(princ "输入无效"))
		(t (setq pre key))
	)
	(cond
		((and pre (null (wcmatch pre *pre*)))
			(setq *pre* pre)
			(setq key (getint (strcat "请输入起始编号[重新编号<1>]:<前缀" key ",起始" (itoa (1+ *num*)) ">")))
			(cond
				((null key))
				((zerop key)(princ "输入无效"))
				(t (setq *num* (1- key)))
			)
		)
		(t nil)
	)
	;取消选择并恢复反应器
	(sssetfirst nil)
	(if v (vlr-add av:totalreader_pickfirst_reactor))
	;关闭重叠对象的默认绘图次序
	(and (getvar "DRAWORDERCTL") (setvar "DRAWORDERCTL" 0))
	;开始计时
	(setq t1 (getvar "millisecs"))
	;取得坐标列表
	(setq n 0)
	(repeat ssl
		(princ (strcat "\r正在分析坐标..." (setq *sbar (Spinbar *sbar))))
    (setq getbox (getbox-midp-minl (ssname ss n)))
    (setq midp (car getbox))
    (setq l (cadr getbox))
    (setq lst (append lst (list (list midp l))))
    (setq n (1+ n))
	)
	;整理列表,确认第1点
	(cond
		(fristna
			(setq fgetbox (getbox-midp-minl fristna))
			(setq flst (list (car fgetbox) (cadr fgetbox)))
			(setq lst (vl-remove-if '(lambda(x) (equal x flst 0.01)) lst))
			(setq lst (cons flst lst))
		)
		(t nil)
	)
	;开始标注编号
	(and
		av:setactivelayer
		(av:setactivelayer "LB_辅助计算层" 7)
	)
	(setq n 0 m (length lst))
	(while lst
		(setq n (1+ n))
		(setq t2 (/ (- (getvar "millisecs") t1) 1000.000))
		(setq t3 (* (- m n) (/ t2 n)))
		(setq *sbar (Spinbar *sbar))
		(princ (strcat "\r正在标注编号<" (itoa n) "/" (itoa m) ",耗时" (rtos t2 2 0) "秒,等待" (rtos t3 2 0) "秒>..." *sbar))
		(setq *num* (1+ *num*))
		(setq str (strcat *pre* (itoa *num*)))
		(setq s (car lst) midp (car s) h (cadr s))
		(setq h (getscalen h 3.000 300))
		(entmaketext str midp (av:ang->rad 45) h -0.5)
		;生成点位坐标列表
		(setq ptlst (cons midp ptlst))
		;对余下坐标进行重新排序
		(setq lst (vl-sort (cdr lst)
								'(lambda (x y) (< (distance midp (car x)) (distance midp (car y))))
							)
		)
	)
	;编号连线
	(setq ptlst (reverse ptlst))
	(setq pl
		(entmakex
			(append
				(list
					'(0 . "LWPOLYLINE")
					'(100 . "AcDbEntity")
					'(100 . "AcDbPolyline")
					'(62 . 250)
					(cons 90 (length ptlst))
				)
				(mapcar '(lambda (pt)(cons 10 pt)) ptlst)
			)
		)
	)
	;(command "DRAWORDER" (ssadd pl) "B")
	;最终显示标注信息
	(*error* msg)
)

;;===================================================
;;说明:按比例尺转换测量结果为字符串
;;参数:v:测量结果，单位mm
;;参数:e:长度为1，面积为2
;;参数:n:保留小数位数
;;返回:转换后的字符串值
;;(ValToStr 2000 2 3)
(defun ValToStr(v e n)
	(setq v (float v))
	(setq v (/ v (expt 1e3 e)))
	(rtos v 2 n)
)
;;说明:设置绘图比例
;;返回:无
(defun av:setdrawscale(/ dist1 dist2 key msg oldsc oldscstr pt1 pt2 vlrstate)
	(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0));信息正常显示
	(if (and
				av:totalreader_pickfirst_reactor
				(vlr-added-p av:totalreader_pickfirst_reactor)
			)
		(setq vlrstate "/自动计算(关C)")
		(setq vlrstate "/自动计算(开O)")
	)
	(setq oldsc *key:drawscale*)
	(setq oldscstr (av:startips nil))
	(setq msg
		(strcat
			"\n指定测量比例[1/10/50/100/1000/参照(R)"
			vlrstate 
			"]<当前1:"
			oldscstr
			(if (wcmatch oldscstr "1") ">" "，回车1:1>")
			" 1:"
		)
	)
	(initget "R C O")
	(setq key (getreal msg))
	(cond
		((null key)
			(setq *key:drawscale* 1.0)
		)
		((member (type KEY) (list 'REAL 'INT))
			(setq *key:drawscale* key)
			;(princ "yes")
		)
		((= key "C")
			(if (vlr-remove av:totalreader_pickfirst_reactor)
				(princ "已关闭自动选中计算功能")
			)
		)
		((= key "O")
			(if (vlr-add av:totalreader_pickfirst_reactor)
				(princ "已打开自动选中计算功能")
			)
		)
		((= key "R")
			(progn
				(setq pt1 (getpoint "\n请指定开始点: "))
				(setq pt2 (getpoint pt1 ">>>请指定下一点:"))
				(grdraw pt1 pt2 7 0)
				;(getdist pt1)
				(if (setq dist1 (distance pt1 pt2))
					(progn
						(princ (strcat "图上距离=" (rtos dist1 2 3) "mm"))
						(setq dist2 (getreal "\n实际距离<单位:mm>="))
					)
					(setq *key:drawscale* oldsc)
				)
				(if dist2
					(setq *key:drawscale* (/ dist2 dist1))
					(setq *key:drawscale* oldsc)
				)
			)
		)
		(t nil)
	)
	(cond
		((and
			 key
			 (= 'str (type key))
			 (member (strcase key) (list "C" "O"))
		 )
		)
		((= oldsc *key:drawscale*)
			(princ (strcat ">>>>原比例尺1:" oldscstr "没有变化！"))
		)
		(t (princ (strcat ">>>>新的测量比例已指定为1:" (av:startips nil))))
	)
	(princ)
)

;=====================================================

;;说明:计算比例长度，例如(getscalen 5000 9.000 800)
;;参数:l:原始长度
;;参数:m:缩小倍数；如是放大(/ 1 8)
;;参数:maxl:最大长度
;;返回:计算后实际长度
(defun getscalen(l m maxl)(cond ((> (setq l (/ l (float m))) maxl) maxl) (t l)))
;;说明:生成单行文本
;;参数:str:需要写入的文本内容
;;参数:pt:插入点
;;参数:ang:文字旋转角度
;;参数:h:文字高度
;;参数:sc:相对插入点文字偏移字高比例（可正可负）
;;返回:entmake生成的文本属性
(defun entmaketext(str pt rad h sc)
	(setq pt (polar pt 
						 (+ rad (/ pi 2)) ;指定角度
						 (* h sc) ;偏移距离
					 )
	)
	(entmake
		(list
			'(0 . "TEXT")
			(cons 1 str) ;文字内容
			(cons 11 pt) ;插入点
			(cons 40 h) ;字高
			(cons 50 rad) ;旋转弧度
			(cons 10 '(0 0 0)) ;控制是否显示
			(cons 72 1) ;控制点位置
		)
	)
)
;;说明:获取围盒中心点及短边长
;;参数:en:单个对象
;;返回:列表list，第1个参数，围盒中心点，第2个参数，围盒短边长
(defun getbox-midp-minl(en / ab minl midpt minpt maxpt obj)
	(setq obj (vlax-ename->vla-object en))
	(vla-getboundingbox obj 'minpt 'maxpt)
	;取得中点坐标并Z轴归零
	(setq midpt (mapcar
								(function (lambda(e1 e2 e3) (* (+ e1 e2) e3)))
								(setq minpt (vlax-safearray->list minpt))
								(setq maxpt (vlax-safearray->list maxpt))
								(list 0.5 0.5 0)
							)
	)
	;围盒边长
	(setq ab (mapcar
						 (function (lambda (e1 e2) (- (max e1 e2) (min e1 e2))))
						 minpt
						 maxpt
					 )
	)
	;围盒短边长
	(setq minl (min (car ab) (cadr ab)))
	(list midpt minl)
)

;=======================================
(vl-load-reactors)
;选择自动统计测量
(defun av:totalreader-pickfirst (a b / en i l lst s ss suml sums)
	(eq 1 (logand 1 (getvar "pickfirst")))
	(cond
		((setq ss (ssget "_i" '((0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE,ARC,LINE"))))
			(setq i 0 suml 0 sums 0)
			(repeat (sslength ss)
				(setq en (ssname ss i))
				(setq i (1+ i))
				(setq lst (av:calent en))
				(setq l (car lst))
				(setq s (cadr lst))
				(setq suml (+ l suml))
				(setq sums (+ s sums))
			)
			(av:endtips (1+ i) suml sums)
		)
		(t nil)
	)
)
;;;测量统计反应器开关，在比例设置与工具箱设置中控制
(or av:totalreader_pickfirst_reactor
	(setq av:totalreader_pickfirst_reactor
		(vlr-set-notification
			(vlr-miscellaneous-reactor nil '((:vlr-pickfirstmodified . av:totalreader-pickfirst)))
			'active-document-only
		)
	)
)
;(or *nomuttoff* (setq *renomuttoff* (vlr-command-reactor nil '((:vlr-commandWillStart . av:nomutt)))))
;;;反应器复原nomutt参数
(defun av:nomutt (a b)(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0)))
(or *nomuttoff*
	(setq *nomuttoff*
		(vlr-command-reactor nil
			'((:vlr-commandWillStart . av:nomutt)
				 (:vlr-unknownCommand . av:nomutt)
				 (:vlr-commandEnded . av:nomutt)
				 (:vlr-commandCancelled . av:nomutt)
				 (:vlr-commandFailed . av:nomutt)
			 )
		)
	)
)
;反应器开关，在info功能设置中手动控制
(defun totalreader_pickfirst_reactor (/ env v)
	(setq v (vlr-added-p av:totalreader_pickfirst_reactor))
	(setq env (getenv "autototalreader"))
	(if (= "1" env)
		(and (null v) (vlr-add av:totalreader_pickfirst_reactor))
		(and v (vlr-remove av:totalreader_pickfirst_reactor))
	)
)
(totalreader_pickfirst_reactor)
;;显示最后绘制的单个图形
(defun sssentlast()
	(while (= 1 (getvar "cmdactive")) (command pause))
	(if (vlr-added-p av:totalreader_pickfirst_reactor)
		(sssetfirst nil (ssadd (entlast)))
	)
	(princ)
)



(princ)



