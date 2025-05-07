(defun c:fastprinter(/ *error* ab cmdplot ddev devnil fr getdef getdev h index istilm l layplot ls lst n otab pdev pt1 pt2 sdev set3plotdev str x1 x2 y1 y2)
	(defun *error*(msg)
		;(setvar "PLOTTRANSPARENCYOVERRIDE" 0)
		(cond
			(devnil
				(princ "\n>>>设备出错：")
				(and slstnil (princ "找不到打印样式"))
				(and dlstnil (princ "，无打印设备"))
				(and plstnil (princ "，没有图纸列表"))
			)
			(t nil)
		)
		(setvar "CMDECHO" 1)
		(princ)
	)
	(defun cmdplot(/ lst lst1 lst2 lst3 lst4 lst5)
		(setq lst1 (list
								 ;"plot" ;打印命令
								 "y" ;是否需要详细打印配置
								 (getvar "ctab") ;输入布局、模型名称
								 ddev ;输入输出设备的名称
								 pdev ;输入图纸尺寸
								 "M" ;输入图纸单位(I:英寸 M:毫米)
								 ab ;输入图形方向(纵向:P 横向:L)
								 "N" ;是否反向打印
							 )
		)
		(setq lst2
			(cond
				(pt2 (list
							 "W" ;输入打印区域(显示:D 范围:E 图形界限:L 视图:V 窗口:W)
							 pt1 ;左下角
							 pt2 ;右上角
						 )
				)
				(t (list
						 "E";输入打印区域 [显示(D)/范围(E)/布局(L)/视图(V)/窗口(W)] <范围>: e
					 )
				)
			)
		)
		(setq lst3 (list
								 "F" ;输入打印比例(F:布满)
								 "C" ;输入打印偏移(居中打印:C)
								 "Y" ;是否按样式打印
								 sdev ;输入打印样式名称
								 "Y" ;是否打印线宽
							 )
		)
		(setq lst4
			(cond
				((= 1 (getvar "tilemode"))
					(list
						"A" ;输入着色打印设置(按显示:A 线框:W 消隐:H 渲染:R)
					)
				)
				(t (list
						 "y" ;是否按打印比例缩放线宽？
						 "n" ;是否先打印图纸空间？
						 "n" ;是否隐藏图纸空间对象？
					 )
				)
			)
		)
		(setq lst5 (list
								 "n" ;是否打印到文件
								 "Y" ;是否保存对页面设置的修改
								 "y" ;是否继续打印
							 )
		)
		(setq lst (cons "plot" (append lst1 lst2 lst3 lst4 lst5)))
		(apply 'command-s lst)
	)
	(defun index (l n / x)
		(if (> n 0) (cons (cons (itoa (- (length l) (setq x (1- n)))) (nth x l)) (index l x)))
	)
	(defun getdev(lst n / dev idlst k key msg str1)
		(setq lst (reverse lst))
		(setq idlst (index lst (length lst)))
		(initget (apply 'strcat (mapcar '(lambda (x) (strcat (car x) " ")) idlst)))
		(setq str1 (cons "\n可用设备列表： \n" (mapcar '(lambda (x) (strcat "[" (car x) "]=" (cdr x) "\n")) idlst)))
		(setq n (cond (n (itoa n)) (t "1")))
		(setq msg (strcat (apply 'strcat str1) "请输入序号<默认为" n ">:"))
		(setq key (getkword msg))
		(setq k (if key key n))
		(setq dev (cdr (assoc k idlst)))
		(print dev)
		dev
	)
	(defun getdef(def lst / dev l len m n)
		(setq def (strcase def))
		(setq len (length lst))
		(setq n 0)
		(while (setq l (nth n lst))
			(cond
				((wcmatch def (strcase l))
					(setq dev (list l (1+ n)))
					(setq n len)
					(setq m len)
				)
				(t
					(setq n (1+ n))
					(setq m 0)
				)
			)
		)
		(while (setq l (nth m lst))
			(cond
				((vl-string-search def (strcase l))
					(setq dev (list l (1+ m)))
					(setq m len)
				)
				((= (setq m (1+ m)) len)
					(setq dev (list (nth 0 lst) 0))
				)
				(t nil)
			)
		)
		dev
	)
	(defun set3plotdev(flag / darr ddev defd defp defs dlst dlstnil layout m mm msg parr pdev plst plstnil sarr sdev setplotdev setplotpaper setplotstyle slst slstnil)
		(setq defs "黑白细线" defd "pdf" defp "a4")
		(setq layout (vla-get-activelayout *doc*))
		;打印样式手动选择
		(defun setplotstyle(/ sid sn)
			(setq sid (getdef defs slst) sn (cadr sid))
			(av:setenv "plotstyle" (getdev slst sn))
		)
		;打印样式列表
		(setq sarr (vlax-variant-value (vla-GetPlotStyleTableNames layout)))
		(setq slst (vl-catch-all-apply 'vlax-safearray->list (list sarr)))
		(setq slstnil (vl-catch-all-error-p slst))
		;打印样式确认
		(cond
			(slstnil)
			(flag (setq sdev (setplotstyle)))
			((member (setq sdev (av:getenv "plotstyle")) slst))
			(t (setq sdev (setplotstyle)))
		)
		;打印设备手动选择
		(defun setplotdev(/ denv did dn dn1 dn2)
			(and
				(setq denv (av:getenv "plotdev"))
				(setq dn1 (1+ (vl-position denv dlst)))
			)
			(and
				(setq did (getdef defd dlst))
				(setq dn2 (cadr did))
			)
			(setq dn (if dn1 dn1 dn2))
			(av:setenv "plotdev" (getdev dlst dn))
		)
		;打印设备列表
		(setq darr (vlax-variant-value (vla-getplotdevicenames layout)))
		(setq dlst (vl-catch-all-apply 'vlax-safearray->list (list darr)))
		(setq dlstnil (vl-catch-all-error-p dlst))
		;打印机设备与选择
		(cond
			((null sdev))
			(dlstnil)
			(t
				(setq dlst (vl-remove-if (function (lambda(x) (wcmatch x "无"))) dlst))
				(cond
					(flag (setq ddev (setplotdev)))
					((member (setq ddev (av:getenv "plotdev")) dlst))
					(t (setq ddev (setplotdev)))
				)
				(vla-put-ConfigName layout ddev)
			)
		)
		;打印图纸手动选择
		(defun setplotpaper(/ pid pn)
			(setq
				pid (getdef defp plst)
				pn (cadr pid)
			)
			(av:setenv "plotpaper" (getdev plst pn))
		)
		;打印图纸列表
		(setq parr (vlax-variant-value (vla-GetCanonicalMediaNames layout)))
		(setq plst (vl-catch-all-apply 'vlax-safearray->list (list parr)))
		(setq plstnil (vl-catch-all-error-p plst))
		;打印图纸确认
		(cond
			((null ddev))
			(plstnil)
			(t
				(setq plst (vl-remove-if (function (lambda(x) (wcmatch x "none_user_media"))) plst))
				(cond
					(flag (setq pdev (setplotpaper)))
					((member (setq pdev (av:getenv "plotpaper")) plst))
					(t (setq pdev (setplotpaper)))
				)
			)
		)
		;确认是否透明度
		(setq m (getvar "PLOTTRANSPARENCYOVERRIDE"))
		(setq msg (strcat "\n是否使用透明度打印[0否/1按选项/2是]:<默认为" (itoa m) ">"))
		(if (and flag (setq m (getint msg)))
			(setvar "PLOTTRANSPARENCYOVERRIDE" m)
		)
		(setq m (getvar "PLOTTRANSPARENCYOVERRIDE"))
		(setq mm
			(cond
				((= 0 m)
					"不使用透明度打印"
				)
				((= 1 m)
					"根据打印设置中的选项选择是否使用透明度打印"
				)
				((= 2 m)
					"使用透明度打印"
				)
			)
		)
		;当前设备提示
		(cond
			((and sdev ddev pdev)
				(princ (strcat "\n透明样式：" mm))
				(princ (strcat "\n打印比例：布满图纸"))
				(princ (strcat "\n打印样式：" sdev))
				(princ (strcat "\n打印设备：" ddev))
				(princ (strcat "\n图纸尺寸：" pdev))
				(list sdev ddev pdev)
			)
			(t nil)
		)
	)
	;;打印布局
	(defun layplot(/ ab h n obj pt1 pt2 pts ss w)
		(setq ss (ssget "A" (list
													'(0 . "INSERT")
													(cons 2 fr)
													(cons 410 (getvar "ctab"));当前布局
												)
						 )
		)
		(cond
			(ss
				(setq n 0)
				(repeat (sslength ss)
					(setq obj (vlax-ename->vla-object (ssname ss n)))
					(setq pts (lm-get-blkboundingbox obj))
					(setq pt1 (car pts) pt2 (cadr pts))
					(setq w (abs (apply '- (mapcar 'car pts))))
					(setq h (abs (apply '- (mapcar 'cadr pts))))
					(setq ab (cond ((> w h) "L") (T "P")))
					(cmdplot)
					(setq n (1+ n) pt2 nil)
				)
			)
			(t
				(setq ab "L")
				(cmdplot)
			)
		)
	)
	;开始奔跑，判断环境
	(princ "\n命令ltscale(LTS)可设定全局线型比例因子，控制虚线的显示比例。")
	(cond
		((setq ls (cond ((set3plotdev nil)) (t (set3plotdev t))))
			(setq sdev (car ls) ddev (cadr ls) pdev (caddr ls))
		)
		(t
			(setq devnil t)
			(exit)
		)
	)
	(cond
		((setq istilm (= 1 (getvar "tilemode"))))
		(t (vla-put-MSpace *doc* :vlax-false))
	)
	;确认打印范围
	(setq n 1)
	(initget (+ 2 4) "S")
	(setq str (strcat "\n第1个打印范围[设置(S)]<回车布局" (if istilm "批量" "整图") "打印>："))
	(while
		(and
			(setq pt1 (getpoint str))
			(cond
				((eq 'list (type pt1))
					(setq pt2 (getcorner pt1 ""))
				)
				((wcmatch pt1 "S")
					(setq ls (set3plotdev t) sdev (car ls) ddev (cadr ls) pdev (caddr ls))
					(setq
						pt1 (getpoint (strcat "\n第1个打印范围："))
						pt2 (getcorner pt1)
					)
				)
				(t nil)
			)
		)
		(setq n (1+ n))
		(setq str (strcat "\n第" (itoa n) "个打印范围："))
		(setq
			x1 (car pt1)
			y1 (cadr pt1)
			x2 (car pt2)
			y2 (cadr pt2)
			l (abs (- x2 x1))
			h (abs (- y2 y1))
			ab (if (> l h) "l" "p")
		)
		(setq lst (cons (list ab pt1 pt2) lst))
	)
	(setq lst (reverse lst))
	;开始打印
	(setvar "cmdecho" 0)
	(setq fr "*A0*,*A1*,*A2*,*A3*,*A4*,*图框*")
	(cond
		(lst
			(foreach l lst
				(setq ab (car l) pt1 (cadr l) pt2 (caddr l))
				(cmdplot)
			)
		)
		(istilm
			(setq lst (lm:listbox "布局批量打印【可多选】" (layoutlist) t))
			(setq otab (getvar "ctab"))
			(foreach l lst
				(setvar "ctab" l)
				(layplot)
			)
			(setvar "ctab" otab)
			(if lst (princ (strcat "\n图框块名识别码：" fr)))
		)
		(t
			(layplot)
			(princ (strcat "\n图框块名识别码：" fr))
		)
	)
	(*error* nil)
)


