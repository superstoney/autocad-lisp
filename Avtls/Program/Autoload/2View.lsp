;判断并加载双屏菜单
(defun c:2viewset (/ key m1 msg)
	(av:reset2view nil nil);刷新软双屏插件透明定义
	;根据情况判断是否加载双屏菜单，及相应的提示信息
	(cond
		((menugroup "fstl") (if (menugroup "2view") (command "menuunload" "2View")))
		((null (menugroup "2view")) (av:load-menu-2view))
	)
	(if (menugroup "2view") (setq m1 "/或卸载U"))
	(cond (xjymultiviewN) (T (setq xjymultiviewN 1)))
	;开始参数设定或命令执行
	(setq xjymultiviewN (1+ xjymultiviewN))
	(setq msg (strcat "\n请输入新的屏幕数量[2/3/4" (if m1 m1 "") "]:<当前" (itoa xjymultiviewN) "屏>"))
	(if (menugroup "2view") (initget "U "));为卸载双屏菜单做准备
	(setq key (getint msg))
	(cond
		((null key))
		((= key "U")(command "menuunload" "2View"))
		((<= key 2)(setq xjymultiviewN 2))
		((= key 3)(setq xjymultiviewN 3))
		((>= key 4)(setq xjymultiviewN 4))
	)
	(princ (strcat "\n提示：屏幕数量已设为" (itoa xjymultiviewN) "个，请按F1键切换。"))
	(setq xjymultiviewN (1- xjymultiviewN));恢复参数
	(princ)
)
;具体执行的双屏命令，透明命令，并在设置中双击使用
(defun av:2View()
	;根据情况判断是否加载双屏菜单，及相应的提示信息
	(if (null (or (menugroup "fstl") (menugroup "2View")))
		(av:load-menu-2view)
	)
	;判断是否加载双屏功能
	(cond
		((= (getvar "tilemode") 1) (av:2viewcmd))
		(t (princ ">>>在布局中不双屏！"))
	)
	(princ)
)
;根据情况判断是否加载双屏菜单
(defun av:load-menu-2view(/ dcls fn mnu_name)
	(setq 
		mnu_name (vl-filename-mktemp nil "" ".mnu")
		fn (OPEN mnu_name "w")
	)
	(setq dcls (list
							 "***MENUGROUP=2View"
							 "***ACCELERATORS"
							 "[\"F1\"]'2view"
							 "[SHIFT+\"F1\"]^C^C2viewset"
						 ))
	(foreach dcl dcls (write-line dcl fn))
	(close fn)
	(command "menuload" mnu_name)
	(vl-file-delete mnu_name)
	(princ "\n提示：F1键已可实现双屏切换；SHIFT+F1可进行参数设置。")
)
;双屏核心代码，代码来源于"懒人许"
(defun av:2viewcmd(/ h h/2 i midpt pix pt pta ptb sc w w/2 x0 x1 x2 xjymodiarray y0 y1 y2)
	;将当前屏幕两角坐标传给pt
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
	)
	(setq pt (list x1 y1 x2 y2))
	;;;============以下代码为多屏服务=======================================
	;给各编号的屏幕区定义初值,计数器xjymultiviewcount定义初值
	(setq i 0)
	(if (= xjymultiviewN nil)(setq xjymultiviewN 1));设定屏幕数量为N+1
	(if (= xjymultiviewPT nil)
		(while (< i (* 4 xjymultiviewN))
			(setq xjymultiviewPT (cons pt xjymultiviewPT))
			(setq i (+ 1 i))
		)
	)
	(if (= xjymultiviewcount nil) (setq xjymultiviewcount 0))
	;数组表中第n位数据改为new的函数
	(defun xjymodiarray (lst n new / it lst2 le)
		(setq 
			it (nth n lst) 
			lst2 (reverse lst) 
			le (- (length lst) n 1)
		)
		(while (/= (length (setq lst2 (cdr (member it lst2)))) n))
		(while (/= (length (setq lst (cdr (member it lst)))) le))
		(append (reverse lst2) (list new) lst)
	)
	;保存当前屏幕
	(setq xjymultiviewPT (xjymodiarray xjymultiviewPT xjymultiviewcount pt))
	;计数器累加
	(if (< xjymultiviewcount xjymultiviewN)
		(setq xjymultiviewcount (1+ xjymultiviewcount))
		(setq xjymultiviewcount 0)
	)
	;显示下一屏幕
	(setq pt (nth xjymultiviewcount xjymultiviewPT))
	(princ (strcat ">>>第" (itoa (1+ xjymultiviewcount)) "/" (itoa (1+ xjymultiviewN)) "屏"))
	;;;============以上代码为多屏服务=======================================
	;返回旧屏位置
	(setq ptA (list (nth 0 pt) (nth 1 pt)))
	(setq ptB (list (nth 2 pt) (nth 3 pt)))
	(vla-zoomwindow *acad* (vlax-3d-point ptA) (vlax-3d-point ptB))
)
;====================================================

;判断双屏菜单的加载与卸载
(cond
	((menugroup "fstl")
		(if (menugroup "2view") (command "menuunload" "2View"))
	)
	((null (menugroup "2View"))
		(av:load-menu-2view)
	)
)
;=======================================================
;文档激活反应器，定义透明操作命令
(defun av:reset2view (a b)
	(and (getvar "ribbonstate") (vlax-remove-cmd "2view"))
	(vlax-add-cmd "2view" 'av:2view "2View" 5)
)
;文档首次加载时刷新
(av:reset2view nil nil)
;文件激活刷新反应器
(or *DocumentReturnActivated2views* 
	(setq *DocumentReturnActivated2views* (vlr-docmanager-reactor nil '((:vlr-documentToBeActivated . av:reset2view))))
)



(princ)