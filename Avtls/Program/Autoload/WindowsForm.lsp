;生成窗表
(defun c:wf()(c:windowsform))
(defun c:windowsform(/ *error* ass d dict dis e enlst ent entmakedata getfilter getk h key keyhig l la layers lst mccnt mckey mclst mcname method msg n na1 na2 oldcl os pt0 pt01 pt02 pt03 pt11 pt12 pt13 pts s ss ss_visible sss sswindows strh tabstr total ty)
	(defun *error*(msg)
		(cond
			(msg
				(ss_visible (ssget "x") 1)
				(sssetfirst nil ss)
			)
			(t nil)
		)
		(setvar "clayer" oldcl)
		(setvar "OSMODE" os)
		(princ)
	)
	;定义选择集显隐开关，mode：1显示 2隐藏 3亮显 4不亮显
	(defun ss_visible(ss mode)
		(repeat (setq n (sslength ss))
			(redraw (ssname ss (setq n (1- n))) mode)
		)
	)
	;定义选择模式
	(defun getfilter(layers mckey)
		(list
			(cons -4 "<or")
			(cons -4 "<and") (cons 0 "*TEXT") (cons 1 mckey) (cons -4 "and>")
			(cons -4 "<and") (cons 0 "TCH_OPENING") (cons 302 mckey) (cons -4 "and>")
			(cons -4 "<and") (cons 0 "INSERT") (cons 66 1) (cons -4 "and>")
			(cons -4 "or>")
			(cons 8 layers)
		)
	)
	;定义选择集
	(defun sswindows(layers mckey / enlst filter method msg n pts ss ssx)
		(sssetfirst)
		(cond
			((setq ss (ssget (getfilter layers mckey))))
			(t
				(setq msg "\n未有选中内容，请指定门窗层:")
				(setq layers (cdr (assoc 8 (entget (car (entsel msg))))))
				(setq ss (ssget (getfilter layers mckey)))
			)
		)
		(cond
			(ss
				(setq ssx (ssnamex ss))
				;图元列表
				(setq enlst
					(vl-remove-if-not
						(function (lambda (x) (= (type x) 'ENAME)))
						(mapcar 'cadr ssx)
					)
				)
				;窗口参数
				(setq n (caar ssx))
				(setq method (cond ((= 2 n) "WP") ((= 3 n) "CP")))
				;鼠标坐标列表(左上 右上 右下 左下)
				(setq pts (mapcar 'cadr
										(vl-remove-if
											(function (lambda(x) (= -1 x)))
											(assoc -1 ssx)
										)
									)
				)
				(list ss enlst method pts)
			)
			(t
				(princ "提示：没有选中门窗，请修改图层名称识别码！")
				;(exit)
			)
		)
	)
	(princ "-->统计门窗表")
	(setq os (getvar "OSMODE"))
	(setvar "osmode" 0)
	;设置标注层
	(setq oldcl (getvar "clayer"))
	(and
		av:setactivelayer
		(av:setactivelayer "LB_辅助计算层" 7)
	)
	;門窗文字所在层
	(setq layers (strcat "*WIND*,*门*,*窗*,*_TEXT"))
	(princ (strcat "\n图层名识别码:" layers))
	;門窗名识别码
	(setq mckey (strcase "*M*,*C*"))
	(princ (strcat "\n门窗名识别码:" mckey))
	;选择提示
	(princ "\n请选择门窗(文字或属性块均可，无需分解块)")
	;正式开始
	(setq sss (sswindows layers mckey))
	(setq ss (car sss))
	(setq enlst (cadr sss))
	(setq method (caddr sss))
	(setq pts (cadddr sss))
	(sssetfirst nil ss)
	(defun getk()
		(initget "A S D F")
		(getkword (strcat "\n检查当前选择是否需要修改[单+(A)/单-(S)/层+(D)/层-(F)]:<默认为空>"))
	)
	;修改选择集
	(while (setq key (getk))
		(setq key (strcase key))
		(cond
			((wcmatch key "A") ;单个添加
				(ss_visible SS 2)
				(cond
					((setq s (car (sswindows "*" mckey)))
						(setq n (sslength s))
						(repeat n (ssadd (ssname s (setq n (1- n))) ss))
					)
				)
				(ss_visible SS 1)
			)
			((wcmatch key "S") ;单个删减
				;(ss_visible (ssget "x" (list (cons 8 layers))) 2)
				;(ss_visible ss 1)
				(cond
					((setq s (car (sswindows "*" mckey)))
						(setq n (sslength s))
						(repeat n (ssdel (ssname s (setq n (1- n))) ss))
						(ss_visible s 2)
					)
				)
				;(ss_visible (ssget "x") 1)
			)
			((wcmatch key "D") ;按层添加
				(ss_visible SS 2)
				(sssetfirst)
				(while (setq ent (entsel "\n请点选需要添加的门窗图层"))
					(setq la (cdr (assoc 8 (entget (car ent)))))
					(setq s (ssget method pts (getfilter la mckey)))
					(setq n (sslength s))
					(repeat n (ssadd (ssname s (setq n (1- n))) ss))
					(ss_visible S 2)
				)
				(ss_visible SS 1)
			)
			((wcmatch key "F") ;按层删减
				(ss_visible (ssget "x" (list (cons 8 layers))) 2)
				(ss_visible ss 1)
				(sssetfirst)
				(while (setq ent (entsel "\n请点选需要删减的门窗图层"))
					(setq la (cdr (assoc 8 (entget (car ent)))))
					(setq s (ssget method pts (getfilter la mckey)))
					(setq n (sslength s))
					(repeat n (ssdel (ssname s (setq n (1- n))) ss))
					(ss_visible S 2)
				)
				(ss_visible (ssget "x") 1)
			)
		)
		(sssetfirst nil ss)
	)
	;排除特殊项
	(setq lst (list nil "" "C" "M" "A/C" "2A/C" "C户型"));需全部大写
	;(setq l (vl-remove-if (function (lambda(x) (member (strcase (car x)) lst))) l))
	;开始形成列表
	(while (> (sslength ss) 0)
		;提取名称
		(setq e (ssname ss 0))
		(ssdel e ss)
		(setq d (entget e))
		(setq ty (cdr (assoc 0 d))) ;取得图元类型
		(cond
			((member ty (list "TEXT" "MTEXT"))
				(setq mcname (cdr (assoc 1 d)))
			)
			((wcmatch ty "TCH_OPENING")
				(setq mcname (cdr (assoc 302 d)))
			)
			((wcmatch ty "INSERT")
				(setq
					mclst (mapcar 'cadr (getatts e))
					na1 (car mclst)
					na2 (cadr mclst) ;台湾图纸会有两个属性
					mcname (if na2 (strcat na1 "/" na2) na1)
				)
			)
			(t nil)
		)
		;核心建构列表(门窗 数量)
		(cond
			((member mcname lst)) ;排除特殊项
			((member mcname (setq ass (assoc mcname l)))
				(setq mccnt (1+ (cdr ass)))
				(setq l (subst (cons mcname mccnt) ass l))
			)
			(t (setq l (cons (cons mcname 1) l)))
		)
	)
	;按名称进行排序
	(setq l (vl-sort l (function (lambda (l1 l2) (< (car l1) (car l2))))))
	;确认表格名称
	(cond
		((null l) (princ "没有门窗") (exit))
		((setq ent (entsel "\n点选表格名称(建议图名)"))
			(setq tabstr (cdr (assoc 1 (entget (car ent)))))
		)
	)
	;字高设置
	(setq dict "avtls"  keyhig "texthight")
	(cond
		((setq h (vlax-ldata-get dict keyhig)))
		(t (setq h (vlax-ldata-put dict keyhig 400)))
	)
	(setq strh (cond ((= (type h) 'int) (itoa h)) (t (rtos h 2 2))))
	(setq msg (strcat "\n点取表格左上角<当前字高" strh "[字高设置(S)],默认前表右侧>："))
	;开始标注
	(initget "S ")
	(setq pt01 (getpoint msg))
	(cond
		((null pt01))
		((= 'list (type pt01)))
		((wcmatch "S" (strcase pt01))
			(and
				(setq dis (getdist "请输入字高<默认不变>："))
				(setq h (vlax-ldata-put dict keyhig dis))
			)
			(progn (princ "当前字高") (princ h))
			(setq pt01 (getpoint "\n点取表格左上角,默认前表右侧>："))
		)
		(t nil)
	)
	(cond
		((= 'list (type pt01)))
		(t
			;确认起始点位置
			(and *tabpt03* (setq pt01 (mapcar '(lambda(x y) (+ x y)) *tabpt03* (list (* 2 h) 0 0))))
			(while (null (or pt01 (setq pt01 (getpoint "\n当前首个表格，请点取左上角：")))))
		)
	)
	;标注表格名称
	(setq pt0 (mapcar '(lambda(x y) (+ x y)) pt01 (list 0 (* 0.5 h) 0)))
	(cond (tabstr) (t (setq tabstr "请修改表格名")))
	(entmake (list '(0 . "TEXT") (cons 1 tabstr) (cons 10 pt0) (cons 40 (* 1.5 h))))
	;标注表格第一条线
	(setq pt02 (mapcar '(lambda(x y)(+ x y)) pt01 (list (* 11 h) 0 0)))
	(setq pt03 (mapcar '(lambda(x y)(+ x y)) pt02 (list (* 4 h) 0 0)))
	(entmake (list '(0 . "LINE") (cons 10 pt01) (cons 11 pt03)))
	(setq pt11 pt01 pt12 pt02 pt13 pt03)
	;为下次表格提供定位点
	(setq *tabpt03* pt03)
	;标注数据与横线
	(defun entmakedata(str1 str2)
		(setq pt1 (mapcar '(lambda(x y) (+ x y)) pt11 (list h (* -1.5 h) 0)))
		(setq pt2 (mapcar '(lambda(x y) (+ x y)) pt12 (list h (* -1.5 h) 0)))
		(entmake (list '(0 . "TEXT") (cons 1 str1) (cons 10 pt1) (cons 40 h)))
		(entmake (list '(0 . "TEXT") (cons 1 str2) (cons 10 pt2) (cons 40 h)))
		(setq pt21 (mapcar '(lambda(x y) (+ x y)) pt11 (list 0 (* -2 h) 0)))
		(setq pt22 (mapcar '(lambda(x y) (+ x y)) pt12 (list 0 (* -2 h) 0)))
		(setq pt23 (mapcar '(lambda(x y) (+ x y)) pt13 (list 0 (* -2 h) 0)))
		(entmake (list '(0 . "LINE") (cons 10 pt21) (cons 11 pt23)))
	)
	;统计数据标注
	(setq total 0)
	(while (> (length l) 0)
		(setq n (cdar l))
		(setq total (+ n total))
		(entmakedata (caar l) (itoa n))
		(setq pt11 pt21 pt12 pt22 pt13 pt23)
		(setq l (cdr l))
	)
	(entmakedata "∑:门窗合计" (itoa total))
	;表格竖向分割线
	(entmake (list '(0 . "LINE") (cons 10 pt01) (cons 11 pt21)))
	(entmake (list '(0 . "LINE") (cons 10 pt02) (cons 11 pt22)))
	(entmake (list '(0 . "LINE") (cons 10 pt03) (cons 11 pt23)))
	(*error* nil)
)
(defun getatts(blk / array att isattblk obj)
	(setq obj
		(cond
			((= (type blk) 'ename) (vlax-ename->vla-object blk))
			(t blk)
		)
	)
	(setq isattblk
		(equal
			(vl-catch-all-apply 'vlax-get-property (list obj 'hasattributes) )
			:vlax-true
		)
	)
	(cond
		((vl-catch-all-error-p (setq att (vl-catch-all-apply 'vlax-invoke-method (list obj 'GetAttributes)))))
		(t (setq array (vlax-safearray->list (vlax-variant-value att))))
	)
	(if isattblk
		(mapcar (function (lambda(x) (list (strcase (vla-get-TagString x)) (vla-get-TextString x) x))) array)
	)
)
