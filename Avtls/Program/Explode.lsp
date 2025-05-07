
;;获取批量对象的范围对角点
(defun av:GgetSSBoxPoint(ss 3d / maxp maxx maxy minp minx miny obj pt1 pt2 sslst x1 x2 y1 y2)
	(setq sslst (av:ss->ssnlst ss))
	(foreach l sslst
		(setq obj (vlax-ename->vla-object l))
		(vla-GetBoundingBox obj 'minp 'maxp)
		(setq
			minp (vlax-safearray->list minp)
			maxp (vlax-safearray->list maxp)
			x1 (car minp)
			y1 (cadr minp)
			x2 (car maxp)
			y2 (cadr maxp)
		)
		(if minx nil (setq minx x1))
		(if miny nil (setq miny y1))
		(if maxx nil (setq maxx x2))
		(if maxy nil (setq maxy y2))
		(setq
			minx (min minx x1)
			miny (min miny y1)
		)
		(setq
			maxx (max maxx x2)
			maxy (max maxy y2)
		)
	)
	(setq
		pt1 (list minx miny 0.0)
		pt2 (list maxx maxy 0.0)
	)
	(cond
		(3d
			(setq pt1 (vlax-3D-point pt1))
			(setq pt2 (vlax-3D-point pt2))
		)
	)
	(list pt1 pt2)
)


;;炸碎图形及文字
(defun c:BlastAll(/ fn ismax n obj pt12 ss ssn)
	(setvar "cmdecho" 0)
	(cond
		((and (= 0 (getvar "tilemode")) (/= 1 (getvar "cvport")))
			(command "vpmax")
			(setq ismax t)
		)
	)
	(while (setq ss (ssget '((0 . "~VIEWPORT"))))
		;(setq lst (av:GgetSSBoxPoint ss t))
		;(setq pt1 (car lst))
		;(setq pt2 (cadr lst))
		;(Vlax-Invoke-Method *acad* 'ZoomWindow pt1 pt2)
		(command "zoom" "o" ss "")
		(setq pt12 (cadddr (av:getscr4pt)))
		(setq fn (vl-filename-mktemp nil nil ".wmf"))
		;(while (= 1 (getvar "cmdactive")) (command pause))
		(command "wmfout" fn ss "")
		(repeat (setq n (sslength ss))
			(setq ssn (ssname ss (setq n (1- n))))
			(setq obj (vlax-ename->vla-object ssn))
			(vla-Erase obj)
		)
		(command "wmfin" fn pt12 "2" "" "")
		(setq obj (vlax-ename->vla-object (entlast)))
		(vla-Explode obj)
		(vla-Erase obj)
		(vl-file-delete fn)
		(Vlax-Invoke-Method *acad* 'ZoomPrevious)
	)
	(cond
		(ismax
			(command "vpmin")
			(vla-put-MSpace *doc* :vlax-true)
		)
	)
	(if (and ss acet-sys-lmouse-down) ;判断是否安装了ET
		(princ "\n提示：如若有实体文字没能炸碎，可尝试命令TXTEXP，文字分解成线")
	)
	(setvar "cmdecho" 1)
	(princ)
)

;======================================================================================
;一炸到底
(defun c:ExplNestBlk(/ fil ss)
	(setvar "cmdecho" 0)
	(setvar "qaflags" 1)
	(setq fil (list (cons 0 "insert")))
	(setq ss (ssget fil))
	(while (setq ss (ssget "p" fil))
		(command-s ".explode" ss "")
	)
	(setvar "qaflags" 0)
	(setvar "cmdecho" 1)
	(princ)
)
;======================================================================================
;智能炸块
(defun c:smartexpl(/ *error* *sbar attlst blolst cutlst data enlst filter inslst m msg n na obj os reflst ss1 ss2 ssn thco total ty)
	(defun *error*(str)
		(vla-EndUndoMark *doc*)
		(setvar "osmode" os)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(vla-StartUndoMark *doc*)
	(setq filter (list (cons 0 "INSERT,TCH_OPENING")))
	(setq
		ss1 (ssget filter)
		ss2 ss1
		thco 0
	)
	(repeat (setq n (sslength ss1))
		(setq ssn (ssname ss1 (setq n (1- n))))
		(setq ty (cdr (assoc 0 (entget ssn))))
		(cond
			((wcmatch ty "TCH_OPENING")
				(command "explode" ssn)
				(ssdel ssn ss2)
				(setq thco (1+ thco))
			)
		)
	)
	(setq enlst (av:ss->ssnlst ss2))
	(foreach l enlst
		(setq obj (vlax-ename->vla-object l))
		(cond
			((and
				 (setq msg (vl-catch-all-apply 'vla-getobject (list (vla-getextensiondictionary obj) "ACAD_FILTER")))
				 (null (vl-catch-all-error-p msg))
			 )
				(setq cutlst (cons l cutlst))
			)
			((vlax-property-available-p obj 'path)
				(setq reflst (cons l reflst))
			)
			((equal (vla-get-ObjectName obj) "AcDbMInsertBlock")
				(setq inslst (cons l inslst))
			)
			;((equal (cdr (assoc 66 (entget l))) 1)
			;	(setq attlst (cons l attlst))
			;)
			((equal (vla-Get-HasAttributes obj) :vlax-true)
				(setq attlst (cons l attlst))
			)
			(t (setq blolst (cons l blolst)))
		)
	)
	(setq n 0 m (itoa (length enlst)))
	(foreach x cutlst
		(princ (strcat "\r正在分解(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(and
			(vlax-property-available-p (vlax-ename->vla-object x) 'path)
			(setq data (entget x))
			(setq na (cdr (assoc 2 data)))
			(setq obj (vla-Item (vla-get-Blocks *doc*) na))
			(vl-catch-all-apply 'vla-Bind (list obj :vlax-false))
		)
		(exblkcut x)
	)
	(foreach x attlst
		(princ (strcat "\r正在分解(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(exblkatt x)
	)
	(foreach x reflst
		(princ (strcat "\r正在分解(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(exblkref x)
	)
	(foreach x inslst
		(princ (strcat "\r正在分解(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(exblkins x)
	)
	(foreach x blolst
		(princ (strcat "\r正在分解(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(vla-Explode (vlax-ename->vla-object x))
		(entdel x)
	)
	(if enlst
		(setq total (+ thco (length enlst)))
		(setq total thco)
	)
	(and (null (zerop total)) (princ (strcat "\n共分解" (itoa total) "个块对象，其中：")))
	(and (null (zerop thco)) (princ (strcat "天正門窗" (itoa thco) "个、")))
	(and reflst (princ (strcat "外部参照" (itoa (length reflst)) "个、")))
	(and inslst (princ (strcat "多重插入" (itoa (length inslst)) "个、")))
	(and cutlst (princ (strcat "裁剪块" (itoa (length cutlst)) "个、")))
	(and attlst (princ (strcat "属性块" (itoa (length attlst)) "个、")))
	(and blolst (princ (strcat "普通块" (itoa (length blolst)) "个。")))
	(*error* nil)
)

(defun exblkatt (x / attb attnam da ent h k n obj p)
	(setq
		attnam (entnext x)
		da (entget attnam)
		k (cdr (assoc 1 da))
	)
	(while (= "ATTRIB" (cdr (assoc 0 da)))
		(cond
			((= "STAR" (cdr (assoc 2 da))))
			(t
				(entmakex
					(list
						'(0 . "TEXT")
						(cons 1 k) ;文本
						(cons 10 (cdr (assoc 10 da))) ;坐标
						(cons 40 (cdr (assoc 40 da))) ;字高
						(cons 50 (cdr (assoc 50 da))) ;角度
						(cons 41 (cdr (assoc 41 da))) ;宽高比
						(cons 7 (cdr (assoc 7 da))) ;文字样式
						(cons 8 (cdr (assoc 8 da))) ;图层
					)
				)
			)
		)
		(setq
			attnam (entnext attnam)
			da (entget attnam)
			k (cdr (assoc 1 da))
		)
	)
	(setq obj (vlax-ename->vla-object x))
	(vla-Explode obj)
	(vla-Erase obj)
	(cond
		((setq ent (ssget "x" (list (cons 0 "ATTDEF"))))
			(repeat (setq n (sslength ent))
				(entdel (ssname ent (setq n (1- n))))
			)
		)
		(t nil)
	)
)

(defun exblkins (x / ent1 ent2 obj)
	(setq ent1 (entget x))
	(setq ent2 (entmakex (list '(0 . "INSERT") (assoc 2 ent1) (assoc 10 ent1))))
	(entdel x)
	(setq obj (vlax-ename->vla-object ent2))
	(vla-Explode obj)
	(entdel ent2)
)

(defun exblkref(x / data msg na obj)
	(setq data (entget x))
	(setq na (cdr (assoc 2 data)))
	(setq obj (vla-Item (vla-get-Blocks *doc*) na))
	(vl-catch-all-apply 'vla-Bind (list obj :vlax-false))
	(setq msg (vl-catch-all-apply 'vla-Explode (list obj)))
	(if (vl-catch-all-error-p msg)
		(vl-cmdf "explode" x)
		(vla-Erase obj)
	)
)

;;分解剪切图块
(defun exblkcut(enl / filter-list in-ss limits lst maxlst minlst n auxptlst limptlst auxl obj pt pt1 pt-list ssn)
	;;    (setq enl (car (entsel)))
	;;生成辅助范围线
	(vl-cmdf "xclip" (ssadd enl) "" "p")
	(setq limits (entlast))
	(setq limptlst (av:getptn (entget limits)))
	;;分解与范围线相交的图块和填充
	(setq pt1 (nth 0 limptlst))
	(setq pt-list (append limptlst (list pt1)))
	(setq filter-list '((0 . "INSERT,HATCH,REGION")))
	(cond
		((setq in-ss (ssget "f" pt-list filter-list))
			(setq n (sslength in-ss))
			(while (setq ssn (ssname in-ss (setq n (1- n))))
				(vl-cmdf "explode" ssn)
			)
		)
		(t nil)
	)
	;;当前选择图元列表
	(setq maxlst (av:ss->ssnlst (ssget "p")))
	;;生成裁剪范围辅助线
	;(vl-cmdf "offset" 1 limits "-1000,-1000" "")
	(vla-Offset (vlax-ename->vla-object limits) 1)
	(setq auxl (entlast))
	(setq auxptlst (av:getptn (entget auxl)))
	(entdel auxl) ;删除辅助线
	;;裁剪范围相交线
	(setq auxptlst (append auxptlst (list pt1 "" "")))
	(setq n 0)
	(vl-cmdf "trim" limits "" "f")
	(while (setq pt (nth (setq n (1+ n)) auxptlst))
		(vl-cmdf pt)
	)
	(entdel limits)
	;;删除范围外图元
	(setq minlst (av:ss->ssnlst (ssget "cp" limptlst)))
	(setq lst
		(vl-remove-if
			(function (lambda(x) (member x minlst)))
			maxlst
		)
	)
	(foreach l lst
		(setq obj (vlax-ename->vla-object l))
		(if (vlax-erased-p obj) nil (entdel l))
	)
)

;======================================================================================
;;解散群组
(defun c:ExplodeGroup(/ *error* ent group grouplst handlelst mp:cdrs obj objlst ss ss-to-objlst)
	(defun *error*(str) (princ))
	(defun ss-to-objlst (ss / n obj objlst ssn)
		(repeat (setq n (sslength ss))
			(setq ssn (ssname ss (setq n (1- n))))
			(setq obj (vlax-ename->vla-object ssn))
			(setq objlst (cons obj objlst))
		)
	)
	;;根据联合列表的前值筛选列表后值
	(defun mp:cdrs (DxfKey ImpLst / TmpLst OutLst)
		(while (setq TmpLst (assoc DxfKey ImpLst))
			(setq
				OutLst (cons (cdr TmpLst) OutLst)
				ImpLst (cdr (member TmpLst ImpLst))
			)
		)
		(reverse OutLst)
	)
	(setq ss (ssget))
	(setq objlst (ss-to-objlst ss))
	(foreach obj objlst
    (setq handlelst
			(cons (vla-get-handle obj) handlelst)
		)
	)
	(setq ent (dictsearch (namedobjdict) "ACAD_GROUP"))
  (setq grouplst (mp:cdrs 350 ent))
  (foreach l grouplst
    (setq group (mp:cdrs 340 (entget l)))
    (foreach grl group
      (setq obj (vlax-ename->vla-object grl))
      (if (vl-position (vla-get-handle obj) handlelst) (entdel l))
		)
	)
	(*error* nil)
)

;======================================================================================
;AutoCAD合并图层命令【laymrg】
;如果图纸用参照方法导入图元，图层名称会有【$】这样的内容添加，会造成图层很多的情况，
;如果把【$】后面名称相同的图层合并，手动的话就比较麻烦。
;要合并相同后缀的图层，可以使用程序来解决.
;一键所有图层去除【#】及【$】前缀的命名
(defun c:unxreflapre(/ ocl ss5)
	;(princ "-->消除外部参照图层名前缀")
	(setvar "cmdecho" 0)
	(vla-StartUndoMark *doc*)
	(princ "\n消除绑定外部参照，在图层名称中所形成的【$】这样内容添加")
	(setvar "blipmode" 0)
	(if (null vlax-dump-object)(vl-load-com))
	(setq ocl (getvar "clayer"))
	(setq ss5 (x1812031));检查图层是否有冻结，锁定或者关闭
	(if (car ss5) (x1812032 ss5));还原图层
	(setvar "clayer" ocl)
	(if (s1811301) ;修改图层名称
		(progn(terpri)(princ ">>>已消除图层前缀并完成合并"))
		(princ "\n提示：没有外部参照绑定后的图层前缀")
	)
	(vla-EndUndoMark *doc*)
	(setvar "cmdecho" 1)
	(princ)
)

;取得所有图层名称
(defun w1810232 (doc / doc lay obj ss tc)
	(setq lay (vla-get-layers doc) ss '())
	(vlax-for obj lay
		(setq tc (vla-get-name obj));取得图层名称
		(setq ss (cons (list tc obj) ss))
	)
	ss
)
;修改图层名称
(defun s1811301 (/ doc n obj s2 ss2 ss3 tc1 tc2 x)
	(setq
		doc (vla-get-activedocument (vlax-get-acad-object));取得当前所有对象集合
		ss2 (w1810232 doc);图层集合
		ss3 (mapcar 'strcase (mapcar 'car ss2));图层名称集合
		;tc1 (getvar "clayer");取得当前图层名称
	)
	;(if (or (vl-string-search "\#" tc1 0)(vl-string-search "$" tc1 0))(setvar "clayer" "0"));如果当前图层需要修改，就转换图层为"0"
	(setvar "clayer" "0")
	(while (setq s2 (car ss2));处理图层
		(setq ss2 (cdr ss2) tc1 (car s2) tc2 tc1 obj (cadr s2))
		(while (vl-string-search "\#" tc2 0)(setq tc2 (vl-string-subst "" "\#" tc2)));处理有#的图层名称
		(while (setq n (vl-string-search "$" tc2 0))(setq tc2 (substr tc2 (+ 2 n))));处理有$的图层名称
		;(while (setq n (vl-string-search "A-" tc2 0))(setq tc2 (substr tc2 (+ 3 n))));处理有A-的名称
		(if (= tc2 "")(setq tc2 "0"));如果是空就修改图层为"0"
		(if (/= tc2 tc1);如果名称发生变化
			(progn
				(if (member (strcase tc2) ss3);2;如果已经有这个图层名称
					(progn
						(if (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list obj)));删除这个图层
							(progn
								(vl-catch-all-apply 'vl-cmdf (list "laymrg" "N" tc1 "" "N" tc2 "Y"))
								;(Command "laymrg" "N" tc1 "" "N" tc2 "Y");如果图层不能删除就合并
							)
						)
					)
					(progn
						(if ;如果没有相同命名的图层就改变图层名称
							(null (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-name (list obj tc2))))
							(setq ss3 (cons (strcase tc2) ss3))
						)
					)
				)
				(setq x t)
			)
		)
	)
	x
)
;还原图层状态
(defun x1812032 (ss / ss x y)
	(setq ss (vl-remove-if '(lambda (x)(vlax-erased-p (cadr x))) ss));排除已经删除的图层
	(mapcar '(lambda (y)(vla-put-lock y :vlax-true));锁定
		(mapcar 'cadr (vl-remove-if-not '(lambda (x)(= (car x) 1)) ss))
	)
	(mapcar '(lambda (y)(vla-put-Freeze y :vlax-true));冻结
		(mapcar 'cadr (vl-remove-if-not '(lambda (x)(= (car x) 2)) ss))
	)
	(mapcar '(lambda (y)(vla-put-LayerOn y :vlax-false));关闭
		(mapcar 'cadr (vl-remove-if-not '(lambda (x)(= (car x) 3)) ss))
	)
)
;;提取图层状态
(defun x1812031 ( / lay ss)
	(setq ss '())
	(vlax-for lay (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
		(if (= (vla-get-lock lay) :vlax-true);如果图层锁定
			(progn
				(vla-put-lock lay :vlax-false) ;解锁
				(setq ss (cons (list 1 lay) ss))
			)
		)
		(if (= (vlax-get-property lay "Freeze") :vlax-true);冻结
			(progn
				(vla-put-Freeze lay :vlax-False);解冻
				(setq ss (cons (list 2 lay) ss))
			)
		)
		(if (= (vlax-get-property lay "LayerOn") :vlax-false);关闭
			(progn
				(vla-put-LayerOn lay :vlax-true);打开
				(setq ss (cons (list 3 lay) ss))
			)
		)
	)
	ss
)
;======================================================================================









(princ)
