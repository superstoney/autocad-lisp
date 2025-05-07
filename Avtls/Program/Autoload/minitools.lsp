(vl-load-com)

;按需加载快捷键（命令unpgp必须为全局函数！！！）
(defun av:acad-pgp(/ av:acad-pgp-autodesk)
	(defun av:acad-pgp-autodesk(/ lst p runcmd spl)
		(defun spl(sym n)(terpri)(repeat n (princ sym)))
		(defun runcmd(lst s / a1 a2 a3 cmd)
			(setq a1 (car lst) a2 (cadr lst) a3 (caddr lst))
			(setq cmd (strcat "(defun c:" a1 "()(princ \"" a2 "\")(command " a3 ")(princ))"))
			(eval (read cmd)) (terpri)
			(and s (princ (strcat a1 " " a2)))
		)
		(setq lst
			(list
				(list "RA" "-->射线 " "\"ray\"")
				
				(list "FV" "-->移动 " "\"move\"")
				(list "CC" "-->连续复制 " "\"copy\" (ssget) \"\" \"m\"")
				(list "WW" "-->镜像 " "\"mirror\"")
				(list "FS" "-->通过偏移 " "\"offset\" \"t\"")
				(list "SC" "-->参照缩放 " "\"scale\" (ssget) \"\" pause \"r\" pause pause pause")
				(list "MA" "-->特性匹配 " "\"matchprop\"")
				
				(list "DY" "-->动态拉伸 " "\"lengthen\" \"dy\"")
				(list "3R" "-->三维动态观察器 " "\"3dorbit\"")
				
				(list "TMP" "-->临时文件夹 " "(av:opendir (getenv \"tmp\") nil)")
			)
		)
		(setq p (/= 1 (av:getenv "mypgp")))
		(and p (spl "=" 40))
		(foreach l lst (runcmd l p))
		(and p (spl "=" 40))
		(av:setenv "mypgp" 1)
	)
	;开始运行主程序
	(av:acad-pgp-autodesk)
	;此含义有在seting中使用
	(defun c:unpgp()
		(foreach l
			(mapcar '(lambda(x) (eval (read (strcat "'c:" x))))
				(list "fg" "ra" "fv" "cc" "ww" "fs" "sc" "dy" "3r" "tmp" "unpgp" "ar")
			)
			(vl-acad-undefun l)
		)
		(av:setenv "mypgp" 0)
		(princ)
	)
	(princ)
)
;开图自动运行左手键
(and (= 1 (av:getenv "mypgp"))(av:acad-pgp))


;经典阵列(有对话框)
(defun c:ar(/ cmd)
	(setq cmd
		(cond
			((>= *ver4* 2012) "arrayclassic")
			(t "array")
		)
	)
	(command-s cmd)
)

;;==============================================================
;用VBA新建模板
(defun c:qqnew()
	(vla-activate (vla-add *docs* (findfile "acadiso.dwt")))
)
;特性开关
(defun c:PrONOFF ()
	(IF (= (getvar "opmstate") 1)(vl-cmdf "propertiesclose")(vl-cmdf "properties"))
	(princ)
)
;文件重命名
(defun c:av:rename(/ rename1 rename2)
	;弹窗重命令，自由灵活
	(defun rename1(/ dclid dcls fname1 fname2 key name1 path)
		(setq dcls
			(list
				"RENAME:dialog {"
				"initial_focus = \"name\" ;"
				"    label = \"修改文件名：\" ;"
				"    :row {"
				"        children_fixed_height = true ;"
				"        :edit_box {"
				"            key = \"name\" ;"
				"        }"
				"        ok_only;"
				"    }"
				"    :button {"
				"        is_cancel = true ;"
				"        is_enabled = false ;"
				"        label = \"原理：文件按新命名另存于原路径，并删除原文件!\" ;"
				"    }"
				"}"
			))
		(setq fname1 (vla-get-FullName *doc*))
		(if (wcmatch "" fname1)
			(vla-SendCommand *doc* "qsave ")
			(progn
				(setq path (vl-filename-directory fname1))
				(setq name1 (vl-filename-base fname1))
				(setq dclid (av:loaddialog dcls));新建临时DCL加载并删除
				(new_dialog "RENAME" dclid)
				(set_tile "name" name1)
				(action_tile "accept"
					"(setq name2 (get_tile \"name\"))(done_dialog 1)"
				)
				(setq key (start_dialog))
				(unload_dialog dclid)
			)
		)
		(cond
			((/= key 1))
			((equal name2 name1)
				(princ ">>>文件名没有变化！")
			)
			(name2
				(setq fname2 (strcat path "\\" name2 ".dwg"))
				(vla-SaveAs *doc* fname2 ac2004_DWG)
				(princ (strcat ">>>文件名已改为:" name2))
				(command-s "qsave")
				(vl-file-delete fname1)
			)
			(t nil)
		)
	)
	;快速重命名，添加时间后缀
	(defun rename2(/ dd file1 hh id mm mo n name1 name2 path pre ss suf time yy)
		(setq file1 (vla-get-FullName *doc*))
		(cond
			((wcmatch "" file1)
				(vla-SendCommand *doc* "qsave ")
			)
			(t
				(setq path (vl-filename-directory file1))
				(setq name1 (vl-filename-base file1))
				(setq id " - ");前后标识符
				(setq n (vl-string-search id name1))
				(setq pre (if n (substr name1 1 n) name1))
				(setq time (av:gettime "YYMODDHHMMSS"))
				(setq suf (strcat id time))
				(setq name2 (strcat path "\\" pre suf ".dwg"))
			)
		)
		(cond
			((null name2))
			((findfile name2))
			(name2
				(vla-SaveAs *doc* name2 ac2004_DWG)
				(command-s "qsave")
				(vl-file-delete file1)
				(setq yy (substr time 1 2))
				(setq mo (substr time 3 2))
				(setq dd (substr time 5 2))
				(setq hh (substr time 7 2))
				(setq mm (substr time 9 2))
				(setq ss (substr time 11 2))
				(setq time (strcat yy "年" mo "月" dd "日" hh "时" mm "分" ss "秒"))
				(princ (strcat ">>>文件名时间后缀已修改为:" time))
			)
			(t nil)
		)
	)
	;奔跑吧，少年！
	(setvar "CMDECHO" 0)
	(cond
		((av:is2cmd 60)(rename1))
		(t (rename2))
	)
	(setvar "CMDECHO" 1)
	(princ)
)

;;==============================================================

;打开文件夹
(defun c:Opendir(/ fname path winshell)
	(princ "-->打开当前图纸所在文件夹")
	(setvar "cmdecho" 0)
	(setq fname (vla-get-FullName *doc*))
	(cond
		((wcmatch fname "")
			(vla-SendCommand *doc* "qsave ")
		)
		(t (setq path (vl-filename-directory fname))
			(av:opendir path nil)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

;打开文本，没有则新建
;txt:文本文件；strlst:写入文本的信息列表；v:是否打开文本
(defun av:newtxt(txt strlst v / f)
	(cond
		((findfile txt))
		(t (setq f (open txt "a"))
			(foreach str strlst (write-line str f))
			(close f)
		)
	)
	(and v (startapp "notepad" txt))
)

;工程备忘录
(defun c:bwl (/ file fname path strlst)
	(princ "-->打开工程备忘录记事本")
	(setvar "cmdecho" 0)
	(setq fname (vla-get-FullName *doc*))
	(setq path (vl-filename-directory fname))
	(setq file (strcat path "\\工程备忘录.txt"))
	(cond
		((wcmatch fname "")
			(vla-SendCommand *doc* "qsave ")
		)
		(t (setq strlst (list
											"此记事本为本工程计算过程中的注意事项记录；"
											"备忘录文件与工程图纸保存于同一目录下；"
											"请在此记录即可。"
											"======================================================="
											" "
										))
			(av:newtxt file strlst t)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

;旋转屏幕
(defun c:cadzp (/ *error* ang ang1 ang2 enterview islocked ismode msg1 msg2 pointnil pt1 pviewportobj rotatevp tips w)
	;恢复鼠标与图纸角度
	(defun pointnil(/ view)
		;恢复target只读变量初始值(存储目标点的UCS坐标以用于当前视口中的透视投影)
		(cond
			((equal (getvar "TARGET") '(0.0 0.0 0.0)))
			(t (and
					 (setq view (vla-item (vla-get-Viewports *doc*) 0))
					 (vla-put-target view (vlax-3d-point 0 0 0))
					 (vla-put-ActiveViewport *doc* view)
					 (princ "恢复target至原点")
				 )
			)
		)
		(command-s "ucs" "w");扶正当前坐标系
		;(vl-cmdf "-view" "top" "shademode" "2")
	)
	;进入特定视口
	(defun enterview(/ e obj)
		(cond
			((and
				 (setq e (car (entsel "\n选取视口进入: ")))
				 (= "VIEWPORT" (cdr (assoc 0 (entget e))))
			 )
				(vla-put-mspace *doc* :vlax-true);进入视口
				(setq obj (vlax-ename->vla-object e))
				(vla-put-activepviewport *doc* obj);激活选定视口
			)
			(t (exit))
		)
	)
	;旋转并返回视口
	(defun rotatevp(/ $screen atio ce ch ch2 hh hh2 pt1 pt2)
		(setq $screen (getvar "SCREENSIZE")) 
		(setq ch (getvar "viewsize")) 
		(setq ch2 (/ ch 2)) 
		(setq ce (getvar "viewctr")) 
		(setq atio (/ (car $screen) (cadr $screen))) 
		(setq hh (* atio ch)) 
		(setq hh2 (/ hh 2))
		(setq pt1 (polar (polar ce 0 hh2) (* 1.5 pi) ch2))
		(setq pt2 (polar (polar ce pi hh2) (* 0.5 pi) ch2))
		(command-s "plan" "c")
		(command-s "zoom" "w" pt1 pt2);可以避免返回错位
		;(vla-zoomwindow *acad* (vlax-3d-point pt1) (vlax-3d-point pt2))
	)
	(defun *error* (str)
		(and islocked (vla-put-DisplayLocked pviewportObj :vlax-true))
		(setvar "cmdecho" 1)(princ)
	)
	;开始奔跑
	(vl-load-com)
	(setvar "cmdecho" 0)
	(setq ismode (= (getvar "tilemode") 1))
	(cond ;视口操作
		(ismode) ;模型状态无动作
		((= (getvar ' CVPORT) 1)
			(command-s "ucs" "w");扶正当前坐标系
			(enterview);进入特定视口
		)
		(t nil)
	)
	(and ;解锁当前视口
		(null ismode)
		(setq pviewportObj (vla-get-ActivePViewport *doc*))
		(setq islocked (= (vla-get-DisplayLocked pviewportObj) :vlax-true))
		(vla-put-DisplayLocked pviewportObj :vlax-false)
	)
	;确定旋转角度
	(setq msg1 (strcat "\n指定方向或输入角度:" (if islocked "<回车结束命令>" "<回车恢复视图>")))
	(setq msg2 "\n指定下一点:<回车恢复视图> ")
	(initget 128)
	(setq pt1 (getpoint msg1))
	(cond ;确认角度
		((= (type pt1) 'list) ;当前点选
			(and
				(setq ang (getangle pt1 msg2))
				(setq ang (angtos ang 0 10))
				(setq ang1 (rtos (atof ang) 2 2))
			)
		)
		((= (type pt1) 'str) ;当手工输入角度
			(setq ang (rtos (* -1 (atof pt1)) 2))
			(setq ang2 (rtos (atof pt1) 2 2))
		)
		(t nil)
	)
	(cond ;旋转坐标系
		(ang (command-s "ucs" "z" ang))
		(pt1 (pointnil))
		(islocked (setq w t))
		(t (pointnil))
	)
	(rotatevp);旋转并返回视口
	;提示旋转效果
	(setq tips (cond
							 (ang1 (strcat ">>>图形已向右旋转" ang1 "度"))
							 (ang2 (strcat ">>>图形已向左旋转" ang2 "度"))
							 (t ">>>图形已恢复至世界状态")
						 )
	)
	(and (null w) (princ tips))
	;暂停待缩放结束
	(and islocked pt1
		(getint "\n提示：当前可自由缩放，回车结束命令！")
	)
	(*error* nil)
)




;设置命令行文字靠左
(defun c:habitleft(/ *error*)
	(princ "-->设置命令行文字左对齐")
	(defun *error*(msg)
		(startapp "explorer" "shell:::{80F3F1D5-FECA-45F3-BC32-752C152E456E}")
		(princ)
	)
	(textpage)
	(princ "\n如若出现命令行文字靠右的情况，可在\"平板电脑设置\"中，选择\"惯用左手\"，恢复文字左对齐。")
	(princ "\n若无此选项，则有可能是硬件驱动未能正确安装所致。")
	(getint "\n回车继续...")
	(*error* nil)
)

;=============================================================

;循环切换标签
(defun c:tabup()(tab-loop 1))
(defun c:tabdown()(tab-loop -1))
(defun tab-loop (dir / len n obj tab tablist)
	(vlax-for layout *docs*
		(setq tablist (cons (list (vla-get-Name layout) layout) tablist))
	)
	(setq tablist (reverse tablist))
	(setq tab (assoc (vla-get-Name *doc*) tablist))
	(setq n (vl-position tab tablist))
	(setq len (length tablist))
	(setq n
		(cond
			((= dir 1)
				(if (= n 0)(setq n len))
				(1- n)
			)
			((= dir -1)
				(if (= n (1- len))(setq n -1))
				(1+ n)
			)
			(t nil)
		)
	)
	(setq tab (cadr (nth n tablist)))
	(vla-activate tab)
)
;循环切换布局
(defun c:layoutup()(layout-loop 1))
(defun c:layoutdown()(layout-loop -1))
(defun layout-loop (dir / count layouts n tab tablist)
	(setq layouts (vla-get-layouts *doc*))
	(vlax-for layout layouts
		(setq tablist (cons (vla-get-Name layout) tablist))
	)
	(setq tablist (reverse tablist))
	(setq n (vl-position (getvar "ctab") tablist))
	(setq count (vla-get-count layouts))
	(cond
		((= dir 1)
			(if (= n 0)(setq n count))
			(setq tab (nth (1- n) tablist))
		)
		((= dir -1)
			(if (= n (1- count))(setq n -1))
			(setq tab (nth (1+ n) tablist))
		)
	)
	(setvar "CTAB" tab)
)

;=============================================

;删除DWG文件夹无用文件
(defun cleanviru (/ file)
	(foreach f
		(list ;定义删除文件
			"acad.lsp" "acad.vlx" "acad.fas" "acaddoc.lsp" "acaddoc.fas"
			"acadapp.lsp" "acadapq.lsp" "logo.gif" "plot.txt"
		)
		(setq file (findfile (strcat (getvar "dwgprefix") f)))
		(if (null file) nil
			(progn
				(if (= (av:get-att-RO file) 1)(av:put-att-RO file 0))
				(if (vl-file-delete file)
					(princ (strcat "\n删除文件:" f))
					(princ (strcat "\n无法删除:" f))
				)
			)
		)
	)
)
(cleanviru)


;;说明:清理系统临时文件夹
(defun c:cleantemp(/ fn fso l msg na tmp)
	(princ "-->临时文件夹清理")
  (setq fso (vlax-create-object "scripting.FileSystemObject"))
	(setq tmp (getenv "tmp"))
	(foreach l (kk-getFolds tmp 0)
		(setq msg (vl-catch-all-apply 'vlax-invoke (list fso 'DeleteFolder l :vlax-true)))
		(cond
			((vl-catch-all-error-p msg))
			(t
				(setq na (cadr (fnsplitl l)))
				(princ (strcat "\n已清理文件夹：" na))
			)
		)
	)
	(foreach l (kk-getfiles tmp "*.*" 0)
		(setq msg (vl-catch-all-apply 'vlax-invoke (list fso 'deletefile l :vlax-true)))
		(cond
			((vl-catch-all-error-p msg))
			(t
				(setq l (fnsplitl l))
				(setq fn (strcat (cadr l) (caddr l)))
				(princ (strcat "\n已清理文件：" fn))
			)
		)
	)
  (vlax-release-object fso)
  (princ)
)

;;说明:反向删除
(defun c:redel(/ lst ss sslst)
	(princ "\n选择要保留的图元，未选择的将会被删除！")
	(cond
		((setq ss (ssget))
			(setq sslst (av:ss->ssnlst ss))
			(setq lst (vl-remove-if
									(function (lambda(x) (member x sslst)))
									(av:ss->ssnlst (ssget "A"))
								)
			)
			(foreach l lst (entdel l))
			(vla-ZoomExtents *acad*)
		)
		(t nil)
	)
	(princ)
)

;;=====================================================================
;;做无名块
(defun c:PasteToBlock(/ pt ss)
	(setvar "CMDECHO" 0)
	(vla-StartUndoMark *doc*)
	(while (setq ss (ssget))
		(setq pt (av:getboxpt ss))
		(command-s "cutclip" ss "")
		(command-s "pasteblock" (car pt))
	)
	(vla-EndUndoMark *doc*)
	(setvar "CMDECHO" 1)
	(princ)
)

;;128.1 [功能] 选择集->无名块
;;注意 函数对选择集中存在具有属性的图块及复杂多义线无效
;(defun MJ:BLK-MakeUnNameBlock (/ count entlist ent blk pt)
;	(setq ss (ssget))
;  (setq pt (car (av:getboxpt ss)))
;  (entmake (list
;						 '(0 . "BLOCK")
;						 '(2 . "*U")
;						 '(70 . 1)
;						 (cons 10 pt)
;					 )
;  )
;  (setq count 0)
;  (repeat (sslength ss)
;		(setq ent (ssname ss count))
;    (setq entlist (entget ent))
;    (setq count (1+ count))
;    (entmake entlist)
;  )
;  (setq count 0)
;  (repeat (sslength ss)
;    (setq ent (ssname ss count))
;    (setq count (1+ count))
;    (entdel ent)
;  )
;  (setq blk (entmake '((0 . "ENDBLK"))))
;  (if (princ blk)
;    (entmake (list
;							 (cons 0 "INSERT")
;							 (cons 2 blk)
;							 (cons 10 pt)
;						 )
;    )
;  )
;  blk
;)

;;=====================================================================






(princ)
