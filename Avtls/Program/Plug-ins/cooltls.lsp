;图纸处理冷门工具集
(defun c:cooltls (/ *error* dclid dcls key b)
	(defun *error* (msg)(setvar "cmdecho" 1)(princ))
	(setvar "cmdecho" 0)
	(setq dcls (list
							 "cooltls:dialog {"
							 "    initial_focus = \"cancel\" ;"
							 "    label = \"【A维制造】图纸处理冷门工具集\" ;"
							 "    :boxed_column {label = \"1.弹窗消除\" ;"
							 "        :row {"
							 "            :button {key = \"delrecover\" ;label = \"消Recover(&F)\" ;width = 16 ;}"
							 "            :button {key = \"linkclip\" ;label = \"通粘贴板(&B)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"2.戳记消除\" ;"
							 "        :row {"
							 "            :button {key = \"EPS\" ;label = \"屏蔽戳记(&C)\" ;width = 16 ;}"
							 "            :button {key = \"delstamp\" ;label = \"文件去戳(&W)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"3.版本转换\" ;"
							 "        :row {"
							 "            :button {key = \"GetHverLst\" ;label = \"高版查询(&G)\" ;width = 16 ;}"
							 "            :button {key = \"DwgConvert\" ;label = \"DWG 转换(&Z)\" ;width = 16 ;}"
							 "        }"
							 "        :row {"
							 "            :button {key = \"AcmeCAD\" ;label = \"Acme CAD Converter 2022 (&A)\" ;}"
							 "        }}"
							 "    :boxed_column {label = \"4.图形导出\" ;"
							 "        :row {"
							 "            :button {key = \"loadtch\" ;label = \"天正导图(&T)\" ;width = 16 ;}"
							 "            :button {label = \"关闭本面板\" ; is_cancel = true ; width = 16 ;}"
							 ;"            :button {key = \"BClipbrd\" ;label = \"BetterWmf(&E)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"5.杀毒清理\" ;"
							 "        :row {"
							 "            :button {key = \"kill-hy\" ;label = \"鸿业杀毒(&K)\" ;width = 16 ;}"
							 "            :button {key = \"dwg-purge-batch\" ;label = \"垃圾清理(&L)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"6.审图工具\" ;"
							 "        :row {"
							 "            :button {key = \"sync\" ;label = \"视口同步(&D)\" ;width = 16 ;}"
							 "            :button {key = \"compare\" ;label = \"DWG 比较(&Q)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"7.其它工具\" ;"
							 "        :row {"
							 "            :button {key = \"batchprint\" ;label = \"批量打印(&P)\" ;width = 16 ;}"
							 "            :button {key = \"jpgtodxf\" ;label = \"位图矢量(&S)\" ;width = 16 ;}"
							 "        }}"
							 ;"cancel_button;"
							 ;;":button {is_cancel = true ;"
							 ;;"is_enabled = false ;"
							 ;;"label = \"取消对话框\" ;}"
							 ;;":button {fixed_width = true ; is_cancel = true ;width = 5 ;}"
							 "}"
						 )
	)
	(setq dclid (av:loaddialog dcls));新建临时DCL加载并删除
	(new_dialog "cooltls" dclid)
	
	(action_tile "delrecover" "(setq b \"delrecover\")(done_dialog 1)");消除recover复制弹窗
	(action_tile "linkclip" "(setq b \"linkclip\")(done_dialog 1)");消除无法复制到粘贴板的弹窗
	
	(action_tile "EPS" "(setq b \"EPS\")(done_dialog 1)");外挂屏蔽戳记弹窗
	(action_tile "delstamp" "(setq b \"delstamp\")(done_dialog 1)");文件导出去戳记
	
	(action_tile "GetHverLst" "(setq b \"GetHverLst\")(done_dialog 1)");查询CAD无法打开的DWG高版文件
	(action_tile "DwgConvert" "(setq b \"DwgConvert\")(done_dialog 1)");DWG转换
	(action_tile "AcmeCAD" "(setq b \"AcmeCAD\")(done_dialog 1)");DWG版本转换器
	
	(action_tile "loadtch" "(setq b \"loadtch\")(done_dialog 1)");天正
	
	(action_tile "kill-hy" "(setq b \"kill-hy\")(done_dialog 1)");病毒清理
	(action_tile "dwg-purge-batch" "(setq b \"dwg-purge-batch\")(done_dialog 1)");批量清理文件垃圾
	
	(action_tile "sync" "(setq b \"sync\")(done_dialog 1)");同步对图
	(action_tile "compare" "(setq b \"compare\")(done_dialog 1)");审图比较
	
	(action_tile "batchprint" "(setq b \"batchprint\")(done_dialog 1)");批量打印
	(action_tile "jpgtodxf" "(setq b \"jpgtodxf\")(done_dialog 1)");位图矢量化
	
	(action_tile "cancel" "(done_dialog 0)");取消按钮说明
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((null b))
		((wcmatch b "sync")(av:SyncDwgs));视口同步对图
		((wcmatch b "compare")
			(princ "\n说明：选择图形与当前图形对比，允许您高亮显示同一图形的两个修订或不同图形之间的差异")
			(vla-SendCommand *doc* "compare ")
		)
		((wcmatch b "loadtch")
			(princ "说明：天正插件导出T3与分解对象")
			(vla-SendCommand *doc* "loadtchdcl ")
		)
		((wcmatch b "delrecover")
			(princ "说明：清除图形复制时的Recover弹窗")
			(av:delrecover)
		)
		((wcmatch b "linkclip")
			(princ "说明：消除\"无法复制到剪贴板\"弹窗")
			(terpri)
			(av:linkclip)
		)
		((wcmatch b "delstamp")
			;乱刀补丁或DXF方式去教育版戳记
			(av:delstamp)
		)
		((wcmatch b "EPS")
			(princ "说明：EPS2.0打印戳记补丁，已运行，并隐藏于电脑右下角")
			(startapp (av:findfile "EPS2.0.exe"))
		)
		((wcmatch b "GetHverLst")
			(princ "说明：查询无法打开的DWG高版文件")
			(vla-SendCommand *doc* "GetHverLst ")
		)
		((wcmatch b "DwgConvert")
			(princ "说明：DWG版本转换")
			(vla-SendCommand *doc* "DwgConvert ")
		)
		((wcmatch b "AcmeCAD")
			(princ "说明：CAD文件版本转换器，可同时图形查看、转PDF、转位图、去教育版戳记、去复制弹窗、文件修复和压缩")
			(startapp (av:findfile "AcmeCAD.exe"))
		)
		((wcmatch b "batchprint")
			(princ "说明：批量打印图纸(langjs)")
			(cond (c:pldy) (t (vl-load-all (av:findfile "PLDY批量打印.vlx"))))
			(c:pldy)
		)
		((wcmatch b "jpgtodxf")
			(princ "\n说明：位图矢量化")
			(startapp (av:findfile "DocRdr.exe"))
		)
		((wcmatch b "dwg-purge-batch")
			(princ "说明：不开图批量清理DWG文件垃圾")
			(vla-SendCommand *doc* "dwg-purge-batch ")
		)
		((wcmatch b "kill-hy")
			(princ "说明：鴻业CAD病毒查杀工具")
			(startapp (av:findfile "CAD病毒查杀工具.exe"))
		)
		(t nil)
	)
	(*error* nil);恢复原有参数
)
;;=========================================================
;同步对图
(defun av:SyncDwgs(/ agdt)
	(cond
		((and
			 (>= *ver4* 2013)
			 (setq agdt (av:findfile "AganVportSyn2013.dll"))
		 )
			(command-s "netload" agdt)
			(command-s "agdt")
			(princ "\n也可用命令agdt来执行")
		)
		(t (princ "没有此CAD版本的同步插件！"))
	)
)
;;保存当前空白文档并删除旧文件
(defun av:saveasdraw(file suf / ext fullname name path)
	(vla-zoomextents *acad*)
	(setq path (vl-filename-directory file))
	(setq name (vl-filename-base file))
	(setq ext (vl-filename-extension file))
	(setq fname (strcat path "\\" name suf ".dwg"))
	(vla-SaveAs *doc* fname ac2004_DWG)
	(command-s "qsave")
	(while (null (vl-file-delete file))
		(vla-close (vla-item *docs* (strcat name ext)) :vlax-false)
	)
)
;消除复制时的RECOVER弹窗
(defun av:delrecover (/ *error* av:xref file1 file2 noexp path)
	;判断是否分解成功
	(defun noexp (name / lst xrefs)
		(vlax-for b (vla-get-Blocks *doc*)
			(if (= (vla-get-IsXRef b) :vlax-true)
				(setq xrefs (cons (list b (vla-get-name b)) xrefs))
			)
		)
		(setq lst (vl-remove-if '(lambda (x) (/= (cadr x) name)) xrefs))
		(princ (caar lst))
	)
	;有cmd，所以无法用反应器来完成！
	(defun av:xref (file / e name s ss)
		;外部参照插入绑定并分解
		(command-s "-xref" "A" file "0,0,0" "1" "" "0")
		(and (getvar "bindtype") (setvar "bindtype" 1));图层不带前缀
		(setq name (vl-filename-base file))
		(command-s "-xref" "Bind" name)
		(if (setq s (ssget "X" (list'(0 . "insert")(cons 2 name))))
			(while (setq e (ssname s 0))
				(ssdel e s)
				(vla-Explode (Vlax-Ename->Vla-Object e))
				(entdel e)
			)
		)
		;判断是否需要删除旧有文档
		(if (noexp file)
			(progn
				(alert "RECOVER弹窗未能消除，可用天正转T3来实现！")
				(command-s "close" "y")
			)
			(progn
				(av:saveasdraw file "-QC")
				(princ "\n>>>复制时出现的RECOVER弹窗已清除完毕！")
				(princ "如果依然存在弹窗，请转T3格式。")
				;原理就是删除字典中的天正标识
				;(dictremove (namedobjdict) "TCH_DBCONFIG")
			)
		)
	)
	(defun *error*(str)(setvar "cmdecho" 1)(princ))
	;奔跑吧，少年！
	(setvar "cmdecho" 0)
	(cond
		((setq file1 (findfile (vla-get-FullName *doc*)))
			(command-s "qsave")
			(vl-bb-set "pathforxref" file1)
			(alert "功能：消除复制时的RECOVER弹窗；\n\n原因：天正版本不兼容；\n\n方案：请在即将打开的空白文档中再次运行此命令！！！")
			(vla-activate (vla-add *docs* ""))
		)
		((setq file2 (vl-bb-ref "pathforxref"))
			(vl-bb-set "pathforxref" nil)
		)
		(t (setq path (vla-item (vlax-get (vlax-create-object "wscript.shell") 'specialfolders) "desktop"))
			(setq file2 (getfiled "选择需要消除弹窗的文件" (strcat path "\\") "dwg" 0))
			;(setq file file2)
		)
	)
	(if file2 (av:xref file2))
	(*error* nil)
)
;消除复制时\"无法复制到剪贴板\"弹窗
(defun av:linkclip(/ *error* file)
	(defun *error*(str)(setvar "cmdecho" 1)(princ))
	;奔跑吧，少年！
	(setvar "cmdecho" 0)
	(cond
		((setq file (findfile (vla-get-FullName *doc*)))
			(command-s "qsave")
			(vl-bb-set "linkclip" file)
			(command-s "copylink")
			(alert
				"功能：消除复制时\"无法复制到剪贴板\"弹窗；
\n\n作法：请在即将打开的空白文档中，直接\"CTRL+V\"粘贴，或再次运行此命令！！！"
			)
			(vla-activate (vla-add *docs* ""))
		)
		((setq file (vl-bb-ref "linkclip"))
			(vl-bb-set "linkclip" nil)
			(command-s "pasteclip" "0,0")
			(av:saveasdraw file "-QC")
			(princ "\n>>>复制时出现的\"无法复制到剪贴板\"弹窗已清除完毕")
		)
		(t (princ "请在有\"无法复制到剪贴板\"弹窗的文档中运行本命令。"))
	)
	(*error* nil)
)
;教育版戳记消除
(defun av:delstamp (/ bit dwgname dxf file fn)
	(setq bit (cdr (assoc *acadlen* '((39 . "") (47 . "_X64")))))
	(setq fn (strcat "BladeR" (itoa *ver2*) bit ".arx"))
	(cond
		((setq file (av:findfile fn))
			(if (null (member (strcase fn t) (arx))) (arxload file))
		)
		((null (setq dwgname (findfile (vla-get-FullName *doc*))))
			(princ "\n提醒：文件尚未保存，哪来戳记要去！")
		)
		(t (setq dxf (vl-filename-mktemp nil nil nil))
			(vla-SaveAs *doc* dxf ac2004_dxf)
			(av:saveasdraw dwgname "-QC")
			(princ "\n>>>已通过DXF方式消除教育版戳记，并保存为2004格式！")
		)
		(t nil)
	)
)

;=====================================================================================
;查询CAD无法打开的DWG高版文件
(defun c:GetHverLst(/ acmecad av:getdwgver dir f file key len lst lst-all lst-all-msg lst-high lst-high-msg msg n)
	(defun av:getdwgver(file / cadver dwgver handle header lst)
		(setq handle (open file "r"))
		(setq header (substr (read-line Handle) 1 6))
		(close handle)
		(setq lst
			(list
				;(list "AC1018" 3000) ;对应不对，仅测试用
				(list "AC1032" 2018)
				(list "AC1027" 2013)
				(list "AC1024" 2010)
				(list "AC1021" 2007)
				(list "AC1018" 2004)
				(list "AC1015" 2000)
				(list "AC1014" 1998)
			)
		)
		(cadr (assoc header lst))
	)
	(setvar "cmdecho" 0)
	;加载多选文件对话框
	;取得文件列表
	(cond
		((progn
			 (setq lst (av:getexpdirlst) len (length lst))
			 (setq dir (cond (lst (nth (1- len) lst)) (t nil)))
			 (setq lst-all (try-getfiles "请选择DWG文件【取消选择会穿透当前打开的文件夹】" dir nil "*.dwg"))
		 )
		)
		(T
			(setq  n 0)
			;确认穿透文件夹
			(cond
				((null lst))
				((= len 1)(setq dir (car lst)))
				((> len 1)
					(princ "\n没有选择文件，将查询并穿透打开的文件夹")
					(princ "\n当前打开的有下列文件夹")
					(foreach dir lst (princ (strcat "\n文件夹" (itoa (setq n (1+ n))) ": " dir)))
					(setq key (getint (strcat "\n请选择查询文件夹:<默认" (itoa len) ">")))
					(setq dir (nth (1- (cond (key) (t len))) lst))
				)
			)
			;提示当前文件夹
			(if dir
				(progn
					(princ (strcat "\n查询并穿透文件夹：" dir))
					;确认文件列表
					(setq lst-all (vl-catch-all-apply 'kk-getfiles (list dir "*.dwg" 1)))
					;确认文件夹层级过多
					(if (vl-catch-all-error-p lst-all) (setq msg (vl-catch-all-error-message lst-all)))
					;根据打开文件夹类型分别提示信息
					(cond
						((and msg (wcmatch msg "文件类型定义太长"))
							(princ "\n文件夹层级太多，请进入子文件夹工程目录")
							(setq lst-all nil) ;确保没有后续信息提示
						)
						(T nil)
					)
				)
				(princ "\n当前没有打开的文件夹")
			)
		)
	)
	(cond ;取得高版文件列表
		(lst-all
			(setq lst-all-msg (strcat "发现" (itoa (length lst-all)) "个dwg文件"))
			(setq lst-high
				(mapcar '(lambda(f) (if (> (av:getdwgver f) *ver4*) f)) lst-all)
			)
			(setq lst-high (vl-remove-if '(lambda (l) (null l)) lst-high))
		)
		(t nil)
	)
	(cond ;在记事本中列出文件清单
		(lst-high
			(setq lst-high-msg (strcat "查询到" (itoa (length lst-high)) "个高版文件"))
			(setq msg (strcat lst-all-msg "，" lst-high-msg "，当前CAD" (itoa *ver4*) "打不开，请进行版本转换或用高版CAD打开"))
			(setq lst (append (list msg "") lst-high))
			(setq file	(vl-filename-mktemp nil nil ".txt"))
			(setq f (open file "a"))
			(foreach l lst (write-line l f))
			(close f)
			(startapp "notepad" file)
			(and lst-high (setq AcmeCAD (av:findfile "AcmeCAD.exe"))
				(progn(getint "\n回车开启版本转换器")(startapp AcmeCAD))
			)
		)
		((and dir lst-all)
			(princ (strcat "\n>>>在当前打开的文件夹及其子文件夹中，" lst-all-msg "，没有当前CAD打不开的高版本dwg文件"))
		)
		(lst-all
			(princ (strcat "\n>>>选择的文件中，" lst-all-msg "，没有当前CAD打不开的高版本dwg文件"))
		)
		(msg nil) ;文件夹无法穿透
		(dir (princ "\n>>>文件夹及其子文件夹中没有发现dwg文件"))
		(T nil)
	)
	(setvar "cmdecho" 1)
	(princ)
)
;=====================================================================================

;打开鹏业CAD批量转换程序
(defun av:transcad (/ fn)
	(setq fn "TransCADFile.exe")
	(cond
		((and AV-EXE-IsRun-Kill (AV-EXE-IsRun-Kill fn nil))
			(princ "\n提示：CAD云服务已在后台运行，请于右下角打开！")
		)
		(t (startapp (av:findfile fn))
			(princ "\n提示：鹏业CAD云服务转换天正文件效果比较勉强，建议选择天正插件导出！")
		)
	)
	(princ)
)
;加载天正插件面板并操作
(defun c:loadtchdcl (/ b cmdtch dclid dcls fn key)
	(setq dcls
		(list
			"tchkernal:dialog {"
			"    initial_focus = \"13\" ;"
			"    label = \"天正导出T3格式\" ;"
			"    :boxed_column {"
			"        label = \"天正插件\" ;"
			"        :radio_button {"
			"            key = \"11\" ;"
			"            label = \"分解对象\" ;"
			"        }"
			"        :radio_button {"
			"            key = \"12\" ;"
			"            label = \"整图导出(t3)\" ;"
			"        }"
			"        :radio_button {"
			"            key = \"13\" ;"
			"            label = \"批量导出\" ;"
			"        }"
			"        :radio_button {"
			"            key = \"14\" ;"
			"            label = \"图纸保护\" ;"
			"        }"
			"    }"
			"    :boxed_column {"
			"        label = \"鹏业CAD\" ;"
			"        :radio_button {"
			"            key = \"21\" ;"
			"            label = \"云服务批量导出\" ;"
			"        }"
			"    }"
			"    :row {"
			"        :button {"
			"            is_default = true ;"
			"            key = \"accept\" ;"
			"            label = \"确认\" ;"
			"        }"
			"        :button {"
			"            is_cancel = true ;"
			"            key = \"cancel\" ;"
			"            label = \"取消\" ;"
			"        }"
			"    }"
			"}"
		)
	)
	(defun cmdtch(b)
		(cond
			((= b 11)
				(command (cond ((> *ver4* 2012) "texplode") (t "T81_TExplode")))
			)
			((= b 12)
				(command (cond ((> *ver4* 2012) "tsaveas") (t "T81_TSaveAs")))
			)
			((= b 13)
				(cond ((> *ver4* 2012) (command "tbatsave")) (t (princ "低版本无此命令")))
			)
			((= b 14)
				(getint "本命令会生成不能分解和打印的TCH_PROTECT_ENTITY图块，且后续无法恢复，请谨慎操作 <回车继续>")
				(command "tprotect")
			)
			((= b 21) (av:transcad))
			(t nil)
		)
	)
	(cond
		((av:tcharx))
		((setq fn (av:tchfile)) (arxload fn "\n警告：未能成功加载天正插件!"))
		(t (av:transcad))
	)
	(cond
		((av:tcharx)
			(setq dclid (av:loaddialog dcls));新建临时DCL加载并删除
			(new_dialog "tchkernal" dclid)
			(action_tile "11" "(setq b 11)")
			(action_tile "12" "(setq b 12)")
			(action_tile "13" "(setq b 13)")
			(action_tile "14" "(setq b 14)")
			(action_tile "21" "(setq b 21)")
			(action_tile "accept" "(done_dialog 1)")
			(setq key (start_dialog))
			(unload_dialog dclid)
			(if (= 1 key)(cmdtch b))
			(setq b nil)
		)
		(t nil)
	)
	(princ)
)
;=========================================================
;判断是否存在某个词典
(defun av:indict (key)
	(member key
		(mapcar 'cdr
			(vl-remove-if
				'(lambda (a) (/= 3 (car a)))
				(entget (namedobjdict))
			)
		)
	)
)
;判断是否有代理,天正(av:inproxy "AcDbZombieEntity")
(defun av:inproxy (proxy / i j)
  (vl-load-com)
  (setq i 0 j 0)
  (vlax-for o1 (vla-get-blocks *doc*)
    (if (= proxy (vla-get-objectname o1))
      (setq i (1+ i))
    )
    (vlax-for o2 o1
      (if (= proxy (vla-get-objectname o2))
        (setq j (1+ j))
      )
    )
  )
	(or (> i 0)(> j 0))
)

;删除字典中的天正标识
;(dictremove (namedobjdict) "TCH_DBCONFIG")

;图纸打开时，判断是否为天正文件，按需加载插件
(defun loadtchkernal ()
	(cond
		((av:tcharx))
		((or (av:indict "TCH_DBCONFIG") (av:inproxy "AcDbZombieEntity"))
			(if (arxload (av:tchfile) "\n警告：天正插件无法正常加载，可能有图元无法正常显示。")
				(princ "\n>>>天正插件已加载，请检查图形是否显示正常。")
			)
		)
		(t nil)
	)
	(vl-acad-undefun 'loadtchkernal)
)
(loadtchkernal)
;命令直达，天正单文件导出，命令入口位于菜单中
(defun c:tsaveass (/ key loadtcharx msg tchfile)
	(defun loadtcharx(/ key msg)
		(initget "Y N")
		(setq msg "\n是否强制加载天正插件[是(Y)/否(N)]？<默认为否>")
		(setq key (getkword msg))
		(and
			(= key "Y")
			(arxload (av:tchfile))
			(princ "\n>>>天正插件已经加载完成，如需导出，请再次运行本命令。")
		)
	)
	;主程序开始
	(setvar "cmdecho" 0)
	(setq tchfile (av:tchfile))
	(cond
		((av:tcharx)
			;(princ "-->天正整图导出T3格式")
			(command (cond ((> *ver4* 2012) "tsaveas") (t "T81_TSaveAs")))
		)
		((and
			 (or (av:indict "TCH_DBCONFIG") (av:inproxy "AcDbZombieEntity"))
			 tchfile
		 )
			;(princ "-->天正插件加载并整图导出T3格式")
			(arxload tchfile "\n警告：未能成功加载天正插件!")
			(command (cond ((> *ver4* 2012) "tsaveas") (t "T81_TSaveAs")))
		)
		(tchfile
			(princ "\n提示：当前文件非天正所生成，无需此特殊操作！")
			(loadtcharx)
		)
		(T
			(getint "\n没有此版本所对应的天正插件，回车继续，将加载鹏业CAD批量转换程序")
			(av:transcad)
		)
	)
	(if av:initotittle (av:initotittle))
	(setvar "CMDECHO" 1)
	(princ)
)
(princ)












