(defun av:RemoveSupportPath(lst / av:list->string av:string->list l sup)
	(defun av:string->list (str key / lst n str-n xlen)
		(setq xlen (strlen key))
		(while (setq n (vl-string-search key str))
			(setq str-n (substr str 1 (+ n xlen)))
			(if (/= str-n "") (setq lst (cons str-n lst)))
			(setq str (substr str (+ 1 xlen n)))
		)
		(if (/= str "") (setq lst (cons str lst)))
		(reverse lst)
	)
	(defun av:list->string (lst str)
		(setq b (car lst))
		(foreach l lst (setq b (strcat b str l)))
	)
	(setq sup (av:string->list (strcat (strcase (getenv "ACAD") T) ";") ";"))
	(foreach l lst
		(setq l (strcase (strcat l ";") T))
		(setq sup (vl-remove l sup))
	)
	(setenv "ACAD" (av:list->string sup ""))
)
(defun av:unsetup(/ appath av:getenv cur desktop download fslisp key plist reginfodelete removelist val)
	(defun av:getenv(val-name)
		(setq n (rtos (atof (getvar "acadver")) 2 1))
		(setq reg-key (strcat "HKEY_CURRENT_USER\\Software\\Autodesk\\AutoCAD\\R" n "\\Avtls"))
		(vl-registry-read reg-key val-name)
	)
	;界面恢复
	(setvar "cursorsize" 5)
	(cond
		((getvar "ribbonstate")
			(setvar "menubar" 0)
			(command "Ribbon")
			(command "toolbar" "all" "h")
		)
		(t (foreach l (list "绘图" "修改" "标准" "图层" "样式")(command "toolbar" l "s")))
	)
	;背景颜色恢复
	(cond ((getvar "ribbonstate") (setenv "Background" "3156001")) (t (setenv "Background" "0")))
	(setenv "Layout background" "16777215")
	;卸载菜单文件
	(setvar "filedia" 0)
	(if (menugroup "fstl") (command "menuunload" "fstl"))
	;打开标签
	(and (getvar "filetabstate")(command "Filetab"))
	;恢复LSP默认打开方式
	(setq key "HKEY_CLASSES_ROOT\\AutoLISPFile\\Shell\\open\\command")
	(setq fslisp (strcat "C:\\Windows\\system32\\notepad.exe \"%1\""))
	(vl-registry-write key "@" fslisp)
	;注册信息删除
	(defun reginfodelete(p / fold)
		(defun Fold(p);参数d为全局函数
			(setq d (cons p d))
			(foreach x (vl-registry-descendents p) (Fold (strcat p "\\" x)))
			d ;倒序的文件列表，reverse可倒置扶正后返回
		)
		(foreach f (Fold p) (vl-registry-delete f))
	)
	;注册表清除的两种办法
	(setq appath (strcat "HKEY_CURRENT_USER\\"(vlax-product-key)"\\Applications\\"))
	(setq plist (list
								(strcat "HKEY_CURRENT_USER\\Software\\Autodesk\\AutoCAD\\R" (rtos (atof (getvar "acadver")) 2 1) "\\Avtls")
								(strcat "HKEY_CURRENT_USER\\"(vlax-product-key) "\\Profiles\\" (getvar "cprofile") "\\AVVADrawingPurge-072")
								(strcat appath "AutoCAD DuoTab.dll")
								(strcat appath "HotKeys.NET")
								(strcat appath "HotKeysCaps")
								(strcat appath "DrawingCombiner")
							))
	(foreach p plist (vl-catch-all-apply 'reginfodelete (list p)))
	;恢复选项参数
	(foreach l (list "FontMappingFile" "PrinterConfigDir" "PrinterDescDir" "PrinterStyleSheetDir")
		(vl-catch-all-apply 'setenv (list l (av:getenv l)))
	)
	;卸载支持路径
	(setq desktop (vla-item (vlax-get (vlax-create-object "WScript.Shell") 'SpecialFolders) "Desktop"))
	(setq key "HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders")
	(setq val "{374DE290-123F-4565-9164-39C4925E467B}")
	(setq download (vl-registry-read key val))
	(cond
		((= (type download) 'list) (setq download (cdr download)))
		(t nil)
	)
	(setq RemoveList
		(list download desktop
			(strcat *FSTL_DIR* "\\Support")
			(strcat *FSTL_DIR* "\\Program\\Autoload")
			(av:getenv "Autodir")
		)
	)
	(vl-catch-all-apply 'av:RemoveSupportPath (list removelist))
	(cond
		(AV-EXE-IsRun-Kill
			(AV-EXE-IsRun-Kill "acad.exe" T)
		)
		(t
			;(foreach name reglist (command "SHELL" (strcat "reg delete " name " /f")));批处理删除法
			;不保存关闭并退出程序
			;(vl-load-com)
			(vlax-for item (vla-get-Documents (vlax-get-acad-object))
				(if (equal (vla-get-active item) :vlax-false)
					(vla-close item :vlax-false)
					(setq cur item)
				)
			)
			(vla-sendcommand cur "CLOSE")
			(command "quit" "y")
		)
	)
)


