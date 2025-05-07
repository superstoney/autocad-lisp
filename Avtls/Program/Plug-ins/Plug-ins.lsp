
;;此程序在mnl文件自动加载

(defun c:ov()
	(princ "-->清理重复对象 ")
	(cond
		((>= *ver4* 2012) (command "overkill"))
		(T (if c:scty nil (load (av:findfile "scty.fas") "\n文件scty.fas加载失败"))
			(c:scty)
		)
	)
)

;;;================================================================

;复制并加载DUOTAB标签插件
(defun av:loadduotab(/ copyed f1 f2 f2n fn ls lst m n reg-key v val-data)
	(setvar "CMDECHO" 0)
	;;查询版本号,确定文件年号
	(setq lst (list 2006 2007 2009 2013))
	(setq v t n (length lst))
	(while v
		(setq m (nth (setq n (1- n)) lst))
		(if (>= *ver4* m) (setq v nil))
	)
	;;定义文件名
	(setq fn (strcat "AutoCAD DuoTab for " (itoa m) ".dll"))
	(setq f1 (av:findfile (strcat fn "重命名")))
	(setq f2n (strcat (getenv "temp") "\\" fn))
	;按需复制文件
	(cond ((setq f2 (findfile f2n)))
		(f1 (setq copyed (vl-file-copy f1 f2n)) (setq f2 (findfile f2n)));复制并查询
		(t (princ "\n没有发现适应当前CAD版本的DuoTab标签插件！"))
	)
	;;加载注册表设置参数
	(setq reg-key (strcat "HKEY_CURRENT_USER\\"(vlax-product-key)"\\Applications\\AutoCAD DuoTab.dll"))
	(cond ((setq val-data (vl-registry-read reg-key "LOADER")))
		(f2 (setq ls (list
									 (list "SHOWTHUM" "0");不显示缩略图
									 (list "HideExtension" "1");不显示扩展名
									 (list "UseFlowTab" "True");多行标签显示
									 (list "UseLockLauncher" "1");锁定用该版本CAD打开文件
									 (list "IMEC" "1");命令输入突破输入法限制，直接上屏
									 (list "MenuShowed" "False,False,False,False");按钮隐藏
								 )
				)
			(foreach l ls (vl-catch-all-apply 'vl-registry-write (cons reg-key l)))
		)
		(t
			(and (getvar "filetabstate") (command "Filetab"))
			(princ "没有发现duo标签插！")
		)
	)
	;;根据情况按需加载插件
	(cond
		(copyed (vl-cmdf "netload" f2)) ;垃圾清理后加载
		((and f2 val-data (wcmatch (strcase f2) (strcase val-data)))) ;正常情况不重复加载
		(f2 ;初次安装或位置变化时加载
			(vl-cmdf "netload" f2)
			(and (getvar "filetabstate") (vl-cmdf "Filetabclose"))
		)
		(t nil)
	)
	(setvar "CMDECHO" 1)
	(princ)
)
(av:loadduotab)

;;;================================================================

;热键管理器
(defun av:loadhotkeys(/ cmd file fn loaded reg-key val-name vern vla-data)
	(setvar "cmdecho" 0)
	(cond
		((> *ver2* 17)
			(and
				(setq vern (atof (getvar "acadver")))
				(setq vern (cond ((>= vern 23) 23) ((>= vern 19) 19) (t nil)))
				(setq fn (strcat "hotKeys_r" (rtos vern) ".dll"))
				(setq file (av:findfile fn))
				(setq reg-key (strcat "HKEY_CURRENT_USER\\"(vlax-product-key)"\\Applications\\HotKeys.NET"))
				(setq val-name "LOADER")
				(setq vla-data (vl-registry-read reg-key val-name))
				(setq loaded (wcmatch (strcase file) (strcase vla-data)))
			)
			(cond
				(loaded)
				(file (setq loaded (vl-cmdf "NETLOAD" file)))
				(t nil)
			)
			(setq cmd "keyedit")
		)
		(t
			(and
				(setq fn (strcat "YJT_AdTool_r" (itoa *ver2*) ".arx"))
				(setq file (av:findfile fn))
				(setq loaded (member (strcase fn t) (arx)))
			)
			(cond
				(loaded)
				(file (setq loaded (arxload file)))
				(t nil)
			)
			(setq cmd "YJT_DJKJ")
		)
	)
	(setvar "cmdecho" 1)
	(cond
		((null file) (princ "\n没有目前CAD版本对应的热键管理器，待更新！"))
		((null loaded) (princ "\n热键管理器无法加载！"))
		(t cmd)
	)
)
;加载热键管理器
(av:loadhotkeys)
;打开热键管理器
(defun c:hotkeys()
	(vl-cmdf (av:loadhotkeys))
)






(princ)




