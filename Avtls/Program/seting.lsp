(vl-load-reactors)
;定义空白处双击函数
(or *blankdoubleclick*
	(setq *blankdoubleclick*
		(vlr-Mouse-Reactor nil '((:VLR-beginDoubleClick . blankdoubleclickcallback)))
	)
)
(defun blankdoubleclickcallback (reactorObject point / owner)
	(setq owner (nentselp (trans (car point) 0 1)));判断是否有单击
	(and
		(= 1 (getvar "tilemode"))
		(null owner)
		(= 0 (getvar "cmdactive"))
		(= 1 (av:getenv "2click"))
		(c:seting) ;双击发起的具体命令
	)
)
;文档激活后判断其它文档中的双击功能开关状态
(or *2clickvar*
	(setq *2clickvar* (vlr-docmanager-reactor nil '((:vlr-documentToBeActivated . 2clickvar))))
)
(defun 2clickvar(a b)
	(if (= 0 (av:getenv "2click"))(vlr-remove *blankdoubleclick*)(vlr-add *blankdoubleclick*))
)
;反应器设定cad一直保证这里设置的捕捉点模式
(or *autoosmode*
	(setq *autoosmode*
		(vlr-command-reactor nil
			'((:vlr-unknownCommand . av:setosmode)
				 (:vlr-commandEnded . av:setosmode)
				 (:vlr-commandCancelled . av:setosmode)
				 (:vlr-commandFailed . av:setosmode)
			 )
		)
	)
)
(defun av:setosmode (a b / osmode)
	(and ;启用注册表中记录的捕捉信息
		(= 1 (av:getenv "autoosmode"))
		(setq osmode (av:getenv "osmode"))
		(/= osmode (getvar "osmode"))
		(setvar "osmode" osmode)
	)
	(setvar "nomutt" 0)
)
;====================================================
;个人信息及环境设定
(defun c:set()(c:seting))
(defun c:seting (/ *allss* allss dclid f fn fname gettitle ini key newdwg oldautodir recommend setfkeytoini settitle)
	;(princ "-->工具箱设置")
	;设置控件显示值
	(defun gettitle(/ 2click autoosmode cursorsize dyn f len ltls mypgp n osmode right sl theui1)
		;设置功能键打开状态
		(repeat (setq n 13)
			(set_tile (setq f (strcat "F" (itoa (setq n (1- n))))) (getfkey f))
		)
		;=========================================================
		;自动加载文件夹
		(av:setautodir)
		(set_tile "path" (av:getenv "Autodir"))
		;(mode_tile "rev" 1);锁定按钮
		;=========================================================
		;鼠标右键确认控制，0-1-2-4-8-16
		(setq right (getvar "shortcutmenu"))
		(if (= right 2)(setq right 1)(setq right 0))
		(set_tile "right" (itoa right))
		;指针输入控制，0-1-2-3
		(setq dyn (getvar "dynmode"))
		(if (= dyn 0)(setq dyn 0)(setq dyn 1))
		(set_tile "dyn" (itoa dyn))
		;全程启用捕捉设定
		(setq autoosmode (av:getenv "autoosmode"))
		(if (null autoosmode)(setq autoosmode 0))
		(set_tile "autoosmode" (itoa autoosmode))
		;对像捕捉控制
		(setq osmode (getvar "osmode"))
		(set_tile "osmode" (itoa osmode))
		;光标尺寸控制
		(setq cursorsize (getvar "cursorsize"))
		(set_tile "cursorsize" (itoa cursorsize))
		;=========================================================
		;快捷键控制
		(setq mypgp (av:getenv "mypgp"))
		(if (null mypgp)(setq mypgp 0))
		(set_tile "mypgp" (itoa mypgp))
		;双击控制
		(setq 2click (av:getenv "2click"))
		(if (null 2click)(setq 2click 0))
		(set_tile "2click" (itoa 2click))
		;开图是否启用屏幕菜单
		(setq ltls (av:getenv "ltls"))
		(if (= 1 ltls)(setq ltls 1)(setq ltls 0))
		(set_tile "ltls" (itoa ltls))
		;自动测量长度与面积
		(if (vlr-added-p av:totalreader_pickfirst_reactor)(setq sl "1")(setq sl "0"))
		(set_tile "autototalreader" sl)
		;界面循环包含精简模式
		(cond
			((getvar "ribbonstate")
				(setq theui1 (av:getenv "theui1"))
				(and (null theui1) (setq theui1 0))
				(set_tile "theui1" (itoa theui1))
			)
			(t (set_tile "theui1" "1")
				(mode_tile "theui1" 1)
			)
		)
		;常用状态栏，长度缩短
		(cond
			((getvar "ribbonstate")
				(setq len (av:getenv "len"))
				(and (null len) (setq len 0))
				(set_tile "len" (itoa len))
			)
			(t (set_tile "len" "0")
				(mode_tile "len" 1)
			)
		)
	)
	;创建ini文件
	(cond
		((setq ini (av:findfile (setq fn "info.ini"))))
		(T (setq ini (strcat *FSTL_DIR* "\\Support\\" fn))
			(setq f	(open ini "w"))
			(close f)
		)
	)
	;设置功能键全选按钮控制方案
	(setq *Allss* (vl-ini-get (list ini "Fkey" "Allss")))
	(defun allss(/ setfkey)
		(defun setfkey(v / n)
			(repeat (setq n 13) (set_tile (strcat "F" (itoa (setq n (1- n)))) v))
			(setq *Allss* v)
		)
		(cond
			((null *Allss*) (setfkey "1"))
			((wcmatch "1" *Allss*) (setfkey "0"))
			(t (setfkey "1"))
		)
	)
	;更新菜单按钮写入INI功能键状态
	(defun setfkeytoini(/ key n)
		(princ ">>>刷新菜单")
		(vl-ini-set (list ini "Fkey" "Allss" *Allss*))
		(repeat (setq n 13)
			(setq key (strcat "F" (itoa (setq n (1- n)))))
			(vl-ini-set (list ini "Fkey" key (get_tile key)))
		)
	)
	;设置推荐值
	(defun recommend(/ defkey0 defkey1)
		(defun defkey1()
			(set_tile "right" "1")
			(set_tile "dyn" "0")
			(set_tile "autoosmode" "1")
			(set_tile "osmode" (itoa 7223))
			(set_tile "cursorsize" "100")
			;=================================
			(set_tile "mypgp" "1")
			(set_tile "2click" "0")
			(set_tile "ltls" "0")
			(set_tile "autototalreader" "1")
			(and (getvar "ribbonstate")(set_tile "theui1" "0"))
			(and (getvar "ribbonstate")(set_tile "len" "1"))
			(vl-bb-set "defkey" 1)
		)
		(defun defkey0()
			(set_tile "right" "0")
			(set_tile "dyn" "1")
			(set_tile "autoosmode" "0")
			(set_tile "osmode" (itoa 4643))
			(set_tile "cursorsize" "5")
			;=================================
			(set_tile "mypgp" "0")
			(set_tile "2click" "0")
			(set_tile "ltls" "0")
			(set_tile "autototalreader" "0")
			(and (getvar "ribbonstate")(set_tile "theui1" "0"))
			(and (getvar "ribbonstate")(set_tile "len" "0"))
			(vl-bb-set "defkey" 0)
		)
		(cond
			((= 1 (vl-bb-ref "defkey")) (defkey0))
			(t (defkey1))
		)
	)
	;写入设定值
	(defun settitle (/ 2click autoosmode cursorsize dyn len ltls mypgp osmode right theui1)
		(princ ">>>更新设置")
		;鼠标右键确认
		(setq right (atoi (get_tile "right")))
		(av:setenv "right" right)
		(if (= right 1)(setq right 2)(setq right 11))
		(setvar "shortcutmenu" right)
		;指针输入控制
		(setq dyn (atoi (get_tile "dyn")))
		(av:setenv "dyn" dyn)
		(if (= dyn 0)(setq dyn 0)(setq dyn 3))
		(setvar "dynmode" dyn)
		;全程启用捕捉设定
		(setq autoosmode (atoi (get_tile "autoosmode")))
		(av:setenv "autoosmode" autoosmode)
		;对像捕捉控制
		(setq osmode (atoi (get_tile "osmode")))
		(av:setenv "osmode" osmode)
		(setvar "osmode" osmode)
		;十字光标尺寸控制
		(setq cursorsize (atoi (get_tile "cursorsize")))
		(cond
			((< cursorsize 5)(setq cursorsize 5))
			((> cursorsize 100)(setq cursorsize 100))
			(t nil)
		)
		(av:setenv "cursorsize" cursorsize)
		(setvar "cursorsize" cursorsize)
		;==================================================================
		;左手键开图自动运行
		(setq mypgp (atoi (get_tile "mypgp")))
		(cond
			((= mypgp 1)(av:acad-pgp))
			((= mypgp 0)(if c:unpgp (c:unpgp)))
			(t nil)
		)
		;控制双击面板开关
		(setq 2click (atoi (get_tile "2click")))
		(av:setenv "2click" 2click)
		(if (= 2click 0)(vlr-remove *blankdoubleclick*)(vlr-add *blankdoubleclick*))
		;开图是否启用屏幕菜单
		(setq ltls (atoi (get_tile "ltls")))
		(av:setenv "ltls" ltls)
		(cond
			((and (= 1 ltls) (/= 1 (vl-bb-ref "ltls")))
				(c:ltoolsswitch)
			)
			((and (= 0 ltls) (= 1 (vl-bb-ref "ltls")))
				(dcl-form-close ltools/menu);关闭面板
				(vl-bb-set "ltls" 0);在侧向面板程序中进行相关设定
			)
			(t nil)
		)
		;自动测量长度与面积
		(setenv "autototalreader" (get_tile "autototalreader"))
		(totalreader_pickfirst_reactor);命令位于测量函数文件中
		;循环界面加入精简模式
		(setq theui1 (atoi (get_tile "theui1")))
		(av:setenv "theui1" theui1)
		;常用状态栏，长度缩短
		(setq len (atoi (get_tile "len")))
		(cond
			((= len (av:getenv "len")))
			((= 1 len) (setupstatusbarnum t))
			(t (setupstatusbarnum nil))
		)
		(av:setenv "len" len)
	)
	;主程序开始
	(setvar "cmdecho" 0)
	(setq newdwg nil)
	(setq fname (av:findfile "seting.dcl"))
	(setq dclid (load_dialog fname))
	(new_dialog "seting" dclid "")
	(gettitle);写入控件默认显示值
	(setq oldautodir (av:getenv "Autodir"))
	(action_tile "wechat" "(done_dialog 11)");功能键推荐值
	(action_tile "allss" "(allss)");功能键推荐值
	(action_tile "setfkey" "(setfkeytoini)(done_dialog 13)");设定功能键
	(action_tile "rev" "(av:modifyautodir)");路径修改
	(action_tile "just" "(recommend)");推荐
	(action_tile "accept" "(settitle)(done_dialog 1)");确定
	(action_tile "cancel" "(done_dialog 0)");取消
	(action_tile "reavtlsenv" "(done_dialog 41)") ;环境初始化
	(action_tile "hotkeys" "(done_dialog 42)") ;单键快捷
	(action_tile "1colour" "(done_dialog 43)");单色环境
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((= key 11)(c:AvtlsWechat)) ;二维码
		((= key 13)(createmnu))
		((= key 41)(c:reavtlsenv))
		((= key 42)(c:hotkeys))
		((= key 43)(c:1colour))
		((and (= 0 key) newdwg)
			(vla-activate (vla-add *docs* "")) ;新建文档
		)
		(t nil)
	)
	(setvar "cmdecho" 1)
	(princ)
)
(defun c:AvtlsWechat(/ fn fn1 fn2 lst name)
	(setq fn2 (strcat (getenv "tmp") "\\wechat.dwg"))
	(setq fn1 (av:findfile "nyistjz.svg"))
	(cond
		((findfile fn2))
		(fn1 (vl-file-copy fn1 fn2))
		(t nil)
	)
	(cond
		((findfile fn2)
			(vla-open *docs* fn2 :vlax-true)
			(setq name (strcat (cadr (setq fn (fnsplitl fn2))) (caddr fn)))
			(vlax-for a *docs* (setq lst (cons (cons (vla-get-Name a) a) lst)))
			(vla-activate (cdr (assoc name lst)))
		)
		(t (princ "\n缺少二维码文件！"))
	)
)
;;;=====================================================================
;;根据功能键情况创建mnu菜单文件
(defun createmnu(/ f lst mnu)
	(setq lst (createmnulist))
	(setq mnu	(av:findfile "fstl.mnu"))
	(setq f	(open mnu "w"))
	(cond
		((= (type lst) 'str) (vl-catch-all-apply 'write-line (list lst f)))
		((= (type lst) 'list) (foreach l lst (vl-catch-all-apply 'write-line (list l f))))
	)
	(close f)
	(regenmnu)
)
;;加载造价菜单
;(defun c:ree()((regenmnu)))
(defun regenmnu (/ cui cui1 cui2 ext)
	;(princ "-->刷新菜单")
	(setvar "cmdecho" 0)
	(setvar "filedia" 0)
	(setq ext (if (getvar "ribbonstate") ".cuix" ".cui"))
	(setq cui1 (findfile (strcat "acad" ext)))
	(setq cui2 (av:findfile (strcat "acad" (itoa *ver4*) ext)))
	(setq cui (cond (cui1 cui1) (cui2 cui2) (t (princ "无法找到菜单文件！"))))
	(if (menugroup "fstl") (command-s "menuunload" "fstl"))
	(cond ((menugroup "acad")) (t (command-s "menuload" cui)))
	(command-s "menuload" (av:findfile "fstl.mnu"))
	;添加下拉菜单
	;(setq i 1 n 3);参数N，设置局部下拉菜单数量
	;(while (menucmd (strcat "P" (itoa i) ".1=?")) (setq i (1+ i)));确认菜单数量
	;(setq b (if (getvar "ribbonstate") 2 2));高版退3位，低版退2位
	;(repeat n
	;	(menucmd (strcat "p" (itoa (- i b)) "=+fstl.POP" (itoa n)))
	;	(setq n (1- n))
	;)
	;添加工具条
	(setvar "qaflags" 0)
	(command-s "toolbar" "av_value" "s")
	(setvar "filedia" 1)
	(setvar "cmdecho" 1)
	(princ)
)
;加载本程序后，如果菜单显示异常，则自动更新菜单显示
(if (null (menugroup "acad")) (regenmnu))
;;;;;;=====================================================================
;设定自动加载文件夹路径
(defun av:modifyautodir(/ autoloadcmd new path)
	(defun autoloadcmd(path / file fn lst)
		(alert "路径已修改！\n\n请认真阅读打开的文本文件。")
		;自动加载子文件夹中插件的文件名
		(setq fn "AutoLoadDemo.lsp")
		(setq lst (list
								(strcat ";|文件名：" fn)
								"文件位置：设置的自动加载文件夹内"
								"自动加载文件夹，只能自动启动根目录中的vlx、fas、lsp、arx、dll文件，不包含子文件夹中的插件；"
								"如需自动加载子文件夹中的插件，可在此文本中分割线下编写简单程序即可。"
								"需注意，自动加载文件下的所有子文件夹中，需加载的插件，不得有相同的文件名。"
								""
								";(av:load-fn-cmd fn cmd)"
								"fn：子文件夹中插件文件名"
								"cmd：插件中命令名"
								""
								"例如："
								";(defun c:rjs()(av:load-fn-cmd \"RegFromApp64.exe\" nil))"
								";(defun c:vjs()(av:load-fn-cmd \"系统变量监视器vjs.lsp\" \"vjs\"))"
								";(defun c:ls()(av:load-fn-cmd \"Lisp_v1.5.vlx\" \"lisp\"))"
								""
								"另外：如需自定义侧边栏，可把路径Avzztls\\Program\\Autoload中的文件“Ltoolsider.ini”"
								"复制进自己的自动加载文件夹内;"
								"如些操作，以便在下次更新工具箱时，不会影响到自定义侧边栏。"
								""
								"===========================================|;"
							))
		(setq file (strcat path "\\" fn))
		(av:newtxt file lst t) ;函数位于minitools中
		;(vla-activate (vla-add *docs* "")) ;新建文档
		(setq newdwg t)
	)
	(cond
		((wcmatch
			 (setq path (get_tile "path"))
			 (av:getenv "Autodir")
		 )
			(if (setq new (LM:DirectoryDialog "\n请选择需自动加载的目录" nil 512))
				(progn
					(setq new (av:setenv "Autodir" (strcase new t)))
					(set_tile "path" new)
					(av:AddSupportPath (list new))
					(av:RemoveSupportPath (list oldautodir))
					(autoloadcmd new)
					;(wyl:autoloadapp);导致死机
				)
			)
		)
		((findfile path)
			(av:setenv "Autodir" path)
			(av:AddSupportPath (list path))
			(av:RemoveSupportPath (list oldautodir))
			(autoloadcmd path)
			;(wyl:autoloadapp);导致死机
		)
		((wcmatch path "")
			(av:setenv "Autodir" "请粘贴或选择路径")
			(av:RemoveSupportPath (list oldautodir))
			(set_tile "path" (av:getenv "Autodir"))
			(alert "路径已清除！")
		)
		(t (alert "路径不正确！"))
	)
	;(wyl:autoloadapp);导致死机
)
;建立注册表临时文件（参数非空为缩短，参数为空则恢复全显）
(defun setupstatusbarnum (flag / app-key bar-key ls0 ls1)
	(setq bar-key (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles\\" (getvar "cprofile") "\\StatusBar"))
	(foreach l (list "DockAboveStatusbar" "ForcedDocAbove")
		(vl-registry-write bar-key l 0)
	)
	(setq ls1
		(list
			(list "CursorCoordinatesPane" 0);坐标
			(list "Paper/ModelPane" 1);模型空间
			(list "GridPane" 1);格栅
			(list "SnapPane" 0);捕捉模式
			(list "InferRelationshipPane" 0);推断约束
			(list "DynInputPane" 1);动态输入
			(list "OrthoPane" 1);正交模式
			(list "PolarPane" 1);极轴追踪
			(list "IsoDraftPane" 0);等轴测草图
			(list "OTrackPane" 1);对象捕捉追踪
			(list "OSnapPane" 1);二维对象捕捉
			(list "LineWeight" 0);线宽
			(list "TransparencyPane" 0);透明度
			(list "SelectionCycling" 0);选择循环
			(list "3DOSnapPane" 0);三维动态捕捉
			(list "DynamicUCSPane" 0);动态UCS
			(list "SelectionFilterPane" 0);选择过滤
			(list "GizmoPane" 0);小控件
			(list "AnnotationVisibility" 0);注释可见性
			(list "AutoScale" 0);自动缩放
			(list "AnnotationScales" 0);注释比例
			(list "WorkspaceSwitchingIconPane" 0);切换工作空间
			(list "AnnoMonitorState" 0);注释监视器
			(list "UnitsPane" 0);单位
			(list "QuickProperties" 0);快捷特性
			(list "LockUIPane" 0);锁定用户界面
			(list "IsolateObjectPane" 0);隔离对象
			(list "HardwareAccelerationPane" 0);图形性能
			(list "CleanScreenPane" 0);全屏显示
		))
	(setq ls0 (list "CursorCoordinatesPane" "Paper/ModelPane" "GridPane" "SnapPane" "InferRelationshipPane" "DynInputPane" "OrthoPane" "PolarPane" "IsoDraftPane" "OTrackPane" "OSnapPane" "LineWeight" "TransparencyPane" "SelectionCycling" "3DOSnapPane" "DynamicUCSPane" "SelectionFilterPane" "GizmoPane" "AnnotationVisibility" "AutoScale" "AnnotationScales" "WorkspaceSwitchingIconPane" "AnnoMonitorState" "UnitsPane" "QuickProperties" "LockUIPane" "IsolateObjectPane" "HardwareAccelerationPane" "CleanScreenPane")
	)
	(setq app-key (strcat bar-key "\\Application"))
	(cond
		(flag
			(foreach l ls1 (vl-catch-all-apply 'vl-registry-write (cons app-key l)))
			(princ "\n提示：控制状态栏功能按钮显隐，将在AutoCAD下次启动时生效！")
		)
		(t
			(foreach l ls0 (vl-registry-write app-key l 1))
			(princ "\n提示：状态栏功能按钮已全部打开，将在AutoCAD下次启动时生效！")
		)
	)
)

;;;=================================================================
;确认用户文件夹
(defun av:setautodir(/ autodir)
	(cond
		((null (setq autodir (av:getenv "Autodir")))
			(setq autodir (av:setenv "Autodir" "请粘贴或选择路径"))
		)
		((findfile autodir))
		(t (setq autodir (av:setenv "Autodir" "请粘贴或选择路径")))
	)
	autodir
)

;自动加载文件内全部程序，需要比较优先启动
(defun wyl:autoloadapp(/ ext inf lst_dir lst_fnm)
	(setq lst_dir
		(list
			(strcat *FSTL_DIR* "\\Program\\Autoload")
			(av:setautodir)
		)
	)
	(av:AddSupportPath lst_dir)
	(foreach l lst_dir
		(setq lst_fnm (append (mapcar '(lambda (x) (strcat l "\\" x))(vl-directory-files l nil 1)) lst_fnm))
	)
	(foreach x lst_fnm
		(setq ext (strcase (vl-filename-extension x) t))
		(cond
			((member ext (list ".vlx" ".fas" ".lsp"))
				(vl-catch-all-apply 'load (list x))
			)
			((wcmatch ext ".arx")
				(vl-catch-all-apply 'arxload (list x))
			)
			((wcmatch ext ".dll")
				(cond ((vl-cmdf "NETLOAD")) (t (princ (strcat "\n文件" x "加载失败！"))))
			)
			(t nil)
		)
		(terpri)
	)
)
;设置自动加载文件夹,需在此加载，原位加载会导致死机
(wyl:autoloadapp)

;===========================================================================
;取得自启动文件夹文件路径
(defun av:getfile(fn / f lst)
	(foreach f (kk-getfiles (av:getenv "Autodir") "*.*" 1)
		(setq f (strcase f t))
		(setq lst (cons (list (av:getfname f) f) lst))
	)
	(cadr (assoc (strcase fn t) lst))
)
;加载自启动文件夹及其子文件夹中的文件
;fn:文件名； cmd:运行命令
(defun av:load-fn-cmd(fn cmd / def ext file msg)
	(setvar "CMDECHO" 0)
	(setq file (av:getfile fn))
	(setq ext (strcase (vl-filename-extension file) t))
	(cond
		((member ext (list ".vlx" ".fas" ".lsp"))
			(cond ((setq def (eval (read (strcat "c:" cmd))))) (t (load file)))
			(def)
		)
		((wcmatch ext ".exe")(startapp file))
		((wcmatch ext ".dll")
			((if command-s command-s command) "netload" file)
			(command-s cmd)
		)
		((wcmatch ext ".arx")
			(if (vl-catch-all-error-p (setq msg (vl-catch-all-apply 'arxload (list file))))
				(princ (vl-catch-all-error-message msg))
				(command-s cmd)
			)
		)
		(t nil)
	)
	(setvar "CMDECHO" 1)
	(princ)
)

;;;===========================================================

;查询INI参数值
(defun getfkey(F)
	(cond
		((vl-ini-get (list (av:findfile "Info.ini") "Fkey" (strcase F))))
		(T "0")
	)
)
;;说明:创建菜单
;;返回:列表
(defun createmnulist(/ isfkey ish)
	(defun isfkey(fkey)(wcmatch "1" (getfkey fkey)))
	(setq ish (getvar "ribbonstate"))
	(list
		"***MENUGROUP=FSTL"
		"***ACCELERATORS"
		(if (isfkey "f1") "[\"F1\"]'2view ")
		(if (isfkey "f3") "[\"F3\"]'zoom 0.4XP")
		(if (isfkey "f4") "[\"F4\"]'zoom extents")
		(if (isfkey "f6") "[\"F6\"] ^C^Cm&pspace")
		(if (isfkey "f7") "[\"F7\"] ^C^CPrONOFF")
		(if (isfkey "f8") "[\"F8\"] ^C^Copendir")
		(if (isfkey "f9") "[\"F9\"] ^C^Cbwl")
		(if (isfkey "f10") "[\"F10\"] ^C^Ctheui")
		(if (isfkey "f11") "[\"F11\"] ^C^CCleanScreen")
		(if (isfkey "f12") "[\"F12\"] ^C^CCleanRubbish")
		"[CONTROL+\"F\"] ^C^CFINDOUT"
		;"[Alt+\"W\"]^C^C_close"
		"***POP1"
		"[效率与输出]"
		"[cad-->txt\\tWB]^C^C_av-dcwb"
		"[cad-->xls\\tTTT]^C^C_TTT"
		"[统计门窗表\\tWF]^C^C_windowsform"
		"[--]"
		"[天正加载并整图导出\\tT3]^C^C_TSAVEASS"
		"[临时文件导出\\t]^C^C_sv2dwg"
		"[分图 | 布局批量导出\\tFT]^C^C_ExportDwg"
		"[合并CAD图形文件]^C^C_MergeDwg"
		"[PDF文件整体导入]^C^C_PDFIMPORTS"
		"[--]"
		"[简单压缩<三维变二维>]^C^C_SimpleFlatten"
		"[屏幕旋转与恢复\\tZP]^C^C_CADZP"
		"[局部重生成\\tRR]^C^C_RR"
		;"[--]"
		;"[消除图层名前缀]^C^Cunxreflapre"
		;"[群组打散\\t]^C^C_explodegroup"
		"[--]"
		"[冷门工具集...\\tLM]^C^C_cooltls"
		"[贱人工具箱...\\tY]^C^C_Y"
		"[特殊标注与说明...\\tNN]^C^C_Label-A"
		"[--]"
		"[字体乱码解决...\\tKK]^C^C_KKfonts"
		"[清除多行文字格式]^C^C_CleanMtextFormat"
		"[冗余元素清理]^C^Cdwg-purge"
		"[--]"
		"[工具箱功能设置...\\tSET]^C^Cseting"
		"[绘图环境初始化\\tDS]^C^C_REAVTLSENV"
		"[->其它设置]"
		"[飞诗CAD管理器...\\tFST]^C^CFSTL"
		"[选项配置重置\\tREOP]^C^C_REOP"
		"[命令行文字左对齐]^C^C_habitleft"
		"[临时文件夹清理]^C^C_cleantemp"
		"[单色环境]^C^C_1colour"
		"[<-]"
		"[--]"
		"[沟通或升级]^c^cAvtlsWechat"
		;"[明径通道交流区]^C^C(AV:OPENWEB \"http://bbs.mjtd.com/thread-184305-1-1.html\")"
		;"[AVTLS工具箱升级]^C^C(AV:OPENWEB \"https://www.123pan.com/s/yOtA-BSM9d\")"
		"***POP2"
		"[计算与修改]"
		"[快速选择\\tSS]^C^Ceasyselect"
		"[线长选择\\tSSL]^C^C_selectbycurvelength"
		"[过滤\"零\"长线\\t0LL]^C^C_0LLine"
		"[文字|查找...\\tCTRL+F]^C^C_FINDOUT"
		"[--]"
		"[反向删除\\tREDEL]^C^Credel"
		"[--]"
		"[测量长度面积\\tT]^C^C_T"
		"[测量结果标注\\tTT]^C^C_TT"
		"[绘线并标注\\t]^C^C_pline-dim"
		"[自动计数并标注]^C^C_MarkPileNum"
		"[->文本统计与修改]"
		"[文字求和与统计\\tTS]^C^C_texttotal"
		"[字母大小写转换\\t]^C^C_TCASE"
		"[图块统计表\\tKTJ]^C^C_blockcount"
		"[连接文字\\twbh]^C^C_nt2ot"
		"[单行批量转多行\\t]^C^C_nt2mt"
		"[文本编号\\t]^C^C_textID"
		"[文本加框\\t]^C^C_TextBox"
		;"[清除多行文字格式]^C^C_CleanMtextFormat"
		"[<-]"
		;"[--]"
		;"[区域覆盖]^C^C_wipeout"
		"[--]"
		"[重建填充边界\\tEF]^C^C_EF"
		"[生成边界轮廓\\tEFF]^C^C_EFF"
		"[--]"
		"[布尔运算\\tBOO]^C^C_BooleanRegion"
		"[多段线编辑\\tDG]^C^C_pl_pt_ed"
		"[多段线模糊连接\\tPEE]^C^C_PEE"
		"[->多段线其它]"
		"[多段线连接\\tPE]^C^C_PE"
		"[超级修剪\\tTRR]^C^C_TRR"
		"[寻找区域开口\\t]^C^C_CHKOPEN"
		"[封闭区域开口\\t]^C^C_closepline"
		"[生成多段线边框]^C^C_mPolyOutline"
		"[复线宽度\\tPLW]^C^C_plw"
		"[<-]"
		"[--]"
		"[智能分解块\\tXX]^C^C_SMARTEXPL"
		"[一炸到底【嵌套块】]^C^C_ExplNestBlk"
		"[炸碎一切\\t]^C^C_BlastAll"
		"[批量群组打散\\t]^C^C_explodegroup"
		"[--]"
		"[沿线旋转\\tRT]^C^C_RT"
		"[中心旋转\\tRTT]^C^C_RTT"
		"[--]"
		"[旋转复制\\tRC]^C^C_YY-RO&CO"
		"[动态连续复制\\tCCC]^C^C_CCC"
		"[--]"
		"[动态双向偏移\\tFSS]^C^C_FSS"
		"***POP3"
		"[图层与控制]"
		"[建层并选择\\tNL]^C^C_putclayer"
		"[删指定层\\tDL]^C^C_DL"
		"[删关闭层\\t]^C^C_hh-delhidelayer"
		"[--]"
		"[设为随层\\tSL]^C^C_put-color-bylayer"
		"[指定当前层\\t]^C^C_Activelayer"
		"[移至当前\\tTDQ]^C^C_TDQ"
		"[图层刷子\\tMAA]^C^C_LayerMatch"
		"[--]"
		"[->布局与视窗控制]"
		"[快速布局\\t]^C^C_FastVports"
		"[新建视口\\tVS]^C^C_VportsPlus"
		"[--]"
		"[在模型与布局中穿越]^C^C_ChangeSpace"
		"[--]"
		"[视窗|锁定\\tSK]^C^C_SK"
		"[视窗|解锁\\tKS]^C^C_KS"
		"[模型布局切换\\t1E]^C^C_1E"
		"[视口切换\\t2E]^C^C_M&Pspace"
		"[<-]"
		"[--]"
		"[关闭所选\\tGG]^C^C_GG"
		"[关闭其它\\tZD]^C^C_ZD"
		"[反向关闭\\tFH]^C^C_FH"
		"[全部打开\\tQB]^C^C_QB"
		"[--]"
		"[释放图层控制\\t]^C^C_LayerAllF"
		"[--]"
		"[->图层锁定与冻结]"
		"[锁定所选\\tSD]^C^C_layerlock"
		"[解锁所选]^C^C_LayerUnlock"
		"[--]"
		"[冻结所选\\tDJ]^C^C_LayerFreeze"
		"[全部解冻]^C^C_LayerFreeAll"
		"[反向冻结]^C^C_LayerFreeSwap"
		"[<-]"
		"[->图形元素显隐]"
		"[隐藏所选\\tHH]^C^C_Hideobj"
		"[隐藏其它]^C^C_isoobj"
		"[反向隐藏]^C^C_HideSwap"
		"[全部显示]^C^C_Showall"
		"[<-]"
		"[--]"
		"[消除图层名前缀]^C^Cunxreflapre"
		"***TOOLBARS"
		"**TB_av_value"
		"[_Toolbar(\"AV_value\", _Top, _Show, 0, 0, 1)]"
		"[_Button(\"窗口设置\", RCDATA_16_WALK-THROUGH_SAVE,)]^C^C_APIWINSET"
		"[_Button(\"新建\", RCDATA_16_NEW,)]^C^C_qqnew"
		"[_Button(\"打开\", RCDATA_16_OPEN,)]^C^C_open"
		"[_Button(\"保存\", RCDATA_16_SAVE,)]^C^C_qsave"
		"             [--]"
		(strcat "[_Button(\"快速打印\"," (if ish "RCDATA_16_FRAME_DISPLAYPLOT" "RCDATA_16_PRINT") ",)]^C^C_fastprinter")
		(strcat "[_Button(\"分图与布局导出FT\", " (if ish "RCDATA_16_ARRSCHANGE" "RCDATA_16_ARRREC") ",)]^C^C_ExportDwg")
		;"[_Button(\"分图与布局导出FT\", RCDATA_16_MATERIALS_PAINTER,)]^C^C_ExportDwg"
		(strcat "[_Button(\"保存为图片\", " (if ish "RCDATA_16_WMFOUT" "RCDATA_16_FOG") ",)]^C^C_WMFPLUS")
		(strcat "[_Button(\"文件重命名\", " (if ish "RCDATA_16_RENAME" "RCDATA_16_TOOMOD") ",)]^C^C_av:rename")
		"             [--]"
		(if dcl-project-load
			(strcat "[_Button(\"侧向面板开关\", " (if ish "RCDATA_16_AUTHORPALETTE_NEW" "RCDATA_16_TPALETTE") ",)]^C^C_ltoolsswitch")
		)
		;(strcat "[_Button(\"视口同步\", " (if ish "RCDATA_16_VPSYNC" "RCDATA_16_WINVRT") ",)]^C^C_AGDT")
		"[_Button(\"视口同步\", RCDATA_16_VISUALCOMPARE,)]^C^C_AGDT"
		"[_Button(\"设置参数\", RCDATA_16_SET_VARIABLE,)]^C^Cseting"
		"[_Button(\"双屏参数设定 F1\", RCDATA_16_BNGFWD,)]^C^C2viewset"
		"[_Button(\"屏幕旋转zp\", RCDATA_16_UCSZAR,)]^C^C_CADZP"
		"[_Button(\"乱码修正KK\", RCDATA_16_SPELL,)]^C^C_KKfonts"
		;(strcat "[_Button(\"乱码修正KK\", " (if ish "RCDATA_16_UPDFIELD" "RCDATA_16_SPELL") ",)]^C^C_KKfonts")
		"[_Button(\"简单压缩\", RCDATA_16_SM_EXTEND,)]^C^C_SimpleFlatten"
		"             [--]"
		"[_Control(_Undo)]"
		"[_Control(_Redo)]"
		"             [--]"
		(strcat "[_Button(\"连续测距\", " (if ish "RCDATA_16_WALK_3DWALK" "RCDATA_16_3DWALK") ",)]^C^C_pt-pt-dis")
		(if ish "[_Button(\"快速测量\", RCDATA_16_QUICKMEASURE, RCDATA_32_QUICKMEASURE)]^C^C_MEASUREGEOM _M _Y")
		"[_Button(\"画线标长\", RCDATA_16_DIST, RCDATA_16_DIVIDE)]^C^C_pline-dim"
		"[_Button(\"画面标注\", RCDATA_16_AREA, RCDATA_32_AREA)]^C^C_manuarea"
		;"[_Button(\"统计门窗表WF\", RCDATA_16_TABLE,)]^C^C_windowsform"
		"             [--]"
		"[_Button(\"多段线fg\", RCDATA_16_PLINE,)]^C^CFG"
		"[_Button(\"自由矩形rec\", RCDATA_16_RECTAN,)]^C^CREC"
		(strcat "[_Button(\"矩形云线ryy\", " (if ish "RCDATA_16_REVCLOUD_RECTANGLE" "RCDATA_16_REVCLOUD") ",)]^C^C_reccloud")
		"             [--]"
		;"[_Button(\"炸碎一切\", RCDATA_16_MOCORO,)]^C^C_BlastAll"
		"[_Button(\"智能分解块xx\", RCDATA_16_EXPLODE,)]^C^C_smartexpl"
		(strcat "[_Button(\"动态连续复制CCC\"," (if ish "RCDATA_16_COPYM" "RCDATA_16_COPYOB") ",)]^C^C_CCC")
		(if acet-sys-lmouse-down ;判断是否安装了ET
			"[_Button(\"复制嵌套图元\", RCDATA_16_NCOPY,)]^C^C_NCOPY"
		)
		;(strcat "[_Button(\"解除编组\", " (if ish "RCDATA_16_UNGROUP" "RCDATA_16_SELGRO") ",)]^C^C_ExplodeGroup")
		"[_Button(\"线打断\", RCDATA_16_BRE2PT, RCDATA_32_BRE2PT)]^C^C_dynbreak"
		"[_Button(\"多线合并pe\", RCDATA_16_JOIN,)]^C^C_PE"
		(strcat "[_Button(\"对齐al\", " (if ish "RCDATA_16_ALIGN_NEW" "RCDATA_16_ML_ALIGN") ",)]^C^C_align")
		;"[_Button(\"对齐al\", RCDATA_16_ML_ALIGN,)]^C^C_align"
		"[_Button(\"特性匹配ma\", RCDATA_16_MATCH,)]'_matchprop"
		(strcat "[_Button(\"清理重复对象ov\", " (if ish "RCDATA_16_OVERKILL" "RCDATA_16_PURGE") ",)]^C^C_ov")
		;(strcat "[_Button(\"删指定层dl\", " (if ish "RCDATA_16_LAYER_DELETE" "RCDATA_16_REFED_DISC") ",)]^C^Cdlayer")
		"             [--]"
		"[_Button(\"文本编号\", RCDATA_16_JUSTIFYTXT,)]^C^C_textID"
		(strcat "[_Button(\"自动计数与标注\", " (if ish "RCDATA_16_TCOUNT" "RCDATA_16_POINT") ",)]^C^C_MarkPileNum")
		"             [--]"
		;"[_Button(\"标注样式管理器\", RCDATA_16_DIMSTY,)]'_dimstyle"
		;"[_Control(_dimstyle)]"
		;"             [--]"
		"[_Control(_Color)]"
		"[_Button(\"图层特性管理器\", RCDATA_16_LAYERS,)]'_layer"
		"[_Control(_Layer)]"
		
		"**TB_VIEWPORTS"
		"[_Toolbar(\"AV_视口\", _Floating, _Hide, 100,210, 1)]"
		(strcat "[_Button(\"快速布局\", " (if ish "RCDATA_16_ALIGN_SPACE" "RCDATA_16_VPDLG") ",)]^C^C_FastVports")
		"[_Button(\"新建视口VS\", RCDATA_16_VPONE,)]^C^C_VportsPlus"
		(strcat "[_Button(\"将对象转换为视口\", " (if ish "RCDATA_16_VIEW_VP_OBJECT" "RCDATA_16_VPOBJ") ",)]$M=$(if,$(eq,$(getvar,tilemode),0),^C^C_-vports _o,^C^C^P(ai_viewports_alert)^P)")
		"[_Button(\"剪裁现有视口\", RCDATA_16_VPCLIP, RCDATA_16_VPCLIP)]^C^C_vpclip"
		"[_Control(_ViewportScale)]"
		"[--]"
		(strcat "[_Button(\"更改空间\", " (if ish "RCDATA_16_CHANGE_SPACE" "RCDATA_16_TOLERA") ",)]^C^C_ChangeSpace")
		(if (>= *ver4* 2009)
			"[_Button(\"将布局输出到模型\", RCDATA_16_EXPORT_LAYOUT,)]^C^C_EXPORTLAYOUT" ;RCDATA_16_SECTIONPLANETOBLOCK
		)
		(strcat "[_Button(\"隔离视口\", " (if ish "RCDATA_16_ISOLATEOBJECTS" "RCDATA_16_ONLAY") ",)]^C^C_actvp")
		"[--]"
		(strcat "[_Button(\"锁定视口SK\", " (if ish "RCDATA_16_VP_DISPLAY_LOCKED" "RCDATA_16_LCKLAY") ",)]^C^C_SK")
		(strcat "[_Button(\"解锁视口KS\", " (if ish "RCDATA_16_LAYOUT_VIEWPORT_UNLOCKED" "RCDATA_16_ULKLAY") ",)]^C^C_KS")
		(strcat "[_Button(\"上一布局\", " (if ish "RCDATA_16_VIEW_BACK" "RCDATA_16_HLNK_BACK") ",)]^C^C_LAYOUTUP")
		(strcat "[_Button(\"下一布局\", " (if ish "RCDATA_16_VIEW_FWD" "RCDATA_16_HLNK_FWD") ",)]^C^C_LAYOUTDOWN")
		(strcat "[_Button(\"视口进出2E\", " (if ish "RCDATA_16_VP_MINIMIZE" "RCDATA_16_REVOLV") ",)]^C^C_M&Pspace")
		
		;"**TB_LAYERS"
		;"[_Toolbar(\"AV_图层控制\", _Floating, _Hide, 100,150,1)]"
		;;"[_Button(\"删指定图层\", RCDATA_16_LAYER_DELETE,)]^C^C_11928511"
		;;"[--]"
		;"[_Button(\"释放图层控制\", RCDATA_16_LIGHT,)]^C^C_1111"
		;"[--]"
		;"[_Button(\"图层关闭\", RCDATA_16_OFFLAY,)]^C^C_22"
		;"[_Button(\"图层全开\", RCDATA_16_ONLAY,)]^C^C_2112"
		;"[_Button(\"图层隔离\", RCDATA_16_LAYISO,)]^C^C_29022"
		;"[--]"
		;"[_Button(\"图层锁定\", RCDATA_16_LCKLAY,)]^C^C_12323"
		;"[_Button(\"图层解锁\", RCDATA_16_ULKLAY,)]^C^C_12344"
		;"[--]"
		;"[_Button(\"图层冻结\", RCDATA_16_FRZLAY,)]^C^C_1233"
		;"[_Button(\"图层解冻\", RCDATA_16_LAYTHW,)]^C^C_124"
		;"[--]"
		;"[_Button(\"对象隐藏\", RCDATA_16_HIDEOBJECTS,)]^C^C_2224"
		;"[_Button(\"对象全显\", RCDATA_16_UNISOLATEOBJECTS,)]^C^C_28224"
		;"[--]"
		
		
	)
)

;============适时打开视口工具条==============
(or *av:layoutswitchvpbar* ;切入布局时打开视口工具栏
	(setq *av:layoutswitchvpbar* (vlr-miscellaneous-reactor nil '((:vlr-layoutSwitched . av:layoutswitchvpbar))))
)
(defun av:layoutswitchvpbar(a b)
	(cond
		((null MJ:Toolbar))
		((= 0 (getvar "tilemode"))
			(MJ:Toolbar "FSTL" "AV_视口" 1)
		)
		(t (MJ:Toolbar "FSTL" "AV_视口" 0))
	)
)
(or *av:ifquitoffvpbar* ;检测到特定命令时的动作
	(setq *av:ifquitoffvpbar* (vlr-command-reactor nil '((:vlr-commandwillstart . av:ifquitoffvpbar))))
)
(defun av:ifquitoffvpbar (calling-reactor startcommandinfo)
	(cond
		((null MJ:Toolbar))
		((wcmatch (car startcommandinfo) "QUIT")
			(MJ:Toolbar "FSTL" "AV_视口" 0)
		)
		(t nil)
	)
)



(princ)





