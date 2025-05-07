
;;分图与布局导出
(defun c:ExportDwg (/ *error* dwg file fn istim msexports name path psexport psexports ss)
	(princ "-->图纸拆分导出")
	(defun *error*(msg)
		(if (findfile file)
			(progn
				(av:opendir (vl-filename-directory path) t)
				(prompt "\n>>>拆分出来的图纸已保存在打开的文件夹中")
			)
		)
		(setvar "cmdecho" 1)
		(princ)
	)
	(defun msexports(ss / file i pts sdwgout time)
		(setq i 0)
		(while (or ss (setq ss (ssget)))
			(setq i (1+ i))
			(setq time (av:gettime "+hh-mm-ss-"))
			(setq sDwgOut (strcat name time (itoa i) ".dwg"))
			(setq pts (av:getboxpt ss))
			(vla-ZoomWindow *acad* (vlax-3D-point (car pts)) (vlax-3D-point (cadr pts)))
			(setq file (strcat path sDwgOut))
			(vla-WBlock *doc* file (ss->vlass ss))
			(av:sound (av:findfile "chimes.wav"))
			(vla-ZoomPrevious *acad*)
			(prompt (strcat "\n已保存：" sDwgOut))
			(setq ss nil)
		)
		file
	)
	(defun psexport(na / file sdwgout)
		(setq sdwgout (strcat name "_" na ".dwg"))
		(setq file (strcat path sdwgout))
		(if (vl-cmdf "exportlayout" file)
			(progn
				(prompt (strcat "\n已保存：" sDwgOut "\n"))
				(av:sound (av:findfile "chimes.wav"))
			)
		)
		file
	)
	(defun psexports(/ lst sdwgout stab)
		(setq stab (getvar "ctab"))
		(setq lst (lm:listbox "布局输出到模型文件【可多选】" (layoutlist) t))
		(foreach l lst (setvar "ctab" l) (setq file (psexport l)))
		(setvar "ctab" stab)
		file
	)
	;;开始奔跑
	(setvar "cmdecho" 0)
	(cond
		((setq dwg (cadr (av-mkdir-todwg t)))
			(setq fn (fnsplitl dwg) path (car fn) name (cadr fn))
			(prompt (strcat "\n文件保存位置:" path))
		)
		(t nil)
	)
	(cond
		((null dwg))
		((and
			 (setq istim (= (getvar "tilemode") 1))
			 (princ "\n提示：回车可批量将布局导出！")
			 (setq ss (ssget))
		 )
			(setq file (msexports ss))
		)
		(istim (setq file (psexports)))
		((progn
			 (if (/= 0 (vla-get-MSpace *doc*)) (vla-put-MSpace *doc* 0))
			 (princ "\n提示：回车可本布局单张整体导出！")
			 (setq ss (ssget))
		 )
			(setq file (msexports ss))
		)
		(t
			(setq file (psexport (getvar "ctab")))
			(princ "\n在模型中空选可布局批量导出！")
		)
	)
	(*error* nil)
)
;自动保存文件导出
(defun c:sv2dwg(/ dwg extlst f2 flst i obj_wsh path savepath str winshell)
	(setq savepath (vl-filename-directory (getvar "savefilepath")))
	(setq extlst (list ".bak" ".dwg" ".sv$"))
	(setq flst
		(vl-remove-if-not
			(function
				(lambda(x / ext)
					(and
						(setq ext (vl-filename-extension x))
						(member (strcase ext t) extlst)
					)
				)
			)
			(kk-getfiles savepath "*.*" 0)
		)
	)
	(setq obj_wsh (vlax-create-object "wscript.shell"))
	(setq path (car (av-mkdir-todwg nil)))
	(setq i 0)
	(foreach f1 flst
		(setq dwg (strcat (cadr (fnsplitl f1)) ".dwg"))
		(setq f2 (strcat path "\\" dwg))
		(setq str (strcat "cmd /c copy " f1 " " f2))
		(vlax-invoke obj_wsh 'run str 0 0)
		(princ (strcat "\n导出 " dwg))
		(setq i (1+ i))
	)
	(vlax-release-object obj_wsh)
	(cond
		((> i 0)
			(princ (strcat "\n提示：共导出" (itoa i) "个临时文件！"))
		)
		(t
			(princ "\n提示：没有发现临时文件！")
		)
	)
	(cond
		(flst
			(av:opendir path t)
			;(textpage)
		)
	)
	(princ)
)

;加强版wmf图形导出
(defun c:wmfplus(/ *error* dwg file i ismax layout path pts ss stn time)
	(defun *error*(msg)
		;(setvar "lwdisplay" 0)
		(ShowPlotStyles :vlax-false)
		(cond
			(file
				(princ (strcat "\n报告：已生成" stn "个WMF图片;"))
				(av:opendir path t)
			)
			(msg (princ "如果想得到多彩图形,可运行命令WMFOUT获取。"))
			(t nil)
		)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(princ "\n所导出的WMF格式，在插入文档时，为矢量透明高清大图，可无限缩放，如需底色可自由填充。")
	;(setvar "lwdisplay" 1)
	(cond
		((= 1 (getvar "tilemode"))
			(if (< *ver4* 2019) (princ "\n提醒：低版本的CAD，仅在布局中才能以单色样式显示！"))
		)
		((= 1 (getvar "cvport"))
			(princ "\n当前状态下，如需按当前显示截图，请用分图程序导出布局后打开再操作。")
		)
		(t (command-s "vpmax") (setq ismax t))
	)
	(setq	layout (vla-get-activelayout *doc*))
	(vla-Put-StyleSheet layout "monochrome.ctb")
	(ShowPlotStyles :vlax-true)
	(setq path (car (av-mkdir-todwg nil)))
	(prompt (strcat "\n文件保存位置:" path))
	(setq dwg (vl-filename-base (getvar "dwgname")))
	(setq i 1)
	(princ "\n请选择导出MWF的第1个图形")
	(while (setq ss (ssget '((0 . "~VIEWPORT"))))
		(setq time (av:gettime "+hh-mm-ss-"))
		(setq stn (itoa i))
		(setq na (strcat dwg time stn ".wmf"))
		(setq file (strcat path "\\" na))
		(setq pts (av:getboxpt ss))
		(vla-ZoomWindow *acad* (vlax-3D-point (car pts)) (vlax-3D-point (cadr pts)))
		(while (= 1 (getvar "cmdactive")) (command pause))
		(command-s "wmfout" file ss "")
		(av:sound (av:findfile "snip.wav"))
		(vla-ZoomPrevious *acad*)
		(princ (strcat "\n已保存:" na))
		(setq i (1+ i))
		(princ (strcat "\n请选择第" (itoa i) "个图形"))
	)
	(cond
		(ismax
			(command-s "vpmin")
			(vla-put-MSpace *doc* :vlax-true)
		)
	)
  (*error* nil)
)

;显示单色环境
(defun c:1colour(/ layout)
	(setq	layout (vla-get-activelayout *doc*))
	;(setq Background (getenv "Background"))
	;(setq Layoutbackground (getenv "Layout background"))
	(cond
		(
			isnip
			;(and
			;	Background
			;	Layoutbackground
			;	(wcmatch Background "16777215")
			;)
			(ShowPlotStyles :vlax-false)
			(Backgroundcolor Background Layoutbackground)
			(setq isnip nil)
			(princ "当前绘图环境")
		)
		(t
			(setq Background (getenv "Background"))
			(setq Layoutbackground (getenv "Layout background"))
			(vla-Put-StyleSheet layout "monochrome.ctb")
			(ShowPlotStyles :vlax-true)
			(Backgroundcolor 16777215 16777215)
			(setq isnip t)
			(princ "当前打印显示")
		)
	)
	(princ)
)

(defun ShowPlotStyles(value)
	(vla-put-ShowPlotStyles layout value)
	(vla-regen *doc* AcAllViewPorts)
)
;;说明:确认路径及文件名
;;参数:flag:确认是否重新选择路径
;;返回:路径及文件全名列表
(defun av-mkdir-todwg (flag / file folder path)
	(cond
		((and
			 (setq path (av:getenv "DwgExportPath"))
			 (findfile path)
		 )
		)
		(t
			(setq folder (strcat "分图" (av:gettime "m-d")))
			(setq path (getenv "UserProfile"))
			(setq path (strcat path "\\" folder))
			(vl-mkdir path)
		)
	)
	(cond
		((and
			 flag
			 (setq path (strcat path "\\" (getvar "dwgname")))
			 (setq file (getfiled "图形文件分区导出" path "dwg;dwt" 1))
			 (setq path (vl-filename-directory file))
		 )
		)
		(t (setq file nil))
	)
	(av:setenv "DwgExportPath" path)
	(list path file)
)


