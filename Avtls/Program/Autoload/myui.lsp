
;;开图自动运行
((lambda()
	 (setvar "cmdecho" 0)
	 ;控制可用于布局视口、页面布局和打印的缩放比例的列表；解决复制粘贴卡顿甚至死机的问题，有返回信息
	 (vl-catch-all-apply 'command-s (list "-scalelistedit" "r" "y" "e"))
	 ;开图关格栅
	 (cond
		 ((= 1 (getvar "ribbonstate"))(setvar "gridmode" 1))
		 (t (setvar "gridmode" 0))
	 )
 )
)

;;===================绘图环境初始化=====================
(defun c:ds()(c:reavtlsenv))
(defun c:reavtlsenv (/ *error* acadpref ctb cursorsize d2b display dyn layout lst lts os1 os2 preferences right txt)
	(defun *error*(str)
		(foreach l (list preferences display AcadPref)
			(vlax-release-object l)
		)
		;(princ "运行完成！")
		(setvar "cmdecho" 1)
		(princ)
	)
	(princ "-->绘图环境初始化 ")
	(setvar "cmdecho" 0)
	(if (= 1 (getvar "nomutt")) (setvar "nomutt" 0));禁止消息反馈
	(setq preferences (vlax-get-property *acad* 'Preferences))
	(setq Display (vlax-get-property preferences 'Display))
	(setq AcadPref (vla-get-OpenSave preferences))
	(setq	layout (vla-get-activelayout *doc*))
	;=====================================================
	;解决CAD在win11下无法调用记事本的问题
	;(setvar "MTextEd" "notepad");多行文本双击用记事本编辑
	(setvar "MTextEd" ".");用默认编辑器
	;=====================================================
	;程序及文档窗口最大化(1=acmin,2=acnorm,3=acmax)
	(vla-put-windowstate *acad* acmax);程序窗口最大化
	(vla-put-windowstate *doc* acmax);文档窗口最大化
	(setenv "ShowFullPathInTitle" "1");在标题中显示完整路径
	;=====================================================
	(setvar "proxynotice" 0);代理图形对话框（不显示）
	(setvar "proxyshow" 1);显示代理图形
	(setvar "recoverymode" 1);记录修复信息，但是系统出现故障后不自动显示“图形修复”窗口
	(setvar "dwgcheck" 2);打开错误文件命令行警告
	(setvar "reporterror" 0);不发送错误报告
	(and (getvar "qpmode")(setvar "qpmode" -1));不显示快捷特性面板
	(setvar "tooltips" 1);显示工具提示：是
	(setvar "acadlspasdoc" 0);打开文件不加载lsp
	(and (getvar "rollovertips")(setvar "rollovertips" 0));显示鼠标悬停提示：否(适用于cad2009及更高版本)
	(and (getvar "secureload")(setvar "secureload" 0));任何位置加载不警告，或用命令trustedpaths加载受信任文件夹路径
	(setvar "expert" 1);禁止显示“准备重生成，是否继续？”以及“是否确实要关闭当前图层？”
	(setvar "filedia" 1);打开文件对话框
	;=====================================================
	(if (/= (getvar "sdi") 0)(setvar "sdi" 0));单文档模式开关；此参数会联动原生标签开启
	;标签工具条,低版关闭，高版打开。与sdi参数有关联
	(and (= 1 (getvar "filetabstate"))(command "filetabclose"))
	(command-s "taskbar" "0");任务栏标签合并,cmd可立即合并
	;=====================================================
	(and (getvar "startmode")(setvar "startmode" 0));关闭"开始"选项卡。
	(and (getvar "filetabpreview")(setvar "filetabpreview" 0));控制将光标悬停在图形文件选项卡上方时是否显示缩略图
	;指定当您将光标悬停在文件选项卡缩略图上时，是否在图形窗口中加载相应的模型或布局。
	(and (getvar "filetabthumbhover")(setvar "filetabthumbhover" 0))
	(vl-catch-all-apply 'vl-cmdf (list "-plotstamp" "l" "n" "" ""));打印戳记及高级选项，设置不创建日志文件
	;=====================================================
	(and (getvar "touchmode")(setvar "touchmode" 0));关闭触摸模式功能区面板
	;(vla-put-DisplayScrollBars display :vlax-false);关闭滚动条
	(setenv "Scrollbars" "0");关闭滚动条
	(and (getvar "startup")(setvar "startup" 0));关闭欢迎界面(2015及以上)
	(and (getvar "vpcontrol")(setvar "vpcontrol" 0));关闭左上角视图显示
	(and (getvar "displayviewcubein2d")(setvar "displayviewcubein2d" 0));关闭东南西北
	(and (getvar "navbardisplay")(setvar "navbardisplay" 0));视图导航工具面板
	(and (getvar "gripmultifunctional")(setvar "gripmultifunctional" 1));动态夹点菜单关闭(适用于cad2010及更高版本)
	;(setvar "ucsicon" 0);关闭左下角ucs图标
	;控制文件保存
	(setvar "isavebak" 0);控制备份文件 (bak) 的创建。1-创建, 0-不创建
	(setvar "isavepercent" 0);完全保存,使文件最小
	(setvar "savetime" 10);设置自动保存的时间为10分钟
	;(setenv "DefaultFormatForSave" "36");设置CAD默认保存文件格式2013(24-2004,36-2007,48-2010,60-2013)
  (vla-put-SaveAsType AcadPref ac2007_dwg);修正当前文档保存格式
	(setvar "fontalt" "tssdeng.shx") ;指定找不到指定的字体文件时要使用的替换字体(选项中的替换字体文件)
	;;;=====================================================
	;选择方式设定
	(setvar "pickfirst" 1);选择集模式(先选择后执行)，与qaflags相匹配
	(and (getvar "qaflags")(setvar "qaflags" 0));带基点复制，与pickfirst相匹配
	;QAFLAGS是个未公布的变量，用来控制LISP中使用EXPLODE炸开一个选择集的时候的表现。
	;如果QAFLAGS＝0，那么用LISP执行(command "explode" ss "")的时候仅仅能炸开选择集的第一个实体，其他实体炸不了.
	;如果QAFLAGS＝1，可以炸开选择集所有实体。
	;这个变量不影响标准的EXPLODE命令的执行。
	;你可以在命令行试验分别设置变量QAFLAGS为0或者1时候：(command "explode" (ssget) "")
	(setvar "pickadd" 1);选择方式为传统
	(and (getvar "selectioncycling")(setvar "selectioncycling" 1));允许重叠对象选择循环，只显示标记，无对话框
	(setvar "selectionpreview" 3);选择集预览,1未激活命令时，2命令处于活动状态时，3两者都
  (setvar "previewfilter" 31);选择集预览模式
  (setvar "xclipframe" 0);块剪裁边界不显示（若原图参数不为0，执行时会重生成）
	(setvar "DRAGMODE" 2) ;控制进行拖动的对像的显示方式，0关，1开，2自动(复制图形的预览)。
	;;;=====================================================
	;光标与右键相关设置
	(setenv "AutoSnapSize" "7");自动捕捉标记大小
	(setvar "pickbox" 7);拾取框调整，默认值5
	(setvar "apbox" 0);关闭靶框
	(setvar "gripsize" 6);设置夹点框的大小
	(setvar "gripobjlimit" 3000);选择对象时限制显示的夹点数
	(setvar "mbuttonpan" 1);中键功能
	(and ;由seting面板进行参数设置
		(setq cursorsize (av:getenv "cursorsize"))
		(setvar "cursorsize" cursorsize);十字光标大小
	)
	(and ;由seting面板进行参数设置
		(setq right (av:getenv "right"))
		(if (= right 1)(setq right 2)(setq right 11))
		(setvar "shortcutmenu" right);右键单击快捷菜单
	)
	;=====================================================
	(setvar "DRAGP1" 1);设置重生成拖动模式下的输入采样率
	(setvar "DRAGP2" 1);设置快速拖动模式下的输入采样率
	(setvar "TREEMAX" 1000000);通过限制空间索引（八叉树）中的节点数目，从而限制重生成图形时占用的内存
	;缩放速度相关设置
	(setvar "regenmode" 1);控制图形的自动重生成，0禁止命令的自动重生成（例如，在解冻图层时），1允许某些命令的自动重生成
	(vl-catch-all-apply 'vl-cmdf (list "viewres" "y" "1000"));弧形显示平滑度，不平顺时可re重生成
	(setvar "vtenable" 0);缩放动画控制，0为无动画，1为有动画
	(and (getvar "hpmaxlines")(setvar "hpmaxlines" 100000));ansi填充线条数大于此参数时，按solid显示以提高速度，默认值1000000.
	(setvar "vtfps" 4);vtfps 1~7 默认7 控制平滑缩放的速度
	(setvar "zoomfactor" 60);鼠标滑轮缩放比例（默认为60）
	(and (getvar "gfxdx12")(setvar "gfxdx12" 0));关闭硬件加速，解决鼠标跳动、抖动、移动缓慢滞后的问题，存在于2022及以上版本
	(and (getvar "blockmrulist")(setvar "blockmrulist" 5));控制在“块”选项板的“最近使用”选项卡中显示的最近使用块的数量
	(setvar "fillmode" 1);指定实体填充、渐变填充、二维实体和宽多段线是否被填充
	(setvar "hpscale" 1);设定填充图案比例因子
	(setenv "maxhatch" "1000000");填充图案的最大数目，解决不会出现“图案填充间距太密，或短划尺寸太小”
	(setvar "HPQUICKPREVIEW" 0) ;关闭填充图案显示预览
	(setvar "MAXACTVP" 30) ;最大激活视口数(实际N-1个)，默认N=64
	;=====================================================
	(setvar "COORDS" 0);控制状态栏上的光标位置是连续进行更新还是仅在特定时间更新。0关，1开，2激活时适时相对极坐标
	(setvar "HPDLGMODE" 0);控制“图案填充和渐变色HATCH”对话框以及“图案填充编辑GRADIENT”对话框的显示。默认2
	;(setvar "SELECTIONEFFECT" 0);指定对象处于选中状态时所使用的视觉效果。0虚线,1当硬件加速处于启用状态时，将显示光晕线亮显效果
	(setvar "TEMPOVERRIDES" 0);打开或关闭用于绘图辅助的F8临时替代键。0关，1开
	;系统 → 硬件加速 → 取消勾选 自动检查证书更新
	(vl-registry-write
		(strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\3DGS Configuration")
		"AutoUpdateCertDB" 0
	)
	;=====================================================
	(setvar "dynprompt" 0);不在十字光标附近显示提示和命令输入
	(and ;由info面板进行参数设置
		(setq dyn (av:getenv "dyn"))
		(if (= dyn 0)(setq dyn 0)(setq dyn 3))
		(setvar "dynmode" dyn);关闭光标指针输入
	)
	;在RIBBON模式下打开格栅，其它情况下关闭格栅
	(cond ((= 1 (getvar "ribbonstate"))(setvar "gridmode" 1))(t (setvar "gridmode" 0)))
	(setvar "snapmode" 0);关闭栅格捕捉
	(setvar "autosnap" 63);打开对角追踪(与正交开关关联)
	(setvar "polarmode" 2);用所有极轴角设置追踪
	(setenv "AutoSnapTrackPath" "0");显示全屏及极轴追踪矢量
	(setvar "polarang" (/ pi 4));设置极轴追踪增量角为：45°
	(setvar "trackpath" 0);控制显示极轴和对象捕捉追踪的对齐路径
	(setvar "polardist" 1);设置极轴捕捉增量
	(and (getvar "3dosmode")(setvar "3dosmode" 11));关闭三维对象捕捉
	;=====================================================
	;(setvar "osmode" (logior (getvar "osmode") 16384));关辅捉
	(setvar "osmode" (logand (getvar "osmode") 49151));开辅捉
	(setq os1 (getvar "osmode"))
	;启用工具箱设置中的对象捕捉
	(setq os2 (cond ((setq os2 (av:getenv "osmode"))) (t 7223)))
	;十进制转换为二进制，并且补全 15 个二进制位
	;示例：(d2b 4133)，返回 (0 0 1 00 0 0 0 0 1 0 0 1 0 1)
	(defun d2b (i / s) (repeat 15 (setq s (cons (logand i 1) s) i (lsh i -1))) s)
	;提示当前捕捉
	(setq txt '("捕捉关闭" "平行线" "延长线" "外观交点" "几何中心" "最近点"
							 "切点" "垂足" "插入点" "交点" "象限点" "节点" "圆心" "中点" "端点"))
	(setq lst (reverse (vl-remove nil (mapcar '(lambda (x y) (if (= 1 x) y)) (d2b os2) txt))))
	(cond ((= os1 os2))
		(t (princ "\n对象捕捉已设置为：") (foreach l lst (princ (strcat l "、"))))
	)
	;=====================================================
	(setvar "blipmode" 0);取消左键单击白点标记
	(and (getvar "layouttab") (setvar "layouttab" 1));显示布局和模型选项卡
	(and (getvar "statusbarautowrap") (vl-catch-all-apply 'setvar (list "statusbarautowrap" "off")));关掉状态栏自动换行
	(setenv "CursorCoordinatesPane" "0");不显示图形坐标
	;(setvar "UCSICON" 2);关闭坐标显示
	(setvar "lwdisplay" 0);线宽关闭
	(setvar "traynotify" 0);控制是否在状态栏系统托盘上显示服务通知
	(and (getvar "sysmon")(setvar "sysmon" 0));关闭系统变量监视器中的变化通知
	;=====================================================
	(vl-catch-all-apply 'vl-cmdf (list "commandline"));打开命令行
	(setenv "CmdLine.FontFace" "consolas");命令行字体样式
	(and (getvar "inputsearchoptionflags")(setvar "inputsearchoptionflags" 4));关闭命令自动完成和更正(-inputsearchoptions)
	(and (getvar "clipromptlines")(setvar "clipromptlines" 3));命令行提示历史记录的行数
	;(setenv "CmdHistLines" "500");设置命令行历史记录行数
	(vla-put-historylines display 500);同上
	(and (getvar "inputsearchdelay")(setvar "inputsearchdelay" 100));建议列表延迟时间
	(setvar "lockui" 0)
	;=====================================================
	;布局区域功能设置
	(setvar "layoutregenctl" 2);选项卡切换时重生成控制，0=当前禁其它可，1=当前布局禁止重生成，2=当前首次其它禁止
	(setvar "ucsfollow" 0);ucs更改时，对其它视口的影响；0=禁，1=可。
	;=====================================================
	(setvar "maxactvp" 64) ;显示布局中全部视口，最大值
	;(vla-put-LayoutDisplayMargins display :vlax-false);在布局不显示可打印区域
	;(vla-put-LayoutDisplayPaper display :vlax-false);在布局不显示图纸背景
	;(vla-put-LayoutDisplayPaperShadow display :vlax-false);在布局不显示图纸阴影
	(setenv "ShowPaperMargins" "0");不显示可打印区域
	(setenv "ShowPaperBackground" "0");不显示布局图纸阴影
	(setenv "ShowPrintBorder" "0");不显示布局图纸背景
	(cond ;关闭在新布局中创建视口
		((getvar "layoutcreateviewport")(setvar "layoutcreateviewport" 0));高版本
		(t (setenv "CreateViewports" "0"));低版本
	)
	(setvar "PSTYLEPOLICY" 1) ;新图形默认使用颜色相关的打印样式
	(if (av:findfile (setq ctb "黑白细线.ctb"))
		(vla-Put-StyleSheet (vla-get-activelayout *doc*) ctb) ;选择相应的打印样式
	)
	;=====================================================
	(setenv "BEditBackground" "0") ;设置块编辑器背景色为<黑色>
	;=====================================================
	;设置线型特性
	(setvar "cecolor" "bylayer");颜色随层
	(setvar "celtype" "bylayer");线型随层
	(setvar "celweight" -1);线宽随层
	(setvar "psltscale" 0);缩放时不使用视口比例
	(setvar "celtscale" 1);设置当前对象比例为1
	(cond ;全局修改新建和现有对象的线型比例，显示虚线，默认值是1
		((= (getvar "ltscale") (setq lts 1000)))
		(t
			(setvar "ltscale" lts)
			(vla-Regen *doc* acActiveViewport)
		)
	)
	;至此结束
	(*error* nil)
)

;===============================================================

;取得obj集合中特定en的VLA名
(defun MJ:GetToolVla(obj en / lst n sym val)
	(vlax-for l obj (setq lst (cons (vla-get-name l) lst)))
	(setq lst (reverse lst))
	(setq lst (mapcar 'strcase lst))
	(setq sym (strcase en))
	(setq n (vl-position sym lst))
	(cond (n (vla-Item obj n)) (t nil))
)
;;说明:取得自定义组中工具条集或菜单集的VLA名
;;参数:mnugs:自定义菜单组名
;;参数:mb: 1=菜单组名  2=工具条组名
;;返回:菜单组或工具条组的VLA名
(defun MJ:GetMenusOrBars (mnugs mb / gs obj)
	(setq gs (vla-get-menugroups *ACAD*))
	(setq obj (MJ:GetToolVla gs mnugs))
	(setq mb (cond ((= 1 mb) vla-get-Menus) ((= 2 mb) vla-get-toolbars) (t nil)))
	(cond ((and mb obj)(mb obj)) (t nil))
)
;工具条开关
;示例(MJ:Toolbar "FSTL" "av_value" v);开与关
;参数 mgroup:菜单组 tbname:工具条名 v:开关控制
;参数V  -1和1开,0关,任意=开关循环
(defun MJ:Toolbar (mgroup tbname v / lst n tb)
	(setq tb (MJ:GetToolVla (MJ:GetMenusOrBars mgroup 2) tbname))
	(cond
		((member v (list -1 0 1 :vlax-true :vlax-false)))
		(T (setq lst '((:vlax-true . 1)(:vlax-false . 0)))
			(setq n (cdr (assoc (vla-get-visible tb) lst)))
			(setq v (abs (1- n)))
		)
	)
	(vla-put-visible tb v)
)


;;删除或插入下拉菜单
;(foreach l (list "参数(&P)" "帮助(&H)")
;	(vl-catch-all-apply 'vla-removefrommenubar (list (MJ:GetToolVla (MJ:GetMenusOrBars "ACAD" 1) l)))
;)

;vla-get-menus
;vla-get-onmenubar
;vla-insertinmenubar
;vla-removefrommenubar



;;=============界面设置==============
;CAD高版精简界面
(defun cad-theui1(/ layout menu statu)
	(and (setq menu (getvar "menubar"))(if (/= menu 0)(setvar "menubar" 0)));菜单栏
	(and (getvar "ribbonstate")(command-s "RibbonClose"))
	(and (setq layout (getvar "layouttab"))(if(/= layout 0)(setvar "layouttab" 0)));布局选项卡
	(and (setq statu (getvar "statusbar"))(if(/= statu 0)(setvar "statusbar" 0)));状态栏关闭
	(and (/= (getvar "gridmode") 0) (setvar "gridmode" 0));栅格，模型开，布局关
	(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(MJ:Toolbar "FSTL" "av_value" 1)
	;(and (getvar "RIBBONSTATE")(Backgroundcolor 0 0));模型黑，背景黑
	(Backgroundcolor 0 0);模型黑，背景黑
	(setenv "LayoutXhairPickboxEtc" "16777215");布局光标白色
	;(setenv "XhairPickboxEtc" "65535");十字光标颜色
	;(setenv "Model Xhair use tint" "1");为X、Y、Z轴染色
	(av:setenv "theui" 1)
	(princ "+精简")
)
;CAD高版经典界面
(defun cad-theui2()
	(and (getvar "menubar")(if(/=(getvar "menubar")1)(setvar "menubar" 1)));菜单栏
	(and (getvar "RIBBONSTATE")(command-s "RibbonClose"))
	(and (getvar "layouttab")(if(/=(getvar "layouttab")1)(setvar "layouttab" 1)));布局选项卡
	(and (getvar "statusbar")(if(/=(getvar "statusbar")1)(setvar "statusbar" 1)));状态栏
	(and (/= (getvar "gridmode") 0) (setvar "gridmode" 0));栅格，模型开，布局关
	(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(MJ:Toolbar "FSTL" "av_value" 1)
	(MJ:Toolbar "ACAD" "绘图" 1)
	(MJ:Toolbar "ACAD" "修改" 1)
	;(and (getvar "RIBBONSTATE")(Backgroundcolor 0 0));模型黑，背景黑
	(Backgroundcolor 0 0);模型黑，背景黑
	(setenv "LayoutXhairPickboxEtc" "16777215");布局光标白色
	;(setenv "XhairPickboxEtc" "16777215");十字光标颜色
	;(setenv "Model Xhair use tint" "0");为X、Y、Z轴染色
	(av:setenv "theui" 2)
	(princ "+经典")
)
;CAD高版常规界面
(defun cad-theui3()
	(and (getvar "menubar")(if(/=(getvar "menubar")0)(setvar "menubar" 0)));菜单栏
	(and (getvar "RIBBONSTATE")(command-s "Ribbon"));ribbon
	(and (getvar "layouttab")(if(/=(getvar "layouttab")1)(setvar "layouttab" 1)));布局选项卡
	(and (getvar "statusbar")(if(/=(getvar "statusbar")1)(setvar "statusbar" 1)));状态栏
	(cond
		((= (getvar "tilemode") 1) (setvar "gridmode" 1));栅格，模型开
		(t (setvar "gridmode" 0));栅格，布局关
	)
	(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(MJ:Toolbar "FSTL" "av_value" 0)
	;(and (getvar "RIBBONSTATE")(Backgroundcolor 3156001 16777215));模型灰，背景白
	(Backgroundcolor 3156001 16777215);模型灰，背景白
	(setenv "LayoutXhairPickboxEtc" "0");布局光标黑色
	(av:setenv "theui" 3)
	(princ "+常规")
)
;首次安装时初始化当前环境
(cond
	((av:getenv "theui"))
	(t
		(cad-theui2);切换到经典界面
		(princ "\n按F10键可切换不同界面！")
		(c:reavtlsenv);环境初始化
	)
)
;CAD界面调整
(defun c:theui(/ ct ui)
	(princ "-->界面切换")
	(setvar "cmdecho" 0)
	;(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(setq ui (av:getenv "theui"))
	(setq ct (av:getenv "theui1"))
	(if (null ct) (setq ct 0))
	(if (getvar "ribbonstate")
		(cond
			((= 1 ui)(cad-theui2))
			((= 2 ui)(cad-theui3))
			((= 1 ct)(cad-theui1))
			((= 0 ct)(cad-theui2))
			(t nil)
		)
		(cond
			((= 1 ui)(cad-theui2))
			((= 2 ui)(cad-theui1))
			((= 3 ui)(cad-theui1))
			(t nil)
		)
	)
	(if (= 1 (getvar "cleanscreenstate"))
		(setvar "cleanscreenstate" 0)
	)
	(command "commandline")
	(setvar "cmdecho" 1)
	(princ)
)


;==============全屏切换=================
(defun c:CleanScreen(/ cs-cad-theui2-0 cs-cad-theui2-1 cs-cad-theui3-0 cs-cad-theui3-1)
	;经典界面全屏
	(defun cs-cad-theui2-0()
		(and (getvar "layouttab")(setvar "layouttab" 0));模型布局标签栏
		(and (getvar "statusbar")(setvar "statusbar" 0));状态栏
		(and (getvar "menubar")(setvar "menubar" 0));菜单栏
		(command "cleanscreenon")
	)
	;经典界面正常
	(defun cs-cad-theui2-1()
		(and (getvar "layouttab")(setvar "layouttab" 1))
		(and (getvar "statusbar")(setvar "statusbar" 1))
		(and (getvar "menubar")(setvar "menubar" 1))
		(command "cleanscreenoff")
	)
	;高版界面全屏
	(defun cs-cad-theui3-0()
		(and (getvar "layouttab")(setvar "layouttab" 0))
		(and (getvar "statusbar")(setvar "statusbar" 0))
		(command "cleanscreenon")
		(MJ:Toolbar "FSTL" "av_value" :vlax-true)
	)
	
	;关闭全部工具条
	;	(foreach l (mapcar '(lambda(x) (MJ:GetMenusOrBars x 2)) (list "ACAD" "FSTL"))
	;	(vlax-for s l (vla-put-visible s 0))
	;)
	
	;高版界面正常
	(defun cs-cad-theui3-1()
		(and (getvar "layouttab")(setvar "layouttab" 1))
		(and (getvar "statusbar")(setvar "statusbar" 1))
		(command "cleanscreenoff")
		(MJ:Toolbar "FSTL" "av_value" :vlax-false)
	)
	;主程序开始
	(princ "-->全屏切换")
	(setvar "cmdecho" 0)
	(cond
		((and (= 1 (av:getenv "theui")))
			(MJ:Toolbar "FSTL" "av_value" "开关")
		)
		((and
			 (= 2 (av:getenv "theui"))
			 (= 0 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui2-0)
		)
		((and
			 (= 2 (av:getenv "theui"))
			 (= 1 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui2-1)
		)
		((and
			 (= 3 (av:getenv "theui"))
			 (= 0 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui3-0)
		)
		((and
			 (= 3 (av:getenv "theui"))
			 (= 1 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui3-1)
		)
		(t nil)
	)
	(setvar "cmdecho" 1)
	(princ)
)



;=================还原当前配置================

(defun c:reop(/ info)
	(setq info "此命令会初始化全部配置信息！\n\n是否继续？")
	(if (= 1 (api-msgbox "风险提示" info (+ 1 48 256 0 262144)))
		(progn
			(vlax-Invoke-Method
				(vla-get-Profiles (vlax-get-Property *ACAD* 'Preferences))
				'ResetProfile
				(getvar "cprofile")
			)
			(and (getvar "RIBBONSTATE")(setvar "gridmode" 1));栅格开
			(and (getvar "secureload")(setvar "secureload" 0));加载任意位置插件不提示
			(av:AddSupportPath (list (strcat *fstl_dir* "\\support")))
			(if (av:getenv "cursorsize")
				(vl-catch-all-apply 'vl-registry-delete (list *avzzts-reg-key* "cursorsize"))
			)
			(if av:unpgp (av:unpgp))
		)
	)
	(princ)
)

;===========程序结束处===================
(princ)




