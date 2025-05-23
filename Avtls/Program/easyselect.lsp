;快速选择
(defun c:easyselect (/ en g1 g2 getsamecolorss mouse my_qselect n ob ss)
  (defun My_qselect (en code / ss lst styp)
		(setq lst (entget (car en)))
		(setq styp (assoc code lst))
		(cond
			(styp (setq ss (ssget (list styp))))
			(t (setq ss (ssget (list (cons code "bylayer"))))))
    ss
	)
	;提取同色图元
	(defun GetSameColorSS (en / c c2 lst ss ss1 xh)
		(setq
			lst (entget (car en))
			c (cdr (assoc 62 lst))
		)
		(if (null c);如果图元颜色为随层
			(setq c (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 lst))))));提取图层颜色
		)
		(setq ss1 (ssget) ss (ssadd));新建选择集
		(repeat (setq xh (sslength ss1));循环选择集的每一个图元
			(setq
				lst (entget (ssname ss1 (setq xh (1- xh))))
				c2  (cdr (assoc 62 lst));图元颜色
			)
			(if (null c2);如果图元颜色为随层
				(setq c2 (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 lst))))));提取图层颜色
			)
			(if (= c2 c)(ssadd (ssname ss1 xh) ss));加入到选择集
		)
		ss
	)
	;开始奔跑
	(if (= 1 (getvar "nomutt"))(setvar "nomutt" 0))
	(princ "\n请选择筛选类型[图层<A>/颜色<S>/类型<D>/小菜选择易<F>](左键F,右键A,空格S):")
	(setq mouse (grread nil 4 0));不追踪鼠标位置
	;(princ mouse);显示动作返回值
	(terpri)
	(setq en (entsel "请选择参照对象"))
	(setq g1 (car mouse) g2 (cadr mouse))
	(cond
		((null en)(setq ss nil))
		((or
			 (member g2 '(65 97));按键A
			 (= g1 25);右键设置为回车时
			 (equal mouse '(2 13));回车
		 )
			(setq ob "图层")
			(setq ss (My_qselect en 8));图层
		)
		((or
			 (member g2 '(83 115));按键S
			 (equal mouse '(2 32));空格
		 )
			(setq ob "颜色")
			(setq ss (GetSameColorSS en));颜色
		)
		((member g2 '(68 100));按键D
			(setq ob "类型")
			(setq ss (My_qselect en 0));类型
		)
		((or
			 (member g2 '(70 102));按键F
			 (= g1 3);左键点击
		 )
			(setq ob "条件")
			(setq ss (easysel-xc en));小菜选择易
		)
		(t (princ "请按提示操作"))
	)
	(cond
		((and ss (/= 0 (setq n (sslength ss))))
			(princ (strcat "\n已选择" (itoa n) "个相同" ob "的对象"))
		)
		(en (princ "没有发现该类型的图元！"))
		(t nil)
	)
	(if (= 0 (getvar "nomutt"))(setvar "NOMUTT" 1))
	(and ss (sssetfirst nil ss))
	ss
)
;====================================================================
;小菜选择易
(defun easysel-xc (ent / attdis code color dcl_name entl f filter fjflt fjlst flag hand index_value kl_pre klst ktmp lst2 lst3 lst4 lst5 slent ss ssl strtmp) ;ss_saved_lst ;全局变量; lst5 ?
	(setq
		kl_pre (last ss_saved_lst)
		slent (car ent)
	)
	(if slent
		(progn
			(setq
				attdis "Y" ;打开块属性显示，关闭为"N"
				fjflt nil filter nil entl (entget slent)
			)
			(setq lst2 '(
										("通用" ((0 "实体类型") (6 "实体线型") (8 "所在图层") (48 "线型比例") 
															(62 "实体颜色" 
																((256 "随层") (0 "随块") (1 "红色") (2 "黄色") (3 "绿色")
																	(4 "青色") (5 "蓝色") (6 "紫色") (7 "黑白")
																)
															) 
															(370 "实体线宽")
														)
										)
										("ARC" ((-4 "圆弧") (10 "圆心坐标") (40 "圆弧半径") (39 "实体厚度") (50 "起点角度") (51 "终点角度")) ("FJ" ("FJ1" "圆弧长度" (len slent))))
										("CIRCLE" ((-4 "圆形") (10 "圆心坐标") (40 "圆形半径") (39 "实体厚度")))
										("SOLID" ((-4 "SOLID") (39 "实体厚度")))
										("POINT" ((-4 "点") (10 "点的位置") (39 "实体厚度") (50 "旋转角度")))
										("LINE" ((-4 "直线段") (10 "起点坐标") (11 "终点坐标") (39 "实体厚度"))
											("FJ" ("FJ1" "线段长度" (len slent))
												("FJ2" "线段角度" (REM (ATOF (ANGTOS (ANGLE (DXF 10 slent) (DXF 11 slent)))) 180))));end 
										("ELLIPSE" ((-4 "椭圆") (10 "椭圆中心") (11 "长轴端点") (40 "长短轴比") (41 "开始参数") (42 "结束参数")))
										("INSERT" ((-4 "图块") (10 "图块位置") (2 "图块名称") (41 "X 轴比例") (42 "Y 轴比例") (43 "Z 轴比例") (50 "旋转角度")))
										;("FJ" ("FJ1" "属性标志" (car (attstr slent))) ("FJ2" "属性数值" (cadr (attstr slent)))))
										("LWPOLYLINE" ((-4 "轻多义线") (38 "复线标高") (43 "固定宽度") (90 "顶点个数") (39 "复线厚度") (70 "是否闭合" ((0 "不闭合") (1 "闭合"))))
											("FJ" ("FJ1" "曲线长度" (len slent))));end 
										("POLYLINE" ((-4 "重多义线") (70 "是否闭合" ((0 "不闭合") (1 "闭合")))) ("FJ" ("FJ1" "曲线长度" (len slent))))
										("HATCH" ((-4 "图案填充") (2 "填充图案") (41 "填充比例") (52 "填充角度") (71 "边界关联" ((0 "不关联") (1 "关联")))
															 (76 "图案类型" ((0 "用户定义") (1 "预定义") (2 "自定义")))));end 
										("TEXT" ((-4 "文字") (1 "文字内容") (7 "文字样式") (10 "插入位置") (40 "文字高度") (41 "宽度系数") (50 "旋转角度") (51 "倾斜角度")
															(71 "文字镜像" ((0 "默认") (2 "文字反向") (4 "文字倒置") (6 "反向倒置")))
															(72 "水平对齐" ((0 "左对齐") (1 "居中对齐") (2 "右对齐") (3 "对齐") (4 "中间") (5 "拟合")))
															(73 "垂直对齐" ((0 "基线对齐") (1 "底端对齐") (2 "居中对齐") (3 "顶端对齐")))) ("FJ" ("FJ1" "文字数值" (ATOF (DXF 1 slent)))));end 
										("ATTDEF" ((-4 "属性定义") (2 "属性标记") (7 "字型样式") (10 "插入位置") (40 "文字高度") (50 "旋转角度") (51 "倾斜角度")
																(71 "文字镜像" ((0 "默认") (2 "文字反向") (4 "文字倒置") (6 "反向倒置")))
																(72 "水平对齐" ((0 "左对齐") (1 "居中对齐") (2 "右对齐") (3 "对齐") (4 "中间") (5 "拟合")))
																(73 "垂直对齐" ((0 "基线对齐") (1 "底端对齐") (2 "居中对齐") (3 "顶端对齐")))) ("FJ" ("FJ1" "标记数值" (ATOF (DXF 2 slent)))));end 
										("MTEXT" ((-4 "多行文字") (10 "插入位置") (1 "文字内容") (7 "文字样式") (40 "文字高度") (50 "旋转角度")))
										("SPLINE" ((-4 "样条曲线") (70 "曲线标志") (71 "曲线阶数") (72 "节点数量") (73 "控制点数") (74 "拟合点数")
																(42 "节点公差") (43 "控点公差") (44 "拟合公差")) ("FJ" ("FJ1" "曲线长度" (len slent))));end 
										("DIMENSION" ((-4 "尺寸标注") (1 "标注文字") (42 "测量值") (3 "标注样式")
																	 (70 "标注类型" ((32 "水平垂直") (33 "对齐标注") (34 "角度标注")
																										(35 "直径标注") (36 "半径标注") (37 "三点角度") (38 "坐标标注")))));end 
									)
			)
			(if (and (= attdis "Y") (= "INSERT" (dxf 0 slent))) (kldc_1));;对块实体，增加属性过滤表，slent及lst2作为全局变量传递
			(setq
				lst3 (car (dxf "通用" lst2))
				lst5 (dxf (dxf 0 entl) lst2)
				lst4 (car lst5)
				lst5 (cadr lst5)
			)
			(foreach tmp lst3
				(if (and (not (dxf (car tmp) entl)) (/= (car tmp) 62)) (setq lst3 (vl-remove tmp lst3)))
			)
			(setq
				dcl_name (strcat (getenv "temp") "\\sel" ".dcl")
				f (OPEN dcl_name "w"))
			(write-line "sl:dialog{label=\"我的选择易--By 小菜\";" f)
			(write-line ":column{" f)
			(write-line ":boxed_column{label=\"过滤条件\";" f)
			(write-line ":boxed_column{label=\"通用\";" f)
			(foreach tmp lst3
				(write-line ":row{fixed_width=true;" f)
				(write-line (strcat ":toggle{key=\"" (itoa (car tmp)) "\";label=\"" (cadr tmp) "\";width=12;}") f)
				(write-line (strcat ":popup_list{edit_width=5;key=\"pop" (itoa (car tmp)) "\";}") f)
				(setq ktmp (list (strcat "pop" (itoa (car tmp))) (itoa (car tmp))))
				(if (/= 62 (car tmp))
					(progn
						(setq ktmp (write f ktmp (car tmp) (vl-princ-to-string (dxf (car tmp) entl)) "txt" "16"))
						(if (= 48 (car tmp))
							(setq ktmp (write f ktmp 48 "容差" "txta" "7"))
						)
					)
					(progn
						(setq color (dxf 62 entl)) (if (not color) (setq color 256))
						(setq ktmp (write f ktmp 62 (itoa color) "txt" "16"))
						(write-line (strcat ":edit_box{value=\"" (vl-princ-to-string (car (dxf color (caddr tmp)))) "\";edit_width=7 ;allow_accept=true;}") f)
					)
				)
				(write-line "}" f)
				(setq klst (cons (reverse ktmp) klst))
			)
			(write-line "}" f)
			(write-line (strcat ":boxed_column{label=\"" (vl-princ-to-string (car (dxf -4 lst4))) "\";") f)
			(setq lst4 (cdr lst4));;去掉前面的-4组码 
			(foreach tmp lst4
				(setq code (car tmp) ktmp nil)
				(if (dxf code entl)
					(progn
						(write-line ":row{fixed_width=true;" f)
						(setq ktmp (list (strcat "pop" (itoa code)) (itoa code)))
						(write-line (strcat ":toggle{key=\"" (itoa code) "\";label=\"" (vl-princ-to-string (cadr tmp)) "\";width=12;}") f)
						(write-line (strcat ":popup_list{edit_width=5;key=\"pop" (itoa code) "\";}") f)
						(cond
							((or (= code 10) (= code 11))
								(setq ktmp (write f ktmp code (vl-princ-to-string (car (dxf code entl))) "txt_x" "6.5"))
								(setq ktmp (write f ktmp code (vl-princ-to-string (cadr (dxf code entl))) "txt_y" "6"))
								(setq ktmp (write f ktmp code (vl-princ-to-string (caddr (dxf code entl))) "txt_z" "7"))
							)
							((member code '(1 2 3 7 90 38 39 40 41 42 43 44 50 51 52 70 71 72 73 74 76))
								(setq strtmp (vl-princ-to-string (dxf code entl)))
								(if (= code 1)
									(foreach tmp '("\r\n" "\\P" "\\") 
										(while (vl-string-search tmp strtmp) (setq strtmp (vl-string-subst " " tmp strtmp)))
									);消除acad2005中的mtext中的换行符(shift+enter)导致对话框不正常
								)
								(setq ktmp (write f ktmp code strtmp "txt" "16"));原strtmp=(vl-princ-to-string (dxf code entl))
								(cond
									((member code '(38 39 40 41 42 43 44 50 51 52))
										(setq ktmp (write f ktmp code "容差" "txta" "7"))
									)
									((member code '(70 71 72 73 74 76))
										(if (car (dxf (dxf code entl) (cadr (dxf code lst4))))
											(write-line (strcat ":edit_box{value=\"" (vl-princ-to-string (car (dxf (dxf code entl) (cadr (dxf code lst4))))) "\";edit_width=7;allow_accept=true;}") f)
										)
									)
								)
							)
						)
						(write-line "}" f)
					))
				(if ktmp (setq klst (cons (reverse ktmp) klst)))
			)
			(write-line "}" f)
			(if lst5
				(progn (setq lst5 (cdr lst5));去掉lst5第一个元素"FJ" 
					(write-line ":boxed_column{label=\"附加过滤\";" f)
					(foreach tmp lst5
						(write-line ":row{fixed_width=true;" f)
						(write-line (strcat ":toggle{key=\"" (car tmp) "\";label=\"" (cadr tmp) "\";width=12;}") f)
						(write-line (strcat ":popup_list{edit_width=5;key=\"pop" (car tmp) "\";}") f)
						(setq ktmp (list (strcat "pop" (car tmp)) (car tmp)))
						(setq ktmp (write f ktmp (car tmp) (vl-princ-to-string (eval (caddr tmp))) "txt" "16"))
						(setq ktmp (write f ktmp (car tmp) "容差" "txta" "7"))
						(setq fjlst (cons (reverse ktmp) fjlst));;fjlst是附加过滤条件的变量表 
						(write-line "}" f)
					)
					(write-line "}" f)
				))
			(write-line "}:row{:boxed_radio_row{label=\"过滤范围\";" f)
			(write-line ":radio_button{label=\"手选\";key=\"hand\";value=\"1\";}" f)
			(write-line ":radio_button{label=\"预选\";key=\"pre\";}" f)
			(write-line ":radio_button{label=\"全图\";key=\"all\";}" f)
			(write-line "}}:row{ok_cancel;}}}" f)
			(close f)
			(setq klst (reverse klst))
			(setq index_value (load_dialog dcl_name));_加载dcl文件 
			(new_dialog "sl" index_value);_开始新对话框 
			(foreach tmp klst;;klst为变量表，第三项开始含有变量名及初始值 
				;;如：'(("0" "pop0" ("txt0" "INSERT")) ("8" "pop8" ("txt8" "_消防报警")) ("62" "pop62" ("txt62" "256")) 
				;;("10" "pop10" ("txt_x10" "3431.58") ("txt_y10" "-17355.0") ("txt_z10" "0.0")) ("2" "pop2" ("txt2" "RXF008")) 
				;;("41" "pop41" ("txt41" "-64.0") ("txta41" "容?.. 
				(cond
					((member (car tmp) '("0" "1" "2" "3" "6" "7" "8"))
						(show_list (cadr tmp) '("=" "<>")))
					((member (car tmp) '("10" "11" "38" "39" "40" "41" "42" "43" "44" "48" "50" "51" "52"))
						(show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>")))
					((member (car tmp) '("62" "70" "71" "72" "73" "74" "76" "90"))
						(show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>" "&" "&=")))
				)
			);foreach 显示下拉选单信息 
			(if fjlst (foreach tmp fjlst (show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>"))));;;;end if fjlst;显示附加过滤下拉选单信息 
			;;;;fjlst是附加过滤条件的变量表,如：'(("FJ3" "popFJ3" ("txtFJ3" "0.0") ("txtaFJ3" "容差")) ("FJ2" "popFJ2" ("txtFJ2" "0.0") 
			;;("txtaFJ2" "容差")) ("FJ1" "popFJ1" ("txtFJ1" "0.0") ("txtaFJ1" "容差"))) 
			(if kl_pre
				(foreach tmp (cdr kl_pre)
					(if (= (dxf 0 entl) (car kl_pre))
						(set_tile (car tmp) "1")
						(if (member (car tmp) '("0" "6" "8" "48" "62" "370"))
							(set_tile (car tmp) "1")
						)
					)
				)
			);把上次选中的复选框设为选中状态 
			(action_tile "accept" "(get_filter) (done_dialog 1)")
			(setq flag (start_dialog))
			(unload_dialog index_value)
		)
		(setq
			hand (car ss_saved_lst)
			fjflt (cadr ss_saved_lst)
			filter (caddr ss_saved_lst)
		)
	)
	(if filter
		(cond
			((= hand "1") (setq ss (ssget filter)))
			((= hand "2") (setq ss (ssget "p" filter)))
			((= hand "3") (setq ss (ssget "x" filter)))
		)
	)
	(if (and (setq ssl (chsget ss)) fjflt)
		(foreach slent ssl (if (not (eval fjflt)) (setq ss (ssdel slent ss))))
	)
	(setq ss_saved_lst (list hand fjflt filter kl_pre));保存至全局变量
	(sssetfirst nil ss)
	ss
)
(defun get_filter (/ tmp pop txt txt1 rc txt2 txt3 pop_1 pop_2 pop_3)
	(cond ((= "1" (get_tile "hand")) (setq hand "1"))
		((= "1" (get_tile "pre")) (setq hand "2"))
		((= "1" (get_tile "all")) (setq hand "3"))
	)
	(foreach tmp klst (if (/= "1" (get_tile (car tmp))) (setq klst (vl-remove tmp klst))))
	(foreach tmp fjlst (if (/= "1" (get_tile (car tmp))) (setq fjlst (vl-remove tmp fjlst))))
	(setq kl_pre (append (list (dxf 0 entl)) klst fjlst));;附加过滤选中的项下次使用也成为缺省选中 
	(foreach tmp klst
		(setq pop (get_tile (cadr tmp)))
		(cond ((member (car tmp) '("0" "1" "2" "3" "6" "7" "8"))
						(setq txt (get_tile (caaddr tmp))
							txt1 (cadr (caddr tmp)));end setq
						(if (= txt txt1) (setq txt (dxf (read (car tmp)) entl)));如果(car tmp)对应的值未被用户修改过，取回原来的值 
						(cond ((= pop "0");(setq txt (get_tile (caaddr tmp)) 
										(setq filter (append (cons '(-4 . "<OR") (cons (cons (read (car tmp)) txt) '((-4 . "OR>")))) filter)
										)
									)
							((= pop "1");(setq txt (get_tile (caaddr tmp)) 
								(setq filter (append (cons '(-4 . "<NOT") (cons (cons (read (car tmp)) txt) '((-4 . "NOT>")))) filter)
								)
							)
						)
					)
			((member (car tmp) '("62" "70" "71" "72" "73" "74" "76" "90"))
				(setq txt (get_tile (caaddr tmp))
					filter (append
									 (cons (cons -4 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>" "&" "&="))) (list (cons (read (car tmp)) (read txt))))
									 filter
								 )
				)
			)
			((member (car tmp) '("38" "39" "40" "41" "42" "43" "44" "48" "50" "51" "52"))
				(setq txt (get_tile (caaddr tmp))
					txt1 (cadr (caddr tmp))
					rc (read (get_tile (car (last tmp))))
				)
				(if (/= txt txt1) (setq txt (atof txt)) (setq txt (dxf (read (car tmp)) entl)));;如果(car tmp)对应的值未被用户修改过，取回原来的实数数值 
				(if (and (or (= (type rc) 'REAL) (= (type rc) 'INT)) (= pop "0"));如果设置了容差，且为数值型，过滤条件为"="时要处理容差 
					(setq filter (append;;处理容差 
												 (cons '(-4 . "<=") (list (cons (read (car tmp)) (+ txt (abs rc)))))
												 (cons '(-4 . ">=") (list (cons (read (car tmp)) (- txt (abs rc)))))
												 filter
											 )
					)
					(setq filter (append;不处理容差 
												 (cons (cons -4 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))) (list (cons (read (car tmp)) txt)))
												 filter
											 )
					)
				);if 容差 
			)
			((member (car tmp) '("10" "11"))
				(setq txt1 (get_tile (caaddr tmp))
					txt2 (get_tile (car (cadddr tmp)))
					txt3 (get_tile (car (last tmp)))
					pop_1 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))
					pop_2 pop_1
					pop_3 pop_1
				)
				(if (= txt1 "") (setq pop_1 "*"))
				(if (= txt2 "") (setq pop_2 "*"))
				(if (= txt3 "") (setq pop_3 "*"))
				(if (/= txt1 (cadr (caddr tmp)))
					(setq txt1 (atof txt1)) (setq txt1 (car (dxf (read (car tmp)) entl))));;如果坐标对应的值未被用户修改过，取回原来的实数数值 
				(if (/= txt2 (cadr (cadddr tmp))) (setq txt2 (atof txt2)) (setq txt2 (cadr (dxf (read (car tmp)) entl))))
				(if (/= txt3 (cadr (last tmp))) (setq txt3 (atof txt3)) (setq txt3 (caddr (dxf (read (car tmp)) entl))))
				(setq filter (append
											 (cons (cons -4 (strcat pop_1 "," pop_2 "," pop_3)) (list (cons (read (car tmp)) (list txt1 txt2 txt3))))
											 filter
										 )
				)
			);end of member (car tmp) '("10" "11") 
		)
	)
	(if fjlst
		(progn
			(if (null filter) (setq filter (list (assoc 0 entl))));;如果仅选中的附加条件，则将filter设为样板实体的类别 
			(setq fjflt '(and))
			(foreach tmp fjlst
				(setq pop (get_tile (cadr tmp))
					txt (get_tile (caaddr tmp))
					txt1 (cadr (caddr tmp))
					rc (read (get_tile (car (last tmp))))
				)
				(if (/= txt txt1)
					(if (/= "INSERT" (dxf 0 slent)) (setq txt (atof txt)));图块实体的附加过滤为字符型，其余为数值型
					(setq txt (eval (cadr (dxf (car tmp) lst5))))
				);如果(car tmp)对应的值未被用户修改过，取回原来的实数数值 
				(if (and (or (= (type rc) 'REAL) (= (type rc) 'INT)) (= pop "0"));;如果设置了容差，且为数值型，过滤条件为"="时要处理容差 
					(setq fjflt (append;;处理容差 
												fjflt
												(list (list 'and
																(list '<= (cadr (dxf (car tmp) lst5)) (+ txt (abs rc)))
																(list '>= (cadr (dxf (car tmp) lst5)) (- txt (abs rc)))
															))
											)
					)
					(setq fjflt (append;不处理容差 
												fjflt
												(list (list (read (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))) (cadr (dxf (car tmp) lst5)) txt))
											)
					)
				);end of if 容差 
			)
		))
)
(defun show_list (key lst)
	(start_list key)
	(mapcar 'add_list lst)
	(end_list)
)
(defun write (f ktmp code value txt width / tmp)
	(setq tmp (strcat txt (vl-princ-to-string code)))
	(write-line (strcat ":edit_box{value=\"" value "\";key=\"" tmp "\";edit_width=" width ";allow_accept=true;}") f)
	(setq ktmp (cons (list tmp value) ktmp))
)
(defun dxf (i ent)
	(if (= (type ent) 'ENAME)
		(setq ent (entget ent))
	)
	(cdr (assoc i ent))
)
(defun chsget (c01 / c02 c03 c04 c05)
	(if c01
		(progn
			(setq c02 0 c03 (sslength c01))
			(while (< c02 c03)
				(setq c04 (ssname c01 c02) c02 (1+ c02))
				(setq c05 (cons c04 c05))
			)
		)
	)
	c05
)
(defun len (ent)
	(if (= (type ent) 'ENAME) (setq ent (vlax-ename->vla-object ent)))
	(if (wcmatch (vla-get-ObjectName ent) "AcDbPolyline,AcDbEllipse,AcDbCircle,AcDbArc,AcDbLine,AcDb2dPolyline,AcDbSpline")
		(vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
	)
)
(defun VxGetAtts (Obj)
	(if (= (type Obj) 'ENAME) (setq Obj (vlax-ename->vla-object Obj)))
	(if (= (vla-get-ObjectName obj) "AcDbBlockReference")
		(mapcar
			'(lambda (Att)
				 (cons (vla-get-TagString Att)
					 (vla-get-TextString Att)
				 )
			 )
			(vlax-invoke Obj "GetAttributes")
		)
	)
)
(defun KLDC_1 (/ attl alen lval ltag aflst aa cc a11 a12 a13)
	(setq attl (VxGetAtts slent))
	(if attl
		(progn
			(setq alen (length attl))
			(while (> alen 0)
				(setq
					a11 (list (cons 'nth (cons (- alen 1) '((VxGetAtts slent)))))
					a12 (cons 'if (list '(VxGetAtts slent) (cons 'cdr a11)))
					a13 (cons 'if (list '(VxGetAtts slent) (cons 'car a11)))
					lval (list (strcat "FJ" (rtos (* 2 alen) 2 0)) ' "属性数值" a12)
					ltag (list (strcat "FJ" (rtos (- (* 2 alen) 1) 2 0)) ' "属性标志" a13)
					aflst (append (list lval ltag) aflst)
					alen (1- alen))
			)
			(setq
				aa (assoc "INSERT" lst2)
				cc (list (car aa) (cadr aa) (append '(FJ) (reverse aflst)))
				lst2 (subst cc aa lst2))
		)
	)
)
(if (not (member "acopm.arx" (arx))) (arxload "acopm.arx"))


(princ)



