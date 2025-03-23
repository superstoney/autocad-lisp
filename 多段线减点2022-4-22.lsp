;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defun c:ddxjd( / XTBL_MingLingTiShi   ang1_input   ang2_input  e  pts  n  have_arc  first_lst  )
	;主程序
	(setq XTBL_MingLingTiShi (getvar "cmdecho"))	;获取当前的普通命令提示状态
	(if (/= dis nil)
		(progn 
			(setq dis (getdist (strcat "\n请指定最小点距<上次是 " (rtos dis 2 3) " ，默认是nil，不要直接回车>：")))
			(while (not dis)
				(setq dis (getdist (strcat "\n请指定最小点距<上次是 nil ，不要直接回车>：")))
			)
		)
		
		(progn 
			(setq dis (getdist "\n请指定最小点距："))
			(while (not dis)
				(setq dis (getdist "\n请指定最小点距："))
			)
		)
	)
	
	(if (or (= myang1 nil) (= myang1 ""))
		(progn
			(setq ang1_input (getstring (strcat "\n请输入三点一线的角度允许误差<单位 度 ，默认为 0.1 >：" )))
			(if (/= ang1_input "")
				(setq myang1 ang1_input)
				(setq myang1 "0.1")
			)			
		); ;当ang1的值为nil或空时，执行此progn
		
		(progn
			(setq ang1_input (getstring (strcat "\n请输入三点一线的角度允许误差<单位 度 ，默认 " myang1 " >：")))
			(if (/= ang1_input "")
				(setq myang1 ang1_input)
			)
		); 当ang1的值为既不是nil也不是空时，执行此progn
	);end if
	
	(if (or (= myang2 nil) (= myang2 ""))
		(progn
			(setq ang2_input (getstring (strcat "\n当相邻三点线段夹角大于多少度时不减点<取值范围是 0~90度 ，默认为 15 >：" )))
			(if (/= ang2_input "")
				(setq myang2 ang2_input)
				(setq myang2 "15")
			)			
		); ;当ang2的值为nil或空时，执行此progn
		
		(progn
			(setq ang2_input (getstring (strcat "\n当相邻三点线段夹角大于多少度时不减点<取值范围是 0~90度 ，默认 " myang2 " >：")))
			(if (/= ang2_input "")
				(setq myang2 ang2_input)
			)
		); 当ang2的值为既不是nil也不是空时，执行此progn
	);end if	
	
	(setq e (car (entsel "\n选择多段线：")))
	(while (/= (cdr (assoc 0 (entget e))) "LWPOLYLINE")
		(setq e (car (entsel "\n选择多段线：")))
	);end while,	
	
	(setq pts (ff:polyline_vertices->lst e))	;获取多段线顶点坐标列表,写入点表pts，如果子表有2个元素，依次表示多段线顶点坐标、圆弧圆心坐标
	(setq n (length pts))	;获取点表pts的子表个数
	(setq have_arc nil)	;定义have_arc变量，用于标记点表pts中是否有圆弧坐标
	(repeat n
		(setq first_lst (car pts))
		(setq pts (cdr pts))
		(if (> (length first_lst) 1) 
			(setq have_arc T) ;当发现子表中有圆弧坐标时，将变量have_arc的值设为T
		);end if,
	)
	
	(command "_.undo" "_be")
	(if (= have_arc nil)
		(progn
			(ff:ddxJD1 e  dis myang1  myang2)
		);处理不含圆弧的多段线
		
		(progn
			(ff:ddxJD2 e  dis myang1  myang2)
		);处理有圆弧的多段线
	)
	(command "_.undo" "_e")
	
	(setvar "cmdecho" XTBL_MingLingTiShi) 	;恢复普通命令提示原来的显示状态
	(princ)
	
)



(defun ff:ddxJD1 ( e  dis  ang1  ang2 / x  pts  mylst)
	;主程序,多段线减点。当多段线包含圆弧时，圆弧将被视为线段，线段的起点和终点分别是圆弧的两个端点
	
	(setq pts (mapcar 'cdr (vl-remove-if  '(lambda (x) (/= (car x) 10))(entget e))))	;获取多段线顶点坐标列表
	(setq mylst (ff:delt_pt  pts  dis  ang1  ang2))
	(ff:makeplc mylst);抽掉中间点，抽掉点间距小于dis的点后的新线
	
	(command "_matchprop" e (entlast) "")	;生成的新线与原线属性匹配
	(command "erase" e "")	;删除原始多段线
	(princ)
)


;==============================子函数==============================
;多线段节点抽稀
(defun ff:delt_pt (pts  dis  ang1  ang2 /  pts1)
	
	;二级子函数，接收主函数传递来的n和lst，当ff:delt_sub_lst函数被调用的时候，n的赋值都是1
	;函数功能：去掉lst的第二个元素后返回表
	(defun ff:delt_sub_lst (n lst)
		(if (null lst) 
			nil
			
			;如果lst不为nil，执行下面的if
			(if (zerop n) 
				(cdr lst)		;当n=0时，将lst去掉第1个元素后返回表
				(cons (car lst) (ff:delt_sub_lst (- n 1) (cdr lst)))	;当n≠0时，去掉lst的第2个元素后返回表
			)
		)
	)
	
	
	;二级子函数，功能：判断三点是否共线，返回T或者nil
	;(defun ff:3pt_in_line (p1 p2 p3 ang1 / v1 v2 mylst)
	;   (setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
	;   (if (equal (car v1) 0.0 1e-4)
	;		(equal (car v1) (car v2) 1e-1)	;当x1-x2=0时，判断(x1-x2)与(x2-x3)是否相等。如果(x1-x2)=(x2-x3)=0，证明p1p2p3三点共线
	;		(equal (/ (cadr v1) (car v1))  (/ (cadr v2) (car v2))  (/ (sin (angtof ang1)) (cos (angtof ang1))));当x1-x2≠0时，判断直线p1p2与直线p2p3的斜率是否相等,此处equal的容差取值为tanA，如果斜率相等，证明p1p2p3三点共线
	;   )
	;)	
	
	
	(defun ff:3pt_in_line (p1 p2 p3 ang1 / boln  v1 v2 )
		(setq boln nil)
		(setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
		
		(if (and 
					(equal (car v1) 0 1e-4)
					(equal (car v2) 0 1e-4)
				)
			(setq boln T)	;当x1-x2=0,且x2-x3=0时，证明p1p2p3三点共线，且该线平行于y轴(斜率无穷大)
		)
		
		(if (and 
					(/= (car v1) 0)
					(/= (car v2) 0)
					(equal   (/ (cadr v1) (car v1))    (/ (cadr v2) (car v2))    (/ (sin (angtof ang1)) (cos (angtof ang1))))
				)
			(setq boln T)	;当x1-x2≠0,且x2-x3≠0，且直线p1p2与直线p2p3的斜率在容差范围内相等时，证明p1p2p3三点共线。此处equal的容差取值为tanA
		)
		boln
	)
	
	
	(setq pts1 nil)
	
	(while (> (length pts) 2)
		
		(if (ff:3pt_in_line (car pts) (cadr pts) (caddr pts) ang1)
			
			(setq pts (ff:delt_sub_lst 1 pts))		;当三点共线时，直接执行ff:delt_sub_lst子函数，去掉pts的第二个元素后返回表
			
			
			(progn 
				(if (< (distance (car pts) (cadr pts)) dis)
					(if (< (ff:calculate_line_intersect_angle (car pts) (cadr pts) (cadr pts) (caddr pts)) (atof  ang2))
						(setq pts (ff:delt_sub_lst 1 pts))	;计算线段p1p2和p2p3的夹角，如果线段夹角小于给定ang2，直接执行ff:delt_sub_lst子函数，去掉pts的第二个元素后返回表
						(setq 
							pts1 (cons (car pts) pts1)	;当线段夹角大于等于给定ang2，那么先把表pts的第1个元素放进表pts1中
							pts (cdr pts)	;当线段夹角大于等于给定ang2，再把表pts去除第1个元素后返回pts
						)						
					);如果p1p2的距离小于给定的距离dis，执行此if
					
					(setq 
						pts1 (cons (car pts) pts1)	;如果lst的第2个元素不用删除，那么先把表pts的第1个元素放进表pts1中
						pts (cdr pts)	;如果lst的第2个元素不用删除，再把表pts去除第1个元素后返回pts
					);如果p1p2的距离大于等于给定的距离dis，执行此setq			
				)
			);当三点不共线时，那么执行此progn
			
		);当表pts的元素个数等于2时结束while循环
		
		(setq mylst (cons (cadr pts) (cons (car pts) pts1)))	;将表pts剩余的2个元素与pts1合并，存入mylst表中，此时点的顺序与原顺序相反
		(reverse mylst)	;倒置表的点顺序，与原顺序一致
	)
)


;==============================子函数==============================
(defun ff:makeplc(pts)
	(entmakex(append(mapcar 'cons (list 0 100 100 90 38 43)
										(list "LWPOLYLINE" "AcDbEntity" "AcDbPolyline" (length pts)  0 0.0))
						 (mapcar '(lambda (p)(cons 10 p)) pts)))
	(princ)
)



(defun ff:ddxJD2 ( e  dis  ang1  ang2 / x  pts  mylst)
	;主程序,多段线减点，多段线包含圆弧的时候也适用
	
	(setq pts (ff:polyline_vertices->lst e))	;获取多段线顶点坐标列表,写入点表pts，如果子表有2个元素，依次表示多段线顶点坐标、圆弧圆心坐标
	;返回结果示例：(((549.574 730.432)) ((560.882 725.683) (563.78 732.584)) ((567.226 725.94)) ((578.562 731.82)) ((588.348 741.904) (593.719 736.692)) ((593.152 744.155)) ((606.16 745.143)))
	(setq pts (ff:pl_lst_add_arc_midpt e pts))	;将多段线圆弧中点坐标加入点表pts中，如果子表有3个元素，依次表示多段线顶点坐标、圆弧中点坐标、圆弧圆心坐标
	;返回结果示例：(((549.574 730.432)) ((560.882 725.683) (564.083 725.106) (563.78 732.584)) ((567.226 725.94)) ((578.562 731.82)) ((588.348 741.904) (590.544 743.469) (593.719 736.692)) ((593.152 744.155)) ((606.16 745.143)))
	
	(setq mylst (ff:delete_pt  pts  dis  ang1  ang2)) 
	(ff:draw_LWPOLYLINE mylst) ;n1不等于n2，说明有圆弧，调用这个子函数生成多段线
	
	(command "_matchprop" e (entlast) "")	;生成的新线与原线属性匹配
	(command "erase" e "")	;删除原始多段线
	(princ)
)


;==============================子函数==============================
;多线段节点抽稀
(defun ff:delete_pt (pts  dis  ang1 ang2 /  pts1)
	
	;二级子函数，接收主函数传递来的n和lst，当ff:delete_ptlst_element函数被调用的时候，n的赋值都是1
	;函数功能：
	;n=0时去掉lst的第1个元素后返回表
	;n=1时去掉lst的第2个元素后返回表
	(defun ff:delete_ptlst_element (n  lst)
		(if (null lst) 
			nil
			
			;如果lst不为nil，执行下面的if
			(if (zerop n) 
				(cdr lst)		;当n=0时，将lst去掉第1个元素后返回表
				(cons (car lst) (ff:delete_ptlst_element (- n 1) (cdr lst)))	;当n≠0时，去掉lst的第2个元素后返回表
			)
		)
	)
	
	
	;二级子函数，功能：判断三点是否共线，返回T或者nil
	;(defun ff:check_3pt_in_line (p1 p2 p3 ang1 / v1 v2 mylst)
	;   (setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
	;   (if (equal (car v1) 0.0 1e-4)
	;		(equal (car v1) (car v2) 1e-1)	;当x1-x2=0时，判断(x1-x2)与(x2-x3)是否相等。如果(x1-x2)=(x2-x3)=0，证明p1p2p3三点共线
	;		(equal (/ (cadr v1) (car v1))  (/ (cadr v2) (car v2))  (/ (sin (angtof ang1)) (cos (angtof ang1))));当x1-x2≠0时，判断直线p1p2与直线p2p3的斜率是否相等,此处equal的容差取值为tanA，A=2°，如果斜率相等，证明p1p2p3三点共线
	;   )
	;)	
	
	;二级子函数，功能：判断三点是否共线，返回T或者nil
	(defun ff:check_3pt_in_line (p1 p2 p3 ang1 / boln  v1 v2 )
		(setq boln nil)
		(setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
		
		(if (and 
					(equal (car v1) 0 1e-4)
					(equal (car v2) 0 1e-4)
				)
			(setq boln T)	;当x1-x2=0,且x2-x3=0时，证明p1p2p3三点共线，且该线平行于y轴(斜率无穷大)
		)
		
		(if (and 
					(/= (car v1) 0)
					(/= (car v2) 0)
					(equal   (/ (cadr v1) (car v1))    (/ (cadr v2) (car v2))    (/ (sin (angtof ang1)) (cos (angtof ang1))))
				)
			(setq boln T)	;当x1-x2≠0,且x2-x3≠0，且直线p1p2与直线p2p3的斜率在容差范围内相等时，证明p1p2p3三点共线。此处equal的容差取值为tanA
		)
		boln
	)	
	
	
	
	(setq pts1 nil)
	
	(while (> (length pts) 2)
		
		;先判断点p1和p2是不是圆弧起点
		(if (or (= (length (car pts)) 3) (= (length (cadr pts)) 3))
			
			(progn 
				(setq pts1 (cons (car pts) pts1))	;先把表pts的第1个元素放进表pts1中
				(setq pts (cdr pts))											;再把表pts去除第1个元素后返回pts				
			);当点表pts的第1个子表或第2个子表是圆弧起点时，执行此progn
			
			(progn 
				
				(if (ff:check_3pt_in_line (car (car pts)) (car (cadr pts)) (car (caddr pts)) ang1)	;取点表pts的前3个元素，判断是否三点一线
					
					(setq pts (ff:delete_ptlst_element 1 pts))		;当三点共线时，直接执行ff:delete_ptlst_element子函数，去掉pts的第二个元素后返回表
					
					(progn 
						(if (< (distance (car (car pts)) (car (cadr pts))) dis)
							(if (< (ff:calculate_line_intersect_angle (car (car pts)) (car (cadr pts)) (car (cadr pts)) (car (caddr pts))) (atof  ang2))
								(setq pts (ff:delete_ptlst_element 1 pts))	;计算线段p1p2和p2p3的夹角，如果线段夹角小于给定ang2，直接执行ff:delete_ptlst_element子函数，去掉pts的第二个元素后返回表
								(setq 
									pts1 (cons (car pts) pts1)	;当线段夹角大于等于给定ang2，那么先把表pts的第1个元素放进表pts1中
									pts (cdr pts)	;当线段夹角大于等于给定ang2，再把表pts去除第1个元素后返回pts
								)						
							);如果p1p2的距离小于给定的距离dis，执行此if
							
							(setq 
								pts1 (cons (car pts) pts1)	;如果lst的第2个元素不用删除，那么先把表pts的第1个元素放进表pts1中
								pts (cdr pts)	;如果lst的第2个元素不用删除，再把表pts去除第1个元素后返回pts
							);如果p1p2的距离大于等于给定的距离dis，执行此setq			
						)
					);当三点不共线时，那么执行此progn
					
				)				
				
			);当点表pts的第1个子表和第2个子表都不是圆弧起点时，执行此progn
			
		);end if,
		
	);end while,当点表pts只有2个元素时退出循环
	
	(setq mylst (cons (cadr pts) (cons (car pts) pts1)))	;将表pts剩余的2个元素与pts1合并，存入mylst表中，此时点的顺序与原顺序相反
	(reverse mylst)	;倒置表的点顺序，与原顺序一致
)


;==============================子函数==============================
(defun ff:draw_LWPOLYLINE ( pts / v1 n i k  p1  p2  p3  sunit)
	;函数功能：根据点表pts，绘制多段线
	;pts示例：(((549.574 730.432)) ((560.882 725.683) (564.083 725.106) (563.78 732.584)) ((567.226 725.94)) ((578.562 731.82)) ((588.348 741.904) (590.544 743.469) (593.719 736.692)) ((593.152 744.155)) ((606.16 745.143)))
	
	(setq v1 (getvar "osmode"))
	
	(setq n (length pts))	;计算点表pts的元素个数
	(setq i 0)
	(setq sunit (ssadd))
	(repeat (1- n)
		
		(setq k (length (car pts)))
		
		(if (= k 3)
			(progn 
				(setq p1 (car (car pts)))
				(setq p2 (cadr (car pts)))
				(setq p3 (car (cadr pts)))
				;用command命令绘制圆弧	
				(setvar "osmode" 0)
				(command "arc" p1 p2 p3)	;根据三个点绘制圆弧
				(setvar "osmode" v1)
				;用entmake函数绘制圆弧
				;(setq arc_lst (ff:calculate_arc_property p1 p2 p3))
				;(setq 
				;	po (car arc_lst)
				;	R (cadr arc_lst)
				;	ang1 (caddr arc_lst)
				;	ang2 (caddr (cdr arc_lst))
				;)
				;(entmake (list '(0 . "ARC") (cons 10 po) (cons 40 R) (cons 50 ang1) (cons 51 ang2)))
				(ssadd (entlast) sunit)	;将上一步绘制的圆弧加入选择集sunit里
				(setq pts (cdr pts))
			);k=3时执行此progn
			
			(progn 
				(setq p1 (car (car pts)))
				(setq p2 (car (cadr pts)))
				;用command命令绘制直线
				;(setvar "osmode" 0)
				;(command "line" p1 p2 "")
				;(setvar "osmode" v1)
				;用entmake函数绘制直线
				(entmake (list '(0 . "LINE") (cons 10 p1) (cons 11 p2)))
				(ssadd (entlast) sunit)	;将上一步绘制的直线加入选择集sunit里
				(setq pts (cdr pts))
			);k≠3时，证明k=1,执行此progn
		)		
		
		(setq i (1+ i))
	);end repeat
	
	(if sunit 
		(command "pedit" "M" sunit "" "Y" "J" 0 "")	;将选择集sunit里的所有对象合并为多段线
	)
	
	(princ)
)


(defun ff:polyline_vertices->lst (m_plent   /  m_pttab   m_pt1     m_pt2
							m_tmp     m_ptlist m_xc   m_radius  m_pt3
							m_ptcenter
						)
	;返回多段线的顶点坐标表，顶点坐标表的元素数量，与多段线的顶点数量相同
	;每个子表里有1个点或者2个点的坐标
	;子表中如果有1个点，则该点就是多段线的顶点坐标
	;子表中如果有2个点，表示该点是圆弧的起始端点，子表里的第1个点是多段线的顶点坐标，第2个点是圆弧的圆心坐标
	;返回的顶点表示例：(((1019.5 2257.22)) ((1013.69 2238.17) (1051.95 2226.5)) ((1019.44 2203.19)) ((1027.84 2191.48)) ((1068.14 2184.99)) ((1078.78 2175.75) (1062.38 2156.88)) ((1085.88 2148.35)) ((1078.7 2128.57)))
	;!!!!!!!!需要注意的是：多段线里有圆弧时，点表里只能找到圆弧两个端点及圆心坐标，这3个坐标还无法确定一个圆弧，圆弧可能是顺时针，也可能是逆时针
	
	(setq m_pttab (entget m_plent))
	
	(while (setq m_pt1 (assoc '10 m_pttab))
		(setq m_tmp (assoc '42 m_pttab))
		
		(if (/= 0.0 (cdr m_tmp))
			(if (setq m_pt2 (assoc '10 (cdr (member m_pt1 m_pttab)))) ;下一点
				(progn
					(setq m_xc (distance (cdr m_pt1) (cdr m_pt2))) ;弦长
					
					(setq	m_radius 
						(abs (/ (* m_xc (1+ (* (cdr m_tmp) (cdr m_tmp))))
									 (* 4 (cdr m_tmp))
								 )
						)
					)    ;半径R
					
					(setq m_pt3 
						(polar (cdr m_pt1)
							(angle (cdr m_pt1) (cdr m_pt2))
							(/ m_xc 2.0)
						)
					)
					
					(if (> 0.0 (cdr m_tmp))
						(setq m_ptcenter
							(polar m_pt3
								(- (angle (cdr m_pt1) (cdr m_pt2))
									(angtof "90")
								)
								
								(- m_radius
									(/ (* (abs (cdr m_tmp)) m_xc) 2.0)
								)
							)
						)
						
						(setq m_ptcenter
							(polar m_pt3
								(- (angle (cdr m_pt1) (cdr m_pt2))
									(angtof "270")
								)
								
								(- m_radius
									(/ (* (abs (cdr m_tmp)) m_xc) 2.0)
								)
							)
						)
					);end if
					
					(setq m_ptlist 
						(append m_ptlist
							(list (list (cdr m_pt1) m_ptcenter))
						)
					)
				);end progn
				(setq m_ptlist (append m_ptlist (list (list (cdr m_pt1)))))
			);end if
			
			(setq m_ptlist (append m_ptlist (list (list (cdr m_pt1)))))
		);end if
		
		(setq m_pttab (cdr (member m_tmp m_pttab)))
	);end while
	
	m_ptlist
)


(defun ff:pl_lst_add_arc_midpt ( ent lst / obj  n  i  sublst1  k  sublst2  p1  p2  lth1  lth2   lth_mid   pt_mid   sublst1_new)
	;函数功能：接收多段线图元名和顶点坐标表，点表示例如下：
	;(((434.099 602.085)) ((526.095 753.303) (601.891 707.192)) ((576.707 792.263)) ((735.127 839.162)) ((816.704 941.702) (747.275 996.937)) ((832.725 1020.8)) ((802.416 1129.32)))
	;如果多段线里有圆弧，则将圆弧的中点坐标写入点表中
	;!!!!!!!!!!注意：本函数需配合ff:polyline_vertices->lst使用，先调用ff:polyline_vertices->lst函数得到初始点表，然后再调用本函数增加圆弧中点坐标
	
	;(setq ent (car (entsel)))	;程序测试语句
	;(setq lst (ff:polyline_vertices->lst ent))	;程序测试语句
	(setq obj (vlax-ename->vla-object ent))	;将多段线ent转为VLA对象
	
	(setq n (length lst))
	(setq i 0)
	(repeat n
		(setq sublst1 (nth i lst))	;读取第i个顶点的点表，存入sublst1中
		(setq k (length sublst1))		;查询sublst1的子表个数，如果k=2，说明当前顶点是圆弧的起点，点表示例((526.095 753.303) (601.891 707.192))，子表依次表示多段线顶点坐标(也是圆弧起点坐标)、圆弧圆心坐标
		(if (= k 2)
			(progn
				(setq sublst2 (nth (+ 1 i) lst))
				(setq 
					p1 (car sublst1)	;获取多段线中圆弧起点坐标。注意：此起点并不一定是按顺时针确定的圆弧的起点
					p2 (car sublst2)	;获取多段线中圆弧终点坐标。注意：此终点并不一定是按顺时针确定的圆弧的终点
					lth1 (vlax-curve-getDistAtPoint obj p1)	;测量多段线起点至p1的长度
					lth2 (vlax-curve-getDistAtPoint obj p2)	;测量多段线起点至p2的长度
					lth_mid (/ (+ lth1 lth2) 2)	;计算多段线起点至圆弧中点的长度
					pt_mid (vlax-curve-getPointAtDist obj lth_mid)	;计算圆弧中点坐标，得到的是三维点，示例：(547.774 777.495 0.0)
					pt_mid (list (car pt_mid) (cadr pt_mid))	;去除Z坐标，得到XY坐标点表，示例：(547.774 777.495)
				)
				
				(setq sublst1_new (list (car sublst1) pt_mid (cadr sublst1)))	;构造新的点表sublst1_new，示例：((526.095 753.303) (547.774 777.495) (601.891 707.192))，子表依次表示多段线顶点坐标(也是圆弧起点坐标)、圆弧中点坐标、圆弧圆心坐标
				
				(setq lst (ff:subst_n i sublst1_new lst))

			)
		)
		
		(setq i (1+ i))
	)
	lst
)


(defun ff:calculate_line_intersect_angle(p1 p2 p3 p4 / k1 k2 tan_ang   ang)
	;函数功能：计算2直线的夹角
	;直线l1的坐标是p1,p2，直线l2的坐标是p3,p4
	(setq k1 (/ (sin (angle p1 p2)) (cos (angle p1 p2))))
	(setq k2 (/ (sin (angle p3 p4)) (cos (angle p3 p4))))
	
	(setq tan_ang (abs (/ (- k1 k2) (+ 1 (* k1 k2)))))
	;(setq ang (angtos (atan tan_ang) 0 8))			;计算夹角，并转为字符串
	(setq ang (/ (* 180 (atan tan_ang)) PI))		;计算夹角，结果为实数，单位是度
)


(defun ff:subst_n (n a lst)
	;函数功能：用指定的子表，替换lst中第n个元素，n从0开始计数
	;语法：(ff:subst_n   顺序号n  子表a  原表lst)
  (cond
    ((numberp n)
			(if (zerop n)
				(append (list a) (cdr lst))
				(cons (car lst) (ff:subst_n (1- n) a (cdr lst)))
			)
    )
    ((listp n)
			(cond
				((equal (length n) 1)
					(if (zerop (car n))
						(append (list a) (cdr lst))
						(cons (car lst) (ff:subst_n (1- (car n)) a (cdr lst)))
					)
				)
				((> (length n) 1)
					(if (zerop (car n))
						(cons (ff:subst_n (cdr n) a (car lst)) (cdr lst))
						(cons (car lst)
							(ff:subst_n (append (list (1- (car n))) (cdr n)) a (cdr lst))
						)
					)
				)
			)
    )
  )
)



