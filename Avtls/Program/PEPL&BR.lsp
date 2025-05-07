

;;修改多段线宽
(defun c:pl_width(/ ass data ex lst plwidth ss str w)
	(setq plwidth 20)
	(setq plwidth (cond
									((progn
										 (setq str (if *plwidth* (itoa *plwidth*) (itoa plwidth)))
										 (initget (+ 2 4))
										 (setq w (getint (strcat "\n请输入多段线宽<" str ">:")))
									 )
									)
									(*plwidth*)
									(t plwidth)
								)
	)
	(setq *plwidth* plwidth)
	(setq ex (cons 43 plwidth))
	(while (setq ss (ssget '((0 . "LWPOLYLINE"))))
		(setq lst (av:ss->ssnlst ss))
		(foreach l lst
			(setq data (entget l))
			(cond
				((and
					 (setq ass (assoc 43 data))
					 (= (cdr ass) plwidth)
				 )
				)
				(ass
					(setq data (subst ex ass data))
				)
				;(t
				;	(setq data (reverse (cons ex (reverse data))))
				;	(princ data)
				;)
			)
			(entmod data)
		)
	)
	(princ)
)


;;动态打断于线（在高版本中可实时看到删除效果）
(defun c:dynbreak (/ *error* ent m os pt1 ss v)
	;(princ "-->动态打断于线")
	(defun *error*(msg)
		(if v (vlr-add av:totalreader_pickfirst_reactor))
		(setvar "osmode" os)
		(setvar "cmdecho" 1)
		(princ)
	)
	(if (vlr-added-p av:totalreader_pickfirst_reactor)
		(setq v (vlr-remove av:totalreader_pickfirst_reactor))
	)
	(setvar "cmdecho" 0)
	(setq os (getvar "osmode"))
	(setq ss (entsel "\n选择对象:"))
	(sssetfirst nil (ssadd (car ss)))
	(setvar "osmode" 2595)
	(and ss
		(setq pt1 (getpoint "\n指定第一个打断点:"))
		(setq m (vl-cmdf "break" ss "f" pt1))
	)
	(while (= 1 (getvar "cmdactive"))(command pause))
	(if (and m (setq ent (entsel "请选择删除项")))
		(entdel (car ent))
	)
	(*error* nil)
)

;;多段线或面域布尔运算
(defun c:BooleanRegion(/ flg obj pt1 pt2 ss)
	(setvar "cmdecho" 0)
	(princ "\n请选择多段线或面域:<回车新建>")
	(setq flg t)
	(while (progn
					 (and flg (setq ss (ssget '((0 . "LWPOLYLINE,REGION")(70 . 1)))))
					 (null ss)
				 )
		(setq pt1 (getpoint "指定第一点:"))
		(setq pt2 (getcorner pt1 "\n指定矩形对角点:<回车切换多段线>"))
		(cond
			(pt2 (command-s "rectang" pt1 pt2))
			(t
				(command-s "pline" pt1)
				(while (= 1 (getvar "cmdactive")) (command-s pause))
				(setq obj (Vlax-Ename->Vla-Object (entlast)))
				(if (= 0 (Vlax-Get obj 'Closed)) (vlax-put obj 'Closed 1))
			)
		)
		(setq ss (ssadd (entlast)))
		(setq flg nil)
	)
	(setq ss (BooleanRegion ss nil))
	(sssetfirst nil ss)
	(setvar "cmdecho" 1)
	(princ)
)


;多段线夹点编辑，添加、删除、布尔运算
(defun c:pl_pt_ed (/ *error* en ent entl entl2 flag isclose ispl ispt m msg n na obj os parampt pt1 pt2 ptl pts ss v)
	(defun *error*(msg)
		(sssetfirst nil)
		(if v (vlr-add av:totalreader_pickfirst_reactor))
		(setvar "osmode" os)
		(setvar "cmdecho" 1)
		(princ)
	)
	;主程序开始
	(setvar "cmdecho" 0)
	(setq os (getvar "osmode"))
	(and
		(vlr-added-p av:totalreader_pickfirst_reactor);查询自动统计状态
		(setq v (vlr-remove av:totalreader_pickfirst_reactor));关闭自动统计
	)
	(cond
		((while (and
							(setq en (entsel "\n请选择需要编辑的多段线:<回车新建>"))
							(setq ent (car en))
							(setq na (cdr (assoc 0 (entget ent))))
							(not (setq ispl (wcmatch na "*POLYLINE")))
						)
			 (cond
				 (en (princ " ->非多段线") (setq ent nil))
				 (t nil)
			 )
		 )
		)
		(ispl)
		((setq pt1 (getpoint "指定第一点:"))
			(cond
				((setq pt2 (getcorner pt1 "\n指定矩形对角点:<回车切换多段线>"))
					(command-s "rectang" pt1 pt2)
				)
				(t
					(command-s "pline" pt1)
					(while (= 1 (getvar "cmdactive")) (command-s pause))
					(setq obj (Vlax-Ename->Vla-Object (entlast)))
					(if (= 0 (Vlax-Get obj 'Closed)) (vlax-put obj 'Closed 1))
				)
			)
			(setq ent (entlast))	
		)
		(t nil)
	)
	(setq obj (vlax-ename->vla-object ent))
	(setq ss (ssadd ent))
	(sssetfirst nil ss)
	(setq flag t)
	(setvar "osmode" 513)
	(while (and flag
					 (progn
						 (setq entl (entget ent))
						 (setq isclose (= 1 (cdr (assoc 70 entl))))
						 (setq msg (strcat "\r确认操作点(夹点:删除；线上:增加；环线外:" (if isclose "布尔运算" "最近点") "):"))
						 (setq pt1 (getpoint msg))
					 )
				 )
		(cond
			(isclose)
			(t (setq pt1 (vlax-curve-getClosestPointTo obj pt1)))
		)
		(foreach l entl (if (= 10 (car l)) (setq pts (cons (cdr l) pts))))
		(setq ispt (member (setq pt2 (mapcar '+ '(0 0) pt1)) pts))
		(cond
			(ispt ;删除节点
				(setq ptl (cons 10 pt2))
				(setq entl2 (list))
				(foreach l entl
					(cond
						((equal l ptl 1e-4))
						(t (setq entl2 (cons l entl2)))
					)
				)
				(setq entl (reverse entl2))
				(entmod entl)
			)
			((setq ParamPt (vlax-curve-getParamAtPoint ent pt1))
				;静态添加偏位夹点（可捕捉）
				(setq n (fix ParamPt))
				(vla-GetWidth obj n 'sw 'ew)
				(setvar "osmode" 7743)
				(setq pt2 (mapcar '+ '(0 0) (getpoint pt1)))
				(vl-catch-all-apply
					'(lambda(obj index pt bugle sw ew)
						 (vlax-invoke obj 'addvertex index pt)
						 (vla-setbulge obj index bugle)
						 (vla-setwidth obj index sw ew)
					 )
					(list obj (1+ n) pt2 0 sw sw)
				)
				
				;;动态移位新夹点
				;(setq p10 (list 10 (car pt2) (cadr pt2)))
				;(setq entl (entget ent))
				;(setq L2 (cdr (member p10 entl)))
				;(setq L1 (reverse (cdr (member p10 (reverse entl)))))
				;(while (and (setq gr (grread 5)) (= (car gr) 5))
				;	(setq Npt (list (list 10 (car (cadr gr)) (cadr (cadr gr)))))
				;	(entmod (append L1 Npt L2))
				;)
				
			)
			(isclose ;布尔运算
				(setq ss (BooleanRegion ss pt1))
				(setq m (sslength ss))
				(cond
					((= 1 m)
						(setq ent (ssname ss 0))
						(setq obj (vlax-ename->vla-object ent))
						(sssetfirst nil ss)
					)
					(t
						(princ (strcat "\n拆分为" (itoa m) "个多边线，程序结束。"))
						(setq flag nil) ;分裂成多个时结束进程
					)
				)
			)
			(t nil)
		)
		(setvar "osmode" 513)
	)
	(*error* nil)
)


;;说明:面域或多段线布尔运算
;;参数:s:需要进行运算的面域或多段线
;;参数:pt0:初始点位，也是激活命令的识别码；可省略。
;;返回:运算后的多段线
(defun BooleanRegion(s pt0 / blst def ent ini n obj pt1 pt2 ss ssn str str1 x y);boolean是全局变量
	(setq n (sslength s) ss (ssadd))
	(while (setq ssn (ssname s (setq n (1- n))))
		(cond
			((equal "REGION" (cdr (assoc 0 (entget ssn))))
				(ssadd ssn ss)
			)
			(t
				(command-s "region" (ssadd ssn) "")
				(ssadd (entlast) ss)
			)
		)
	)
	(sssetfirst nil ss)
	(setq str1 "")
	(setq blst (list (list "U" "并集") (list "S" "差集") (list "I" "交集")))
	(foreach l blst
		(setq str1 (strcat str1 (cadr l) "(" (car l) ")/"))
		(setq ini (av:list->string (reverse (mapcar 'car blst)) " "))
	)
	(setq str1 (strcat "或[" str1 "]"))
	(setq boolean (cond (boolean) (t "U")))
	(while (progn ;确定PT1
					 (cond
						 (pt0 (setq pt1 pt0))
						 (t
							 (while (progn
												(setq def (cadr (assoc boolean blst)))
												(setq str (strcat "\r指定第一点" str1 ":<默认" def ",回车结束布尔运算>"))
												(initget ini)
												(setq pt1 (getpoint str))
												(equal 'STR (type pt1))
											)
								 (setq boolean pt1)
							 )
						 )
					 )
					 pt1
				 )
		(progn ;确定pt2
			(cond
				(pt0
					(while (progn
									 (setq def (cadr (assoc boolean blst)))
									 (setq str (strcat "\r指定矩形对角点" str1 ":<默认" def ",回车切换多段线>"))
									 (initget ini)
									 (setq pt2 (getcorner pt1 str))
									 (equal 'STR (type pt2))
								 )
						(setq boolean pt2)
					)
				)
				(t
					(setq pt2 (getcorner pt1 "\r指定矩形对角点:<回车切换多段线>"))
				)
			)
			(setq pt0 nil)
		)
		(cond
			(pt2 (command-s "rectang" pt1 pt2))
			(t
				(command-s "pline" pt1)
				(while (= 1 (getvar "cmdactive")) (command-s pause))
				(setq obj (Vlax-Ename->Vla-Object (entlast)))
				(if (= 0 (Vlax-Get obj 'Closed)) (vlax-put obj 'Closed 1))
			)
		)
		(command-s "region" (ssadd (entlast)) "")
		(setq ent (entlast))
		(cond
			((equal "U" boolean)
				(command-s "union" (ssadd ent ss) "")
			)
			((equal "S" boolean)
				(command-s "subtract" ss "" (ssadd ent) "")
			)
			((equal "I" boolean)
				(command-s "intersect" (ssadd ent ss) "")
			)
			(t nil)
		)
		(sssetfirst nil ss)
	)
	(setq x (entlast))
	(command-s "select" ss "")
	(setvar "qaflags" 1)
	(while (setq ss (ssget "p")) (command-s "explode" ss ""))
	(setvar "qaflags" 0)
	(setvar "peditaccept" 1);加入这个系统变量的代码,转化为多段线
	(setq y (entlast))
	(command-s "pedit" "m" (av:newss x) "" "J" "J" "E" 0 "")
	(setq ss (av:newss y))
)



(princ)









