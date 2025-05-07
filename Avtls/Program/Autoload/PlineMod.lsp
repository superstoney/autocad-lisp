;直接合并
(defun c:pe (/ ss)
	(princ "-->多线合并,命令PEE可模糊合并")
	(setvar "cmdecho" 0)
	(cond
		(initcommandversion
			(vl-catch-all-apply 'initcommandversion)
			((if command-s command-s command) "join")
		)
		(T
			(setvar "peditaccept" 1)
			(while (setq ss (ssget '((0 . "line,arc,*polyline"))))
				(av:chkopen ss);标记开口位置
				(command-s "pedit" "m" ss "" "J" "J" "E" 0 "")
			)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)
;多段线模糊合并
(defun c:pee(/ *error* ss)
	(defun *error*(msg)(princ "继续操作，你将进入一个奇妙的世界！"))
	(princ "-->多段线模糊合并或封闭")
	(princ (strcat "\n请选择合并对像" (av:petips1) "【回车设置】"))
	(setq ss (ssget '((0 . "line,arc,*polyline"))))
	(av:pefuzzy ss)
)

;参数化多段线模糊合并
(defun av:pefuzzy(ss / *error* is1pl n os x)
	;判断是否只是选择了1根多段线
	(defun is1pl()
		(and
			;判断是否只选择了1根
			ss (= 1 (sslength ss))
			;判断是否是多段线
			(vl-string-search
				(strcase "polyline")
				(cdr(assoc 0(entget(ssname ss 0))))
			)
		)
	)
	;判断是否闭合
	(defun isplclose(ss)(= 1 (logand 1 (cdr (assoc 70 (entget (ssname ss 0)))))))
	;错误返回函数
	(defun *error*(msg)
		(setvar "osmode" os)
		(vla-endundomark *doc*)
		(setvar "cmdecho" 1)
		(princ)
	)
	;开始奔跑
	(setvar "cmdecho" 0)
	(vla-startundomark *doc*);会破坏预先选择集
	(setq os (getvar "osmode"))
	(setvar "peditaccept" 1)
	(and ss (av:chkopen ss));标记开口位置
	;弹出模糊值设置：不是单根多段线，没有模糊值，没有选择内容
	(setq n 0)
	(cond
		((is1pl)
			(if (isplclose ss)(setq ss nil) nil)
		)
		((or (zerop pefuzzylen) (null ss))
			(progn (av:setpefuzzylen) (setq n (1+ n)))
		)
		(t nil)
	)
	;循环选择进行合并操作
	(while n
		;判断是否需要二次选择
		(if (null ss)
			(and
				(princ (strcat "\n请选择合并对像" (av:petips1)))
				(setq ss (ssget '((0 . "line,arc,*polyline"))))
				(av:chkopen ss);标记开口位置
			)
		)
		(if ss
			(cond
				;如果是单根多段线，则可闭合
				((and (is1pl) (null (isplclose ss)))
					(initget "Y N ")
					(setq x (getkword "当前选择为多段线，且只有1条，是否需要闭合[是(Y)/否(N)]:<默认为是>"))
					(if (or (null x) (= x "Y"))
						(and (vl-cmdf "pedit" ss "C" "")(princ ">>>已闭合!"))
					)
				)
				;核心操作，模糊合并
				(ss
					(if (vl-cmdf "pedit" "m" ss "" "J" "J" EAB pefuzzylen "")
						(princ ">>>合并完成")
					)
				)
				(t nil)
			)
			;回车后进行参数设定或结束操作
			(if (= n 0)
				(progn (av:setpefuzzylen)(setq n (1+ n)))
				(setq n nil)
			)
		)
		(setq ss nil)
	)
	(*error* nil)
)
;提示信息1
(defun av:petips1()
	(if (null pefuzzylen)(setq pefuzzylen 0))
	(if (null EAB)(setq EAB "B"))
	(strcat
		"<合并类型="
		(cond ((= EAB "E") "延伸") ((= EAB "A") "添加") ((= EAB "B") "两者都"))
		",模糊距离=" (rtos pefuzzylen) ">"
	)
)
;设定多段线合并模糊距离
(defun av:setpefuzzylen(/ dyn k l os str1 str2)
	(setq str1 (strcat "\n请输入模糊距离[1/5/10/50/100/500/合并类型(J)]" (av:petips1) ":"))
	(initget "J")
	(setq os (getvar "osmode"))(setvar "osmode" 1)
	(setq dyn (getvar "dynmode"))(setvar "dynmode" 2)
	(setq l (getdist str1))
	(setvar "osmode" os)
	(setvar "dynmode" dyn)
	(cond
		((or (null l)(= l pefuzzylen)) nil)
		((= "J" l)
			(initget "E A B ")
			(setq str2 (strcat "\n输入合并类型[延伸(E)/添加(A)/两者都(B)]:<默认" EAB ">"))
			(setq k (getkword str2))
			(if k (setq EAB k))
		)
		(t (setq pefuzzylen l))
	)
)

;;===================================================================================

;区域多段线闭合
(defun c:closepline(/ *error* n ss ssn tt)
	(defun *error*(msg)
		(vla-endundomark *doc*)
		(setvar "cmdecho" 1)
		(princ)
	)
	;开始奔跑
  (setvar "cmdecho" 0)
	(setq ss (entsel "\n请指定单个闭合的多线(回车多选)"))
	;(and ss (av:chkopen (entget (car ss))));标记开口位置
	(vla-startundomark *doc*);会破坏预先选择集
	(setq tt t)
	;优先单选操作
	(if ss
		(while tt
			(if ss nil 
				(if (setq ss (entsel "\n请指定需闭合的单个多线")) nil (setq tt nil))
			)
			(and ss (command-s "pedit" ss "C" ""))
			(setq ss nil)
		)
	)
	;未有单选情况下开始多选操作
	(if tt
		(and
			(princ "请选择需闭合的多线")
			(setq ss (ssget '((0 . "arc,*polyline"))))
			(av:chkopen ss)
			(setq n 0)
			(repeat (sslength ss)
				(setq ssn (ssname ss n))
				(command-s "pedit" ssn "c" "")
				(setq n (1+ n))
			)
		)
	)
	(*error* nil)
)

;标记不封闭区域的开口位置
(defun c:chkopen (/ f olst ss)
	(setvar "cmdecho" 1)
  (setq	f '((0 . "*line,arc,ellipse")
						 (-4 . "<NOT")
						 (0 . "XLINE")
						 (-4 . "NOT>")
					 )
  )
  (or (setq ss (ssget f)) (fsxm-silenceexit))
  (setq olst (itoa (length (av:chkopen ss))))
  (princ (strcat ">>>>本次共找到开口点<" olst ">个....."))
	(setvar "cmdecho" 0)
  (princ)
)
;参数化标记不封闭开口位置
(defun av:chkopen (ss / ep notopen olst sp)
  (defun notopen (pt en / ss)
    (if	(setq ss (ssget "C" pt pt f))
      (ssdel en ss)
    )
    (and ss (ssname ss 0))
  )
  (foreach en (av:ss->ssnlst ss)
    (setq sp (trans (vlax-curve-getStartPoint en) 0 1))
    (setq ep (trans (vlax-curve-getEndPoint en) 0 1))
    (cond ((not (equal sp ep 1e-8))
						(or (notopen sp en) (setq olst (cons sp olst)))
						(or (notopen ep en) (setq olst (cons ep olst)))
					)
    )
  )
  (mapcar '(lambda (a) (fsxm-pt-grdraw a 1)) olst)
  olst
)

;;;;;;以下部分为寻找开口支持函数部分
;(defun fsxm-silenceexit (/ *error*) (t (setq  *error* strcat)))
;(defun fsxm-ss->enlist (ss  / lst n en)  
;	(setq n -1)  
;	(while (and (setq en (ssname ss (setq n (1+ n))))) 
;		(setq  lst (cons en lst))
;	)  
;)
;(defun fsxm-pt-grdraw (upt col / pts sz)  
;	(setq sz (fsxm-pickboxsize)) 
;	(setq pts (fsxm-9ptbox (mapcar '- upt (list sz sz )) (mapcar '+ upt (list sz sz )))) 
;	(grdraw (fsxm-nths '(0 0 ) pts ) (fsxm-nths '(2 2 ) pts ) col ) 
;	(grdraw (fsxm-nths '(2 0 ) pts ) (fsxm-nths '(0 2 ) pts ) col ) 
;	(grdraw (fsxm-nths '(1 0 ) pts ) (fsxm-nths '(1 2 ) pts ) col ) 
;	(grdraw (fsxm-nths '(2 1 ) pts ) (fsxm-nths '(0 1 ) pts ) col )  
;)
;(defun fsxm-pickboxsize() 
;	(* (/ (getvar "pickbox") (car(cdr(getvar "screensize")))) (getvar "viewsize"))
;)
;(defun fsxm-9ptbox (p1 p2  / x x1 x2 y y1 y2)  
;	(setq x1 (car p1)) 
;	(setq y1 (car (cdr p1))) 
;	(setq x2 (car p2)) 
;	(setq y2 (car (cdr p2))) 
;	(setq x (* 0.5 (+ x2 x1))) 
;	(setq y (* 0.5 (+ y2 y1))) 
;	(mapcar '(lambda (a) (mapcar 'list (list x1 x x2) (list a a a))) (list y1 y y2))  
;)
;(defun fsxm-nths ( item lst) 
;	(or (listp item) (setq item (list item))) 
;	(foreach index item (setq lst (nth index lst)))  
;)

;;===================================================================================



(princ)

