;新建默认图层，并指定当前层
(defun c:putclayer(/ la m msg n pre)
	(setq pre "LB_图层")
	;新建图层
	(setq n 0 m 9)
	(repeat m
		(setq n (1+ n))
		(setq la (strcat pre (itoa n)))
		(entmakex (list
								'(0 . "LAYER")
								'(100 . "AcDbSymbolTableRecord")
								'(100 . "AcDbLayerTableRecord")
								'(70 . 0)
								'(6 . "Continuous")
								(cons 2 la)
								(cons 62 n)
							)
		)
	)
	;选择图层
	(initget "D")
	(setq msg (strcat "\n指定当前\"" pre "\"[1/2/3/4/5/6/7/8/9]:<默认7>"))
	(setq n (getint msg))
	(setq n (cond ((> n m) m) ((and n (< n 1)) 1) (n) (t 7)))
	(setq la (strcat pre (itoa n)))
	(setq la (setvar "clayer" la))
	(princ (strcat "\n当前图层：" la))
	(princ)
)
;;=================================================
;;设置当前层
(defun c:Activelayer (/ ent lan)
	;(princ "-->指定当前层")
	(setq lan (cdr (assoc 8 (entget (car (entsel "\n指定参照对象:"))))))
	(setvar "clayer" lan)
	(prompt (strcat "\n当前层已切换为:" lan))
	(princ)
)
;;设为随层
(defun c:put-color-bylayer(/ n obj ss ssn)
	(while (setq ss (ssget))
		(repeat (setq n (sslength ss))
			(setq ssn (ssname ss (setq n (1- n))))
			(setq obj (vlax-ename->vla-object ssn))
			(cond
				((= 256 (vla-get-Color obj)))
				(t (vla-put-Color obj 256))
			)
		)
	)
	(princ)
)
;;=================================================
(defun av:modifyssla (la / ass data lst na obj ss)
	(while (setq ss (ssget))
		(setq lst (av:ss->ssnlst ss))
		(foreach l lst
			(setq data (entget l))
			(setq ass (assoc 8 data))
			(setq na (cdr ass))
			(cond
				((wcmatch na la))
				(t (setq data (subst (cons 8 la) ass data)))
			)
			(entmod data)
			(setq obj (vlax-ename->vla-object l))
			(cond
				((= 256 (vla-get-Color obj)))
				(t (vla-put-Color obj 256))
			)
		)
	)
)
;;移至当前层
(defun c:ToActiveLayer (/ la)
	;(princ "-->移至当前层")
	(setq la (getvar "clayer"))
	(av:modifyssla la)
	(princ)
)
;;图层刷子
(defun c:LayerMatch (/ enp ss)
	(setq la (cdr (assoc 8 (entget (car (entsel "\n选取参照层对象:"))))))
	(av:modifyssla la)
	(princ)
)
;;=================================================


;;删除关闭层上对象
(defun c:hh-delhidelayer (/ la lst n ss)
	(vlax-for obj *layers*
		(cond
			((equal :vlax-true (vla-get-LayerOn obj)))
			(t (setq lst (cons (vla-get-Name obj) lst)))
		)
	)
	(cond
		((null lst)
			(princ "无关闭图层！")
		)
		((progn
			 (setq la (av:list->string lst ","))
			 (setq ss (ssget "A" (list (cons 8 la))))
			 (null ss)
		 )
			(princ "关闭图层无可删对象！")
		)
		(t
			(setq n (sslength ss))
			(repeat n (entdel (ssname ss (setq n (1- n)))))
			(princ "已清除关闭图层上未锁定对象！")
		)
	)
	(princ)
)


;;删除指定层及其层上的对象
(defun c:dlayer(/ ena ent la msg n obj ss)
	(if (= 1 (getvar "nomutt")) (setvar "nomutt" 0))
	(while (setq ent (entsel "\n请指定删除层:"))
		(setq la (cdr (assoc 8 (entget (car ent)))))
		(setq ss (ssget "A" (list (cons 8 la))))
		(repeat (setq n (sslength ss))
			(entdel (ssname ss (setq n (1- n))))
		)
		(setq ena (tblobjname "layer" la))
		(setq obj (vlax-ename->vla-object ena))
		(setq msg (vl-catch-all-apply 'vla-Delete (list obj)))
		(princ (if (vl-catch-all-error-p msg) "只清除!" "已删除!"))
	)
	(princ)
)


(princ)
