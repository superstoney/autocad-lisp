

;;线长选择
(defun C:SELECTBYCURVELENGTH(/ di1 di2 len maxd mind nss ss str strn)
	(setvar "nomutt" 0)
	(setq di1 (cond
							((progn
								 (setq str (if *linedistmin* (rtos *linedistmin* 2 3) "0"))
								 (getdist (strcat "\n线长最小值<" str ">:"))
							 )
							)
							(*linedistmin*)
							(t 0)
						)
	)
	(setq di2 (cond
							((progn
								 (setq str (if *linedistmax* (rtos *linedistmax* 2 3) "极大"))
								 (getdist (strcat "\n线长最大值<" str ">:"))
							 )
							)
							(*linedistmax*)
							(t 1e1000)
						)
	)
	(setq *linedistmin* di1 *linedistmax* di2)
	(setq nss (ssadd))
	(while (setq ss (ssget '((0 . "*line,arc,cirecl,ellipse"))))
		(setq n 0)
		(foreach l (av:ss->ssnlst ss)
			(setq len (vlax-curve-getdistatparam l (vlax-curve-getendparam l)))
			(cond
				((and (<= di1 len) (<= len di2))
					(ssadd l nss)
					(redraw l 3)
					(setq n (1+ n))
				)
				(t nil)
			)
		)
		(princ (strcat "已筛选" (itoa n) "个对象"))
	)
	(sssetfirst nil nss)
	(if (< 0 (setq n (sslength nss)))
		(princ (strcat "共筛选了" (itoa n) "个<" (rtos di1 2 3) "-" (rtos di2 2 3) ">的线条！"))
	)
  (princ)
)


;选出零长度的线
(defun c:0LLine(/ 0lst filter la len lst maxlen n obj ss str)
	(setvar "nomutt" 0)
	(princ "\n功能：查找特定长度的短线并置于特定图层")
	(setq maxlen 0.001)
	(setq maxlen (cond
								 ((progn
										(setq str (if *maxlen* (rtos *maxlen* 2 3) (rtos maxlen 2 3)))
										(getdist (strcat "\n要查找的线段最大长度为：<" str ">"))
									)
								 )
								 (*maxlen*)
								 (t maxlen)
							 )
	)
	(setq *maxlen* maxlen)
	(setq la (strcat "层-长度小于" (rtos maxlen 2 3) "的线"))
	(entmakex (list
							'(0 . "LAYER")
							'(100 . "AcDbSymbolTableRecord")
							'(100 . "AcDbLayerTableRecord")
							'(70 . 0)
							'(6 . "Continuous")
							(cons 2 la)
						)
	)
	(setq obj (vla-Item *layers* la))
	(cond
		((equal :vlax-true (vla-get-Freeze obj)))
		(t (vla-put-Freeze obj :vlax-true))
	)
	(setq filter "arc,*polyline,line,circle,spline")
	(while (setq ss (ssget (list (cons 0 filter))))
		(setq lst (av:ss->ssnlst ss))
		(foreach l lst
			(setq len (vlax-curve-getdistatparam l (vlax-curve-getendparam l)))
			(setq obj (vlax-ename->vla-object l))
			(cond
				((< maxlen len))
				(t
					(setq 0lst (cons l 0lst))
					(redraw l 3)
				)
			)
		)
		(cond
			((null 0lst)
				(princ "\n★提示：无符合长度条件的线条。")
			)
			(t
				(setq 0lst (av:delstsame 0lst))
				(foreach l 0lst
					(setq obj (vlax-ename->vla-object l))
					(vla-put-layer obj la)
				)
				(setq n (length 0lst))
				(princ (strcat "已找到并隐藏" (itoa n) "个短线条"))
			)
		)
		(setq 0lst nil)
	)
	(princ)
)



(princ)




















