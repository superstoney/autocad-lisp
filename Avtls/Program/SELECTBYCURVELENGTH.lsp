

;;�߳�ѡ��
(defun C:SELECTBYCURVELENGTH(/ di1 di2 len maxd mind nss ss str strn)
	(setvar "nomutt" 0)
	(setq di1 (cond
							((progn
								 (setq str (if *linedistmin* (rtos *linedistmin* 2 3) "0"))
								 (getdist (strcat "\n�߳���Сֵ<" str ">:"))
							 )
							)
							(*linedistmin*)
							(t 0)
						)
	)
	(setq di2 (cond
							((progn
								 (setq str (if *linedistmax* (rtos *linedistmax* 2 3) "����"))
								 (getdist (strcat "\n�߳����ֵ<" str ">:"))
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
		(princ (strcat "��ɸѡ" (itoa n) "������"))
	)
	(sssetfirst nil nss)
	(if (< 0 (setq n (sslength nss)))
		(princ (strcat "��ɸѡ��" (itoa n) "��<" (rtos di1 2 3) "-" (rtos di2 2 3) ">��������"))
	)
  (princ)
)


;ѡ���㳤�ȵ���
(defun c:0LLine(/ 0lst filter la len lst maxlen n obj ss str)
	(setvar "nomutt" 0)
	(princ "\n���ܣ������ض����ȵĶ��߲������ض�ͼ��")
	(setq maxlen 0.001)
	(setq maxlen (cond
								 ((progn
										(setq str (if *maxlen* (rtos *maxlen* 2 3) (rtos maxlen 2 3)))
										(getdist (strcat "\nҪ���ҵ��߶���󳤶�Ϊ��<" str ">"))
									)
								 )
								 (*maxlen*)
								 (t maxlen)
							 )
	)
	(setq *maxlen* maxlen)
	(setq la (strcat "��-����С��" (rtos maxlen 2 3) "����"))
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
				(princ "\n����ʾ���޷��ϳ���������������")
			)
			(t
				(setq 0lst (av:delstsame 0lst))
				(foreach l 0lst
					(setq obj (vlax-ename->vla-object l))
					(vla-put-layer obj la)
				)
				(setq n (length 0lst))
				(princ (strcat "���ҵ�������" (itoa n) "��������"))
			)
		)
		(setq 0lst nil)
	)
	(princ)
)



(princ)




















