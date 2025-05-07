(defun c:hatchline (/ *error* dxf ent ess filter makhatl oldcl ss)
	;(princ "-->������")
	(defun *error*(msg)
		(if (null ss) (princ "��ʾ��δ��ָ����䣬������ѡ��"))
		(setvar "clayer" oldcl)
		(vla-endundomark *doc*)
		(command-s "draworder" ess "" "f")
		(setvar "cmdecho" 1)
		(sssetfirst nil ess)
		(princ)
	)
	(defun makhatl(ss ess / n s s1)
		(setq n (sslength ss))
		(while (and
						 (setq s (ssname ss (setq n (1- n))))
						 (= 0 (Vlax-Get (Vlax-Ename->Vla-Object s) 'AssociativeHatch))
					 )
			(setq s1 (entlast))
			(command-s "hatchedit" s "b" "p" "y")
			(while (setq s1 (entnext s1)) (ssadd s1 ess))
			;���ӱ߽���
			(if (< n (sslength ess))
				(vl-cmdf "pedit" "m" ess "" "J" "J" "B" 1 "")
			)
		)
		ess
	)
	(setvar "cmdecho" 0)
	;(if (/= 0 (getvar "qaflags")) (setvar "qaflags" 0))
	(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0));��ֹ��Ϣ����
	(vla-startundomark *doc*)
	(setq oldcl (getvar "clayer"))
	(av:setactivelayer "LB_������" 1);�½�ͼ��
	(setq ess (ssadd))
	(eq 1 (logand 1 (getvar "pickfirst")))
	(cond
		((and
			 (setq ent (car (entsel "\n���ѡ�ο������:<�س���������>")))
			 (setq dxf (entget ent))
			 (= (cdr (assoc 0 dxf)) "HATCH")
		 )
			(setq filter (list
										 '(0 . "HATCH")
										 (cons 2 (math:str-del-esc (cdr (assoc 2 dxf)))) ;���ͼ��
										 (dxf:assoc8 dxf) ;ͼ��
										 (dxf:assoc62 dxf) ;��ɫ
										 '(97 . 0) ;�������߽�
									 )
			)
			(while (setq ss (ssget filter))
				(setq ess (makhatl ss ess))
			)
		)
		(t (while (setq ss (ssget '((0 . "HATCH")(97 . 0))))
				 (setq ess (makhatl ss ess))
			 )
		)
	)
	(*error* nil)
)


