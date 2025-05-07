

;;�޸Ķ���߿�
(defun c:pl_width(/ ass data ex lst plwidth ss str w)
	(setq plwidth 20)
	(setq plwidth (cond
									((progn
										 (setq str (if *plwidth* (itoa *plwidth*) (itoa plwidth)))
										 (initget (+ 2 4))
										 (setq w (getint (strcat "\n���������߿�<" str ">:")))
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


;;��̬������ߣ��ڸ߰汾�п�ʵʱ����ɾ��Ч����
(defun c:dynbreak (/ *error* ent m os pt1 ss v)
	;(princ "-->��̬�������")
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
	(setq ss (entsel "\nѡ�����:"))
	(sssetfirst nil (ssadd (car ss)))
	(setvar "osmode" 2595)
	(and ss
		(setq pt1 (getpoint "\nָ����һ����ϵ�:"))
		(setq m (vl-cmdf "break" ss "f" pt1))
	)
	(while (= 1 (getvar "cmdactive"))(command pause))
	(if (and m (setq ent (entsel "��ѡ��ɾ����")))
		(entdel (car ent))
	)
	(*error* nil)
)

;;����߻����򲼶�����
(defun c:BooleanRegion(/ flg obj pt1 pt2 ss)
	(setvar "cmdecho" 0)
	(princ "\n��ѡ�����߻�����:<�س��½�>")
	(setq flg t)
	(while (progn
					 (and flg (setq ss (ssget '((0 . "LWPOLYLINE,REGION")(70 . 1)))))
					 (null ss)
				 )
		(setq pt1 (getpoint "ָ����һ��:"))
		(setq pt2 (getcorner pt1 "\nָ�����ζԽǵ�:<�س��л������>"))
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


;����߼е�༭����ӡ�ɾ������������
(defun c:pl_pt_ed (/ *error* en ent entl entl2 flag isclose ispl ispt m msg n na obj os parampt pt1 pt2 ptl pts ss v)
	(defun *error*(msg)
		(sssetfirst nil)
		(if v (vlr-add av:totalreader_pickfirst_reactor))
		(setvar "osmode" os)
		(setvar "cmdecho" 1)
		(princ)
	)
	;������ʼ
	(setvar "cmdecho" 0)
	(setq os (getvar "osmode"))
	(and
		(vlr-added-p av:totalreader_pickfirst_reactor);��ѯ�Զ�ͳ��״̬
		(setq v (vlr-remove av:totalreader_pickfirst_reactor));�ر��Զ�ͳ��
	)
	(cond
		((while (and
							(setq en (entsel "\n��ѡ����Ҫ�༭�Ķ����:<�س��½�>"))
							(setq ent (car en))
							(setq na (cdr (assoc 0 (entget ent))))
							(not (setq ispl (wcmatch na "*POLYLINE")))
						)
			 (cond
				 (en (princ " ->�Ƕ����") (setq ent nil))
				 (t nil)
			 )
		 )
		)
		(ispl)
		((setq pt1 (getpoint "ָ����һ��:"))
			(cond
				((setq pt2 (getcorner pt1 "\nָ�����ζԽǵ�:<�س��л������>"))
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
						 (setq msg (strcat "\rȷ�ϲ�����(�е�:ɾ��������:���ӣ�������:" (if isclose "��������" "�����") "):"))
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
			(ispt ;ɾ���ڵ�
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
				;��̬���ƫλ�е㣨�ɲ�׽��
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
				
				;;��̬��λ�¼е�
				;(setq p10 (list 10 (car pt2) (cadr pt2)))
				;(setq entl (entget ent))
				;(setq L2 (cdr (member p10 entl)))
				;(setq L1 (reverse (cdr (member p10 (reverse entl)))))
				;(while (and (setq gr (grread 5)) (= (car gr) 5))
				;	(setq Npt (list (list 10 (car (cadr gr)) (cadr (cadr gr)))))
				;	(entmod (append L1 Npt L2))
				;)
				
			)
			(isclose ;��������
				(setq ss (BooleanRegion ss pt1))
				(setq m (sslength ss))
				(cond
					((= 1 m)
						(setq ent (ssname ss 0))
						(setq obj (vlax-ename->vla-object ent))
						(sssetfirst nil ss)
					)
					(t
						(princ (strcat "\n���Ϊ" (itoa m) "������ߣ����������"))
						(setq flag nil) ;���ѳɶ��ʱ��������
					)
				)
			)
			(t nil)
		)
		(setvar "osmode" 513)
	)
	(*error* nil)
)


;;˵��:��������߲�������
;;����:s:��Ҫ������������������
;;����:pt0:��ʼ��λ��Ҳ�Ǽ��������ʶ���룻��ʡ�ԡ�
;;����:�����Ķ����
(defun BooleanRegion(s pt0 / blst def ent ini n obj pt1 pt2 ss ssn str str1 x y);boolean��ȫ�ֱ���
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
	(setq blst (list (list "U" "����") (list "S" "�") (list "I" "����")))
	(foreach l blst
		(setq str1 (strcat str1 (cadr l) "(" (car l) ")/"))
		(setq ini (av:list->string (reverse (mapcar 'car blst)) " "))
	)
	(setq str1 (strcat "��[" str1 "]"))
	(setq boolean (cond (boolean) (t "U")))
	(while (progn ;ȷ��PT1
					 (cond
						 (pt0 (setq pt1 pt0))
						 (t
							 (while (progn
												(setq def (cadr (assoc boolean blst)))
												(setq str (strcat "\rָ����һ��" str1 ":<Ĭ��" def ",�س�������������>"))
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
		(progn ;ȷ��pt2
			(cond
				(pt0
					(while (progn
									 (setq def (cadr (assoc boolean blst)))
									 (setq str (strcat "\rָ�����ζԽǵ�" str1 ":<Ĭ��" def ",�س��л������>"))
									 (initget ini)
									 (setq pt2 (getcorner pt1 str))
									 (equal 'STR (type pt2))
								 )
						(setq boolean pt2)
					)
				)
				(t
					(setq pt2 (getcorner pt1 "\rָ�����ζԽǵ�:<�س��л������>"))
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
	(setvar "peditaccept" 1);�������ϵͳ�����Ĵ���,ת��Ϊ�����
	(setq y (entlast))
	(command-s "pedit" "m" (av:newss x) "" "J" "J" "E" 0 "")
	(setq ss (av:newss y))
)



(princ)









