;�½�Ĭ��ͼ�㣬��ָ����ǰ��
(defun c:putclayer(/ la m msg n pre)
	(setq pre "LB_ͼ��")
	;�½�ͼ��
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
	;ѡ��ͼ��
	(initget "D")
	(setq msg (strcat "\nָ����ǰ\"" pre "\"[1/2/3/4/5/6/7/8/9]:<Ĭ��7>"))
	(setq n (getint msg))
	(setq n (cond ((> n m) m) ((and n (< n 1)) 1) (n) (t 7)))
	(setq la (strcat pre (itoa n)))
	(setq la (setvar "clayer" la))
	(princ (strcat "\n��ǰͼ�㣺" la))
	(princ)
)
;;=================================================
;;���õ�ǰ��
(defun c:Activelayer (/ ent lan)
	;(princ "-->ָ����ǰ��")
	(setq lan (cdr (assoc 8 (entget (car (entsel "\nָ�����ն���:"))))))
	(setvar "clayer" lan)
	(prompt (strcat "\n��ǰ�����л�Ϊ:" lan))
	(princ)
)
;;��Ϊ���
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
;;������ǰ��
(defun c:ToActiveLayer (/ la)
	;(princ "-->������ǰ��")
	(setq la (getvar "clayer"))
	(av:modifyssla la)
	(princ)
)
;;ͼ��ˢ��
(defun c:LayerMatch (/ enp ss)
	(setq la (cdr (assoc 8 (entget (car (entsel "\nѡȡ���ղ����:"))))))
	(av:modifyssla la)
	(princ)
)
;;=================================================


;;ɾ���رղ��϶���
(defun c:hh-delhidelayer (/ la lst n ss)
	(vlax-for obj *layers*
		(cond
			((equal :vlax-true (vla-get-LayerOn obj)))
			(t (setq lst (cons (vla-get-Name obj) lst)))
		)
	)
	(cond
		((null lst)
			(princ "�޹ر�ͼ�㣡")
		)
		((progn
			 (setq la (av:list->string lst ","))
			 (setq ss (ssget "A" (list (cons 8 la))))
			 (null ss)
		 )
			(princ "�ر�ͼ���޿�ɾ����")
		)
		(t
			(setq n (sslength ss))
			(repeat n (entdel (ssname ss (setq n (1- n)))))
			(princ "������ر�ͼ����δ��������")
		)
	)
	(princ)
)


;;ɾ��ָ���㼰����ϵĶ���
(defun c:dlayer(/ ena ent la msg n obj ss)
	(if (= 1 (getvar "nomutt")) (setvar "nomutt" 0))
	(while (setq ent (entsel "\n��ָ��ɾ����:"))
		(setq la (cdr (assoc 8 (entget (car ent)))))
		(setq ss (ssget "A" (list (cons 8 la))))
		(repeat (setq n (sslength ss))
			(entdel (ssname ss (setq n (1- n))))
		)
		(setq ena (tblobjname "layer" la))
		(setq obj (vlax-ename->vla-object ena))
		(setq msg (vl-catch-all-apply 'vla-Delete (list obj)))
		(princ (if (vl-catch-all-error-p msg) "ֻ���!" "��ɾ��!"))
	)
	(princ)
)


(princ)
