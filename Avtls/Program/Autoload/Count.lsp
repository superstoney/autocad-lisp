
;;;;;===================================================================

;;˵��:�������������
(defun c:t()
	(princ "-->��ѯ���������")
	(av:sscalandtips nil)
)
;;˵��:��ע���������
(defun c:tt()
	(princ "-->��ע���������")
	(av:sscalandtips T)
)
;;���ƶ����
(defun c:fg()
	(princ "-->����� ")
	(command "pline")
	(sssentlast)
)

;;˵��:�����������������
;;����:dim:�����Ƿ����ÿ�������ļ�����Ϣ��T��NIL
;;����:��
(defun av:sscalandtips(dim / en ess i l lst oldcl s ss strl strs suml sums)
	(av:startips t)
	(cond
		((setq ss (ssget '((0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE,ARC,LINE"))))
			(setq oldcl (getvar "clayer"))
			(setq i 0 suml 0 sums 0 ess (ssadd))
			(repeat (sslength ss)
				(setq en (ssname ss i))
				(setq i (1+ i))
				(setq lst (av:calent en))
				(setq l (car lst))
				(setq s (cadr lst))
				(av:calentips i l s)
				(setq suml (+ l suml))
				(setq sums (+ s sums))
				;׼����ע
				(setq strl (strcat "L=" (ValToStr l 1 3) "m"))
				(cond
					((and s (> s 0))
						(setq strs (strcat "S=" (ValToStr s 2 3) "m2"))
					)
					(t (setq strs nil))
				)
				(and dim
					(setq ess (av:dimentls en strl strs ess))
				)
			)
			(setvar "clayer" oldcl)
			(sssetfirst nil ess)
			(av:endtips (1+ i) suml sums)
		)
		(t (av:setdrawscale))
	)
	(princ)
)
;;˵��:����ͼ�μ���
;;����:en:ͼԪ����ѡ���еĵ�������ename
;;����:i:ͼԪ���
;;����:�б����Ⱥ������ֵ
(defun av:calent(en / l obj pt2 s)
	(setq obj (vlax-ename->vla-object en))
	(setq pt2 (vlax-curve-getEndParam obj))
	(setq l (vlax-curve-getDistAtParam obj pt2))
	(setq s (vl-catch-all-apply 'vlax-curve-getArea (list obj)))
	(setq l (* (float l) (expt *key:drawscale* 1)))
	(setq s (* (float s) (expt *key:drawscale* 2)))
	(list l s)
)
;;˵��:����ͼԪ�����������ʾ
;;����:i:���
;;����:l:����
;;����:s:���
;;����:����������ı�
(defun av:calentips(i l s)
	(princ (strcat "\n��" (itoa i) "��ͼԪ, ����=" (rtos l 2 0)  "mm"))
	(princ (cond ((zerop s) "���������Ϣ" ) (t (strcat ", ���=" (rtos s 2 0) "mm2"))))
)
;;˵��:��ʼ����ʱȷ�ϵ�ǰ����
;;����:s:T��nil�����������Ƿ���ʾ��ǰ������������Ϣ��ʾ
;;����:��ǰ��ͼ�������ı���ʽ
(defun av:startips(s / dsc2str msg)
	;(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0))
	(cond
		((member (type *key:drawscale*) (list 'INT 'REAL)))
		(T (setq *key:drawscale* 1.0))
	)
	(setq dsc2str (rtos *key:drawscale* 2 3))
	(setq msg (strcat "\n��ǰ��������1:" dsc2str "����mmΪ��λ��ֱ�ӻس������ò�������"))
	(and s (princ msg))
	dsc2str
)
(av:startips nil)
;;˵��:��������ʱ��Ϣ����
;;����:m:��������
;;����:suml:�ۼ��ܳ���
;;����:sums:�ۼ������
;;����:�ı���Ϣ��ʾ
(defun av:endtips(m suml sums)
	(if (/= 1 (getvar "nomutt"))(setvar "nomutt" 1))
	(princ (strcat "\nע�⣺��������1:" (av:startips nil) "������ͳ�ƣ���mmΪ��λ��"))
	(princ (strcat "\n>>>������" (itoa (1- m)) "��"))
	(cond
		((zerop suml) (princ "��û�о�����Ϣ�������²���!"))
		(t (princ (strcat "���ۼ��ܳ���= " (ValToStr suml 1 3) "m")))
	)
	(and sums (> sums 0)
		(princ (strcat "���ۼ������= " (ValToStr sums 2 3) "m2"))
	)
	(terpri)
)
;;˵��:��ע����ͼԪ���Ⱥ����
;;����:en:ͼԪ
;;����:str1:������Ϣ
;;����:strs:�����Ϣ
;;����:ess:ѡ�񼯣���Ϊnil
;;����:ess��Ϊ�գ���ֻ���ص�ǰ�������ɱ�ע
(defun av:dimentls(en str1 strs ess / box cl co ent h midp minl)
	(setq ent (entget en))
	(setq cl (cdr (assoc 8 ent)))
	(setq co (cdr (assoc 70 ent)))
	(if av:setactivelayer (av:setactivelayer cl co))
	(or ess (setq ess (ssadd)))
	(cond
		(strs
			(setq box (getbox-midp-minl en))
			(setq midp (car box))
			(setq minl (cadr box))
			(setq h (getscalen minl 15 1000))
			(entmaketext strl midp 0 h 0.2)
			(setq ess (ssadd (entlast) ess))
			(entmaketext strs midp 0 h -1.2)
			(setq ess (ssadd (entlast) ess))
		)
		(t (member (cdr (assoc 0 (entget en))) (list "LINE" "LWPOLYLINE"))
			(av:dimentl en strl)
			(setq ess (ssadd (entlast) ess))
		)
	)
	ess
)
;;˵��:��ע�߳�
;;����:en:ֱ��
;;����:��ע�ı�����
(defun av:dimentl(en strl / ang h l lst midp pt1 pt2)
	(setq pt1 (vlax-curve-getStartPoint en))
	(setq pt2 (vlax-curve-getEndPoint en))
	(setq midp (mapcar '(lambda(x y)(/ (+ x y) 2)) pt1 pt2))
	;ȷ���ָ�
	(setq l (distance pt1 pt2))
	(setq h (getscalen l 20 150))
	;�����������ֱ�ע������
	(setq lst (vl-sort (list pt1 pt2) '(lambda(x y) (< (cadr x) (cadr y)))))
	(setq lst (vl-sort lst '(lambda(x y) (< (car x) (car y)))))
	(setq ang (angle (car lst) (cadr lst)))
	;��ʼ��ע����
	(cond ((zerop l))
		(t (entmaketext strl midp ang h 0.2))
	)
)

;;;===========================================================

;���ɾ���
(defun c:rec(/ ang geta4pt2 isa4 pt1 pt2 ss)
	(defun GETA4PT2(w h / x1 x2 y1 y2)
		(setq
			x1 (car pt1)
			y1 (cadr pt1)
			x2 (+ x1 w)
			y2 (+ y1 h)
		)
		(list x2 y2 0)
	)
	(princ "-->���ɾ��� ")
	(if (= 1 (getvar "nomutt"))(setvar "nomutt" 0))
	(sssetfirst nil nil)
	(setq pt1 (getpoint "\nָ�����ε�һ���ǵ㣺"))
	(initget "A B")
	(cond
		((and
			 (= (type pt1) 'list)
			 (setq pt2 (getcorner pt1 "\n[/����A4(A)/����A4(B)]ָ���Խǵ㣺<�س������>"))
			 (= (type pt2) 'list)
		 )
			(setq ang 0)
		)
		((and
			 (null pt2)
			 (setq ang (getangle pt1 "\nָ����ת�ǣ�"))
		 )
			(setq ang (/ (* ang 180.0) pi))
			(princ ang)
			(setq pt2 pause)
		)
		((wcmatch pt2 "A")
			(setq pt2 (GETA4PT2 210 297))
			(setq ang 0)
			(setq isa4 t)
		)
		((wcmatch pt2 "B")
			(setq pt2 (GETA4PT2 297 210))
			(setq ang 0)
			(setq isa4 t)
		)
		(t nil)
	)
	(setvar "cmdecho" 0)
	(command-s "rectang" pt1 "r" ang pt2)
	(command-s "rectang" pt1 "r" 0 pt1)
	(entdel (entlast))
	(cond
		(isa4
			(setq ss (ssadd) ss (ssadd (entlast) ss))
			(command-s "scale" ss "" pt1 "R" pt1 pt2)
		)
	)
	(setvar "cmdecho" 1)
	(sssentlast);�����ڼ��㺯����
)

;��������
(defun c:reccloud(/ al dis1 dis2 dis plst pp1 pp2 pp3 pp4)
	(c:rec);���ƾ���
	;�趨��������
  (setq plst (av:getptn (entget (entlast))))
	(setq
		pp1 (car plst)
		pp2 (cadr plst)
		pp3 (caddr plst)
		pp4 (cadddr plst)
	)
	(setq
		dis1 (distance pp1 pp2)
		dis2 (distance pp2 pp3)
		dis (min dis1 dis2)
	)
	(setq al (getscalen dis 8 dis))
	(setvar "cmdecho" 0)
	(command "revcloud" "a" al al "s" "c" "o" "l" "")
	(setvar "cmdecho" 1)
	(princ)
)


;;;===========================================

;;˵��:���������࣬������ʱ���� 
(defun c:pt-pt-dis(/ *error* +dist dist1 dist2 endcmf m pt1 pt1s pt2)
	(defun *error*(msg)(endcmf +dist))
	(defun endcmf (+dist)
		(setq +dist (vl-string-subst "" "+" +dist))
		(cond
			(av:putcliptext
				(av:putcliptext +dist)
				(princ (strcat "\n�Ѹ��ƣ�" +dist))
			)
			(t nil)
		)
		(and pt2 (av:endtips m dist1 nil))
		(terpri)
		(princ)
	)
	;������ʼ
	(princ "-->�������")
	(av:startips t)
	(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0))
	(setq pt1 (getpoint "\n��ָ����ʼ��: "))
	(and pt1 (setq pt2 (getpoint pt1 ">>>��ָ����һ��:")))
	(cond
		(pt2
			(grdraw pt1 pt2 1 1)
			(princ " >>>���¿�ʼ[F]")
			(setq dist1 (distance pt2 pt1))
			(setq dist1 (* dist1 *key:drawscale*))
			(setq +dist "")
			(setq +dist (strcat +dist "+" (ValToStr dist1 1 3)))
			(setq m 1)
			(princ (strcat "\n��" (itoa m) "�γ���=" (rtos dist1 2 0) "mm"))
			(setq pt1s (cons pt2 (list pt1)))
			(setq dist1 (distance (car pt1s) (last pt1s)))
			(setq dist1 (* dist1 *key:drawscale*))
			(princ (strcat "���ۼƳ���=" (rtos dist1 2 0) "mm��"))
			(setq m (1+ m))
			(while
				(if pt1 (progn(initget 128 "f")(setq pt1 (getpoint pt2))))
				(if (= pt1 "f")
					(progn 
						(setq pt2 (getpoint "\n��ָ�����¿�ʼ��: ")) 
						(setq pt1 (getpoint pt2 " ��ָ����һ��: "))
						(grdraw pt1 pt2 1 1)
						(if pt2 (progn (setq pt1s (cons pt2 (list pt1)))))
					)
				)
				(setq pt1s (cons pt1 pt1s))
				(if (>= (length pt1s) 2) (grdraw pt1 pt2 1 1))
				(setq dist2 (distance pt2 pt1))
				(setq dist2 (* dist2 *key:drawscale*))
				(setq +dist (strcat +dist "+" (ValToStr dist2 1 3)))
				(princ (strcat "\n��" (itoa m) "�γ���=" (rtos dist2 2 0) "mm"))
				(setq dist1 (+ dist2 dist1))
				(princ (strcat "���ۼƳ���=" (rtos dist1 2 0) "mm��"))
				(setq pt2 pt1)
				(setq m (1+ m))
			)
			(endcmf +dist)
		)
		(pt1 (princ ">>>���ͣ���������"))
		(t (av:setdrawscale));���ò�������
	)
)

;;;===========================================================

;;˵��:���߱�ע���ȣ����ʱ�����
(defun c:pline-dim(/ *error* +dist data dist distsum endcmf fent h h1 n oldcl p1 p2 ss ssl startpt str)
	(defun *error*(msg)((endcmf +dist)))
	(defun endcmf (+dist / ent ess obj s strl strs)
		;���Ƶ�ճ����
		(setq +dist (vl-string-subst "" "+" +dist))
		(cond
			(av:putcliptext
				(av:putcliptext +dist)
				(princ (strcat "\n�Ѹ��ƣ�" +dist))
			)
			(t nil)
		)
		(cond
			;�����߶�ʱ��������
			((< n 3)(entdel fent))
			(T (setvar "peditaccept" 1)
				(command "pedit" "m" ssl "" "J" 1 "")
			)
		)	
		;�������ʱ��ע������ܳ�
		(if (equal startPT p1 1)
			(progn
				(setq ent (entlast))
				(setq obj (vlax-ename->vla-object ent))
				(setq s (vla-get-area obj))
				(setq s (* s (expt *key:drawscale* 2)))
				(setq strl (cdr (assoc 1 data)))
				(setq strl (vl-string-subst "" "��" strl))
				(setq strs (strcat "S=" (ValToStr s 2 2) "m2"))
				(setq ess (av:dimentls (entlast) strl strs nil))
				(entdel fent)
			)
		)
		(av:endtips n distsum s)
		(setvar "CLAYER" oldcl)
		;(sssetfirst nil ess)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(setq oldcl (getvar "CLAYER"))
	(if av:setactivelayer (av:setactivelayer "LB_���߱�ע" 7))
	;��λ��ʾ������
	(av:startips T)
	(while (/= (type (setq p1 (getpoint "\n��ʼ��:"))) 'list)
		(av:setdrawscale)
	)	
	(setq startPT p1 n 1 distsum 0)
	(setq ssl (ssadd) +dist "")
	;������һ��
	(princ " >>>��һ��")
	(while (setq p2 (getpoint p1))
		;���㻭��
		(entmake (list '(0 . "LINE") (cons 10 p1) (cons 11 p2)))
		;��עÿ���߳�
		(setq ss (entlast))
		(setq ssl (ssadd (entlast) ssl))
		(setq dist (distance p1 p2))
		(setq dist (* dist *key:drawscale*))
		(setq str (strcat "l=" (ValToStr dist 1 3) "m"))
		(av:dimentl ss str)
		;;��ע�ܳ���
		(setq distsum (+ dist distsum))
		(setq str (strcat "L��=" (ValToStr distsum 1 3) "m"))
		(if (= 1 n)
			(progn ;�״��ܳ��߱�ע
				(setq h (getscalen dist 20 500))
				(and (= 1 n) (setq h1 h))
				(entmaketext str startPT 0 h1 0.4)
				(setq fent (entlast) data (entget fent))
			)
			(progn ;�滻�ܳ��߱�ע
				(setq data (subst (cons 1 str) (assoc 1 data) data))
				(entmod data)
			)
		)
		;;��ʾ�������
		(princ (strcat "\n��" (rtos n 2 0) "�γ�=" (rtos dist 2 0)))
		(princ (strcat "���ۼƳ�=" (rtos distsum 2 0)))
		(setq +dist (strcat +dist "+" (ValToStr dist 1 3)))
		;׼��ѭ��
		(setq p1 p2)
		(setq n (1+ n))
	)	
	(endcmf +dist)
)

;;;===========================================================

;;�������
(defun c:manuarea(/ *error* ent obj oldc oldt vlr)
	(princ "-->�����������")
	(defun *error*(msg / l lst s strl strs)
		(cond
			(oldc
				(if vlr (vlr-remove vlr))
				(if hatchent (progn (vla-delete hatchent) (setq hatchent nil)))
				(vla-put-Closed obj 1)
				(setvar "transparencydisplay" oldt)
				(setvar "cetransparency" oldc)
			)
			(t (vla-endundomark *doc*))
		)
		;��ע���������
		(setq lst (av:calent ent) l (car lst) s (cadr lst))
		(setq strl (strcat "L=" (ValToStr l 1 3) "m"))
		(setq strs (strcat "S=" (ValToStr s 2 3) "m2"))
		(av:dimentls ent strl strs nil)
		(setvar "CMDECHO" 1)
		(princ)
	)
	(setvar "CMDECHO" 0)
	(cond
		((setq oldc (getvar "cetransparency"))
			(vl-cmdf "pline")
			(command pause)
			;����ָ������������� ByLayer ��͸���������ǿɼ����Ǳ����á�
			(and (setq oldt (getvar "transparencydisplay")) (setvar "transparencydisplay" 1))
			;ȷ�϶���
			(setq ent (entlast))
			(setq obj (vlax-ename->vla-object ent))
			;��ʱǳɫ��ʾ�������
			(setq vlr (vlr-object-reactor (list obj) nil '((:vlr-modified . xgm:vlrmodht))))
			(while (= 1 (getvar "cmdactive")) (command pause))
		)
		(T
			(vla-startundomark *doc*)
			(command "wipeout")
			(while (= 1 (getvar "cmdactive")) (command pause))
			(command "explode" (entlast) "")
			(setvar "peditaccept" 1)
			(command "pedit" "m" (ssget "p") "" "J" 1 "")
			(setq ent (entlast))
		)
	)
	(*error* nil)
)
;��Ӧ��ִ���������
(defun xgm:vlrmodht(obj vlr d / dxf ent)
	(setq ent (vlax-vla-object->ename obj))
	(setq dxf (entget ent))
	(if hatchent (vla-Delete hatchent))
	;�趨�¶����͸���ȼ���
	(setvar "cetransparency" 80)
	(dxf:makehatch dxf)
	(setq hatchent (vlax-ename->vla-object (entlast)))
)
;�������
(defun dxf:makehatch(dxf / ptlst)
	(setq ptlst (av:getptn dxf))
	(entmake (append
						 (list
							 '(0 . "HATCH")
							 '(100 . "AcDbEntity")
							 '(67 . 0) ;ֵΪ�ջ� 0 ʱ��ָ������ģ�Ϳռ䣬���Ϊ 1 ָ��ͼ�οռ�
							 '(410 . "Model")
							 (cons 8 (getvar "CLAYER")) ;ͼ����
							 '(62 . 100) ;��ɫ
							 '(100 . "AcDbHatch") ;������
							 '(10 0.0 0.0 0.0) ;��������
							 '(210 0.0 0.0 1.0) ;��������ѡ���ԣ�Ĭ��=0��0��1��
							 '(2 . "SOLID") ;���ͼ����
							 '(70 . 1) ;ʵ������־��0 = ͼ����䣻1 = ʵ����䣩
							 '(71 . 0) ;�����Ա�־��0 = �޹�����1 = ������
							 '(91 . 1) ;�߽�·����������
							 '(92 . 3) ;�߽�·�����ͱ�־
							 '(72 . 0) ;����͹�ȡ���־
							 '(73 . 1) ;���رա���־
						 )
						 (list (cons 93 (length ptlst)) ) ;����߶�����
						 (mapcar '(lambda (x) (cons 10 x)) ptlst) ;��������
						 (list
							 '(97 . 0) ;Դ�߽������
							 '(75 . 0) ;�µ������ʽ��0 = ��ͨ��1 = �ⲿ��2 = ����
							 '(76 . 1) ;���ͼ�����ͣ�0 = �û�����, 1 = Ԥ����, 2 = �Զ���
							 '(98 . 0) ;����������0�ͺã�������㡣
							 '(93 . 3) ;����߶�����
						 )
					 )
	)
)
;;==================================================================
;;˵��:�Զ�����
(defun c:MarkPileNum(/ *error* dxf ent ess fgetbox filter flst fristna getbox h key l lst m midp mn msg1 msg2 n oldcl pl pre pt1 pt2 ptlst s ss ssl str t1 t2 t3 v)
	(princ "\n˵�����Զ���������ע,��Ӧ����ҧ��׮������ͼ��ȱ��")
	;���򷵻���Ϣ
	(defun *error*(msg / out t2)
		(setvar "clayer" oldcl)
		;��ֹʱ������Ϣ
		(cond
			((member msg '("������ȡ��" "������ȡ��" ";����:quit / exit abort"))
				(setq out "���㱻ǿ����ֹ��Ŀǰ")
			)
			(T (setq out ""))
		)
		;������ʱ��ͳ������ʱ��
		(setq t2 (rtos (/ (- (getvar "millisecs") t1) 1000.000) 2 3))
		;�㱨��Ϣ
		(cond
			(m (princ (strcat "\n>>>" out "����ע" (itoa m) "�����󣬱����" str "���ܺ�ʱ" t2 "�롣")))
			(t (princ "���㱻ǿ����ֹ����δ���ü���ע"))
		)
		(princ)
	)
	;���Ѷ�ѡ����
	(if (= 1 (getvar "nomutt"))(setvar "nomutt" 0))
	;��ʼ����
	(setq oldcl (getvar "clayer"))
	;�رշ�Ӧ��
	(if (and
				av:totalreader_pickfirst_reactor
				(setq v (vlr-added-p av:totalreader_pickfirst_reactor))
			)
		(vlr-remove av:totalreader_pickfirst_reactor)
	)
	;ѡ�����
	(cond
		((and
			 dxf:assoc8
			 dxf:assoc62
			 (setq ent (entsel "\n���ѡ���ͼԪ������:<�س���������>"))
		 )
			(setq fristna (car ent))
			(setq dxf (entget fristna))
			(setq filter
				(list
					(cons 0 (cdr (assoc 0 dxf))) ;����
					(dxf:assoc8 dxf) ;ͼ��
					(dxf:assoc62 dxf) ;��ɫ
				)
			)
			(princ ">>>���ѡ��Χ")
		)
		(t (princ "\n��ʾ���ȵ��ѡ����ʼλ�ã��ٿ�ѡ��Χ"))
	)
	(setq ss (ssget filter))
	(sssetfirst nil ss)
	(setq ssl (sslength ss))
	(setq msg1 (if ss (strcat "��ѡ��" (itoa ssl) "������") ""))
	(if (> ssl (setq mn 1500)) ;����ͼԪ����
		(setq msg1 (strcat msg1 "������ʱ����ܹ��ã��������������򵥴�����" (itoa mn) "����"))
	)
	(prompt msg1)
	;ȷ��ǰ׺����ʼ���
	(if (null *num*) (setq *num* 0))
	(if (null *pre*) (setq *pre* ""))
	(setq msg2 (strcat
							 "\n������ǰ׺����ʼ���[<A>/<B>/<C>/��ǰ׺<NULL>/���±��<1>]:"
							 "<" (cond ((wcmatch *pre* "") "��ǰ׺") (t (strcat "ǰ׺" *pre*)))
							 ",��ʼ" (itoa (1+ *num*)) ">"
						 )
	)
	(setq key (getstring msg2))
	(cond
		((null key))
		((wcmatch key ""))
		((> (atoi key) 0) (setq *num* (1- (atoi key))))
		((wcmatch key "NULL") (setq pre ""))
		((wcmatch key "0")(princ "������Ч"))
		(t (setq pre key))
	)
	(cond
		((and pre (null (wcmatch pre *pre*)))
			(setq *pre* pre)
			(setq key (getint (strcat "��������ʼ���[���±��<1>]:<ǰ׺" key ",��ʼ" (itoa (1+ *num*)) ">")))
			(cond
				((null key))
				((zerop key)(princ "������Ч"))
				(t (setq *num* (1- key)))
			)
		)
		(t nil)
	)
	;ȡ��ѡ�񲢻ָ���Ӧ��
	(sssetfirst nil)
	(if v (vlr-add av:totalreader_pickfirst_reactor))
	;�ر��ص������Ĭ�ϻ�ͼ����
	(and (getvar "DRAWORDERCTL") (setvar "DRAWORDERCTL" 0))
	;��ʼ��ʱ
	(setq t1 (getvar "millisecs"))
	;ȡ�������б�
	(setq n 0)
	(repeat ssl
		(princ (strcat "\r���ڷ�������..." (setq *sbar (Spinbar *sbar))))
    (setq getbox (getbox-midp-minl (ssname ss n)))
    (setq midp (car getbox))
    (setq l (cadr getbox))
    (setq lst (append lst (list (list midp l))))
    (setq n (1+ n))
	)
	;�����б�,ȷ�ϵ�1��
	(cond
		(fristna
			(setq fgetbox (getbox-midp-minl fristna))
			(setq flst (list (car fgetbox) (cadr fgetbox)))
			(setq lst (vl-remove-if '(lambda(x) (equal x flst 0.01)) lst))
			(setq lst (cons flst lst))
		)
		(t nil)
	)
	;��ʼ��ע���
	(and
		av:setactivelayer
		(av:setactivelayer "LB_���������" 7)
	)
	(setq n 0 m (length lst))
	(while lst
		(setq n (1+ n))
		(setq t2 (/ (- (getvar "millisecs") t1) 1000.000))
		(setq t3 (* (- m n) (/ t2 n)))
		(setq *sbar (Spinbar *sbar))
		(princ (strcat "\r���ڱ�ע���<" (itoa n) "/" (itoa m) ",��ʱ" (rtos t2 2 0) "��,�ȴ�" (rtos t3 2 0) "��>..." *sbar))
		(setq *num* (1+ *num*))
		(setq str (strcat *pre* (itoa *num*)))
		(setq s (car lst) midp (car s) h (cadr s))
		(setq h (getscalen h 3.000 300))
		(entmaketext str midp (av:ang->rad 45) h -0.5)
		;���ɵ�λ�����б�
		(setq ptlst (cons midp ptlst))
		;���������������������
		(setq lst (vl-sort (cdr lst)
								'(lambda (x y) (< (distance midp (car x)) (distance midp (car y))))
							)
		)
	)
	;�������
	(setq ptlst (reverse ptlst))
	(setq pl
		(entmakex
			(append
				(list
					'(0 . "LWPOLYLINE")
					'(100 . "AcDbEntity")
					'(100 . "AcDbPolyline")
					'(62 . 250)
					(cons 90 (length ptlst))
				)
				(mapcar '(lambda (pt)(cons 10 pt)) ptlst)
			)
		)
	)
	;(command "DRAWORDER" (ssadd pl) "B")
	;������ʾ��ע��Ϣ
	(*error* msg)
)

;;===================================================
;;˵��:��������ת���������Ϊ�ַ���
;;����:v:�����������λmm
;;����:e:����Ϊ1�����Ϊ2
;;����:n:����С��λ��
;;����:ת������ַ���ֵ
;;(ValToStr 2000 2 3)
(defun ValToStr(v e n)
	(setq v (float v))
	(setq v (/ v (expt 1e3 e)))
	(rtos v 2 n)
)
;;˵��:���û�ͼ����
;;����:��
(defun av:setdrawscale(/ dist1 dist2 key msg oldsc oldscstr pt1 pt2 vlrstate)
	(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0));��Ϣ������ʾ
	(if (and
				av:totalreader_pickfirst_reactor
				(vlr-added-p av:totalreader_pickfirst_reactor)
			)
		(setq vlrstate "/�Զ�����(��C)")
		(setq vlrstate "/�Զ�����(��O)")
	)
	(setq oldsc *key:drawscale*)
	(setq oldscstr (av:startips nil))
	(setq msg
		(strcat
			"\nָ����������[1/10/50/100/1000/����(R)"
			vlrstate 
			"]<��ǰ1:"
			oldscstr
			(if (wcmatch oldscstr "1") ">" "���س�1:1>")
			" 1:"
		)
	)
	(initget "R C O")
	(setq key (getreal msg))
	(cond
		((null key)
			(setq *key:drawscale* 1.0)
		)
		((member (type KEY) (list 'REAL 'INT))
			(setq *key:drawscale* key)
			;(princ "yes")
		)
		((= key "C")
			(if (vlr-remove av:totalreader_pickfirst_reactor)
				(princ "�ѹر��Զ�ѡ�м��㹦��")
			)
		)
		((= key "O")
			(if (vlr-add av:totalreader_pickfirst_reactor)
				(princ "�Ѵ��Զ�ѡ�м��㹦��")
			)
		)
		((= key "R")
			(progn
				(setq pt1 (getpoint "\n��ָ����ʼ��: "))
				(setq pt2 (getpoint pt1 ">>>��ָ����һ��:"))
				(grdraw pt1 pt2 7 0)
				;(getdist pt1)
				(if (setq dist1 (distance pt1 pt2))
					(progn
						(princ (strcat "ͼ�Ͼ���=" (rtos dist1 2 3) "mm"))
						(setq dist2 (getreal "\nʵ�ʾ���<��λ:mm>="))
					)
					(setq *key:drawscale* oldsc)
				)
				(if dist2
					(setq *key:drawscale* (/ dist2 dist1))
					(setq *key:drawscale* oldsc)
				)
			)
		)
		(t nil)
	)
	(cond
		((and
			 key
			 (= 'str (type key))
			 (member (strcase key) (list "C" "O"))
		 )
		)
		((= oldsc *key:drawscale*)
			(princ (strcat ">>>>ԭ������1:" oldscstr "û�б仯��"))
		)
		(t (princ (strcat ">>>>�µĲ���������ָ��Ϊ1:" (av:startips nil))))
	)
	(princ)
)

;=====================================================

;;˵��:����������ȣ�����(getscalen 5000 9.000 800)
;;����:l:ԭʼ����
;;����:m:��С���������ǷŴ�(/ 1 8)
;;����:maxl:��󳤶�
;;����:�����ʵ�ʳ���
(defun getscalen(l m maxl)(cond ((> (setq l (/ l (float m))) maxl) maxl) (t l)))
;;˵��:���ɵ����ı�
;;����:str:��Ҫд����ı�����
;;����:pt:�����
;;����:ang:������ת�Ƕ�
;;����:h:���ָ߶�
;;����:sc:��Բ��������ƫ���ָ߱����������ɸ���
;;����:entmake���ɵ��ı�����
(defun entmaketext(str pt rad h sc)
	(setq pt (polar pt 
						 (+ rad (/ pi 2)) ;ָ���Ƕ�
						 (* h sc) ;ƫ�ƾ���
					 )
	)
	(entmake
		(list
			'(0 . "TEXT")
			(cons 1 str) ;��������
			(cons 11 pt) ;�����
			(cons 40 h) ;�ָ�
			(cons 50 rad) ;��ת����
			(cons 10 '(0 0 0)) ;�����Ƿ���ʾ
			(cons 72 1) ;���Ƶ�λ��
		)
	)
)
;;˵��:��ȡΧ�����ĵ㼰�̱߳�
;;����:en:��������
;;����:�б�list����1��������Χ�����ĵ㣬��2��������Χ�ж̱߳�
(defun getbox-midp-minl(en / ab minl midpt minpt maxpt obj)
	(setq obj (vlax-ename->vla-object en))
	(vla-getboundingbox obj 'minpt 'maxpt)
	;ȡ���е����겢Z�����
	(setq midpt (mapcar
								(function (lambda(e1 e2 e3) (* (+ e1 e2) e3)))
								(setq minpt (vlax-safearray->list minpt))
								(setq maxpt (vlax-safearray->list maxpt))
								(list 0.5 0.5 0)
							)
	)
	;Χ�б߳�
	(setq ab (mapcar
						 (function (lambda (e1 e2) (- (max e1 e2) (min e1 e2))))
						 minpt
						 maxpt
					 )
	)
	;Χ�ж̱߳�
	(setq minl (min (car ab) (cadr ab)))
	(list midpt minl)
)

;=======================================
(vl-load-reactors)
;ѡ���Զ�ͳ�Ʋ���
(defun av:totalreader-pickfirst (a b / en i l lst s ss suml sums)
	(eq 1 (logand 1 (getvar "pickfirst")))
	(cond
		((setq ss (ssget "_i" '((0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE,ARC,LINE"))))
			(setq i 0 suml 0 sums 0)
			(repeat (sslength ss)
				(setq en (ssname ss i))
				(setq i (1+ i))
				(setq lst (av:calent en))
				(setq l (car lst))
				(setq s (cadr lst))
				(setq suml (+ l suml))
				(setq sums (+ s sums))
			)
			(av:endtips (1+ i) suml sums)
		)
		(t nil)
	)
)
;;;����ͳ�Ʒ�Ӧ�����أ��ڱ��������빤���������п���
(or av:totalreader_pickfirst_reactor
	(setq av:totalreader_pickfirst_reactor
		(vlr-set-notification
			(vlr-miscellaneous-reactor nil '((:vlr-pickfirstmodified . av:totalreader-pickfirst)))
			'active-document-only
		)
	)
)
;(or *nomuttoff* (setq *renomuttoff* (vlr-command-reactor nil '((:vlr-commandWillStart . av:nomutt)))))
;;;��Ӧ����ԭnomutt����
(defun av:nomutt (a b)(if (/= 0 (getvar "nomutt"))(setvar "nomutt" 0)))
(or *nomuttoff*
	(setq *nomuttoff*
		(vlr-command-reactor nil
			'((:vlr-commandWillStart . av:nomutt)
				 (:vlr-unknownCommand . av:nomutt)
				 (:vlr-commandEnded . av:nomutt)
				 (:vlr-commandCancelled . av:nomutt)
				 (:vlr-commandFailed . av:nomutt)
			 )
		)
	)
)
;��Ӧ�����أ���info�����������ֶ�����
(defun totalreader_pickfirst_reactor (/ env v)
	(setq v (vlr-added-p av:totalreader_pickfirst_reactor))
	(setq env (getenv "autototalreader"))
	(if (= "1" env)
		(and (null v) (vlr-add av:totalreader_pickfirst_reactor))
		(and v (vlr-remove av:totalreader_pickfirst_reactor))
	)
)
(totalreader_pickfirst_reactor)
;;��ʾ�����Ƶĵ���ͼ��
(defun sssentlast()
	(while (= 1 (getvar "cmdactive")) (command pause))
	(if (vlr-added-p av:totalreader_pickfirst_reactor)
		(sssetfirst nil (ssadd (entlast)))
	)
	(princ)
)



(princ)



