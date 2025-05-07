;���ɴ���
(defun c:wf()(c:windowsform))
(defun c:windowsform(/ *error* ass d dict dis e enlst ent entmakedata getfilter getk h key keyhig l la layers lst mccnt mckey mclst mcname method msg n na1 na2 oldcl os pt0 pt01 pt02 pt03 pt11 pt12 pt13 pts s ss ss_visible sss sswindows strh tabstr total ty)
	(defun *error*(msg)
		(cond
			(msg
				(ss_visible (ssget "x") 1)
				(sssetfirst nil ss)
			)
			(t nil)
		)
		(setvar "clayer" oldcl)
		(setvar "OSMODE" os)
		(princ)
	)
	;����ѡ���������أ�mode��1��ʾ 2���� 3���� 4������
	(defun ss_visible(ss mode)
		(repeat (setq n (sslength ss))
			(redraw (ssname ss (setq n (1- n))) mode)
		)
	)
	;����ѡ��ģʽ
	(defun getfilter(layers mckey)
		(list
			(cons -4 "<or")
			(cons -4 "<and") (cons 0 "*TEXT") (cons 1 mckey) (cons -4 "and>")
			(cons -4 "<and") (cons 0 "TCH_OPENING") (cons 302 mckey) (cons -4 "and>")
			(cons -4 "<and") (cons 0 "INSERT") (cons 66 1) (cons -4 "and>")
			(cons -4 "or>")
			(cons 8 layers)
		)
	)
	;����ѡ��
	(defun sswindows(layers mckey / enlst filter method msg n pts ss ssx)
		(sssetfirst)
		(cond
			((setq ss (ssget (getfilter layers mckey))))
			(t
				(setq msg "\nδ��ѡ�����ݣ���ָ���Ŵ���:")
				(setq layers (cdr (assoc 8 (entget (car (entsel msg))))))
				(setq ss (ssget (getfilter layers mckey)))
			)
		)
		(cond
			(ss
				(setq ssx (ssnamex ss))
				;ͼԪ�б�
				(setq enlst
					(vl-remove-if-not
						(function (lambda (x) (= (type x) 'ENAME)))
						(mapcar 'cadr ssx)
					)
				)
				;���ڲ���
				(setq n (caar ssx))
				(setq method (cond ((= 2 n) "WP") ((= 3 n) "CP")))
				;��������б�(���� ���� ���� ����)
				(setq pts (mapcar 'cadr
										(vl-remove-if
											(function (lambda(x) (= -1 x)))
											(assoc -1 ssx)
										)
									)
				)
				(list ss enlst method pts)
			)
			(t
				(princ "��ʾ��û��ѡ���Ŵ������޸�ͼ������ʶ���룡")
				;(exit)
			)
		)
	)
	(princ "-->ͳ���Ŵ���")
	(setq os (getvar "OSMODE"))
	(setvar "osmode" 0)
	;���ñ�ע��
	(setq oldcl (getvar "clayer"))
	(and
		av:setactivelayer
		(av:setactivelayer "LB_���������" 7)
	)
	;�T���������ڲ�
	(setq layers (strcat "*WIND*,*��*,*��*,*_TEXT"))
	(princ (strcat "\nͼ����ʶ����:" layers))
	;�T����ʶ����
	(setq mckey (strcase "*M*,*C*"))
	(princ (strcat "\n�Ŵ���ʶ����:" mckey))
	;ѡ����ʾ
	(princ "\n��ѡ���Ŵ�(���ֻ����Կ���ɣ�����ֽ��)")
	;��ʽ��ʼ
	(setq sss (sswindows layers mckey))
	(setq ss (car sss))
	(setq enlst (cadr sss))
	(setq method (caddr sss))
	(setq pts (cadddr sss))
	(sssetfirst nil ss)
	(defun getk()
		(initget "A S D F")
		(getkword (strcat "\n��鵱ǰѡ���Ƿ���Ҫ�޸�[��+(A)/��-(S)/��+(D)/��-(F)]:<Ĭ��Ϊ��>"))
	)
	;�޸�ѡ��
	(while (setq key (getk))
		(setq key (strcase key))
		(cond
			((wcmatch key "A") ;�������
				(ss_visible SS 2)
				(cond
					((setq s (car (sswindows "*" mckey)))
						(setq n (sslength s))
						(repeat n (ssadd (ssname s (setq n (1- n))) ss))
					)
				)
				(ss_visible SS 1)
			)
			((wcmatch key "S") ;����ɾ��
				;(ss_visible (ssget "x" (list (cons 8 layers))) 2)
				;(ss_visible ss 1)
				(cond
					((setq s (car (sswindows "*" mckey)))
						(setq n (sslength s))
						(repeat n (ssdel (ssname s (setq n (1- n))) ss))
						(ss_visible s 2)
					)
				)
				;(ss_visible (ssget "x") 1)
			)
			((wcmatch key "D") ;�������
				(ss_visible SS 2)
				(sssetfirst)
				(while (setq ent (entsel "\n���ѡ��Ҫ��ӵ��Ŵ�ͼ��"))
					(setq la (cdr (assoc 8 (entget (car ent)))))
					(setq s (ssget method pts (getfilter la mckey)))
					(setq n (sslength s))
					(repeat n (ssadd (ssname s (setq n (1- n))) ss))
					(ss_visible S 2)
				)
				(ss_visible SS 1)
			)
			((wcmatch key "F") ;����ɾ��
				(ss_visible (ssget "x" (list (cons 8 layers))) 2)
				(ss_visible ss 1)
				(sssetfirst)
				(while (setq ent (entsel "\n���ѡ��Ҫɾ�����Ŵ�ͼ��"))
					(setq la (cdr (assoc 8 (entget (car ent)))))
					(setq s (ssget method pts (getfilter la mckey)))
					(setq n (sslength s))
					(repeat n (ssdel (ssname s (setq n (1- n))) ss))
					(ss_visible S 2)
				)
				(ss_visible (ssget "x") 1)
			)
		)
		(sssetfirst nil ss)
	)
	;�ų�������
	(setq lst (list nil "" "C" "M" "A/C" "2A/C" "C����"));��ȫ����д
	;(setq l (vl-remove-if (function (lambda(x) (member (strcase (car x)) lst))) l))
	;��ʼ�γ��б�
	(while (> (sslength ss) 0)
		;��ȡ����
		(setq e (ssname ss 0))
		(ssdel e ss)
		(setq d (entget e))
		(setq ty (cdr (assoc 0 d))) ;ȡ��ͼԪ����
		(cond
			((member ty (list "TEXT" "MTEXT"))
				(setq mcname (cdr (assoc 1 d)))
			)
			((wcmatch ty "TCH_OPENING")
				(setq mcname (cdr (assoc 302 d)))
			)
			((wcmatch ty "INSERT")
				(setq
					mclst (mapcar 'cadr (getatts e))
					na1 (car mclst)
					na2 (cadr mclst) ;̨��ͼֽ������������
					mcname (if na2 (strcat na1 "/" na2) na1)
				)
			)
			(t nil)
		)
		;���Ľ����б�(�Ŵ� ����)
		(cond
			((member mcname lst)) ;�ų�������
			((member mcname (setq ass (assoc mcname l)))
				(setq mccnt (1+ (cdr ass)))
				(setq l (subst (cons mcname mccnt) ass l))
			)
			(t (setq l (cons (cons mcname 1) l)))
		)
	)
	;�����ƽ�������
	(setq l (vl-sort l (function (lambda (l1 l2) (< (car l1) (car l2))))))
	;ȷ�ϱ������
	(cond
		((null l) (princ "û���Ŵ�") (exit))
		((setq ent (entsel "\n��ѡ�������(����ͼ��)"))
			(setq tabstr (cdr (assoc 1 (entget (car ent)))))
		)
	)
	;�ָ�����
	(setq dict "avtls"  keyhig "texthight")
	(cond
		((setq h (vlax-ldata-get dict keyhig)))
		(t (setq h (vlax-ldata-put dict keyhig 400)))
	)
	(setq strh (cond ((= (type h) 'int) (itoa h)) (t (rtos h 2 2))))
	(setq msg (strcat "\n��ȡ������Ͻ�<��ǰ�ָ�" strh "[�ָ�����(S)],Ĭ��ǰ���Ҳ�>��"))
	;��ʼ��ע
	(initget "S ")
	(setq pt01 (getpoint msg))
	(cond
		((null pt01))
		((= 'list (type pt01)))
		((wcmatch "S" (strcase pt01))
			(and
				(setq dis (getdist "�������ָ�<Ĭ�ϲ���>��"))
				(setq h (vlax-ldata-put dict keyhig dis))
			)
			(progn (princ "��ǰ�ָ�") (princ h))
			(setq pt01 (getpoint "\n��ȡ������Ͻ�,Ĭ��ǰ���Ҳ�>��"))
		)
		(t nil)
	)
	(cond
		((= 'list (type pt01)))
		(t
			;ȷ����ʼ��λ��
			(and *tabpt03* (setq pt01 (mapcar '(lambda(x y) (+ x y)) *tabpt03* (list (* 2 h) 0 0))))
			(while (null (or pt01 (setq pt01 (getpoint "\n��ǰ�׸�������ȡ���Ͻǣ�")))))
		)
	)
	;��ע�������
	(setq pt0 (mapcar '(lambda(x y) (+ x y)) pt01 (list 0 (* 0.5 h) 0)))
	(cond (tabstr) (t (setq tabstr "���޸ı����")))
	(entmake (list '(0 . "TEXT") (cons 1 tabstr) (cons 10 pt0) (cons 40 (* 1.5 h))))
	;��ע����һ����
	(setq pt02 (mapcar '(lambda(x y)(+ x y)) pt01 (list (* 11 h) 0 0)))
	(setq pt03 (mapcar '(lambda(x y)(+ x y)) pt02 (list (* 4 h) 0 0)))
	(entmake (list '(0 . "LINE") (cons 10 pt01) (cons 11 pt03)))
	(setq pt11 pt01 pt12 pt02 pt13 pt03)
	;Ϊ�´α���ṩ��λ��
	(setq *tabpt03* pt03)
	;��ע���������
	(defun entmakedata(str1 str2)
		(setq pt1 (mapcar '(lambda(x y) (+ x y)) pt11 (list h (* -1.5 h) 0)))
		(setq pt2 (mapcar '(lambda(x y) (+ x y)) pt12 (list h (* -1.5 h) 0)))
		(entmake (list '(0 . "TEXT") (cons 1 str1) (cons 10 pt1) (cons 40 h)))
		(entmake (list '(0 . "TEXT") (cons 1 str2) (cons 10 pt2) (cons 40 h)))
		(setq pt21 (mapcar '(lambda(x y) (+ x y)) pt11 (list 0 (* -2 h) 0)))
		(setq pt22 (mapcar '(lambda(x y) (+ x y)) pt12 (list 0 (* -2 h) 0)))
		(setq pt23 (mapcar '(lambda(x y) (+ x y)) pt13 (list 0 (* -2 h) 0)))
		(entmake (list '(0 . "LINE") (cons 10 pt21) (cons 11 pt23)))
	)
	;ͳ�����ݱ�ע
	(setq total 0)
	(while (> (length l) 0)
		(setq n (cdar l))
		(setq total (+ n total))
		(entmakedata (caar l) (itoa n))
		(setq pt11 pt21 pt12 pt22 pt13 pt23)
		(setq l (cdr l))
	)
	(entmakedata "��:�Ŵ��ϼ�" (itoa total))
	;�������ָ���
	(entmake (list '(0 . "LINE") (cons 10 pt01) (cons 11 pt21)))
	(entmake (list '(0 . "LINE") (cons 10 pt02) (cons 11 pt22)))
	(entmake (list '(0 . "LINE") (cons 10 pt03) (cons 11 pt23)))
	(*error* nil)
)
(defun getatts(blk / array att isattblk obj)
	(setq obj
		(cond
			((= (type blk) 'ename) (vlax-ename->vla-object blk))
			(t blk)
		)
	)
	(setq isattblk
		(equal
			(vl-catch-all-apply 'vlax-get-property (list obj 'hasattributes) )
			:vlax-true
		)
	)
	(cond
		((vl-catch-all-error-p (setq att (vl-catch-all-apply 'vlax-invoke-method (list obj 'GetAttributes)))))
		(t (setq array (vlax-safearray->list (vlax-variant-value att))))
	)
	(if isattblk
		(mapcar (function (lambda(x) (list (strcase (vla-get-TagString x)) (vla-get-TextString x) x))) array)
	)
)
