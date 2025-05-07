(defun c:fastprinter(/ *error* ab cmdplot ddev devnil fr getdef getdev h index istilm l layplot ls lst n otab pdev pt1 pt2 sdev set3plotdev str x1 x2 y1 y2)
	(defun *error*(msg)
		;(setvar "PLOTTRANSPARENCYOVERRIDE" 0)
		(cond
			(devnil
				(princ "\n>>>�豸����")
				(and slstnil (princ "�Ҳ�����ӡ��ʽ"))
				(and dlstnil (princ "���޴�ӡ�豸"))
				(and plstnil (princ "��û��ͼֽ�б�"))
			)
			(t nil)
		)
		(setvar "CMDECHO" 1)
		(princ)
	)
	(defun cmdplot(/ lst lst1 lst2 lst3 lst4 lst5)
		(setq lst1 (list
								 ;"plot" ;��ӡ����
								 "y" ;�Ƿ���Ҫ��ϸ��ӡ����
								 (getvar "ctab") ;���벼�֡�ģ������
								 ddev ;��������豸������
								 pdev ;����ͼֽ�ߴ�
								 "M" ;����ͼֽ��λ(I:Ӣ�� M:����)
								 ab ;����ͼ�η���(����:P ����:L)
								 "N" ;�Ƿ����ӡ
							 )
		)
		(setq lst2
			(cond
				(pt2 (list
							 "W" ;�����ӡ����(��ʾ:D ��Χ:E ͼ�ν���:L ��ͼ:V ����:W)
							 pt1 ;���½�
							 pt2 ;���Ͻ�
						 )
				)
				(t (list
						 "E";�����ӡ���� [��ʾ(D)/��Χ(E)/����(L)/��ͼ(V)/����(W)] <��Χ>: e
					 )
				)
			)
		)
		(setq lst3 (list
								 "F" ;�����ӡ����(F:����)
								 "C" ;�����ӡƫ��(���д�ӡ:C)
								 "Y" ;�Ƿ���ʽ��ӡ
								 sdev ;�����ӡ��ʽ����
								 "Y" ;�Ƿ��ӡ�߿�
							 )
		)
		(setq lst4
			(cond
				((= 1 (getvar "tilemode"))
					(list
						"A" ;������ɫ��ӡ����(����ʾ:A �߿�:W ����:H ��Ⱦ:R)
					)
				)
				(t (list
						 "y" ;�Ƿ񰴴�ӡ���������߿�
						 "n" ;�Ƿ��ȴ�ӡͼֽ�ռ䣿
						 "n" ;�Ƿ�����ͼֽ�ռ����
					 )
				)
			)
		)
		(setq lst5 (list
								 "n" ;�Ƿ��ӡ���ļ�
								 "Y" ;�Ƿ񱣴��ҳ�����õ��޸�
								 "y" ;�Ƿ������ӡ
							 )
		)
		(setq lst (cons "plot" (append lst1 lst2 lst3 lst4 lst5)))
		(apply 'command-s lst)
	)
	(defun index (l n / x)
		(if (> n 0) (cons (cons (itoa (- (length l) (setq x (1- n)))) (nth x l)) (index l x)))
	)
	(defun getdev(lst n / dev idlst k key msg str1)
		(setq lst (reverse lst))
		(setq idlst (index lst (length lst)))
		(initget (apply 'strcat (mapcar '(lambda (x) (strcat (car x) " ")) idlst)))
		(setq str1 (cons "\n�����豸�б� \n" (mapcar '(lambda (x) (strcat "[" (car x) "]=" (cdr x) "\n")) idlst)))
		(setq n (cond (n (itoa n)) (t "1")))
		(setq msg (strcat (apply 'strcat str1) "���������<Ĭ��Ϊ" n ">:"))
		(setq key (getkword msg))
		(setq k (if key key n))
		(setq dev (cdr (assoc k idlst)))
		(print dev)
		dev
	)
	(defun getdef(def lst / dev l len m n)
		(setq def (strcase def))
		(setq len (length lst))
		(setq n 0)
		(while (setq l (nth n lst))
			(cond
				((wcmatch def (strcase l))
					(setq dev (list l (1+ n)))
					(setq n len)
					(setq m len)
				)
				(t
					(setq n (1+ n))
					(setq m 0)
				)
			)
		)
		(while (setq l (nth m lst))
			(cond
				((vl-string-search def (strcase l))
					(setq dev (list l (1+ m)))
					(setq m len)
				)
				((= (setq m (1+ m)) len)
					(setq dev (list (nth 0 lst) 0))
				)
				(t nil)
			)
		)
		dev
	)
	(defun set3plotdev(flag / darr ddev defd defp defs dlst dlstnil layout m mm msg parr pdev plst plstnil sarr sdev setplotdev setplotpaper setplotstyle slst slstnil)
		(setq defs "�ڰ�ϸ��" defd "pdf" defp "a4")
		(setq layout (vla-get-activelayout *doc*))
		;��ӡ��ʽ�ֶ�ѡ��
		(defun setplotstyle(/ sid sn)
			(setq sid (getdef defs slst) sn (cadr sid))
			(av:setenv "plotstyle" (getdev slst sn))
		)
		;��ӡ��ʽ�б�
		(setq sarr (vlax-variant-value (vla-GetPlotStyleTableNames layout)))
		(setq slst (vl-catch-all-apply 'vlax-safearray->list (list sarr)))
		(setq slstnil (vl-catch-all-error-p slst))
		;��ӡ��ʽȷ��
		(cond
			(slstnil)
			(flag (setq sdev (setplotstyle)))
			((member (setq sdev (av:getenv "plotstyle")) slst))
			(t (setq sdev (setplotstyle)))
		)
		;��ӡ�豸�ֶ�ѡ��
		(defun setplotdev(/ denv did dn dn1 dn2)
			(and
				(setq denv (av:getenv "plotdev"))
				(setq dn1 (1+ (vl-position denv dlst)))
			)
			(and
				(setq did (getdef defd dlst))
				(setq dn2 (cadr did))
			)
			(setq dn (if dn1 dn1 dn2))
			(av:setenv "plotdev" (getdev dlst dn))
		)
		;��ӡ�豸�б�
		(setq darr (vlax-variant-value (vla-getplotdevicenames layout)))
		(setq dlst (vl-catch-all-apply 'vlax-safearray->list (list darr)))
		(setq dlstnil (vl-catch-all-error-p dlst))
		;��ӡ���豸��ѡ��
		(cond
			((null sdev))
			(dlstnil)
			(t
				(setq dlst (vl-remove-if (function (lambda(x) (wcmatch x "��"))) dlst))
				(cond
					(flag (setq ddev (setplotdev)))
					((member (setq ddev (av:getenv "plotdev")) dlst))
					(t (setq ddev (setplotdev)))
				)
				(vla-put-ConfigName layout ddev)
			)
		)
		;��ӡͼֽ�ֶ�ѡ��
		(defun setplotpaper(/ pid pn)
			(setq
				pid (getdef defp plst)
				pn (cadr pid)
			)
			(av:setenv "plotpaper" (getdev plst pn))
		)
		;��ӡͼֽ�б�
		(setq parr (vlax-variant-value (vla-GetCanonicalMediaNames layout)))
		(setq plst (vl-catch-all-apply 'vlax-safearray->list (list parr)))
		(setq plstnil (vl-catch-all-error-p plst))
		;��ӡͼֽȷ��
		(cond
			((null ddev))
			(plstnil)
			(t
				(setq plst (vl-remove-if (function (lambda(x) (wcmatch x "none_user_media"))) plst))
				(cond
					(flag (setq pdev (setplotpaper)))
					((member (setq pdev (av:getenv "plotpaper")) plst))
					(t (setq pdev (setplotpaper)))
				)
			)
		)
		;ȷ���Ƿ�͸����
		(setq m (getvar "PLOTTRANSPARENCYOVERRIDE"))
		(setq msg (strcat "\n�Ƿ�ʹ��͸���ȴ�ӡ[0��/1��ѡ��/2��]:<Ĭ��Ϊ" (itoa m) ">"))
		(if (and flag (setq m (getint msg)))
			(setvar "PLOTTRANSPARENCYOVERRIDE" m)
		)
		(setq m (getvar "PLOTTRANSPARENCYOVERRIDE"))
		(setq mm
			(cond
				((= 0 m)
					"��ʹ��͸���ȴ�ӡ"
				)
				((= 1 m)
					"���ݴ�ӡ�����е�ѡ��ѡ���Ƿ�ʹ��͸���ȴ�ӡ"
				)
				((= 2 m)
					"ʹ��͸���ȴ�ӡ"
				)
			)
		)
		;��ǰ�豸��ʾ
		(cond
			((and sdev ddev pdev)
				(princ (strcat "\n͸����ʽ��" mm))
				(princ (strcat "\n��ӡ����������ͼֽ"))
				(princ (strcat "\n��ӡ��ʽ��" sdev))
				(princ (strcat "\n��ӡ�豸��" ddev))
				(princ (strcat "\nͼֽ�ߴ磺" pdev))
				(list sdev ddev pdev)
			)
			(t nil)
		)
	)
	;;��ӡ����
	(defun layplot(/ ab h n obj pt1 pt2 pts ss w)
		(setq ss (ssget "A" (list
													'(0 . "INSERT")
													(cons 2 fr)
													(cons 410 (getvar "ctab"));��ǰ����
												)
						 )
		)
		(cond
			(ss
				(setq n 0)
				(repeat (sslength ss)
					(setq obj (vlax-ename->vla-object (ssname ss n)))
					(setq pts (lm-get-blkboundingbox obj))
					(setq pt1 (car pts) pt2 (cadr pts))
					(setq w (abs (apply '- (mapcar 'car pts))))
					(setq h (abs (apply '- (mapcar 'cadr pts))))
					(setq ab (cond ((> w h) "L") (T "P")))
					(cmdplot)
					(setq n (1+ n) pt2 nil)
				)
			)
			(t
				(setq ab "L")
				(cmdplot)
			)
		)
	)
	;��ʼ���ܣ��жϻ���
	(princ "\n����ltscale(LTS)���趨ȫ�����ͱ������ӣ��������ߵ���ʾ������")
	(cond
		((setq ls (cond ((set3plotdev nil)) (t (set3plotdev t))))
			(setq sdev (car ls) ddev (cadr ls) pdev (caddr ls))
		)
		(t
			(setq devnil t)
			(exit)
		)
	)
	(cond
		((setq istilm (= 1 (getvar "tilemode"))))
		(t (vla-put-MSpace *doc* :vlax-false))
	)
	;ȷ�ϴ�ӡ��Χ
	(setq n 1)
	(initget (+ 2 4) "S")
	(setq str (strcat "\n��1����ӡ��Χ[����(S)]<�س�����" (if istilm "����" "��ͼ") "��ӡ>��"))
	(while
		(and
			(setq pt1 (getpoint str))
			(cond
				((eq 'list (type pt1))
					(setq pt2 (getcorner pt1 ""))
				)
				((wcmatch pt1 "S")
					(setq ls (set3plotdev t) sdev (car ls) ddev (cadr ls) pdev (caddr ls))
					(setq
						pt1 (getpoint (strcat "\n��1����ӡ��Χ��"))
						pt2 (getcorner pt1)
					)
				)
				(t nil)
			)
		)
		(setq n (1+ n))
		(setq str (strcat "\n��" (itoa n) "����ӡ��Χ��"))
		(setq
			x1 (car pt1)
			y1 (cadr pt1)
			x2 (car pt2)
			y2 (cadr pt2)
			l (abs (- x2 x1))
			h (abs (- y2 y1))
			ab (if (> l h) "l" "p")
		)
		(setq lst (cons (list ab pt1 pt2) lst))
	)
	(setq lst (reverse lst))
	;��ʼ��ӡ
	(setvar "cmdecho" 0)
	(setq fr "*A0*,*A1*,*A2*,*A3*,*A4*,*ͼ��*")
	(cond
		(lst
			(foreach l lst
				(setq ab (car l) pt1 (cadr l) pt2 (caddr l))
				(cmdplot)
			)
		)
		(istilm
			(setq lst (lm:listbox "����������ӡ���ɶ�ѡ��" (layoutlist) t))
			(setq otab (getvar "ctab"))
			(foreach l lst
				(setvar "ctab" l)
				(layplot)
			)
			(setvar "ctab" otab)
			(if lst (princ (strcat "\nͼ�����ʶ���룺" fr)))
		)
		(t
			(layplot)
			(princ (strcat "\nͼ�����ʶ���룺" fr))
		)
	)
	(*error* nil)
)


