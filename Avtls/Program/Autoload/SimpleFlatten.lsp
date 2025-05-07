(vl-load-com)
(defun c:SimpleFlatten(/ *error* 3dblk1 3dblk2 all flg h h/2 k midpt pix pt1 pt2 sc ss ssblk tch view1 w w/2 x0 x1 x2 y0 y1 y2)
	(defun *error*(msg)
		(vla-Regen *doc* acActiveViewport)
		(if (and k (/= 1 k))
			(progn
				(viewto3d 1)
				;(command-s "plan" "c")
				(command-s "zoom" "w" pt1 pt2);���Ա��ⷵ�ش�λ
			)
		)
    (vla-EndUndoMark *doc*)
		(setvar "CMDECHO" 1)
    (princ)
  )
	(setvar "CMDECHO" 0)
	(vla-StartUndoMark *doc*)
	(cond
		((= 1 (vlax-get *doc* 'ActiveSpace)))
		(t (vlax-put *doc* 'ActiveSpace 1))
	)
	(cond
		((= 1 (getvar "blockeditor"))
			(command-s "bclose" "s")
		)
		((/= "" (getvar "refeditname"))
			(command-s "refclose" "s")
		)
		(t nil)
	)
	
	(if (equal (setq view1 (getvar "VIEWDIR")) (list 0.0 0.0 1.0))
		(progn
			(setq
				pix (getvar "screensize") ;��ǰ�ӿڿ������ֵ
				sc (/ (car pix) (cadr pix)) ;��ǰ�ӿڿ�߱�
				h (getvar "viewsize") ;�ӿڸ߶�
				h/2 (/ h 2) ;���
				w (* h sc) ;�ӿڿ��
				w/2 (/ w 2) ;���
				midpt (getvar "viewctr") ;�ӿ����ĵ�
				x0 (car midpt) ;���ĵ�X��
				y0 (cadr midpt) ;���ĵ�Y��
				x1 (- x0 w/2) ;X��������
				x2 (+ x0 w/2) ;X��������
				y1 (- y0 h/2) ;y��������
				y2 (+ y0 h/2) ;y��������
				pt1 (list x1 y1 0)
				pt2 (list x2 y2 0)
			)
			(setq k (viewto3d 5))
		)
	)
	(setvar "qaflags" 1);ը��ѡ������ʵ��
	(setq flg t)
	(while (progn
					 (princ "\n�����Ͷ���ͼ�㲻ѹ����Esc�˳���Enterȫѡ")
					 (and flg
						 (setq ss (cond
												((ssget))
												((setq all (ssget "A" '(
																								 ;(2 . "~`**")
																							 ))) (setq flg nil) all)
												(t nil)
											)
						 )
					 )
				 )
		(while (setq tch (ssget "P" (list (cons 0 "TCH_*,3D*"))))
			(command-s "explode" tch "")
		)
		(flatten-3dto2d ss)
		(setq 3dblk1 (flatten-get3dblk-list ss))
		(setvar "qaflags" 0);��ֹcmd����
		(while 3dblk1
			(setq 3dblk2 (list))
			(foreach l 3dblk1
				(command-s "bedit" l)
				(flatten-3dto2d (ssget "A"))
				(setq ssblk (ssget "A" '((0 . "INSERT") (2 . "~`**"))))
				(setq 3dblk2 (append (flatten-get3dblk-list ssblk) 3dblk2))
				(command-s "bclose" "s")
			)
			(setq 3dblk1 (av:delstsame 3dblk2))
		)
	)
	(*error* nil)
)
;;ѹ��ȫ��ͼԪ��ά��Ϣ
(defun flatten-3dto2d(ssblk / *sbar ismod lst m ssnlst)
	;(setq ssblk (ssget "A"))
	(setq ssnlst (vl-catch-all-apply 'av:ss->ssnlst (list ssblk)))
	(if (vl-catch-all-error-p ssnlst) (setq ssnlst nil))
	(foreach l ssnlst
		(princ (strcat "\r���ڽ�����άѹ��..." (setq *sbar (Spinbar *sbar))))
		(setq ismod nil)
		(setq lst (mapcar (function (lambda(x / k z)
																	(setq k (car x))
																	(cond
																		((and
																			 ;key=10 11 12 13 210��210���׵���ͼ�ζ�ʧ
																			 (member k (list 10 11 12 13))
																			 (setq z (cadddr x))
																			 (/= 0 z)
																		 )
																			(setq ismod t)
																			(list k (cadr x) (caddr x) 0)
																		)
																		((and
																			 (member k (list 38 39))
																			 (/= 0 (cdr x))
																		 )
																			(setq ismod t)
																			(cons k 0)
																		)
																		(t x)
																	)
																)
											)
								(entget l)
							)
		)
		(if ismod (entmod lst))
	)
)
;;�õ�3ά�����б���������Ӱ�죩
;;������άͼ���б�
(defun flatten-get3dblk-list(ssblk / 2dlst 3dslst blklst box lst obj ptz ssnlst)
	;(setq ssblk (ssget "A" '((0 . "INSERT")(2 . "~`**"))))
	(setq ssnlst (vl-catch-all-apply 'av:ss->ssnlst (list ssblk)))
	(if (vl-catch-all-error-p ssnlst) (setq ssnlst nil))
	;����������ͬ�Ŀ�
	(setq lst (list))
	(foreach l ssnlst
		(setq lst (cons (cons l (cdr (assoc 2 (entget l)))) lst))
	)
	(setq ssnlst (list) blklst (list))
	(foreach l lst
		(cond
			((member (cdr l) blklst))
			(t
				(setq ssnlst (cons (car l) ssnlst))
				(setq blklst (cons (cdr l) blklst))
			)
		)
	)
	;ȡ����άͼԪ
	(foreach l ssnlst
		(setq obj (vlax-ename->vla-object l))
		(cond
			((progn
				 (setq box (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur)))
				 (vl-catch-all-error-p box)
			 )
				(setq ptz (mapcar 'caddr (lm-get-blkboundingbox obj)))
			)
			(t
				(setq ptz (mapcar '(lambda(x) (caddr (vlax-safearray->list x))) (list ll ur)))
			)
		)
		(and
			ptz
			(or (< 5 (abs (car ptz))) (< 5 (abs (cadr ptz))))
			(setq 3dslst (cons l 3dslst))
		)
	)
	;ȡ����ά����
	(foreach l 3dslst
		(setq 2dlst (cons (cdr (assoc 2 (entget l))) 2dlst))
	)
	(setq 2dlst (av:delstsame 2dlst))
)
;;˵��:��ͼ��λ����ά��ת
;;����:n:1-10��ֱ�Ӷ�λ��Ӧ��ͼ��nil����ͼѡ��
;;����:��
;;����:(viewto3d nil) (viewto3d 1) (viewto3d 8)
(defun viewto3d(n / i k pt pt11 pt22 s str viewlst)
	(setq
		pt (av:getscr4pt)
		pt11 (vlax-3d-point (car pt))
		pt22 (vlax-3d-point (caddr pt))
	)
	(setq viewlst (list
									(list 1 "top" "����")
									(list 2 "bottom" "����")
									(list 3 "left" "����")
									(list 4 "right" "����")
									(list 5 "front" "ǰ��")
									(list 6 "back" "����")
									(list 7 "sw" "����")
									(list 8 "se" "����")
									(list 9 "ne" "����")
									(list 10 "nw" "����")
								)
	)
	(setq str "")
	(foreach l viewlst (setq str (strcat str (caddr l) "(" (itoa (car l)) ")/")))
	(setq str (strcat "\n��ѡ�ӽ�[" str "]<Ĭ�ϸ���>"))
	(setq s t)
	(while s
		(cond
			((progn
				 (or n (setq i (vl-catch-all-apply 'getint (list str))))
				 (vl-catch-all-error-p i)
			 )
				(setq k "top" i 1 s nil)
			)
			(t
				(if n (progn (setq i n s nil)))
				(setq i (cond ((and (< 0 i) (< i 11)) i) (t (setq s nil) 1)))
				(setq k (cadr (assoc i viewlst)))
			)
		)
		(command-s "view" k)
		(vla-zoomwindow *acad* pt11 pt22)
		(if (/= i 1) (command-s "3dorbit"))
	)
	i
)
