;ֱ�Ӻϲ�
(defun c:pe (/ ss)
	(princ "-->���ߺϲ�,����PEE��ģ���ϲ�")
	(setvar "cmdecho" 0)
	(cond
		(initcommandversion
			(vl-catch-all-apply 'initcommandversion)
			((if command-s command-s command) "join")
		)
		(T
			(setvar "peditaccept" 1)
			(while (setq ss (ssget '((0 . "line,arc,*polyline"))))
				(av:chkopen ss);��ǿ���λ��
				(command-s "pedit" "m" ss "" "J" "J" "E" 0 "")
			)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)
;�����ģ���ϲ�
(defun c:pee(/ *error* ss)
	(defun *error*(msg)(princ "�����������㽫����һ����������磡"))
	(princ "-->�����ģ���ϲ�����")
	(princ (strcat "\n��ѡ��ϲ�����" (av:petips1) "���س����á�"))
	(setq ss (ssget '((0 . "line,arc,*polyline"))))
	(av:pefuzzy ss)
)

;�����������ģ���ϲ�
(defun av:pefuzzy(ss / *error* is1pl n os x)
	;�ж��Ƿ�ֻ��ѡ����1�������
	(defun is1pl()
		(and
			;�ж��Ƿ�ֻѡ����1��
			ss (= 1 (sslength ss))
			;�ж��Ƿ��Ƕ����
			(vl-string-search
				(strcase "polyline")
				(cdr(assoc 0(entget(ssname ss 0))))
			)
		)
	)
	;�ж��Ƿ�պ�
	(defun isplclose(ss)(= 1 (logand 1 (cdr (assoc 70 (entget (ssname ss 0)))))))
	;���󷵻غ���
	(defun *error*(msg)
		(setvar "osmode" os)
		(vla-endundomark *doc*)
		(setvar "cmdecho" 1)
		(princ)
	)
	;��ʼ����
	(setvar "cmdecho" 0)
	(vla-startundomark *doc*);���ƻ�Ԥ��ѡ��
	(setq os (getvar "osmode"))
	(setvar "peditaccept" 1)
	(and ss (av:chkopen ss));��ǿ���λ��
	;����ģ��ֵ���ã����ǵ�������ߣ�û��ģ��ֵ��û��ѡ������
	(setq n 0)
	(cond
		((is1pl)
			(if (isplclose ss)(setq ss nil) nil)
		)
		((or (zerop pefuzzylen) (null ss))
			(progn (av:setpefuzzylen) (setq n (1+ n)))
		)
		(t nil)
	)
	;ѭ��ѡ����кϲ�����
	(while n
		;�ж��Ƿ���Ҫ����ѡ��
		(if (null ss)
			(and
				(princ (strcat "\n��ѡ��ϲ�����" (av:petips1)))
				(setq ss (ssget '((0 . "line,arc,*polyline"))))
				(av:chkopen ss);��ǿ���λ��
			)
		)
		(if ss
			(cond
				;����ǵ�������ߣ���ɱպ�
				((and (is1pl) (null (isplclose ss)))
					(initget "Y N ")
					(setq x (getkword "��ǰѡ��Ϊ����ߣ���ֻ��1�����Ƿ���Ҫ�պ�[��(Y)/��(N)]:<Ĭ��Ϊ��>"))
					(if (or (null x) (= x "Y"))
						(and (vl-cmdf "pedit" ss "C" "")(princ ">>>�ѱպ�!"))
					)
				)
				;���Ĳ�����ģ���ϲ�
				(ss
					(if (vl-cmdf "pedit" "m" ss "" "J" "J" EAB pefuzzylen "")
						(princ ">>>�ϲ����")
					)
				)
				(t nil)
			)
			;�س�����в����趨���������
			(if (= n 0)
				(progn (av:setpefuzzylen)(setq n (1+ n)))
				(setq n nil)
			)
		)
		(setq ss nil)
	)
	(*error* nil)
)
;��ʾ��Ϣ1
(defun av:petips1()
	(if (null pefuzzylen)(setq pefuzzylen 0))
	(if (null EAB)(setq EAB "B"))
	(strcat
		"<�ϲ�����="
		(cond ((= EAB "E") "����") ((= EAB "A") "���") ((= EAB "B") "���߶�"))
		",ģ������=" (rtos pefuzzylen) ">"
	)
)
;�趨����ߺϲ�ģ������
(defun av:setpefuzzylen(/ dyn k l os str1 str2)
	(setq str1 (strcat "\n������ģ������[1/5/10/50/100/500/�ϲ�����(J)]" (av:petips1) ":"))
	(initget "J")
	(setq os (getvar "osmode"))(setvar "osmode" 1)
	(setq dyn (getvar "dynmode"))(setvar "dynmode" 2)
	(setq l (getdist str1))
	(setvar "osmode" os)
	(setvar "dynmode" dyn)
	(cond
		((or (null l)(= l pefuzzylen)) nil)
		((= "J" l)
			(initget "E A B ")
			(setq str2 (strcat "\n����ϲ�����[����(E)/���(A)/���߶�(B)]:<Ĭ��" EAB ">"))
			(setq k (getkword str2))
			(if k (setq EAB k))
		)
		(t (setq pefuzzylen l))
	)
)

;;===================================================================================

;�������߱պ�
(defun c:closepline(/ *error* n ss ssn tt)
	(defun *error*(msg)
		(vla-endundomark *doc*)
		(setvar "cmdecho" 1)
		(princ)
	)
	;��ʼ����
  (setvar "cmdecho" 0)
	(setq ss (entsel "\n��ָ�������պϵĶ���(�س���ѡ)"))
	;(and ss (av:chkopen (entget (car ss))));��ǿ���λ��
	(vla-startundomark *doc*);���ƻ�Ԥ��ѡ��
	(setq tt t)
	;���ȵ�ѡ����
	(if ss
		(while tt
			(if ss nil 
				(if (setq ss (entsel "\n��ָ����պϵĵ�������")) nil (setq tt nil))
			)
			(and ss (command-s "pedit" ss "C" ""))
			(setq ss nil)
		)
	)
	;δ�е�ѡ����¿�ʼ��ѡ����
	(if tt
		(and
			(princ "��ѡ����պϵĶ���")
			(setq ss (ssget '((0 . "arc,*polyline"))))
			(av:chkopen ss)
			(setq n 0)
			(repeat (sslength ss)
				(setq ssn (ssname ss n))
				(command-s "pedit" ssn "c" "")
				(setq n (1+ n))
			)
		)
	)
	(*error* nil)
)

;��ǲ��������Ŀ���λ��
(defun c:chkopen (/ f olst ss)
	(setvar "cmdecho" 1)
  (setq	f '((0 . "*line,arc,ellipse")
						 (-4 . "<NOT")
						 (0 . "XLINE")
						 (-4 . "NOT>")
					 )
  )
  (or (setq ss (ssget f)) (fsxm-silenceexit))
  (setq olst (itoa (length (av:chkopen ss))))
  (princ (strcat ">>>>���ι��ҵ����ڵ�<" olst ">��....."))
	(setvar "cmdecho" 0)
  (princ)
)
;��������ǲ���տ���λ��
(defun av:chkopen (ss / ep notopen olst sp)
  (defun notopen (pt en / ss)
    (if	(setq ss (ssget "C" pt pt f))
      (ssdel en ss)
    )
    (and ss (ssname ss 0))
  )
  (foreach en (av:ss->ssnlst ss)
    (setq sp (trans (vlax-curve-getStartPoint en) 0 1))
    (setq ep (trans (vlax-curve-getEndPoint en) 0 1))
    (cond ((not (equal sp ep 1e-8))
						(or (notopen sp en) (setq olst (cons sp olst)))
						(or (notopen ep en) (setq olst (cons ep olst)))
					)
    )
  )
  (mapcar '(lambda (a) (fsxm-pt-grdraw a 1)) olst)
  olst
)

;;;;;;���²���ΪѰ�ҿ���֧�ֺ�������
;(defun fsxm-silenceexit (/ *error*) (t (setq  *error* strcat)))
;(defun fsxm-ss->enlist (ss  / lst n en)  
;	(setq n -1)  
;	(while (and (setq en (ssname ss (setq n (1+ n))))) 
;		(setq  lst (cons en lst))
;	)  
;)
;(defun fsxm-pt-grdraw (upt col / pts sz)  
;	(setq sz (fsxm-pickboxsize)) 
;	(setq pts (fsxm-9ptbox (mapcar '- upt (list sz sz )) (mapcar '+ upt (list sz sz )))) 
;	(grdraw (fsxm-nths '(0 0 ) pts ) (fsxm-nths '(2 2 ) pts ) col ) 
;	(grdraw (fsxm-nths '(2 0 ) pts ) (fsxm-nths '(0 2 ) pts ) col ) 
;	(grdraw (fsxm-nths '(1 0 ) pts ) (fsxm-nths '(1 2 ) pts ) col ) 
;	(grdraw (fsxm-nths '(2 1 ) pts ) (fsxm-nths '(0 1 ) pts ) col )  
;)
;(defun fsxm-pickboxsize() 
;	(* (/ (getvar "pickbox") (car(cdr(getvar "screensize")))) (getvar "viewsize"))
;)
;(defun fsxm-9ptbox (p1 p2  / x x1 x2 y y1 y2)  
;	(setq x1 (car p1)) 
;	(setq y1 (car (cdr p1))) 
;	(setq x2 (car p2)) 
;	(setq y2 (car (cdr p2))) 
;	(setq x (* 0.5 (+ x2 x1))) 
;	(setq y (* 0.5 (+ y2 y1))) 
;	(mapcar '(lambda (a) (mapcar 'list (list x1 x x2) (list a a a))) (list y1 y y2))  
;)
;(defun fsxm-nths ( item lst) 
;	(or (listp item) (setq item (list item))) 
;	(foreach index item (setq lst (nth index lst)))  
;)

;;===================================================================================



(princ)

