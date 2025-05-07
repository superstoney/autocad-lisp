;�жϲ�����˫���˵�
(defun c:2viewset (/ key m1 msg)
	(av:reset2view nil nil);ˢ����˫�����͸������
	;��������ж��Ƿ����˫���˵�������Ӧ����ʾ��Ϣ
	(cond
		((menugroup "fstl") (if (menugroup "2view") (command "menuunload" "2View")))
		((null (menugroup "2view")) (av:load-menu-2view))
	)
	(if (menugroup "2view") (setq m1 "/��ж��U"))
	(cond (xjymultiviewN) (T (setq xjymultiviewN 1)))
	;��ʼ�����趨������ִ��
	(setq xjymultiviewN (1+ xjymultiviewN))
	(setq msg (strcat "\n�������µ���Ļ����[2/3/4" (if m1 m1 "") "]:<��ǰ" (itoa xjymultiviewN) "��>"))
	(if (menugroup "2view") (initget "U "));Ϊж��˫���˵���׼��
	(setq key (getint msg))
	(cond
		((null key))
		((= key "U")(command "menuunload" "2View"))
		((<= key 2)(setq xjymultiviewN 2))
		((= key 3)(setq xjymultiviewN 3))
		((>= key 4)(setq xjymultiviewN 4))
	)
	(princ (strcat "\n��ʾ����Ļ��������Ϊ" (itoa xjymultiviewN) "�����밴F1���л���"))
	(setq xjymultiviewN (1- xjymultiviewN));�ָ�����
	(princ)
)
;����ִ�е�˫�����͸���������������˫��ʹ��
(defun av:2View()
	;��������ж��Ƿ����˫���˵�������Ӧ����ʾ��Ϣ
	(if (null (or (menugroup "fstl") (menugroup "2View")))
		(av:load-menu-2view)
	)
	;�ж��Ƿ����˫������
	(cond
		((= (getvar "tilemode") 1) (av:2viewcmd))
		(t (princ ">>>�ڲ����в�˫����"))
	)
	(princ)
)
;��������ж��Ƿ����˫���˵�
(defun av:load-menu-2view(/ dcls fn mnu_name)
	(setq 
		mnu_name (vl-filename-mktemp nil "" ".mnu")
		fn (OPEN mnu_name "w")
	)
	(setq dcls (list
							 "***MENUGROUP=2View"
							 "***ACCELERATORS"
							 "[\"F1\"]'2view"
							 "[SHIFT+\"F1\"]^C^C2viewset"
						 ))
	(foreach dcl dcls (write-line dcl fn))
	(close fn)
	(command "menuload" mnu_name)
	(vl-file-delete mnu_name)
	(princ "\n��ʾ��F1���ѿ�ʵ��˫���л���SHIFT+F1�ɽ��в������á�")
)
;˫�����Ĵ��룬������Դ��"������"
(defun av:2viewcmd(/ h h/2 i midpt pix pt pta ptb sc w w/2 x0 x1 x2 xjymodiarray y0 y1 y2)
	;����ǰ��Ļ�������괫��pt
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
	)
	(setq pt (list x1 y1 x2 y2))
	;;;============���´���Ϊ��������=======================================
	;������ŵ���Ļ�������ֵ,������xjymultiviewcount�����ֵ
	(setq i 0)
	(if (= xjymultiviewN nil)(setq xjymultiviewN 1));�趨��Ļ����ΪN+1
	(if (= xjymultiviewPT nil)
		(while (< i (* 4 xjymultiviewN))
			(setq xjymultiviewPT (cons pt xjymultiviewPT))
			(setq i (+ 1 i))
		)
	)
	(if (= xjymultiviewcount nil) (setq xjymultiviewcount 0))
	;������е�nλ���ݸ�Ϊnew�ĺ���
	(defun xjymodiarray (lst n new / it lst2 le)
		(setq 
			it (nth n lst) 
			lst2 (reverse lst) 
			le (- (length lst) n 1)
		)
		(while (/= (length (setq lst2 (cdr (member it lst2)))) n))
		(while (/= (length (setq lst (cdr (member it lst)))) le))
		(append (reverse lst2) (list new) lst)
	)
	;���浱ǰ��Ļ
	(setq xjymultiviewPT (xjymodiarray xjymultiviewPT xjymultiviewcount pt))
	;�������ۼ�
	(if (< xjymultiviewcount xjymultiviewN)
		(setq xjymultiviewcount (1+ xjymultiviewcount))
		(setq xjymultiviewcount 0)
	)
	;��ʾ��һ��Ļ
	(setq pt (nth xjymultiviewcount xjymultiviewPT))
	(princ (strcat ">>>��" (itoa (1+ xjymultiviewcount)) "/" (itoa (1+ xjymultiviewN)) "��"))
	;;;============���ϴ���Ϊ��������=======================================
	;���ؾ���λ��
	(setq ptA (list (nth 0 pt) (nth 1 pt)))
	(setq ptB (list (nth 2 pt) (nth 3 pt)))
	(vla-zoomwindow *acad* (vlax-3d-point ptA) (vlax-3d-point ptB))
)
;====================================================

;�ж�˫���˵��ļ�����ж��
(cond
	((menugroup "fstl")
		(if (menugroup "2view") (command "menuunload" "2View"))
	)
	((null (menugroup "2View"))
		(av:load-menu-2view)
	)
)
;=======================================================
;�ĵ����Ӧ��������͸����������
(defun av:reset2view (a b)
	(and (getvar "ribbonstate") (vlax-remove-cmd "2view"))
	(vlax-add-cmd "2view" 'av:2view "2View" 5)
)
;�ĵ��״μ���ʱˢ��
(av:reset2view nil nil)
;�ļ�����ˢ�·�Ӧ��
(or *DocumentReturnActivated2views* 
	(setq *DocumentReturnActivated2views* (vlr-docmanager-reactor nil '((:vlr-documentToBeActivated . av:reset2view))))
)



(princ)