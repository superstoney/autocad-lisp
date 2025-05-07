;;��̬���
;�����򣬽��б�߱�ע
(defun c:bgbz (/ bg boolean code dx dy ent1 ent2 ent3 ent4 entln1 entln2 entln3 entln4 entstr enttext hfh htxt len36 level_0 motion nscale pt pt1 pt2 pt3 pt4 pt5 pt6 ptbase spc36 spctxt wbg wstxt x x0 y0)
	(princ "\n*** ȫ�ֱ��� nscale level_0 ***")
	(princ "\n*** Ĭ��ͼ��һ����ͼ��λ����1mm������ʵ��ͼֽ�������밴��ʾ�����޸ģ� ***")
	(princ "\n*** ��Ա�ע��С��ʽ�����⣬���޸�74�����²�����htxt���ָ߶�; spctxt��������ߵľ���; hfh�����η��ŵĴ�С; len36�ɵ����߳���; spc36�ɵ�����λ�� ***")
	(fun_begin)
	(if (/= (getvar "userr2") 0)
		(setq nscale (getvar "userr2"))
		(setq nscale 1.0)
	);end if
	(setq
		htxt 450
		wstxt 0.7
		len36 100
		spc36 150
		spctxt 50
		hfh 250
		level_0 (getvar "userr1")
		ptbase (list 0 0 0)
	);end setq
	(while (/= (setq ptbase (get_bgpoint)) nil)
		(cond
			((and (= (type ptbase) 'STR) (= (strcase ptbase nil) "F")) (set_level_0));end cond item
			((and (= (type ptbase) 'STR) (= (strcase ptbase nil) "S")) (set_nscale));end cond item
			((= (type ptbase) 'LIST)
				(setq
					bg (rtos (/ (- (cadr ptbase) level_0) (* nscale 1000)) 2 3)
					wbg (* (strlen bg) htxt wstxt)
				);end setq
				(setq
					x0 (car ptbase)
					y0 (cadr ptbase)
					pt1 (list (+ x0 hfh) y0 0)
					pt2 (list (+ x0 (* 2 hfh)) y0 0)
					pt3 (list x0 (+ y0 hfh) 0)
					pt4 (list (+ x0 (* 2 hfh)) (+ y0 hfh) 0)
					pt5 (list (+ x0 (+ len36 wbg)) (+ y0 hfh) 0)
					pt6 (list (+ x0 spc36 (* 0.5 wbg)) (+ y0 (+ hfh spctxt (* 0.5 htxt))) 0)
				);end setq
				
				(setq entln1 (list (cons 0 "LINE") (cons 10 ptbase) (cons 11 pt2) (cons 62 256) (cons 8 "BGBZ")))
				(entmake entln1)
				(setq ent1 (entget (entlast)))
				(setq entln2 (list (cons 0 "LINE") (cons 10 pt3) (cons 11 pt1) (cons 62 256) (cons 8 "BGBZ")))
				(entmake entln2)
				(setq ent2 (entget (entlast)))
				(setq entln3 (list (cons 0 "LINE") (cons 10 pt1) (cons 11 pt4) (cons 62 256) (cons 8 "BGBZ")))
				(entmake entln3)
				(setq ent3 (entget (entlast)))
				(setq entln4 (list (cons 0 "LINE") (cons 10 pt3) (cons 11 pt5) (cons 62 256) (cons 8 "BGBZ")))
				(entmake entln4)
				(setq ent4 (entget (entlast)))
				(setq entstr (list (cons 0 "TEXT") (cons 1 bg) (cons 7 (getvar "textstyle")) (cons 10 pt6) (cons 11 pt6) (cons 72 4) (cons 73 0) (cons 62 256) (cons 40 htxt) (cons 41 wstxt) (cons 50 0) (cons 8 "BGBZ")))
				(entmake entstr)
				(setq enttext (entget (entlast)))
				
				(setq boolean t)
				(while boolean
					(setq motion (grread t 15 0))
					(setq code (car motion))
					(cond
						((= code 5)
							(setq
								pt (cadr motion)
								x (car pt);x0;
							);end setq
							(if (>= (car pt) (car ptbase))
								(setq dx 1.0)
								(setq dx -1.0)
							);end if
							(if (>= (cadr pt) (cadr ptbase))
								(setq dy 1.0)
								(setq dy -1.0)
							);end if
							(setq
								pt1 (list (+ x (* dx hfh)) y0 0)
								pt2 (list (+ x (* dx hfh 2)) y0 0)
								pt3 (list x (+ y0 (* dy hfh)) 0)
								pt4 (list (+ x (* dx hfh 2)) (+ y0 (* dy hfh)) 0)
								pt5 (list (+ x (* dx (+ len36 wbg))) (+ y0 (* dy hfh)) 0)
								pt6 (list (+ x (* dx (+ spc36 (* 0.5 wbg)))) (+ y0 (* dy (+ hfh spctxt (* 0.5 htxt)))) 0)
							);end setq
							(setq ent1 (subst (cons 11 pt2) (assoc 11 ent1) ent1))
							(entmod ent1)
							(setq ent2 (subst (cons 10 pt3) (assoc 10 ent2) ent2))
							(setq ent2 (subst (cons 11 pt1) (assoc 11 ent2) ent2))
							(entmod ent2)
							(setq ent3 (subst (cons 10 pt1) (assoc 10 ent3) ent3))
							(setq ent3 (subst (cons 11 pt4) (assoc 11 ent3) ent3))
							(entmod ent3)
							(setq ent4 (subst (cons 10 pt3) (assoc 10 ent4) ent4))
							(setq ent4 (subst (cons 11 pt5) (assoc 11 ent4) ent4))
							(entmod ent4)
							(setq enttext (subst (cons 11 pt6) (assoc 11 enttext) enttext))
							(entmod enttext)
							(entupd (entlast))
						);end "("
						((= code 3)
							(setq boolean nil)
						);end "("
						((= code 11)
							(entdel (cdr (assoc -1 ent1)))
							(entdel (cdr (assoc -1 ent2)))
							(entdel (cdr (assoc -1 ent3)))
							(entdel (cdr (assoc -1 ent4)))
							(entdel (cdr (assoc -1 enttext)))
							(setq boolean nil)
						);end "("
					);end cond
				);end while
			);end "(";end cond item
		);end cond
	);end while
	(setvar "userr1" level_0)
	(setvar "userr2" nscale)
	(fun_end)
);end defun

(defun fun_begin (/ clayer_old cmdecho_old osmode_old)
	(setq
		osmode_old (getvar "osmode")
		cmdecho_old (getvar "cmdecho")
		clayer_old (getvar "clayer")
	);end setq
	(setvar "cmdecho" 0)
	(command "undo" "be")
);end defun
(defun fun_end ()
	(setvar "osmode" osmode_old)
	(setvar "cmdecho" cmdecho_old)
	(setvar "clayer" clayer_old)
	(command "undo" "e")
);end defun
;���û�ͼ���� nscale
(defun set_nscale (/ nscale)
	(if (/= (getvar "userr2") 0)
		(setq nscale (getint (strcat "\n��ǰһ��ͼ�ε�λ���� " (rtos (getvar "userr2") 2 0) " mm, ��������ֵ<1mm>:")))
		(setq nscale (getint "\n������һ��ͼ�ε�λ�����ʵ�ʳ���(mm)<1mm>:"))
	);end if
	(if (= nscale nil) (setq nscale 1.0))
	(setvar "userr2" nscale)
);end defun
;���ñ����� level_0
(defun set_level_0 (/ level_1 pt_0 pt_1 ssbg_1)
	(initget "R")
	(setq
		pt_0 (getpoint (strcat "\n������������[��֪��ߵ�(R)]��<�ϴα�����" (rtos (/ (getvar "userr1") 1000) 2 3) ">"))
	);end setq
	(cond
		((= pt_0 nil) (setq level_0 (getvar "userr1")))
		((or (= pt_0 "R") (= pt_0 "r"))
			(progn
				(setq
					pt_1 (getpoint "\n��������֪�ı�ߵ㣺")
					level_1 (getreal "\n���������֪��ߵ�ı��(m):<ѡ����>")
				);end setq
				(while (= level_1 nil)
					(setq
						ssbg_1 (ssget (cadr (entsel "\n��ѡ�������֣�")) '((0 . "TEXT")))
						level_1 (atof (cdr (assoc 1 (entget (ssname ssbg_1 0)))))
					);end setq
				);end while
				(setq level_1 (* level_1 nscale))
				(if (/= pt_1 nil)
					(setq level_0 (- (cadr pt_1) (* level_1 1000)))
				);end if
			);end progn
		);end "("
		(t (setq level_0 (cadr pt_0)))
	);end cond
	(setvar "userr1" level_0)
);end defun
;��ȡ��ߵ�
(defun get_bgpoint (/ promptstr ptbase)
	(initget "S F")
	(setq
		promptstr (strcat "\n��ǰ������(F):" (rtos (/ level_0 1000) 2 3) "m, ��ǰ��ͼ����(S):" (rtos nscale 2 0) "mm, ��������λ��:")
		ptbase (getpoint promptstr) 	
	);end setq
);end defun
