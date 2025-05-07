;;本程序由舟·海の船格式化……2018年10月20日
;;=============================================
;;生成边界轮廓线:fs_outline
;;生成填充边界线:hatchb
;;生成多段线边框:mPolyOutline
;;===============================================================================
;;将有宽度的多段线变线框
(defun C:mPolyOutline (/ d i s)
	(princ "\n=>>将有宽度的多段线变线框")
	(if (setq s (ssget "_:L" '((0 . "LWPOLYLINE")(-4 . "<NOT")(-4 . "<>")(42 . 0.0)(-4 . "NOT>"))))
		(progn
			(initget "Yes No")
			(setq d (/= "No" (getkword "\n是否删除原多段线 [是(Y)/否(N)] <Yes>: ")))
			(repeat (setq i (sslength s))
				(LM:PolyOutline (setq e (ssname s (setq i (1- i)))))
				(if d (entdel e))
			);end repeat
		);end progn
	);end if
	(princ)
);end defun
(defun LM:PolyOutline (ent / _vertices lst)
	(defun _vertices (e)
		(if (setq e (member (assoc 10 e) e))
			(cons
				(list
					(cdr (assoc 10 e))
					(cdr (assoc 40 e))
					(cdr (assoc 41 e))
				);end list
				(_vertices (cdr e))
			);end cons
		);end if
	);end defun
	(setq
		ent (entget ent)
		lst (_vertices ent)
		lst (apply 'mapcar
					(cons
						(function
							(lambda (a b)
								(
									(lambda (c)
										(mapcar
											(function
												(lambda (d)
													(mapcar
														(function
															(lambda (e f)
																(mapcar 'd (car e)
																	(mapcar
																		(function
																			(lambda (g)(* g (/ f 2.0)))
																		);end function
																		c
																	);end mapcar
																);end mapcar
															);end lambda
														);end function
														(list a b)(cdr a)
													);end mapcar
												);end lambda
											);end function
											(list + -)
										);end mapcar
									);end lambda
									(
										(lambda (v / n)
											(setq v (list (- (cadr v))(car v) 0.0)
												n (distance '(0. 0.) v)
											);end setq
											(if (equal 0.0 n 1e-14)
												(list 0.0 0.0 0.0)
												(mapcar '/ v (list n n n))
											);end if
										);end lambda
										(mapcar '- (car a)(car b))
									);end 
								);end 
							);end lambda
						);end function
						(if (= 1 (logand 1 (cdr (assoc 70 ent))))
							(list
								(cons (last lst) lst)
								(append lst (list (car lst)))
							);end list
							(list lst (cdr lst))
						);end if
					);end cons
				);end apply
		lst (
					(lambda (a)
						(if (zerop (logand 1 (cdr (assoc 70 ent))))
							(append
								(list (mapcar 'car (car lst)))
								a
								(list (mapcar 'cadr (last lst)))
							);end append
							a
						);end if
					);end lambda
					(apply 'append
						(mapcar
							(function
								(lambda (a b / c)
									(if
										(setq c
											(apply 'append
												(mapcar
													(function
														(lambda (d e / f)
															(if (setq f (inters (car d)(cadr d)(car e)(cadr e) nil))
																(list f)
															);end if
														);end lambda
													);end function
													a b
												);end mapcar
											);end apply
										);end setq
										(list c)
									);end if
								);end lambda
							);end function
							lst (cdr lst)
						);end mapcar
					);end apply
				);end 
	);end setq
	(mapcar
		(function
			(lambda (a)
				(entmakex
					(append
						(subst (cons 43 0.0)(assoc 43 ent)
							(subst (cons 70 (logior 1 (cdr (assoc 70 ent))))(assoc 70 ent)
								(subst (cons 90 (length a))(assoc 90 ent)
									(reverse (member (assoc 39 ent)(reverse ent)))
								);end subst
							);end subst
						);end subst
						(mapcar '(lambda (p)(cons 10 p)) a)(list (assoc 210 ent))
					);end append
				);end entmakex
			);end lambda
		);end function
		(
			(lambda (a b)
				(if (zerop (logand 1 (cdr (assoc 70 ent))))
					(list
						(append
							(if (equal (car a)(last b) 1e-8)
								(setq a (cdr a))
								a
							);end if
							(if (equal (car b)(last a) 1e-8)
								(setq b (cdr b))
								b
							);end if
						);end append
					);end list
					(list a b)
				);end if
			);end lambda
			(mapcar 'car lst)(reverse (mapcar 'cadr lst))
		);end 
	);end mapcar
);end defun

;;===============================================================================
;;生成边界轮廓线
(defun C:fs_outline (/ a b box h h/2 lst pt pt1 pt2 ss *error*)
	(princ "==>>>绘制出一批线条的边界轮廓")
	(setq *error* strcat)
	(setq ss (ssget))
	(setq box (av:getboxpt ss))
	(setq h (fsxm-pickboxsize))
	(setq h/2 (/ h 2))
	(setq pt1 (mapcar '+ (cadr box)(list h h)))
	(setq pt2 (mapcar '- (car box)(list h h)))
	(setq pt (mapcar '+ (cadr box)(list h/2 h/2)))
	(setq pt (trans pt 0 1))
	;;start
	(setvar "cmdecho" 0)
	(defun *error* (msg)
		(vla-endundomark *doc*)
		(command ".undo" "1")
		(princ msg)
		(princ)
	);end defun
	(vla-startundomark *doc*)
	;;画边框
	(entmake
		(list
			'(0 . "LWPOLYLINE")
			'(100 . "AcDbEntity")
			'(100 . "AcDbPolyline")
			'(90 . 4)
			'(70 . 1)
			'(60 . 1)
			(cons 10 pt1)
			(list 10 (car pt2)(cadr pt1))
			(cons 10 pt2)
			(list 10 (car pt1)(cadr pt2))
		);end list
	);end entmake
	(ssadd (setq box (entlast)) ss)
	;;边界轮廓
	(command ".boundary" "a" "b" "n" ss "" "i" "y" "o" "r" "" "non" pt "")
	(while (/= 0 (getvar "cmdactive"))(command ""))
	(setq lst (fsxm-newenlist box))
	(setq ss (ssadd))
	(foreach en lst (ssadd en ss))
	(setq	lst (mapcar '(lambda (a)
											 (list (vla-get-Area (vlax-ename->vla-object a)) a)
										 );end lambda
							lst
						);end mapcar
		lst (vl-sort lst '(lambda (a b)(> (car a)(car b))))
	);end setq	lst
	;;删除边框面域
	(entdel (cadar lst))
	(if (> (sslength ss) 2)
		(command ".union" ss "")
	);end if
	(command ".explode" ss "")
	(setq ss (fsxm-newselection box))
	(sssetfirst nil ss)
	(if (setq ss (ssget ":S" '((0 . "line,arc,circle"))))
		(command ".pedit" "m" ss "" "y" "j" 1e-6 "")
	);end if
	(setq ss (fsxm-newselection box))
	(command ".change" ss "" "p" "c" 1 "")
	;;删除边框
	(entdel box)
	(vla-startundomark *doc*)
	(sssetfirst nil ss)
	(princ)
);end defun




;(defun fsxm-obj-box(obj / minp maxp)
;	(vla-getboundingbox obj 'minp 'maxp)
;	(mapcar 'vlax-safearray->list (list minp maxp))
;)
;(defun fsxm-ss->enlist( ss / lst n en)
;	(setq n -1)
;	(while (and (setq en (ssname ss (setq n (1+ n)))))
;		(setq lst (cons en lst))
;	)
;)
;(defun fsxm-ss-box(ss / boxlst maxlst minlst objlst)
;	(setq objlst (mapcar 'vlax-ename->vla-object (fsxm-ss->enlist ss)))
;	(setq boxlst (mapcar 'fsxm-obj-box objlst))
;	(setq minlst (mapcar 'car boxlst))
;	(setq maxlst (mapcar 'cadr boxlst))
;	(list
;		(apply 'mapcar (cons 'min minlst))
;		(apply 'mapcar (cons 'max maxlst))
;	)
;)
;(defun fsxm-pickboxsize()
;	(*
;		(/ (getvar "pickbox") (car (cdr (getvar "screensize"))))
;		(getvar "viewsize")
;	)
;)
;(defun fsxm-newenlist(en / lst n ss)
;	(if en 
;		(progn (while (and (setq en (entnext en)))(setq lst (cons en lst))))
;		(progn (setq lst (fsxm-ss->enlist (ssget "x"))))
;	)
;)
;(defun fsxm-newselection(en / ss)
;	(cond
;		(en (setq ss (ssadd))(foreach a (fsxm-newenlist en)(ssadd a ss)) ss)
;		(t (ssget "x"))
;	)
;)
;
;