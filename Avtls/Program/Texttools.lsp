
;;==============文本工具=================
;单行文本变多行文本:nt2mt
;;文本编号:TextID
;;文本统计:TextCount
;;文本加框:TextBox


;;=====================================================================
;TTB--多个单行文本合并为单个多行文本
(defun c:nt2mt (/ *error* a data en enlst entxt h lst nda pt ss txt)
	(princ "-->多个单行文本合并为单个多行文本")
	(setvar "cmdecho" 0)
	(setq *error* strcat)
	(setq ss (ssget '((0 . "*text"))))
	(setq enlst (fsxm-ss->enlist ss))
	(setq h (/ (fsxm-getendxf (car enlst) 40) 2))
	(setq	lst (mapcar '(lambda (a / box ltpt obj)
											 (setq obj (vlax-ename->vla-object a))
											 (setq box (fsxm-obj-box obj))
											 (setq ltpt (list (caar box) (cadadr box)))
											 (list ltpt a)
										 );end lambda
							enlst
						);end mapcar
	);end setq	lst
	;;sort X
	(setq lst (vl-sort lst '(lambda (a b) (< (caar a) (caar b)))))
	;;sort Y
	(setq lst (vl-sort lst '(lambda (a b) (> (- (cadar a) (cadar b)) h))))
	(setq a (car lst))
	(setq pt (car a))
	(setq data (entget (cadr a)))
	(setq	nda
		(vl-remove-if
			'(lambda (a)
				 (member (car a) '(-1 0 330 5 100 10 1 41 51 71 72 11 73))
			 );end lambda
			data
		);end vl-remove-if
	);end setq	nda
	(setq txt (fsxm-getdxf 1 data))
	(foreach b (cdr lst)
		(setq en (cadr b))
		(setq entxt (fsxm-getendxf en 1))
		(if	 (> (- (cadar a) (cadar b)) h)
			(setq txt (strcat txt "\\P" entxt))
			(setq txt (strcat txt entxt))
		);end if	
		(setq a b)
	);end foreach
	(entmake
		(vl-list* '(0 . "MTEXT")
			'(100 . "AcDbEntity")
			'(100 . "AcDbMText")
			'(41 . 0)
			'(71 . 1)
			'(72 . 5)
			(cons 1 txt)
			(cons 10 pt)
			nda
		);end vl-list*
	);end entmake
	(foreach a enlst (entdel a))
	(setvar "cmdecho" 1)
	(princ)
);end defun


;;RCDATA_16_JUSTIFYTXT
;;==============文本编号=================
(defun c:textID (/ *error* getpt str)
	(defun *error* (msg)
		(setvar "cmdecho" 1)
		(princ)
	)
	(defun getpt(/ msg)
		(initget 128 "S")
		(while
			(progn
				(setq msg (strcat "\r指定<" qz (itoa n) hz ">插入点[设置S]:"))
				(setq pt (getpoint msg))
				(cond
					((eq 'list (type pt)) nil)
					((equal "S" pt) t)
					(t (exit))
				)
			)
			(setq qz (getstring (strcat "\n请输入前缀<" qz ",默认为空>:")))
			(setq hz (getstring (strcat "\n请输入后缀<" hz ",默认为空>:")))
			(setq h
				(cond
					((getint (strcat "\n请指定文字高度<默认为" (rtos h) ">:")))
					(t h)
				)
			)
			(setq zn
				(cond
					((getint (strcat "\n请输入序号增量<可为负值，默认为" (itoa zn) ">:")))
					(t zn)
				)
			)
			(setq n
				(cond
					((getint (strcat "\n请输入起始顺序号<默认为" (itoa n) ">:")))
					(t n)
				)
			)
			(initget 1 "S")
		)
		pt
	)
	(setvar "cmdecho" 0)
	(princ "-->插入编号文本\n")
	(cond (qz) (t (setq qz "")))
	(cond (hz) (t (setq hz "")))
	(cond (n) (t (setq n 1)))
	(cond (zn) (t (setq zn 1)))
	(cond (h) (t (setq h 300)))
	(while (getpt)
		(setq str (strcat qz (itoa n) hz))
		(entmakex (list
								(cons 0 "TEXT")
								(cons 1 str)
								(cons 10 pt)
								(cons 40 h)
							)
		)
		(setq n (+ n zn))
	)
	(*error* nil)
)


;;==============文本加框=================
(defun c:TextBox (/ *error* *off* ent enx lst mxv off text-box)
	(defun *error* (msg)
		(setvar "CMDECHO" 1)
		(princ)
	);end defun
	(defun mxv (m v)
		(mapcar '(lambda (r) (apply '+ (mapcar '* r v))) m)
	);end defun
	(defun text-box (enx off / b h j l m n o p r w)
		(if
			(setq l
				(cond
					((= "TEXT" (cdr (assoc 0 enx)))
						(setq b (cdr (assoc 10 enx))
							r (cdr (assoc 50 enx))
							l (textbox enx)
						);end setq
						(list
							(list (- (caar l) off) (- (cadar l) off))
							(list (+ (caadr l) off) (- (cadar l) off))
							(list (+ (caadr l) off) (+ (cadadr l) off))
							(list (- (caar l) off) (+ (cadadr l) off))
						);end list
					);end "("
					((= "MTEXT" (cdr (assoc 0 enx)))
						(setq n (cdr (assoc 210 enx))
							b (trans (cdr (assoc 10 enx)) 0 n)
							r (angle '(0.0 0.0 0.0) (trans (cdr (assoc 11 enx)) 0 n))
							w (cdr (assoc 42 enx))
							h (cdr (assoc 43 enx))
							j (cdr (assoc 71 enx))
							o (list
									(cond
										((member j '(2 5 8)) (/ w -2.0))
										((member j '(3 6 9)) (- w))
										(0.0)
									);end cond
									(cond
										((member j '(1 2 3)) (- h))
										((member j '(4 5 6)) (/ h -2.0))
										(0.0)
									);end cond
								);end list
						);end setq
						(list
							(list (- (car o) off) (- (cadr o) off))
							(list (+ (car o) w off) (- (cadr o) off))
							(list (+ (car o) w off) (+ (cadr o) h off))
							(list (- (car o) off) (+ (cadr o) h off))
						);end list
					);end "("
				);end cond
			);end setq
			((lambda (m) (mapcar '(lambda (p) (mapcar '+ (mxv m p) b)) l))
				(list
					(list (cos r) (sin (- r)) 0.0)
					(list (sin r) (cos r) 0.0)
					'(0.0 0.0 1.0)
				);end list
			);end "("
		);end if
	);end defun
	(setvar "CMDECHO" 0)
	(setq
		dis (rtos (cond (*off*) ((setq *off* 0.2))) 2 2)
		str (strcat "\n指定偏移因子 <" dis ">: ")
	)
	(initget 4)
	(if (setq off (getreal str))
		(setq *off* off)
		(setq off *off*)
	);end if
	(while
		(progn
			(setvar 'errno 0)
			(setq ent (car (entsel "\n选择文字 <退出>: ")))
			(cond
				((= 7 (getvar 'errno))
					(princ "\n错过, 再试.")
				);end "("
				((= 'ename (type ent))
					(if (setq lst (text-box (setq enx (entget ent)) (* off (cdr (assoc 40 enx)))))
						(entmake
							(append
								'(
									 (000 . "LWPOLYLINE")
									 (100 . "AcDbEntity")
									 (100 . "AcDbPolyline")
									 (090 . 4)
									 (070 . 1)
								 );end 
								(list (cons 38 (caddar lst)))
								(mapcar '(lambda (x) (cons 10 x)) lst)
								(list (assoc 210 enx))
							);end append
						);end entmake
						(princ "\n无效的选择对象.")
					);end if
				);end "("
			);end cond
		);end progn
	);end while
	(*error* nil)
);end defun

(princ)
