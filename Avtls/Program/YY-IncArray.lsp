(defun c:yy-incarray(/ *error* ss)
	(defun *error*(msg)
		(redraw)
		(vla-endundomark *doc*)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(vla-startundomark *doc*)
	(princ "\n动态阵列递增复制<回车切换为连续复制>")
	(cond
		((setq ss (ssget "_:l" '((0 . "~viewport"))))
			(u:incarray ss t)
		)
		(t (u:coseries ss))
	)
	(*error* nil)
)
;动态连续复制
(defun c:coseries(/ ss)
	(setq ss (ssget "_:l" '((0 . "~viewport"))))
	(u:coseries ss)
)
;;======================================= 
(defun u:coseries (ss / d en p0 p1 r ss ssnext)
	(defun ssnext (en / ss)
		(setq ss (ssadd))
		(while (setq en (entnext en))
			(ssadd en ss)
		)
	)
	(princ "\n提示：已切换至连续复制")
	(setq ss (ssget))
	(if SS
		(if (setq p0 (getpoint "\n指定基点:"))
			(progn 
				(while t 
					(princ "\n指定下一点或距离:")
					(if d (princ(strcat "<" (rtos d) ">:")))
					(setq en (entlast))
					(command ".copy" ss "" p0 pause)
					(command ".erase" (ssnext en) "")
					(setq p1 (getvar "lastpoint"))
					(if (equal p0 p1) 
						(setq p1 (polar p0 r d)) 
						(setq d (distance p0 p1) r (angle p0 p1))
					)
					(command ".copy" ss "" p0 p1)
					(setq ss (ssnext en) p0 p1)
				)
			)
		)
		(princ "无选择已退出！")
	)
)
;阵列复制，(U:IncArray ss nil)静态，（U:IncArray ss T）动态
(defun u:IncArray(ss dyn / _copyvector _increment _splitstring _ss->lst dx gr ls nl nx ob p0 p1 pd pw px vx)
	(defun _SplitString (str / _isString _isNumber lst)
		;; Original by Gile, modified by Lee Mac
		(defun _isString (x lst / tmp)
			(cond
				((null lst) (list x)
				);end "("
				((< 47 (car lst) 58)
					(cons x (_isNumber (chr (car lst)) (cdr lst)))
				);end "("
				((= 45 (car lst))
					(if
						(and (cadr lst)
							(numberp (read (setq tmp (strcat "-" (chr (cadr lst))))))
						);end and
						(cons x (_isNumber tmp (cddr lst)))
						(_isString (strcat x (chr (car lst))) (cdr lst))
					);end if
				);end "("
				((_isString (strcat x (chr (car lst))) (cdr lst)))
			);end cond
		);end defun
		(defun _isNumber (x lst / tmp)
			(cond
				((null lst) (list x)
				);end "("
				((= 46 (car lst))
					(if
						(and (cadr lst)
							(numberp (read (setq tmp (strcat x "." (chr (cadr lst))))))
						);end and
						(_isNumber tmp (cddr lst))
						(cons x (_isString (chr (car lst)) (cdr lst)))
					);end if
				);end "("
				((< 47 (car lst) 58)
					(_isNumber (strcat x (chr (car lst))) (cdr lst))
				);end "("
				((cons x (_isString (chr (car lst)) (cdr lst))))
			);end cond
		);end defun
		(if (setq lst (vl-string->list str))
			(
				(if
					(or
						(and (= 45 (car lst)) (< 47 (cadr lst) 58))
						(< 47 (car lst) 58)
					);end or
					_isNumber _isString
				);end if
				(chr (car lst)) (cdr lst)
			);end 
		);end if
	);end defun
	(defun _increment (str inc / num prc)
		(cond
			((eq (type (read str)) 'INT)
				(setq num (itoa (+ (atoi str) inc)))
				(repeat (- (strlen str) (strlen num))
					(setq num (strcat "0" num))
				);end repeat
				num
			);end "("
			((eq (type (read str)) 'REAL)
				(setq prc (- (strlen str) (vl-string-position 46 str) 1)
					num (rtos (+ (atof str) inc) 2 prc)
				);end setq
				(repeat (- (vl-string-position 46 str) (vl-string-position 46 num))
					(setq num (strcat "0" num))
				);end repeat
				(repeat (- prc (- (strlen num) (vl-string-position 46 num) 1))
					(setq num (strcat num "0" ))
				);end repeat
				num
			);end "("
			(str)
		);end cond
	);end defun
	(defun _ss->lst (ss / i lst obj)
		(if ss
			(repeat (setq i (sslength ss))
				(setq lst
					(cons
						(cons
							(setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
							(cond
								((wcmatch (vla-get-objectname obj) "AcDb*Text,AcDbMLeader" )
									(list
										(cons 'textstring (_SplitString (vla-get-TextString obj)))
									);end list
								);end "("
								((wcmatch (vla-get-objectname obj) "AcDb*Dimension" )
									(list
										(cons 'textoverride (_SplitString (vla-get-textoverride obj)))
									);end list
								);end "("
								((eq "AcDbAttributeDefinition" (vla-get-objectname obj))
									(list
										(cons 'tagstring (_SplitString (vla-get-TagString obj)))
										(cons 'promptstring (_SplitString (vla-get-promptstring obj)))
										(cons 'textstring (_SplitString (vla-get-TextString obj)))
									);end list
								);end "("
								((and
									 (eq "AcDbBlockReference" (vla-get-objectname obj))
									 (eq :vlax-true (vla-get-hasattributes obj))
								 );end and
									(mapcar
										(function
											(lambda (a)
												(cons 'textstring (_SplitString (vla-get-textstring a)))
											);end lambda
										);end function
										(vlax-invoke obj 'getattributes)
									);end mapcar
								);end "("
							);end cond
						);end cons
						lst
					);end cons
				);end setq
			);end repeat
		);end if
	);end defun
	(defun _CopyVector (objs vec n / i base lst) (setq i 1 base (vlax-3D-point '(0.0 0.0 0.0)))
		(repeat n
			(foreach obj objs
				(vla-move (car (setq lst (cons (vla-copy (car obj)) lst))) base
					(vlax-3D-point (mapcar '* vec (list i i i)))
				);end vla-move
				(if
					(and
						(eq "AcDbBlockReference" (vla-get-objectname (car obj)))
						(eq :vlax-true (vla-get-hasattributes (car obj)))
					);end and
					(mapcar
						(function
							(lambda (a b)
								(vl-catch-all-apply 'vlax-put-property
									(list a (car b)
										(apply 'strcat
											(mapcar (function (lambda (c) (_increment c i))) (cdr b))
										);end apply
									);end list
								);end vl-catch-all-apply
							);end lambda
						);end function
						(vlax-invoke (car lst) 'getattributes)
						(cdr obj)
					);end mapcar
					(foreach prop (cdr obj)
						(vlax-put-property (car lst) (car prop)
							(apply 'strcat
								(mapcar (function (lambda (a) (_increment a i))) (cdr prop))
							);end apply
						);end vlax-put-property
					);end foreach
				);end if
			);end foreach
			(setq i (1+ i))
		);end repeat
		lst
	);end defun
	(if
		(and
			;(setq ss (ssget "_:L" '((0 . "~VIEWPORT" ))))
			(setq ls (_ss->lst ss))
			(setq p0 (getpoint "\n指定基点: " ))
			(setq px (getpoint "\n指定方向与距离: " p0))
			(setq pw (trans p0 1 0)
				pd (trans p0 1 3)
				vx (trans (mapcar '- px p0) 1 0 t)
				dx (distance '(0. 0. 0.) vx)
			);end setq
			(not (equal dx 0.0 1e-14))
		);end and
		(cond
			(dyn
				(princ "\n指定终点: " )
				(while (= 5 (car (setq gr (grread 't 13 0)))) (redraw)
					(setq ob (car (mapcar 'vla-delete ob))
						nx (fix (setq nl (/ (caddr (trans (mapcar '- (cadr gr) p0) 1 vx t)) dx)))
						ob (_copyvector ls (mapcar (if (minusp nx) '- '+) vx) (abs nx))
					);end setq
					(grvecs (list -3 '(0. 0. 0.) (mapcar '* (trans vx 0 3) (list nl nl nl)))
						(list
							(list 1. 0. 0. (car pd))
							(list 0. 1. 0. (cadr pd))
							(list 0. 0. 1. (caddr pd))
							(list 0. 0. 0. 1.)
						);end list
					);end grvecs
				);end while
				(redraw)
			);end dyn
			((setq p1 (getpoint p0 "\n指定终点: " ))
				(setq nx (fix (/ (caddr (trans (mapcar '- p1 p0) 1 vx t)) dx)))
				(_copyvector ls (mapcar (if (minusp nx) '- '+) vx) (abs nx))
			);end "("
		);end cond
	);end if
	(princ)
);end defun

(princ)
