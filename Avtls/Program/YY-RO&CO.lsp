;;;==================={ 旋转复制 }================================;;;
(defun c:YY-RO&CO(/ 0pt err loop num p11 pick ss tmpjiao tmpline no_err)
	(defun err(/ *error* olderr oldosmode)
		(setvar "cmdecho" 0)
		(command "undo" "g")
		(setq oldosmode (getvar "osmode"))
		(setq olderr *error* )
		(defun *error*(msg)
			(princ)
			(setvar "cmdecho" 0)
			(if (and tmpline (entget tmpline)) (command "erase" tmpline ""))
			(if pick (command "erase" ss ""))
			(command "undo" "e")
			(setvar "osmode" oldosmode)
			(setq *error* olderr)
			(princ)
		)
		(defun no_err(/ *error*)
			(setvar "cmdecho" 0)
			(setq *error* olderr)
			(command "undo" "e")
			(princ)
		)
		(princ)
	)
	(princ "-->多重旋转复制")
	(err)
	(setq pick NIL)
	(if (and
				(setq ss (ssget))
				(princ (strcat "已选择 " (itoa (sslength ss)) " 个图元"))
				(setq 0pt (getpoint "\n指定旋转基点 : "))
				(princ "ok 即将复制")
				(entmake (list '(0 . "line") (cons 10 0pt) (cons 11 (polar 0pt 0 1)) (cons 62 256)))
				(setq tmpline (entlast))
			)
		(progn
			(command "rotate" ss "" "non" 0pt 0);这一句的目的是让默认旋转角归0
			(setq loop T  num 0)
			(while loop
				(command "ROTATE" "P" tmpline "" "non" 0pt "c");改用ro'c模式，提高流畅度。注意此时tmpline不再旋转，旋转的是它的副本。
				(princ "\n指定旋转角度 : ")
				(command PAUSE)
				(setq pick T)
				(entdel tmpline)
				(setq tmpline (entlast))
				(setq p11 (cdr (assoc 11 (entget tmpline))))
				(setq tmpjiao (angle 0pt p11));tmpline副本的实际角度
				(if (equal tmpjiao 0 0.000001)
					(progn
						(command "erase" ss tmpline "")
						(princ (strcat "\n右键退出，共完成 " (itoa num) " 次旋转复制"))
						(setq loop NIL)
					)
					(progn
						(entdel (entlast))                                        ;;删除tmpline副本---因为它已经旋转过了
						(setq num (1+ num))
						(princ (strcat "第 " (itoa num) " 次复制"))
						(entmake (list '(0 . "line") (cons 10 0pt) (cons 11 (polar 0pt 0 1)) (cons 62 256)))
						(setq tmpline (entlast))
						(command "rotate" ss "" "non" 0pt 0)                ;;这一句的目的是让默认旋转角归0
						(setq pick NIL)
					)
				)
			)
		)
	)
	(no_err)
	(princ)
)


(princ)

