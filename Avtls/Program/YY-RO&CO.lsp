;;;==================={ ��ת���� }================================;;;
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
	(princ "-->������ת����")
	(err)
	(setq pick NIL)
	(if (and
				(setq ss (ssget))
				(princ (strcat "��ѡ�� " (itoa (sslength ss)) " ��ͼԪ"))
				(setq 0pt (getpoint "\nָ����ת���� : "))
				(princ "ok ��������")
				(entmake (list '(0 . "line") (cons 10 0pt) (cons 11 (polar 0pt 0 1)) (cons 62 256)))
				(setq tmpline (entlast))
			)
		(progn
			(command "rotate" ss "" "non" 0pt 0);��һ���Ŀ������Ĭ����ת�ǹ�0
			(setq loop T  num 0)
			(while loop
				(command "ROTATE" "P" tmpline "" "non" 0pt "c");����ro'cģʽ����������ȡ�ע���ʱtmpline������ת����ת�������ĸ�����
				(princ "\nָ����ת�Ƕ� : ")
				(command PAUSE)
				(setq pick T)
				(entdel tmpline)
				(setq tmpline (entlast))
				(setq p11 (cdr (assoc 11 (entget tmpline))))
				(setq tmpjiao (angle 0pt p11));tmpline������ʵ�ʽǶ�
				(if (equal tmpjiao 0 0.000001)
					(progn
						(command "erase" ss tmpline "")
						(princ (strcat "\n�Ҽ��˳�������� " (itoa num) " ����ת����"))
						(setq loop NIL)
					)
					(progn
						(entdel (entlast))                                        ;;ɾ��tmpline����---��Ϊ���Ѿ���ת����
						(setq num (1+ num))
						(princ (strcat "�� " (itoa num) " �θ���"))
						(entmake (list '(0 . "line") (cons 10 0pt) (cons 11 (polar 0pt 0 1)) (cons 62 256)))
						(setq tmpline (entlast))
						(command "rotate" ss "" "non" 0pt 0)                ;;��һ���Ŀ������Ĭ����ת�ǹ�0
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

