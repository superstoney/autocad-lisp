;�����ı�
(defun c:av-dcwb(/ ent f fn fuzz lst ss txt y1 y2)
	(princ "\n���ѡ���л��������")
	(cond
		((setq ss (ssget '((0 . "text,mtext"))))
			;ȡ��ͼ���б�
			(setq lst
				(vl-remove-if-not
					(function (lambda (x) (= (type x) 'ENAME)))
					(mapcar 'cadr (ssnamex ss))
				)
			)
			;��������
			(setq lst
				(vl-sort lst
					(function
						(lambda (e1 e2 / fuzz x1 x2 y1 y2 zb1 zb2)
							(setq zb1 (assoc 10 (entget e1)))
							(setq zb2 (assoc 10 (entget e2)))
							(setq x1 (cadr zb1) y1 (caddr zb1))
							(setq x2 (cadr zb2) y2 (caddr zb2))
							(setq fuzz (* 1.0 (cdr (assoc 40 (entget e2)))))
							(cond ((equal y1 y2 fuzz) (< x1 x2)) (t (< y2 y1)))
						)
					)
				)
			)
			;�½��ı�
			(setq fn (vl-filename-mktemp nil nil ".txt"))
      (setq f (open fn "w"))
			;д������
			(foreach l lst
				(setq ent (entget l))
				(setq fuzz (* 1.0 (cdr (assoc 40 ent))))
				(setq y2 (caddr (assoc 10 ent)))
				(setq txt (cdr (assoc 1 ent)))
				(cond
					((equal y1 y2 fuzz) (princ txt f))
					(t (princ (strcat "\n" txt) f))
				)
				(setq y1 y2)
			)
      (close f)
			;���ı�
      (startapp "notepad.exe" fn)
			;ɾ���ı�
			(and
				(vl-cmdf "delay" 1000)
				(vl-file-delete fn)
			)
		)
		(t (princ "û���ı�����"))
	)
	(princ)
)
;��������
(defun c:nt2ot(/ ca1 ca2 data str str1 str2 t1 t2)
	(while (setq t1 (entsel "\nѡ������"))
		(vla-startundomark *doc*)
		(setq ca1 (car t1))
		(while (setq t2 (entsel " >>>ѡ��ϲ���"))
			(setq ca2 (car t2))
			(setq data (entget ca1))
			(setq str1 (cdr (assoc 1 data)))
			(setq str2 (cdr (assoc 1 (entget ca2))))
			(setq str (strcat str1 str2))
			(setq data (subst (cons 1 str) (assoc 1 data) data))
			(entmod data)
			(entdel ca2)
		)
		(vla-endundomark *doc*)
	)
	(princ)
)
