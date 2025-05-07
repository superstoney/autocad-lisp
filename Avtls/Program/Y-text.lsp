;导出文本
(defun c:av-dcwb(/ ent f fn fuzz lst ss txt y1 y2)
	(princ "\n请框选单行或多行文字")
	(cond
		((setq ss (ssget '((0 . "text,mtext"))))
			;取得图名列表
			(setq lst
				(vl-remove-if-not
					(function (lambda (x) (= (type x) 'ENAME)))
					(mapcar 'cadr (ssnamex ss))
				)
			)
			;进行排序
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
			;新建文本
			(setq fn (vl-filename-mktemp nil nil ".txt"))
      (setq f (open fn "w"))
			;写入文字
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
			;打开文本
      (startapp "notepad.exe" fn)
			;删除文本
			(and
				(vl-cmdf "delay" 1000)
				(vl-file-delete fn)
			)
		)
		(t (princ "没有文本导出"))
	)
	(princ)
)
;连接文字
(defun c:nt2ot(/ ca1 ca2 data str str1 str2 t1 t2)
	(while (setq t1 (entsel "\n选择字首"))
		(vla-startundomark *doc*)
		(setq ca1 (car t1))
		(while (setq t2 (entsel " >>>选择合并项"))
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
