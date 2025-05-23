;去除共线点
(defun c:fitpl(/ en ent isclose layer m1 plst0 plst1 plst2)
	;(princ "-->多线减肥\n")
	(while (setq en (car (entsel "\r请点击多段线:")))
		(setq ent (entget en))
		(cond
			((wcmatch "LWPOLYLINE" (cdr (assoc 0 ent)))
				(setq plst1 (mapcar 'cdr (vl-remove-if-not '(lambda(x) (= (car x) 10)) ent)))
				(setq m1 (length plst1))
				(while (< 0 (length plst1))
					(setq plst1 (del1linepts plst1))
					(setq plst2 (cons (car plst1) plst2))
					(setq plst1 (cdr plst1))
				)
				(setq plst2 (vl-remove nil (reverse plst2)))
				(setq ent (entmakex (append (list
																			'(0 . "LWPOLYLINE")
																			'(100 . "AcDbEntity")
																			'(100 . "AcDbPolyline")
																			(cons 8 (cdr (assoc 8 ent)))
																			(cons 90 (length plst2))
																			(cons 70 (cdr (assoc 70 ent)))
																			(cons 38 (cond ((nth 2 (car plst2))) (t 0)))
																		)
															(mapcar '(lambda (x) (cons 10 x)) plst2)
														)
									)
				)
				(sssetfirst nil (ssadd ent))
				(entdel en)
				(princ (strcat "去除" (itoa (- m1 (length plst2))) "个中间节点\n"))
				(setq plst2 nil)
			)
			(t (princ "不是多段线\n"))
		)
	)
	(sssetfirst nil nil)
	(princ)
)
;去除一个线段中间共线节点
(defun del1linepts(plst / chaji getchaji)
	(defun getchaji(p0 p1 p2);向量叉积
		(-
			(*
				(- (car p1) (car p0))
				(- (cadr p2) (cadr p0))
			)
			(*
				(- (car p2) (car p0))
				(- (cadr p1) (cadr p0))
			)
		)
	)
  (while (and
					 (> (length plst) 2)
					 (setq chaji (getchaji (car plst) (cadr plst) (caddr plst)))
					 (equal 0 chaji 0.001);等于0代表共线
				 )
    (setq plst (cons (car plst) (cddr plst)))
  )
  plst
)
