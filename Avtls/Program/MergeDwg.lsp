
;ͼֽ�ϲ�
(defun c:MergeDwg(/ col dx dy getfn i ilst lst lst1 lst2 lst3 num obj pt0 pt11 ptl str ylst)
	(princ "-->����ͼֽ�ϲ�")
	;;����λ�����غ�����
	(setq lst (try-getfiles "��ѡ��DWG�ļ�" (last (av:getexpdirlst)) nil "*.dwg"))
  ;;ȡ�ö�λ��
	(while (progn
					 (or col (setq col 5))
					 (setq str (strcat "\n��ǰѡ��" (itoa (length lst)) "���ļ�,��" (itoa col) "��/������[����(S)],��ָ����ʼ��:"))
					 (initget (+ 1 2 4) "S")
					 (and lst (setq pt0 (getpoint str)))
					 (if (and (equal 'STR (type pt0)) (wcmatch "S" pt0))
						 (setq col (getint "�����뵥������:"))
					 )
				 )
	)
	(setvar "nomutt" 1)
	;;��������
	(repeat (1- (setq i (1+ (fix (/ (length lst) col))))) (setq ilst (cons (* col (setq i (1- i))) ilst)))
	;;��ʼ����
	(setq num 0 pt11 pt0)
	(foreach l lst
		(cond ;���벢�жϴ���
			((vl-catch-all-error-p
				 (setq obj (vl-catch-all-apply 'vla-InsertBlock (list *ms* (vlax-3D-point pt0) l 1 1 1 0)))
			 )
				;(princ "\n��ά���²������")
				(setq lst1 (cons l lst1))
			)
			((and
				 (vl-catch-all-error-p
					 (setq ptl (vl-catch-all-apply 'lm-get-blkboundingbox (list obj)))
				 )
				 (av:fontstoshx "AllInOneUni" "AllInOneBig")
				 (vl-catch-all-error-p
					 (setq ptl (vl-catch-all-apply 'lm-get-blkboundingbox (list obj)))
				 )
			 )
				;(princ "\nλ���ƶ�����")
				(setq lst2 (cons l lst2))
			)
		)
		(cond
			(ptl
				(setq ptl (mapcar '(lambda (x) (mapcar '* (list 1 1 0) x)) ptl)) ;z�����
			)
			((member l lst1))
			(t
				;(princ "\n�ⲿ���յ��²������")
				(setq lst3 (cons l lst3))
			)
		)
		(cond
			;((apply 'or (mapcar 'vl-catch-all-error-p (list obj ptl)))
			;	(setq erlst (cons l erlst))
			;)
			((member l (append lst1 lst2 lst3)))
			(t ;�ֽⲢ��ʾ
				(vla-move obj (vlax-3D-point (car ptl)) (vlax-3D-point pt11))
				(vla-Explode obj)
				(vla-Delete obj)
				(setq num (1+ num))
				(princ (strcat "\n��" (itoa num) "�ţ�" l "\n"))
				;;ȷ����һͼ�Ĳ���ԭ��
				;(setq ptl (mapcar 'vlax-safearray->list (list maxp minp)))
				(setq dx (* 1.1 (abs (apply '- (mapcar 'car ptl)))))
				(setq ylst (cons (abs (apply '- (mapcar 'cadr ptl))) ylst))
				(cond
					((member num ilst)
						(setq dy (* 1.1 (apply 'max ylst)))
						(setq pt11 (polar pt0 (* pi 0.5) dy))
						(setq pt0 pt11 ylst nil)
					)
					(t (setq pt11 (polar pt11 (* pi 0) dx)))
				)
			)
		)
	)
	;;���ؽ��
	(setvar "CMDECHO" 0)
	(setvar "nomutt" 0)
	(if (> num 0)
		(progn
			(command "DELAY" 100)
			;(while (= 1 (getvar "cmdactive")) (command pause))
			(princ (strcat "\n>>>�������������ϲ�" (itoa num) "��ͼ��"))
		)
	)
	;;���ص���ʧ����Ϣ
	(defun getfn(filenamelst)
		(mapcar '(lambda(x) (strcat (car x) (cadr x)))
			(mapcar 'cdr (mapcar 'fnsplitl filenamelst))
		)
	)
	(if lst2
		(progn
			(princ (strcat "\n����" (itoa (length lst2)) "��ͼ�������������͵�ԭ��λ������:\n"))
			(foreach l (getfn lst2) (princ l) (terpri))
		)
	)
	(if lst1
		(progn
			(princ (strcat "\n����" (itoa (length lst1)) "��ͼ������ά��ԭ����ʧ��:\n"))
			(foreach l (getfn lst1) (princ l) (terpri))
		)
	)
	(if lst3
		(progn
			(princ (strcat "\n����" (itoa (length lst3)) "��ͼ�����ⲿ���յ�ԭ����ʧ��:\n"))
			(foreach l (getfn lst3) (princ l) (terpri))
		)
	)
	(setvar "CMDECHO" 1)
	(if (or lst1 lst2 lst3) (textscr))
	(princ)
)




;;����PDF
;;��Ҫ����֧�����ļ�:Countpdfpages.dll,itextsharp.dll
;;��Ҫ��ͼ֧���ļ������ƣ��ᵼ�±༭������
(defun c:pdfimports(/ i l maxpt minpt num obj pdf pg1 pg2 pgl1 pgl2 pt str wide)
	(setvar "CMDECHO" 0)
	(cond
		((< *ver4* 2014) (princ "CAD�汾̫�ͣ���֧�ָù��ܡ�"))
		((progn
			 (if (null Countpdfpages) (command-s "netload" (av:findfile "Countpdfpages.dll")))
			 (null Countpdfpages)
		 )
			(princ "֧���ļ�����ʧ��")
		)
		;;����λ�����غ�����
		((setq pdf (try-getfiles "��ѡ�񵥸�PDF�ļ�" (last (av:getexpdirlst)) nil "*.pdf"))
			(setq pdf (last pdf));��ѡʱ�����һ��
			(setq pgl2 (list))
			(cond ;ѡ��ȫ��ҳ��
				((wcmatch "" (setq str (getstring "\n�����ҳ�뷶Χ(����:1,3,5-12)<Ĭ��ȫ��>:")))
					(setq num (Countpdfpages pdf))
					(repeat num
						(setq pgl2 (cons num pgl2))
						(setq num (1- num))
					)
				)
				(t ;�ֹ�ѡ��ҳ��
					(setq pgl1 (split-string str (list "," "��" ";" "/" " ")))
					(foreach l pgl1
						(if (or (vl-string-search "-" l) (vl-string-search "~" l))
							(progn
								(setq l (split-string l (list "-" "~")))
								(setq pg1 (atoi (car l)))
								(setq pg2 (atoi (cadr l)))
								(setq i pg1)
								(repeat (1+ (- pg2 pg1))
									(setq pgl2 (cons (itoa i) pgl2))
									(setq i (1+ i))
								)
							)
							(setq pgl2 (cons l pgl2))
						)
					)
					(setq pgl2 (mapcar '(lambda(x) (atoi x)) (reverse pgl2)))
				)
			)
			;��ʼ����
			(setq pt (getpoint "\nָ������㣺"))
			(foreach pg pgl2
				(command-s "-pdfattach" pdf pg pt 1000 0)
				(setq obj (vlax-ename->vla-object (entlast)))
				(vla-getboundingbox obj 'minPt 'maxPt)
				(setq
					minPt (vlax-safearray->list minPt)
					maxPt (vlax-safearray->list maxPt)
					wide (apply '- (mapcar 'car (list maxpt minpt)))
					pt (polar pt 0 wide)
				)
			)
		)
		(t (princ "δ��ѡ��PDF"))
	)
	(setvar "CMDECHO" 1)
	(princ)
)



(princ)


