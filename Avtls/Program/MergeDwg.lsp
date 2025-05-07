
;图纸合并
(defun c:MergeDwg(/ col dx dy getfn i ilst lst lst1 lst2 lst3 num obj pt0 pt11 ptl str ylst)
	(princ "-->单张图纸合并")
	;;函数位于隐藏函数中
	(setq lst (try-getfiles "请选择DWG文件" (last (av:getexpdirlst)) nil "*.dwg"))
  ;;取得定位点
	(while (progn
					 (or col (setq col 5))
					 (setq str (strcat "\n当前选择" (itoa (length lst)) "个文件,按" (itoa col) "个/行排列[设置(S)],请指定起始点:"))
					 (initget (+ 1 2 4) "S")
					 (and lst (setq pt0 (getpoint str)))
					 (if (and (equal 'STR (type pt0)) (wcmatch "S" pt0))
						 (setq col (getint "请输入单行数量:"))
					 )
				 )
	)
	(setvar "nomutt" 1)
	;;换行序列
	(repeat (1- (setq i (1+ (fix (/ (length lst) col))))) (setq ilst (cons (* col (setq i (1- i))) ilst)))
	;;开始插入
	(setq num 0 pt11 pt0)
	(foreach l lst
		(cond ;插入并判断错误
			((vl-catch-all-error-p
				 (setq obj (vl-catch-all-apply 'vla-InsertBlock (list *ms* (vlax-3D-point pt0) l 1 1 1 0)))
			 )
				;(princ "\n三维导致插入出错：")
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
				;(princ "\n位置移动有误：")
				(setq lst2 (cons l lst2))
			)
		)
		(cond
			(ptl
				(setq ptl (mapcar '(lambda (x) (mapcar '* (list 1 1 0) x)) ptl)) ;z轴归零
			)
			((member l lst1))
			(t
				;(princ "\n外部参照导致插入出错：")
				(setq lst3 (cons l lst3))
			)
		)
		(cond
			;((apply 'or (mapcar 'vl-catch-all-error-p (list obj ptl)))
			;	(setq erlst (cons l erlst))
			;)
			((member l (append lst1 lst2 lst3)))
			(t ;分解并提示
				(vla-move obj (vlax-3D-point (car ptl)) (vlax-3D-point pt11))
				(vla-Explode obj)
				(vla-Delete obj)
				(setq num (1+ num))
				(princ (strcat "\n第" (itoa num) "张：" l "\n"))
				;;确认下一图的插入原点
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
	;;返回结果
	(setvar "CMDECHO" 0)
	(setvar "nomutt" 0)
	(if (> num 0)
		(progn
			(command "DELAY" 100)
			;(while (= 1 (getvar "cmdactive")) (command pause))
			(princ (strcat "\n>>>操作结束，共合并" (itoa num) "张图。"))
		)
	)
	;;返回导入失败信息
	(defun getfn(filenamelst)
		(mapcar '(lambda(x) (strcat (car x) (cadr x)))
			(mapcar 'cdr (mapcar 'fnsplitl filenamelst))
		)
	)
	(if lst2
		(progn
			(princ (strcat "\n另有" (itoa (length lst2)) "张图由于锁定或字型等原因位置有误:\n"))
			(foreach l (getfn lst2) (princ l) (terpri))
		)
	)
	(if lst1
		(progn
			(princ (strcat "\n另有" (itoa (length lst1)) "张图由于三维等原因导入失败:\n"))
			(foreach l (getfn lst1) (princ l) (terpri))
		)
	)
	(if lst3
		(progn
			(princ (strcat "\n另有" (itoa (length lst3)) "张图由于外部参照等原因导入失败:\n"))
			(foreach l (getfn lst3) (princ l) (terpri))
		)
	)
	(setvar "CMDECHO" 1)
	(if (or lst1 lst2 lst3) (textscr))
	(princ)
)




;;导入PDF
;;需要两个支持性文件:Countpdfpages.dll,itextsharp.dll
;;不要试图支持文件二进制，会导致编辑器死机
(defun c:pdfimports(/ i l maxpt minpt num obj pdf pg1 pg2 pgl1 pgl2 pt str wide)
	(setvar "CMDECHO" 0)
	(cond
		((< *ver4* 2014) (princ "CAD版本太低，不支持该功能。"))
		((progn
			 (if (null Countpdfpages) (command-s "netload" (av:findfile "Countpdfpages.dll")))
			 (null Countpdfpages)
		 )
			(princ "支持文件加载失败")
		)
		;;函数位于隐藏函数中
		((setq pdf (try-getfiles "请选择单个PDF文件" (last (av:getexpdirlst)) nil "*.pdf"))
			(setq pdf (last pdf));多选时按最后一个
			(setq pgl2 (list))
			(cond ;选择全部页码
				((wcmatch "" (setq str (getstring "\n请键入页码范围(例如:1,3,5-12)<默认全部>:")))
					(setq num (Countpdfpages pdf))
					(repeat num
						(setq pgl2 (cons num pgl2))
						(setq num (1- num))
					)
				)
				(t ;手工选择页码
					(setq pgl1 (split-string str (list "," "，" ";" "/" " ")))
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
			;开始插入
			(setq pt (getpoint "\n指定插入点："))
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
		(t (princ "未有选择PDF"))
	)
	(setvar "CMDECHO" 1)
	(princ)
)



(princ)


