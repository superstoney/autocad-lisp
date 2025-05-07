;数据清理，单文件及批处理
(defun c:dwg-purge-batch();批处理清理
	(princ ">>>CAD数据清理助手批处理")
	(av:cmddrawpurge "dwg-purge-batch")
	(princ)
)
(defun c:dwg-purge();单文件清理
	(princ "\n>>>CAD数据助手单文件清理")
	(av:cmddrawpurge "dwg-purge")
	(princ)
)
(defun av:cmddrawpurge (cmd / av:drawpurgetips loaddrawpurge regsetdir)
	(defun av:drawpurgetips ()
		(prompt "
【功能说明:】
Drawing Purge是一款强大且宜用的cad清理辅助工具。
【可解决如下问题:】
(1)在文件间拷贝时，\"dgn线型\"会导致文件巨大且无明显数据；
(2)无法复制和粘贴图元；
(3)长时间悬挂在选择文本与\"特性\"选项板上；
(4)图形文件打开时间过长;
(5)命令操作有卡顿感。
")
	)
	;写入注册表路径信息
	(defun regsetdir (path / folderloc reg-key)
		(setq reg-key
			(strcat "HKEY_CURRENT_USER\\"(vlax-product-key) "\\Profiles\\" (getvar "cprofile") "\\AVVADrawingPurge-072")
		)
		;写入设置路径
		(if (setq folderloc (findfile (strcat path "\\DwgPurgeSettings")))
			(progn
				(setq folderloc (strcat folderloc "\\"))
				(vl-registry-write reg-key "AVVADwgPurgeFolderLoc" folderloc)
				(vl-registry-write reg-key "AVVALanguage" "Chinese")
			)
			(progn
				(setq folderloc nil)
				(princ "\n找不到设定放置文件夹！")
			)
		)
		folderloc
	)
	;加载主程序DLL文件
	(defun loaddrawpurge(/ file name path)
		(setq file (av:findfile "AVVADrawingPurge2018.dll"))
		(setq path (vl-filename-directory file))
		(if file
			(and (regsetdir path) (command-s "netload" file))
			(prompt "没有找到清理程序文件！")
		)
	)
	;运行主程序命令
	(cond
		((>= *ver4* 2018)
			(if (vl-cmdf cmd) (progn (loaddrawpurge) (av:drawpurgetips)))
		)
		(t (c:cleanrubbish))
	)
)

;=================================================================
;文件瘦身，垃圾清单
(defun c:cleanrubbish (/ av:get-file-size av:try-time-len av:try-time-start clean1 clean2 dicts>n f fn llsheng:mpurge m n rep size0 size1 size2 time)
	;清理大于特定数量的垃圾词典
	(defun dicts>n (n / mygetcount xc xdictlist xdicts xi xlst xn xname)
		(defun myGetCount (xdicts xname / errobj xitem)
			(setq 
				xitem (vla-item xdicts xname) 
				errobj (vl-catch-all-apply 'vla-get-count (list xitem))
			)
			(if (vl-catch-all-error-p errobj) 000 errobj)
		)
		(setq xdicts (vla-get-dictionaries *doc*))
		(setq xdictlist (entget (namedobjdict)))
		(setq xc (length xdictlist) )
		(setq xi 0)
		(while (< xi xc)
			(setq xlst (nth xi xdictlist))
			(setq xn (car xlst))
			(and
				(= xn 3)
				(setq xname (cdr xlst))
				(< n (myGetCount xdicts xname))
				(setq xlst (nth (1- xi) xdictlist))
				(= (car xlst) 350)
				(entdel (cdr xlst))
			)
			(setq xi (1+ xi))
		)
	)
	;清理程序
	(defun llsheng:mpurge (lst)
		(vl-every
			'(lambda (x / a b)
				 (or
					 (VL-CATCH-ALL-ERROR-P
						 (setq a 
							 (VL-CATCH-ALL-APPLY 'vlax-get-property
								 (list *doc* (car x))
							 )
						 )
					 )
					 (if (cdr x)
						 (vlax-for y a
							 (and
								 (< (vlax-get-property y'count) 1)
								 (VL-CATCH-ALL-APPLY 'vlax-invoke-method (list y 'delete))
							 )
						 )
						 (vlax-for y a (VL-CATCH-ALL-APPLY 'vlax-invoke-method (list y 'delete)))
					 )
					 t
				 )
			 )
			lst
		)
	)
	;第1种瘦身方案
	(defun clean1 (/ n)
		;(repeat 1 (command "-purge" "a" "*" "n"));此办法清理效果稍好，但有大量回显
		(vla-PurgeAll *doc*);删除垃圾的另一种办法，没有回显
		;删除空组及数量为1的组定义
		(vlax-for obj (vla-get-groups *doc*)
			(cond
				((< (vla-get-count obj)2) (vla-delete obj));删除空组及数量为1的组定义
				((wcmatch (vla-get-name obj) "'**") (vla-delete obj));删除匿名组
			)
		)
		;图形中错误修正
		;(princ "+修正错误")
		;(command "_audit" "y");更正检测到的任何错误
	)
	;第2种瘦身方案,会删除布局
	(defun clean2 (/ n)
		(llsheng:mpurge
			'((blocks);清理未引用块
				 (blocks t);清理空图块
				 (groups);打散编组
				 (groups t);清理空组
				 (layers);清理图层
				 (Linetypes);清理线型
				 (textstyles);清理文字样式
				 (dimstyles);清理标注样式
				 ;(Layouts);删除布局
			 )
		)
	)
	;查询文件大小，单位m
	(defun av:get-file-size(fn)(rtos (/ (vl-file-size fn) 1e6) 2 2))
	;开始测试时间，单位秒，结束函数为av:try-time-len
	(defun av:try-time-start()(getvar "millisecs"))
	;结束测试时间，单位秒，开始函数为av:try-time-start
	(defun av:try-time-len (t1)(rtos (/ (- (getvar "millisecs") t1) 1000.000) 2 3))
	;开始奔跑
	(vl-load-com)
	(setvar "cmdecho" 0)
	;(textscr);展示文本框
	(setq time (av:try-time-start));开始测试时间
	(setq fn (strcat (getvar "dwgprefix") (getvar "dwgname")))
	(setq f (findfile fn))
	(if f (setq size0 (av:get-file-size fn)));统计清理前大小
	;以下解锁顽固图层
	(dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
	;开始清理
	(setq rep t size1 size0 m 0)
	(while rep
		(princ (strcat "\r正在执行第" (itoa (setq m (1+ m))) "次清理..."))
		(clean1);第1种闭源清理方案
		;(clean2);第2种开源清理方案
		(and
			(setq n 100);数字大于1000的都是垃圾
			(dicts>n n);清理特定数量的词典
		)
		(cond
			(f
				(vla-Save *doc*)
				(setq size2 (av:get-file-size fn));统计清理后大小
				(if (equal size1 size2 0.05) (setq rep nil))
				(setq size1 size2)
			)
			(t (setq rep nil))
		)
	)
	(princ (strcat
					 "\n>>>清理完成，耗时" (av:try-time-len time) "秒"
					 (cond
						 (size2
							 (strcat "，之前" size0 "m，之后" size2 "m；")
						 )
						 (t "，文件尚未保存；")
					 )
				 )
	)
	(princ "命令DICTS可对其它字典进行清理。")
	(setvar "cmdecho" 1)
	(princ)
)



; 对当前图形里的常规词典进行列表
(defun c:Dicts (/ *error* dictslst i k:adddel4str k:catchapply pos tmppos tmpvar)
	(defun *error* (x) ;出错函数
		;(Graphscr);退出文本屏幕
		(vla-endundomark *DOC*) ;结束编组
		;(setvar "CMDECHO" 1)
		(princ)
	)
	;如果不成功则抓取错误，如果成功就执行程序
	(defun K:CatchApply (fun args / result)
		(setq result (vl-catch-all-apply (if (= 'SYM (type fun)) fun (function fun)) args))
		(cond
			((vl-catch-all-error-p result)
				(setq result nil)
			)
			(t result)
		)
	)
	;用特殊符号补齐字符串,强制满足位数，Flag为T时补在前面
	(defun k:adddel4str (del flag str len)
		(repeat (- len (strlen str))
			(cond
				(flag (setq str (strcat del str)))
				(t (setq str (strcat str del)))
			)
		)
		str
	)
  ;(setq *DOC* (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (vla-startundomark *DOC*) ;记录编组
	(textscr);切换到文本屏幕
	(setq i 0 TmpVar 0 DictsLst nil)
	(setq DictsLst
		(vl-remove-if 'null
			(mapcar
				'(lambda (XX / Result STR)
					 (if
						 (and
							 (eq (car XX) 3)
							 (setq Result (K:CatchApply 'vla-get-count (list (vla-item (vla-get-dictionaries *DOC*) (cdr XX)))))
							 (> Result 0)
						 )
						 (progn
							 (setq i (1+ i))
							 (if (> Result TmpVar) (setq TmpVar Result Pos i))
							 (setq STR (strcat (K:AddDel4STR "0" T (itoa i) 2) ". \"" (cdr XX) "\""))
							 (princ (strcat "\n"  (K:AddDel4STR " " Nil STR 40) " ->>  "  (itoa Result)))
							 (cdr XX)
						 )
					 )
				 )
				(entget (namedobjdict))
			)
		)
	)
	(princ (strcat "\n\n——★★★ 当前文件可删除的词典数量为<" (rtos (length DictsLst) 2 0) "> ★★★——"))
	;(setvar "CMDECHO" 0)
	(while
		(progn
			(initget (+ 2 4)) ;非零非负
			(setq TmpPos
				(cond
					((getint (strcat "\n→按上面的索引,你想删除哪个字典<" (rtos Pos 2 0)">: ")))
					((fix Pos))
				)
			)
			(cond
				((<= TmpPos (length DictsLst))
					(setq Pos TmpPos)
					(dictremove (namedobjdict) (nth (1- Pos) DictsLst));删除指定的字典
					;(dictremove (namedobjdict) "ACAD_DGNLineStyleCOMP");删除字典中的DGN线型
					;(command "_.-scalelistedit" "_R" "_Y" "_E");重置视口比例
					;(command "_.-PUrge" "_R" "*" "_N");删除注册程序
					(vla-PurgeAll *DOC*);清理全部
					;(while (> (getvar "CMDACTIVE") 0) (command PAUSE));等待前面的命令完成
					;(princ (strcat "\n——★★★ 删除字典由Kucha优化，输入命令<DICTS>可再次运行程序 ★★★——"))
					;(Graphscr);退出文本屏幕
					Nil ;退出循环
				)
				((> TmpPos (length DictsLst))
					(princ (strcat "\n——★★★ 超出列表范围,请重新输入！ ★★★——"))
					T ;继续循环
				)
			)
		)
	)
  (*error* nil)
)


(princ)
