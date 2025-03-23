
(setq source_text "") ; 设源文字为全局变量
(defun c:fa (/ en en_dxf en1 en1_dxf ent entype entype_source i ob pt source_text2 ss ss_data str txtst)
  (defun *error* (x) ;出错函数
		(if en (redraw en 4))
		(setvar "ErrNo" 0)
		(setvar "cmdecho" 1)
  )
  (setvar "cmdecho" 0)
  (setvar "ErrNo" 0)
  (if (= source_text "")
		(setq str "\n请选择源文字(右键退出):")
		(setq str (strcat "\n请选择源文字: 默认:" source_text))
	)
	(if (and (not (setq en (car (nentsel str)))) (= (getvar "ErrNo") 52))
		(progn
			(setvar "ErrNo" 0)
			(if (= source_text "")
				(setq txtst nil)
				(setq txtst T)
			)
		)
		(if en
			(progn
				(setq en_dxf (entget en))
				(setq entype_source (cdr (assoc 0 en_dxf)))
				(if (or (setq txtst (cdr (assoc 7 en_dxf)))(= entype_source "TCH_DRAWINGNAME")(= entype_source "MULTILEADER"))
					(progn
						(redraw en 3)
						(setq source_text (cdr (assoc 1 en_dxf)))
						(setq source_text2 "")
						(cond
							((= entype_source "ATTDEF") ;如果是属性字，则取“标记”为源文字
								(setq source_text (cdr (assoc 2 en_dxf)))
							)
							((= entype_source "TCH_MULTILEADER") ;天正引出标注
								(setq source_text2 (cdr (assoc 2 en_dxf)))
							)
							((= entype_source "TCH_ARROW") ;天正箭头引注
								(setq source_text2 (vlax-get-property (vlax-ename->vla-object en) 'Text2))
							)
							((= entype_source "MULTILEADER") ;CAD多重引注
								(setq source_text (cdr (assoc 304 en_dxf)))
							)
						)
					)
				)
			)
			(setvar "ErrNo" 52)
		)
	)
  (if (or txtst (= entype_source "TCH_DRAWINGNAME")(= entype_source "MULTILEADER"))
		(progn
			(prompt "\n请选择要修改内容的文字:")
			(while (/= (getvar "ErrNo") 52)
				(prompt (strcat "\n文字内容将被刷成:" source_text))
				(if (and (setq ss (ssget ":S" '((0 . "*TEXT,TCH_DRAWINGNAME,TCH_ELEVATION,INSERT,ATTDEF,ATTRIB,TCH_MULTILEADER,TCH_ARROW,MULTILEADER")))) source_text)
					(progn
						(if (= (caar (setq ss_data (ssnamex ss 0))) 1)
							(progn  ;点选时
								(setq ent (ssname ss 0)
									pt (trans (CADr (last (car ss_data))) 0 1)
									en1 (car (nentselp pt))
									en1_dxf (entget en1)
									entype (cdr (assoc 0 en1_dxf))
									ob (vlax-ename->vla-object en1)
								)
								(wenzishua entype entype_source ob source_text en1 ent source_text2)
							)
							(progn  ;框选时
								(setq i 0)
								(repeat (sslength ss)
									(setq en1 (ssname ss i)
										en1_dxf (entget en1)
										entype (cdr (assoc 0 en1_dxf))
										ob (vlax-ename->vla-object en1)
									)
									(wenzishua entype entype_source ob source_text en1 en1 source_text2)
									(setq i (1+ i))
								)
							)
						)
					)
				)
			);end while
		)
	)
  (if en (redraw en 4))
  (setvar "ErrNo" 0)
  (setvar "cmdecho" 1)
  (princ)
)

;文字刷子程序
(defun wenzishua (entype entype_source ob source_text en1 ent source_text2)
	;去掉多行文字无用格式符号
	(if (= entype_source "MTEXT")
		(setq source_text (mtext2text source_text))
	)	
  (cond
		;CAD多行文字
		((= entype "MTEXT")
			(vla-put-TextString ob source_text)
			(entupd en1)
			(entupd ent)
		)
		;CAD单行文字、多重引注
		((or (= entype "TEXT") (= entype "MULTILEADER")) 
			(vla-put-TextString ob source_text)
			(entupd en1)
			(entupd ent)
		)
		;天正文字、标高
		((or (= entype "TCH_TEXT")(= entype "TCH_ELEVATION"))
			(vlax-put-property ob 'Text source_text)
			(entupd en1)
			(entupd ent)
		)   
		;天正图名
		((= entype "TCH_DRAWINGNAME")
			(progn
				(vlax-put-property ob 'NameText source_text)
				(entupd en1)
				(entupd ent)
			)
		)
		;属性文字 只改"标记"
		((= entype "ATTDEF")
			(vla-put-TagString ob source_text);改标记
			(entupd en1)
			(entupd ent)
		)
		;块中属性文字 只改"默认"
		((= entype "ATTRIB")
			(vla-put-TextString ob source_text);改默认
			(entupd en1)
			(entupd ent)
		)
		;天正引出标注
		((= entype "TCH_MULTILEADER")
			(progn
				(vlax-put-property ob 'UpText source_text)   ;上标
				(vlax-put-property ob 'DownText source_text2);下标
				(entupd en1)
				(entupd ent)
			)
		)
		;天正箭头引注
		((= entype "TCH_ARROW")
			(vlax-put-property ob 'Text source_text)  ;上标    
			(vlax-put-property ob 'Text2 source_text2);下标
			(entupd en1)
			(entupd ent)
		)
	)
)

;提取多行文字,去除无用格式符号--来自明经
(defun mtext2text(MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0) ;不忽略大小写
  (vlax-put-property regex "Global" 1) ;匹配方式，全文字匹配
  (setq s MTextString)
	;替换\\字符
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
	;替换\{字符
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
	;替换\}字符
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
	;删除段落缩进格式
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除制表符格式
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除堆迭格式
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除字体、颜色、字高、字距、倾斜、字宽、对齐格式
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除下划线、删除线格式
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除不间断空格格式
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除换行符格式
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除换行符格式(针对Shift+Enter格式)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;删除{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	
	;替换回\\,\{,\}字符
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
	
  (vlax-release-object regex)
  s
)