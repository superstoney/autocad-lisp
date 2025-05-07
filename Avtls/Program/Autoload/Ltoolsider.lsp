(vl-load-com)
(defun lm:str->lst (str del / len lst pos)
  (setq len (1+ (strlen del)))
  (while (and (setq pos (vl-string-search del str)))
    (setq lst (cons (substr str 1 pos) lst))
    (setq str (substr str (+ pos len)))
  )
  (reverse (cons str lst))
)
(defun lm:removenth (n l / i)
  (setq i -1)
  (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) l)
)
(defun lm:str-trimblank (strim)
  (if (/= strim (setq strim (vl-string-trim "\t" (vl-string-trim " " strim))))
    (progn (lm:str-trimblank strim))
    (progn strim)
  )
)
(defun lm:removeitem (i l)(vl-remove-if '(lambda (e) (equal e i)) l))
(defun listsep (item lstsource / n i lstfind)
  (while (and lstsource)
    (setq n (length lstsource))
    (if (and
					(member item lstsource)
					(setq i (length (setq lstsource (member item lstsource))))
				)
      (progn (setq lstfind (cons (- n i) lstfind)))
      (progn
				(setq lstfind (cons (length lstsource) lstfind))
				(setq lstsource nil)
      )
    )
    (setq lstsource (cdr lstsource))
  )
  (reverse lstfind)
)
(vl-acad-defun
  (defun c:ltools/menu#oninitialize (/ file ltls readmenutxt str)
    (defun readmenutxt (pathname / doc str lst title lstkey lstclass skey sclass)
      (if (setq doc (open pathname "r"))
				(progn
					(while (and (setq str (read-line doc)))
						(setq str (lm:str-trimblank str))
						(if (and (not (wcmatch str ";*")) (not (= str "")))
							(cond
								((wcmatch str "`[*`]")
									(if sclass
										(progn
											(setq lstclass (list sclass (reverse lstkey)))
											(setq lst (cons lstclass lst))
											(setq lstkey nil)
										)
									)
									(setq sclass (vl-string-left-trim "[" (vl-string-right-trim "]" str)))
								)
								(t (setq skey (lm:str->lst str ","))
									(setq lstkey (cons skey lstkey))
								)
							)
						)
					)
					(if sclass
						(progn
							(setq lstclass (list sclass (reverse lstkey)))
							(setq lst (cons lstclass lst))
							(setq lstkey nil)
						)
					)
				)
      )
      (close doc)
      (reverse lst)
    )
		(setq ltls "Ltoolsider.ini")
		(cond
			((setq file (av:getfile ltls))
				(if (null (vl-bb-ref "ltls"))
					(progn
						(princ "\N当前侧边栏配置文件:")
						(princ file)
					)
				)
			)
			((setq file (av:findfile ltls))
				(if (null (vl-bb-ref "ltls"))
					(progn
						(princ "\n当前配置文件")
						(princ file)
						(princ "\n复制上述文件至SET设置中自动加载文件夹，可优先加载。")
						(terpri)
					)
				)
			)
			(t (alert "未发现菜单配置文件Ltoolsider.ini,程序将无法执行。"))
		)
		(cond
			(file
				(setq *menulst* (readmenutxt file))
				(if (= nil (av:getenv "ltools-menuon"))(av:setenv "ltools-menuon" "0"))
				(setq str (strcat "(nth 1 (nth (1- " (av:getenv "ltools-menuon") ") *menulst*))"))
				(if (/= "0" (av:getenv "ltools-menuon"))
					(setq *menuon* (eval (read str)))
					(setq *menuon* nil)
				)
				(loadtab)
				(loadrectangle)
				(loadbutton)
				(tabclicked)
				(buttonclicked)
				(buttonmousemove)
			)
			(t nil)
		)
  )
)
(defun resetbackcolor (n / i str)
  (setq i 1)
  (repeat 24
    (setq str (strcat "ltools/menu/btn" (rtos i 2 0)))
    (if (/= n i)
      (progn (dcl-control-setbackcolor (eval (read str)) -16))
      (progn (dcl-control-setbackcolor (eval (read str)) 27983871)
      )
    )
    (setq i (1+ i))
  )
)
(defun loadtab (/ i n str val topnum)
  (setq i 1)
  (setq topnum 5)
  (setq n (atoi (av:getenv "ltools-menuon")))
  (repeat 15
    (setq str (strcat "ltools/menu/tab" (rtos i 2 0)))
    (if (setq val (car (nth (1- i) *menulst*)))
      (progn
				(dcl-control-setvisible (eval (read str)) t)
				(dcl-control-setcaption (eval (read str)) val)
				(dcl-control-settop (eval (read str)) topnum)
				(if (= i n)
					(progn
						(dcl-control-setpicture (eval (read str)) 101)
						(setq topnum
							(+ topnum 18
								(* 18 (length (lm:removeitem '("--") *menuon*)))
								(* 6 (length (listsep '("--") *menuon*)))
							)
						)
					)
					(progn
						(dcl-control-setpicture (eval (read str)) 100)
						(setq topnum (+ topnum 18))
					)
				)
      )
      (progn (dcl-control-setvisible (eval (read str)) nil))
    )
    (setq i (1+ i))
  )
)
(defun loadrectangle (/ i n topnum str val)
  (setq i 1)
  (setq n (atoi (av:getenv "ltools-menuon")))
  (setq topnum (+ 5 (* 18 n)))
  (repeat 11
    (setq str (strcat "ltools/menu/rectangle" (rtos i 2 0)))
    (if (and (/= n 0) (setq val (nth (1- i) (listsep '("--") *menuon*))))
      (progn
				(dcl-control-setvisible (eval (read str)) t)
				(dcl-control-settop (eval (read str)) topnum)
				(setq topnum (+ topnum 6 (* 18 val)))
      )
      (progn (dcl-control-setvisible (eval (read str)) nil))
    )
    (setq i (1+ i))
  )
)
(defun loadbutton (/ i n str val topnum)
  (setq i 1)
  (setq n (atoi (av:getenv "ltools-menuon")))
  (setq topnum (+ (* 18 n) 8))
  (repeat 24
    (setq str (strcat "ltools/menu/btn" (rtos i 2 0)))
    (if (and (/= n 0) (setq val (nth (1- i) *menuon*)))
      (progn
				(if (= "--" (nth 0 val))
					(progn
						(setq *menuon* (lm:removenth (1- i) *menuon*))
						(setq topnum (+ topnum 6))
					)
				)
				(dcl-control-setvisible (eval (read str)) t)
				(dcl-control-setcaption
					(eval (read str))
					(nth 0 (nth (1- i) *menuon*))
				)
				(dcl-control-settooltipmaintext
					(eval (read str))
					(nth 2 (nth (1- i) *menuon*))
				)
				(dcl-control-setpicture
					(eval (read str))
					(nth 3 (nth (1- i) *menuon*))
				)
				(dcl-control-settop (eval (read str)) topnum)
				(setq topnum (+ topnum 18))
      )
      (progn (dcl-control-setvisible (eval (read str)) nil))
    )
    (setq i (1+ i))
  )
)
(defun tabclicked (/ i str)
  (setq i 1)
  (repeat 15
    (setq str
			(strcat
				"(defun c:ltools/menu/tab"
				(rtos i 2 0)
				"#onclicked (/)"
				"(setq *menuon* (nth 1 (nth "
				(rtos (1- i) 2 0)
				" *menulst*)))"
				"(if (= (av:getenv \"ltools-menuon\") \""
				(rtos i 2 0)
				"\")"
				"(av:setenv \"ltools-menuon\" \"0\")"
				"(av:setenv \"ltools-menuon\" \""
				(rtos i 2 0)
				"\")"
				")"
				"(loadtab)"
				"(loadrectangle)"
				"(loadbutton)"
				")"
			)
    )
    (setq i (1+ i))
    (eval (read str))
  )
)
(defun buttonclicked (/ i str)
  (setq i 1)
  (repeat 24
    (setq str
			(strcat
				"(defun c:ltools/menu/btn"
				(rtos i 2 0)
				"#onclicked (/ str menuon ml)"
				"(setq str (strcat \"(nth 1 (nth (1- \" (av:getenv \"ltools-menuon\") \") *menulst*))\"))"
				"(setq menuon (lm:removeitem '(\"--\") (eval(read str))))"
				"(setq ml (nth 1 (nth "
				(rtos (1- i) 2 0)
				" menuon)))"
				"(princ ml)"
				"(dcl-sendstring (strcat ml \" \"))"
				")"
			)
    )
    (setq i (1+ i))
    (eval (read str))
  )
)
(defun buttonmousemove (/ i str)
  (setq i 1)
  (repeat 24
    (setq str
			(strcat
				"(defun c:ltools/menu/btn"
				(rtos i 2 0)
				"#onmousemove (flags x y /)"
				"(if (or (< x 3) (> x 77) (< y 2) (> y 16))"
				"(dcl-control-setbackcolor ltools/menu/btn"
				(rtos i 2 0)
				" -16)"
				"(if (/= 27983871 (dcl-control-getbackcolor ltools/menu/btn"
				(rtos i 2 0)
				"))"
				"(progn"
				"(resetbackcolor "
				(rtos i 2 0)
				")"
				"(dcl-control-setbackcolor ltools/menu/btn"
				(rtos i 2 0)
				" 27983871)"
				")"
				")"
				")"
				")"
			)
    )
    (setq i (1+ i))
    (eval (read str))
  )
)
;加载odcl菜单文件
(defun load-odcl-project (projname reload password alias / bytes rtype)
	(cond
		((and
			 (setq bytes (vl-get-resource projname))
			 (eq 'str (setq rtype (type bytes)))
			 (not (eq "" bytes))
		 )
			(dcl-project-import bytes password alias)
		)
		((dcl-project-load projname reload alias))
	)
)
;卸载面板
(defun c:unltools ()
	(princ ">>>卸载")
	(dcl-form-close ltools/menu)
	(vl-bb-set "ltls" 0)
	(dcl-project-unload "ltools")
	(setq *menulst* nil)
	(princ)
)

;屏幕面板开关
(defun c:ltoolsswitch(/ file)
	(cond
		(*menulst*)
		((setq file (av:findfile "Ltoolsider.odcl"))
			(load-odcl-project file nil nil nil)
		)
		(t nil)
	)
	(cond
		((dcl-form-isactive ltools/menu)
			(dcl-form-close ltools/menu)
			(vl-bb-set "ltls" 0);程序打开时的默认开关，在面板中控制
			(princ ">>>关闭")
		)
		(t
			(dcl-form-show ltools/menu)
			(vl-bb-set "ltls" 1);程序打开时的默认开关，在面板中控制
			(princ ">>>打开")
		)
	)
	(princ)
)

;开图启用屏幕菜单
(defun autoloadltoolsider()
	(if (null dcl-project-import)(autoloadodclarx))
	(cond
		((null dcl-project-import))
		((dcl-form-isactive ltools/menu)
			(c:ltools/menu#oninitialize)
		)
		(t (and
				 (= 1 (av:getenv "ltls"));在mnl中设定开关
				 (/= 0 (vl-bb-ref "ltls")) ;程序打开时的默认开关，在面板中控制
				 (c:ltoolsswitch)
			 )
		)
	)
)
(autoloadltoolsider)


(princ)
