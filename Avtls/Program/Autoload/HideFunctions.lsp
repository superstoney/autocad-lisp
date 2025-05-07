;;设定CAD窗口状态
(defun c:apiwinset(/ api-winsettrans ctrlwinpos i str)
	(princ "-->窗口置顶及透明度设置")
	(defun api-WinSetTrans(n / gwl_exstyle ws_ex_layered)
		(setq GWL_EXSTYLE (_run-dll "user32.dll" "GetWindowLongA" *cadhwnd* -20))
		(_run-dll "user32.dll" "SetWindowLongA" *cadhwnd* -20 (boole 7  GWL_EXSTYLE 524288))
		(_run-dll "user32.dll" "SetLayeredWindowAttributes"  *cadhwnd* 0 n 2)
	)
	(defun ctrlwinpos(n) ; n=-1,窗口置顶；n=-2,取消置顶
		(_run-dll "user32.dll" "SetWindowPos"  *cadhwnd* n 0 0 0 0 3)
	)
	(setvar "CMDECHO" 0)
	(initget "T C M B H")
	(setq str (strcat "\n[窗口置顶(T)/取消置顶(C)/透明(M)/半透(B)/恢复(H)]输入窗口透明度1-255:<恢复>"))
	(setq i (getint str))
	(cond
		((eq (type i) 'int)
			(setq i (cond ((> i 255) 255) ((< i 1) 1) (t i)))
			(api-WinSetTrans i)
		)
		((and i (wcmatch i "T")) (ctrlwinpos -1))
		((and i (wcmatch i "C")) (ctrlwinpos -2))
		((and i (wcmatch i "M"))
			(api-WinSetTrans 100)
		)
		((and i (wcmatch i "B"))
			(api-WinSetTrans 200)
		)
		((and i (wcmatch i "H"))
			(api-WinSetTrans 255)
		)
		(t
			(api-WinSetTrans 255)
			(ctrlwinpos -2)
		)
	)
	(setvar "CMDECHO" 1)
	(princ)
)
;;========================================================
;;[可选]参数1：标题（默认为打开）
;;[可选]参数2：起始目录（默认为上一次目录）
;;[可选]参数3：缺省文件名（默认为空）
;;[可选]参数4：过滤器（文件扩展名），默认为*.*
;;示例：4个参数均为可选，也可使用nil
;;(try-getfiles nil nil nil "*.dwg;*.lsp;*.fas")
;;使用场景：图形合并，PDF文件导入
(defun try-getfiles (title defaultdir DefaultFile ext / a b buffer dir filename filter get-addr hwnd is32 lst m n ret runret split struct)
	;;参数合法性检查
	(and title (/= (type title) 'STR) (princ"\ntry-getfiles第1个参数必须为字符串"))
	(and defaultdir (/= (type defaultdir) 'STR) (princ"\ntry-getfiles第2个参数必须为字符串"))
	(and DefaultFile (/= (type DefaultFile) 'STR) (princ"\ntry-getfiles第3个参数必须为字符串"))
	(and ext (/= (type ext) 'STR) (princ"\ntry-getfiles第4个参数必须为字符串"))
	(or ext (setq ext "*.*"))
	(and (= ext "") (setq ext "*.*"))
	;;整理文件格式
	(setq Split (try-StringSplit ext ";"))
	(if (= 1 (length Split))
		(setq filter (strcat "files (" ext ")" (chr 256) ext (chr 256) (chr 256)))
		(setq 
			a (apply 'append(mapcar 'list Split Split));两倍
			b (try-strcat a (chr 256))
			filter (strcat "files (" ext ")" (chr 256) ext (chr 256) b (chr 256)(chr 256))
		)
	)
	;;整理文件名
  (setq buffer (make-string 4096 0))
	(if DefaultFile (setq buffer (strcat DefaultFile buffer)))
	;;整理窗口句柄
	(setq n (vla-get-hwnd *acad*))
	(repeat 4 (setq m (rem n 256) n (/ n 256) hwnd (cons m hwnd)))
	(setq hwnd (reverse hwnd))
	;;获取一个字符串的地址(指针)
	(defun get-addr(str / i ret)
		(cond
			(str
				(setq ret '() i 8)
				(cond
					(is32
						(setq i 8)
						(repeat 4 
							(setq ret(cons (_byte@ str i) ret))
							(setq i(1+ i))
						)
					)
					(T
						(setq i 16)
						(repeat 8 
							(setq ret(cons (_byte@ str i) ret))
							(setq i(1+ i))
						)
					)
				)
				(reverse ret)
			)
			(t (if is32 '(0 0 0 0) '(0 0 0 0 0 0 0 0)))
		)
	)
	;;构建结构
	(setq struct
		(vl-list->string
			(cond
				((setq is32 (= 32 *bit*))
					(append 
						'(76 0 0 0)  ;lStructSize（DWORD）：4字节
						hwnd;hwndOwner（HWND）：4字节
						'(0 0 0 0) ;hInstance（HINSTANCE）：4字节
						(get-addr filter) ;lpstrFilter（LPCSTR）：4字节（指针）
						'(0 0 0 0   ;lpstrCustomFilter（LPSTR）：4字节（指针）
							 0 0 0 0  ;nMaxCustFilter（DWORD）：4字节
							 0 0 0 0)   ;nFilterIndex（DWORD）：4字节
						(get-addr buffer);lpstrFile（LPSTR）：4字节（指针）
						'(0 0 1 0 )  ;nMaxFile（DWORD）：4字节
						'(0 0 0 0 ) ;lpstrFileTitle（LPSTR）：4字节（指针）
						'(0 0 0 0) ;nMaxFileTitle（DWORD）：4字节
						(get-addr defaultdir);lpstrInitialDir（LPCSTR）：（指针）起始目录如果是(NULL)空，则打开上一次目录
						(get-addr title);lpstrTitle（LPCSTR）：（指针）对话框标题栏字符串. 如果是(NULL)空, 系统将使用默认标题
						'(0 18 8 0 ;Flags（DWORD）：4字节
							 0 0 ;nFileOffset（WORD）：2字节
							 0 0  ;nFileExtension（WORD）：2字节
							 0 0 0 0 ;lpstrDefExt（LPCSTR）：4字节（指针）
							 0 0 0 0 ;lCustData（LPARAM）：4字节
							 0 0 0 0 ;lpfnHook（LPOFNHOOKPROC）：4字节（函数指针）
							 0 0 0 0;lpTemplateName（LPCSTR）：4字节（指针）
						 )
					)
				)
				(T
					(append 
						'(152 0 0 0 0 0 0 0)   ;lStructSize（DWORD）：4字节
						hwnd '(0 0 0 0);hwndOwner（HWND）：4字节8
						'(0 0 0 0 0 0 0 0) ;hInstance（HINSTANCE）：4字节8
						(get-addr filter) ;lpstrFilter（LPCSTR）：4字节（指针）8
						'(0 0 0 0 0 0 0 0   ;lpstrCustomFilter（LPSTR）：4字节（指针）8
							 0 0 0 0 ;nMaxCustFilter（DWORD）：4字节
							 0 0 0 0  ;nFilterIndex（DWORD）：4字节 指定当前选定文件类型过滤控制的索引.
						 )(get-addr buffer) ;lpstrFile（LPSTR）：4字节（指针）8
						'(0 0 1 0  0 0 0 0   ;nMaxFile（DWORD）：4字节
							 0 0 0 0 0 0 0 0  ;lpstrFileTitle（LPSTR）：4字节（指针）8
							 0 0 0 0 0 0 0 0 );nMaxFileTitle（DWORD）：4字节
						(get-addr defaultdir);lpstrInitialDir（LPCSTR）：（指针）起始目录如果是(NULL)空，则打开上一次目录
						(get-addr title);lpstrTitle（LPCSTR）：（指针）对话框标题栏字符串. 如果是(NULL)空, 系统将使用默认标题.另存为或打开
						'(0 2 8 0 ;Flags（DWORD）：4字节
							 0 0 ;nFileOffset（WORD）：2字节
							 0 0  ;nFileExtension（WORD）：2字节,指定 lpstrFile 字符串确定的路径开始的文件扩展名的 0 基 TCHAR 偏移.对于 ANSI 版本,这里是字节数; Unicode 版本,这里是字符数.
							 0 0 0 0 0 0 0 0 ;lpstrDefExt（LPCSTR）：4字节（指针）8指向一个缓冲区,其中包含默认的扩展名.
							 0 0 0 0 0 0 0 0 ;lCustData（LPARAM）：4字节8
							 0 0 0 0 0 0 0 0 ;lpfnHook（LPOFNHOOKPROC）：4字节（函数指针）8
							 0 0 0 0 0 0 0 0 ;lpTemplateName（LPCSTR）：4字节（指针）8指向一个空结束的字符串,它命名一个由 HINSTANCE 成员确定模块资源创建的对话框模板.
							 0 0 0 0 0 0 0 0;pvReserved
							 0 0 0 0 ;dwReserved
							 0 0 0 0 ;FlagsEx
						 )
					)
				)
			)
		)
	)
	;;激活DLL
	(setq runret (_run-dll "Comdlg32.dll" "GetOpenFileNameA" struct))
	;;文件多选结果处理
	(cond
		((= 1 runret)
			(setq lst (vl-string->list buffer))
			(setq ret '() filename '())
			(while (or (/= 0 (setq a (car lst))) (/= 0 (cadr lst)))
				(setq lst (cdr lst))
				(cond
					((/= 0 a) (setq filename (cons a filename)))
					(t (setq ret (cons (vl-list->string (reverse filename)) ret) filename nil))
				)
			)
			(setq ret (cons (vl-list->string (reverse filename)) ret) filename nil)
			(setq ret (reverse ret))
			(cond
				((= 1 (length ret)) ret)
				(t
					(setq dir (car ret))
					(mapcar '(lambda(f) (strcat dir"\\"f)) (cdr ret))
				)
			)
		)
		(t nil)
	)
)
(defun try_Str2List(str / a)
	(setq str (vl-string->list str))
	(while 
		(if (< (car str) 129)
			(setq a (cons (chr (car str))a) str (cdr str))
			(setq a (cons (strcat (chr (car str)) (chr (cadr str)))a) str (cddr str))))
	(reverse a)
)
(defun try-StringSplit(str char / a b i )
	(if (= "" char) (try_Str2List str)
		(progn
			(while (setq i(vl-string-search char str))
				(setq a(substr str 1 i)
					b(cons a b)
					str(substr str (+ i (strlen char)1)))
			)
			(reverse(cons str b))
		)
	)
)
(defun try-strcat(lst str)
	(if (> (length lst) 1)
		(substr (apply 'strcat (mapcar '(lambda (x) (strcat str x)) lst))
			(+ (strlen str) 1)
		)
		(car lst)
	)
)