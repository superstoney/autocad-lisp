;;�趨CAD����״̬
(defun c:apiwinset(/ api-winsettrans ctrlwinpos i str)
	(princ "-->�����ö���͸��������")
	(defun api-WinSetTrans(n / gwl_exstyle ws_ex_layered)
		(setq GWL_EXSTYLE (_run-dll "user32.dll" "GetWindowLongA" *cadhwnd* -20))
		(_run-dll "user32.dll" "SetWindowLongA" *cadhwnd* -20 (boole 7  GWL_EXSTYLE 524288))
		(_run-dll "user32.dll" "SetLayeredWindowAttributes"  *cadhwnd* 0 n 2)
	)
	(defun ctrlwinpos(n) ; n=-1,�����ö���n=-2,ȡ���ö�
		(_run-dll "user32.dll" "SetWindowPos"  *cadhwnd* n 0 0 0 0 3)
	)
	(setvar "CMDECHO" 0)
	(initget "T C M B H")
	(setq str (strcat "\n[�����ö�(T)/ȡ���ö�(C)/͸��(M)/��͸(B)/�ָ�(H)]���봰��͸����1-255:<�ָ�>"))
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
;;[��ѡ]����1�����⣨Ĭ��Ϊ�򿪣�
;;[��ѡ]����2����ʼĿ¼��Ĭ��Ϊ��һ��Ŀ¼��
;;[��ѡ]����3��ȱʡ�ļ�����Ĭ��Ϊ�գ�
;;[��ѡ]����4�����������ļ���չ������Ĭ��Ϊ*.*
;;ʾ����4��������Ϊ��ѡ��Ҳ��ʹ��nil
;;(try-getfiles nil nil nil "*.dwg;*.lsp;*.fas")
;;ʹ�ó�����ͼ�κϲ���PDF�ļ�����
(defun try-getfiles (title defaultdir DefaultFile ext / a b buffer dir filename filter get-addr hwnd is32 lst m n ret runret split struct)
	;;�����Ϸ��Լ��
	(and title (/= (type title) 'STR) (princ"\ntry-getfiles��1����������Ϊ�ַ���"))
	(and defaultdir (/= (type defaultdir) 'STR) (princ"\ntry-getfiles��2����������Ϊ�ַ���"))
	(and DefaultFile (/= (type DefaultFile) 'STR) (princ"\ntry-getfiles��3����������Ϊ�ַ���"))
	(and ext (/= (type ext) 'STR) (princ"\ntry-getfiles��4����������Ϊ�ַ���"))
	(or ext (setq ext "*.*"))
	(and (= ext "") (setq ext "*.*"))
	;;�����ļ���ʽ
	(setq Split (try-StringSplit ext ";"))
	(if (= 1 (length Split))
		(setq filter (strcat "files (" ext ")" (chr 256) ext (chr 256) (chr 256)))
		(setq 
			a (apply 'append(mapcar 'list Split Split));����
			b (try-strcat a (chr 256))
			filter (strcat "files (" ext ")" (chr 256) ext (chr 256) b (chr 256)(chr 256))
		)
	)
	;;�����ļ���
  (setq buffer (make-string 4096 0))
	(if DefaultFile (setq buffer (strcat DefaultFile buffer)))
	;;�����ھ��
	(setq n (vla-get-hwnd *acad*))
	(repeat 4 (setq m (rem n 256) n (/ n 256) hwnd (cons m hwnd)))
	(setq hwnd (reverse hwnd))
	;;��ȡһ���ַ����ĵ�ַ(ָ��)
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
	;;�����ṹ
	(setq struct
		(vl-list->string
			(cond
				((setq is32 (= 32 *bit*))
					(append 
						'(76 0 0 0)  ;lStructSize��DWORD����4�ֽ�
						hwnd;hwndOwner��HWND����4�ֽ�
						'(0 0 0 0) ;hInstance��HINSTANCE����4�ֽ�
						(get-addr filter) ;lpstrFilter��LPCSTR����4�ֽڣ�ָ�룩
						'(0 0 0 0   ;lpstrCustomFilter��LPSTR����4�ֽڣ�ָ�룩
							 0 0 0 0  ;nMaxCustFilter��DWORD����4�ֽ�
							 0 0 0 0)   ;nFilterIndex��DWORD����4�ֽ�
						(get-addr buffer);lpstrFile��LPSTR����4�ֽڣ�ָ�룩
						'(0 0 1 0 )  ;nMaxFile��DWORD����4�ֽ�
						'(0 0 0 0 ) ;lpstrFileTitle��LPSTR����4�ֽڣ�ָ�룩
						'(0 0 0 0) ;nMaxFileTitle��DWORD����4�ֽ�
						(get-addr defaultdir);lpstrInitialDir��LPCSTR������ָ�룩��ʼĿ¼�����(NULL)�գ������һ��Ŀ¼
						(get-addr title);lpstrTitle��LPCSTR������ָ�룩�Ի���������ַ���. �����(NULL)��, ϵͳ��ʹ��Ĭ�ϱ���
						'(0 18 8 0 ;Flags��DWORD����4�ֽ�
							 0 0 ;nFileOffset��WORD����2�ֽ�
							 0 0  ;nFileExtension��WORD����2�ֽ�
							 0 0 0 0 ;lpstrDefExt��LPCSTR����4�ֽڣ�ָ�룩
							 0 0 0 0 ;lCustData��LPARAM����4�ֽ�
							 0 0 0 0 ;lpfnHook��LPOFNHOOKPROC����4�ֽڣ�����ָ�룩
							 0 0 0 0;lpTemplateName��LPCSTR����4�ֽڣ�ָ�룩
						 )
					)
				)
				(T
					(append 
						'(152 0 0 0 0 0 0 0)   ;lStructSize��DWORD����4�ֽ�
						hwnd '(0 0 0 0);hwndOwner��HWND����4�ֽ�8
						'(0 0 0 0 0 0 0 0) ;hInstance��HINSTANCE����4�ֽ�8
						(get-addr filter) ;lpstrFilter��LPCSTR����4�ֽڣ�ָ�룩8
						'(0 0 0 0 0 0 0 0   ;lpstrCustomFilter��LPSTR����4�ֽڣ�ָ�룩8
							 0 0 0 0 ;nMaxCustFilter��DWORD����4�ֽ�
							 0 0 0 0  ;nFilterIndex��DWORD����4�ֽ� ָ����ǰѡ���ļ����͹��˿��Ƶ�����.
						 )(get-addr buffer) ;lpstrFile��LPSTR����4�ֽڣ�ָ�룩8
						'(0 0 1 0  0 0 0 0   ;nMaxFile��DWORD����4�ֽ�
							 0 0 0 0 0 0 0 0  ;lpstrFileTitle��LPSTR����4�ֽڣ�ָ�룩8
							 0 0 0 0 0 0 0 0 );nMaxFileTitle��DWORD����4�ֽ�
						(get-addr defaultdir);lpstrInitialDir��LPCSTR������ָ�룩��ʼĿ¼�����(NULL)�գ������һ��Ŀ¼
						(get-addr title);lpstrTitle��LPCSTR������ָ�룩�Ի���������ַ���. �����(NULL)��, ϵͳ��ʹ��Ĭ�ϱ���.���Ϊ���
						'(0 2 8 0 ;Flags��DWORD����4�ֽ�
							 0 0 ;nFileOffset��WORD����2�ֽ�
							 0 0  ;nFileExtension��WORD����2�ֽ�,ָ�� lpstrFile �ַ���ȷ����·����ʼ���ļ���չ���� 0 �� TCHAR ƫ��.���� ANSI �汾,�������ֽ���; Unicode �汾,�������ַ���.
							 0 0 0 0 0 0 0 0 ;lpstrDefExt��LPCSTR����4�ֽڣ�ָ�룩8ָ��һ��������,���а���Ĭ�ϵ���չ��.
							 0 0 0 0 0 0 0 0 ;lCustData��LPARAM����4�ֽ�8
							 0 0 0 0 0 0 0 0 ;lpfnHook��LPOFNHOOKPROC����4�ֽڣ�����ָ�룩8
							 0 0 0 0 0 0 0 0 ;lpTemplateName��LPCSTR����4�ֽڣ�ָ�룩8ָ��һ���ս������ַ���,������һ���� HINSTANCE ��Աȷ��ģ����Դ�����ĶԻ���ģ��.
							 0 0 0 0 0 0 0 0;pvReserved
							 0 0 0 0 ;dwReserved
							 0 0 0 0 ;FlagsEx
						 )
					)
				)
			)
		)
	)
	;;����DLL
	(setq runret (_run-dll "Comdlg32.dll" "GetOpenFileNameA" struct))
	;;�ļ���ѡ�������
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