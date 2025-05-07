(vl-load-reactors)
;����հ״�˫������
(or *blankdoubleclick*
	(setq *blankdoubleclick*
		(vlr-Mouse-Reactor nil '((:VLR-beginDoubleClick . blankdoubleclickcallback)))
	)
)
(defun blankdoubleclickcallback (reactorObject point / owner)
	(setq owner (nentselp (trans (car point) 0 1)));�ж��Ƿ��е���
	(and
		(= 1 (getvar "tilemode"))
		(null owner)
		(= 0 (getvar "cmdactive"))
		(= 1 (av:getenv "2click"))
		(c:seting) ;˫������ľ�������
	)
)
;�ĵ�������ж������ĵ��е�˫�����ܿ���״̬
(or *2clickvar*
	(setq *2clickvar* (vlr-docmanager-reactor nil '((:vlr-documentToBeActivated . 2clickvar))))
)
(defun 2clickvar(a b)
	(if (= 0 (av:getenv "2click"))(vlr-remove *blankdoubleclick*)(vlr-add *blankdoubleclick*))
)
;��Ӧ���趨cadһֱ��֤�������õĲ�׽��ģʽ
(or *autoosmode*
	(setq *autoosmode*
		(vlr-command-reactor nil
			'((:vlr-unknownCommand . av:setosmode)
				 (:vlr-commandEnded . av:setosmode)
				 (:vlr-commandCancelled . av:setosmode)
				 (:vlr-commandFailed . av:setosmode)
			 )
		)
	)
)
(defun av:setosmode (a b / osmode)
	(and ;����ע����м�¼�Ĳ�׽��Ϣ
		(= 1 (av:getenv "autoosmode"))
		(setq osmode (av:getenv "osmode"))
		(/= osmode (getvar "osmode"))
		(setvar "osmode" osmode)
	)
	(setvar "nomutt" 0)
)
;====================================================
;������Ϣ�������趨
(defun c:set()(c:seting))
(defun c:seting (/ *allss* allss dclid f fn fname gettitle ini key newdwg oldautodir recommend setfkeytoini settitle)
	;(princ "-->����������")
	;���ÿؼ���ʾֵ
	(defun gettitle(/ 2click autoosmode cursorsize dyn f len ltls mypgp n osmode right sl theui1)
		;���ù��ܼ���״̬
		(repeat (setq n 13)
			(set_tile (setq f (strcat "F" (itoa (setq n (1- n))))) (getfkey f))
		)
		;=========================================================
		;�Զ������ļ���
		(av:setautodir)
		(set_tile "path" (av:getenv "Autodir"))
		;(mode_tile "rev" 1);������ť
		;=========================================================
		;����Ҽ�ȷ�Ͽ��ƣ�0-1-2-4-8-16
		(setq right (getvar "shortcutmenu"))
		(if (= right 2)(setq right 1)(setq right 0))
		(set_tile "right" (itoa right))
		;ָ��������ƣ�0-1-2-3
		(setq dyn (getvar "dynmode"))
		(if (= dyn 0)(setq dyn 0)(setq dyn 1))
		(set_tile "dyn" (itoa dyn))
		;ȫ�����ò�׽�趨
		(setq autoosmode (av:getenv "autoosmode"))
		(if (null autoosmode)(setq autoosmode 0))
		(set_tile "autoosmode" (itoa autoosmode))
		;����׽����
		(setq osmode (getvar "osmode"))
		(set_tile "osmode" (itoa osmode))
		;���ߴ����
		(setq cursorsize (getvar "cursorsize"))
		(set_tile "cursorsize" (itoa cursorsize))
		;=========================================================
		;��ݼ�����
		(setq mypgp (av:getenv "mypgp"))
		(if (null mypgp)(setq mypgp 0))
		(set_tile "mypgp" (itoa mypgp))
		;˫������
		(setq 2click (av:getenv "2click"))
		(if (null 2click)(setq 2click 0))
		(set_tile "2click" (itoa 2click))
		;��ͼ�Ƿ�������Ļ�˵�
		(setq ltls (av:getenv "ltls"))
		(if (= 1 ltls)(setq ltls 1)(setq ltls 0))
		(set_tile "ltls" (itoa ltls))
		;�Զ��������������
		(if (vlr-added-p av:totalreader_pickfirst_reactor)(setq sl "1")(setq sl "0"))
		(set_tile "autototalreader" sl)
		;����ѭ����������ģʽ
		(cond
			((getvar "ribbonstate")
				(setq theui1 (av:getenv "theui1"))
				(and (null theui1) (setq theui1 0))
				(set_tile "theui1" (itoa theui1))
			)
			(t (set_tile "theui1" "1")
				(mode_tile "theui1" 1)
			)
		)
		;����״̬������������
		(cond
			((getvar "ribbonstate")
				(setq len (av:getenv "len"))
				(and (null len) (setq len 0))
				(set_tile "len" (itoa len))
			)
			(t (set_tile "len" "0")
				(mode_tile "len" 1)
			)
		)
	)
	;����ini�ļ�
	(cond
		((setq ini (av:findfile (setq fn "info.ini"))))
		(T (setq ini (strcat *FSTL_DIR* "\\Support\\" fn))
			(setq f	(open ini "w"))
			(close f)
		)
	)
	;���ù��ܼ�ȫѡ��ť���Ʒ���
	(setq *Allss* (vl-ini-get (list ini "Fkey" "Allss")))
	(defun allss(/ setfkey)
		(defun setfkey(v / n)
			(repeat (setq n 13) (set_tile (strcat "F" (itoa (setq n (1- n)))) v))
			(setq *Allss* v)
		)
		(cond
			((null *Allss*) (setfkey "1"))
			((wcmatch "1" *Allss*) (setfkey "0"))
			(t (setfkey "1"))
		)
	)
	;���²˵���ťд��INI���ܼ�״̬
	(defun setfkeytoini(/ key n)
		(princ ">>>ˢ�²˵�")
		(vl-ini-set (list ini "Fkey" "Allss" *Allss*))
		(repeat (setq n 13)
			(setq key (strcat "F" (itoa (setq n (1- n)))))
			(vl-ini-set (list ini "Fkey" key (get_tile key)))
		)
	)
	;�����Ƽ�ֵ
	(defun recommend(/ defkey0 defkey1)
		(defun defkey1()
			(set_tile "right" "1")
			(set_tile "dyn" "0")
			(set_tile "autoosmode" "1")
			(set_tile "osmode" (itoa 7223))
			(set_tile "cursorsize" "100")
			;=================================
			(set_tile "mypgp" "1")
			(set_tile "2click" "0")
			(set_tile "ltls" "0")
			(set_tile "autototalreader" "1")
			(and (getvar "ribbonstate")(set_tile "theui1" "0"))
			(and (getvar "ribbonstate")(set_tile "len" "1"))
			(vl-bb-set "defkey" 1)
		)
		(defun defkey0()
			(set_tile "right" "0")
			(set_tile "dyn" "1")
			(set_tile "autoosmode" "0")
			(set_tile "osmode" (itoa 4643))
			(set_tile "cursorsize" "5")
			;=================================
			(set_tile "mypgp" "0")
			(set_tile "2click" "0")
			(set_tile "ltls" "0")
			(set_tile "autototalreader" "0")
			(and (getvar "ribbonstate")(set_tile "theui1" "0"))
			(and (getvar "ribbonstate")(set_tile "len" "0"))
			(vl-bb-set "defkey" 0)
		)
		(cond
			((= 1 (vl-bb-ref "defkey")) (defkey0))
			(t (defkey1))
		)
	)
	;д���趨ֵ
	(defun settitle (/ 2click autoosmode cursorsize dyn len ltls mypgp osmode right theui1)
		(princ ">>>��������")
		;����Ҽ�ȷ��
		(setq right (atoi (get_tile "right")))
		(av:setenv "right" right)
		(if (= right 1)(setq right 2)(setq right 11))
		(setvar "shortcutmenu" right)
		;ָ���������
		(setq dyn (atoi (get_tile "dyn")))
		(av:setenv "dyn" dyn)
		(if (= dyn 0)(setq dyn 0)(setq dyn 3))
		(setvar "dynmode" dyn)
		;ȫ�����ò�׽�趨
		(setq autoosmode (atoi (get_tile "autoosmode")))
		(av:setenv "autoosmode" autoosmode)
		;����׽����
		(setq osmode (atoi (get_tile "osmode")))
		(av:setenv "osmode" osmode)
		(setvar "osmode" osmode)
		;ʮ�ֹ��ߴ����
		(setq cursorsize (atoi (get_tile "cursorsize")))
		(cond
			((< cursorsize 5)(setq cursorsize 5))
			((> cursorsize 100)(setq cursorsize 100))
			(t nil)
		)
		(av:setenv "cursorsize" cursorsize)
		(setvar "cursorsize" cursorsize)
		;==================================================================
		;���ּ���ͼ�Զ�����
		(setq mypgp (atoi (get_tile "mypgp")))
		(cond
			((= mypgp 1)(av:acad-pgp))
			((= mypgp 0)(if c:unpgp (c:unpgp)))
			(t nil)
		)
		;����˫����忪��
		(setq 2click (atoi (get_tile "2click")))
		(av:setenv "2click" 2click)
		(if (= 2click 0)(vlr-remove *blankdoubleclick*)(vlr-add *blankdoubleclick*))
		;��ͼ�Ƿ�������Ļ�˵�
		(setq ltls (atoi (get_tile "ltls")))
		(av:setenv "ltls" ltls)
		(cond
			((and (= 1 ltls) (/= 1 (vl-bb-ref "ltls")))
				(c:ltoolsswitch)
			)
			((and (= 0 ltls) (= 1 (vl-bb-ref "ltls")))
				(dcl-form-close ltools/menu);�ر����
				(vl-bb-set "ltls" 0);�ڲ����������н�������趨
			)
			(t nil)
		)
		;�Զ��������������
		(setenv "autototalreader" (get_tile "autototalreader"))
		(totalreader_pickfirst_reactor);����λ�ڲ��������ļ���
		;ѭ��������뾫��ģʽ
		(setq theui1 (atoi (get_tile "theui1")))
		(av:setenv "theui1" theui1)
		;����״̬������������
		(setq len (atoi (get_tile "len")))
		(cond
			((= len (av:getenv "len")))
			((= 1 len) (setupstatusbarnum t))
			(t (setupstatusbarnum nil))
		)
		(av:setenv "len" len)
	)
	;������ʼ
	(setvar "cmdecho" 0)
	(setq newdwg nil)
	(setq fname (av:findfile "seting.dcl"))
	(setq dclid (load_dialog fname))
	(new_dialog "seting" dclid "")
	(gettitle);д��ؼ�Ĭ����ʾֵ
	(setq oldautodir (av:getenv "Autodir"))
	(action_tile "wechat" "(done_dialog 11)");���ܼ��Ƽ�ֵ
	(action_tile "allss" "(allss)");���ܼ��Ƽ�ֵ
	(action_tile "setfkey" "(setfkeytoini)(done_dialog 13)");�趨���ܼ�
	(action_tile "rev" "(av:modifyautodir)");·���޸�
	(action_tile "just" "(recommend)");�Ƽ�
	(action_tile "accept" "(settitle)(done_dialog 1)");ȷ��
	(action_tile "cancel" "(done_dialog 0)");ȡ��
	(action_tile "reavtlsenv" "(done_dialog 41)") ;������ʼ��
	(action_tile "hotkeys" "(done_dialog 42)") ;�������
	(action_tile "1colour" "(done_dialog 43)");��ɫ����
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((= key 11)(c:AvtlsWechat)) ;��ά��
		((= key 13)(createmnu))
		((= key 41)(c:reavtlsenv))
		((= key 42)(c:hotkeys))
		((= key 43)(c:1colour))
		((and (= 0 key) newdwg)
			(vla-activate (vla-add *docs* "")) ;�½��ĵ�
		)
		(t nil)
	)
	(setvar "cmdecho" 1)
	(princ)
)
(defun c:AvtlsWechat(/ fn fn1 fn2 lst name)
	(setq fn2 (strcat (getenv "tmp") "\\wechat.dwg"))
	(setq fn1 (av:findfile "nyistjz.svg"))
	(cond
		((findfile fn2))
		(fn1 (vl-file-copy fn1 fn2))
		(t nil)
	)
	(cond
		((findfile fn2)
			(vla-open *docs* fn2 :vlax-true)
			(setq name (strcat (cadr (setq fn (fnsplitl fn2))) (caddr fn)))
			(vlax-for a *docs* (setq lst (cons (cons (vla-get-Name a) a) lst)))
			(vla-activate (cdr (assoc name lst)))
		)
		(t (princ "\nȱ�ٶ�ά���ļ���"))
	)
)
;;;=====================================================================
;;���ݹ��ܼ��������mnu�˵��ļ�
(defun createmnu(/ f lst mnu)
	(setq lst (createmnulist))
	(setq mnu	(av:findfile "fstl.mnu"))
	(setq f	(open mnu "w"))
	(cond
		((= (type lst) 'str) (vl-catch-all-apply 'write-line (list lst f)))
		((= (type lst) 'list) (foreach l lst (vl-catch-all-apply 'write-line (list l f))))
	)
	(close f)
	(regenmnu)
)
;;������۲˵�
;(defun c:ree()((regenmnu)))
(defun regenmnu (/ cui cui1 cui2 ext)
	;(princ "-->ˢ�²˵�")
	(setvar "cmdecho" 0)
	(setvar "filedia" 0)
	(setq ext (if (getvar "ribbonstate") ".cuix" ".cui"))
	(setq cui1 (findfile (strcat "acad" ext)))
	(setq cui2 (av:findfile (strcat "acad" (itoa *ver4*) ext)))
	(setq cui (cond (cui1 cui1) (cui2 cui2) (t (princ "�޷��ҵ��˵��ļ���"))))
	(if (menugroup "fstl") (command-s "menuunload" "fstl"))
	(cond ((menugroup "acad")) (t (command-s "menuload" cui)))
	(command-s "menuload" (av:findfile "fstl.mnu"))
	;��������˵�
	;(setq i 1 n 3);����N�����þֲ������˵�����
	;(while (menucmd (strcat "P" (itoa i) ".1=?")) (setq i (1+ i)));ȷ�ϲ˵�����
	;(setq b (if (getvar "ribbonstate") 2 2));�߰���3λ���Ͱ���2λ
	;(repeat n
	;	(menucmd (strcat "p" (itoa (- i b)) "=+fstl.POP" (itoa n)))
	;	(setq n (1- n))
	;)
	;��ӹ�����
	(setvar "qaflags" 0)
	(command-s "toolbar" "av_value" "s")
	(setvar "filedia" 1)
	(setvar "cmdecho" 1)
	(princ)
)
;���ر����������˵���ʾ�쳣�����Զ����²˵���ʾ
(if (null (menugroup "acad")) (regenmnu))
;;;;;;=====================================================================
;�趨�Զ������ļ���·��
(defun av:modifyautodir(/ autoloadcmd new path)
	(defun autoloadcmd(path / file fn lst)
		(alert "·�����޸ģ�\n\n�������Ķ��򿪵��ı��ļ���")
		;�Զ��������ļ����в�����ļ���
		(setq fn "AutoLoadDemo.lsp")
		(setq lst (list
								(strcat ";|�ļ�����" fn)
								"�ļ�λ�ã����õ��Զ������ļ�����"
								"�Զ������ļ��У�ֻ���Զ�������Ŀ¼�е�vlx��fas��lsp��arx��dll�ļ������������ļ����еĲ����"
								"�����Զ��������ļ����еĲ�������ڴ��ı��зָ����±�д�򵥳��򼴿ɡ�"
								"��ע�⣬�Զ������ļ��µ��������ļ����У�����صĲ������������ͬ���ļ�����"
								""
								";(av:load-fn-cmd fn cmd)"
								"fn�����ļ����в���ļ���"
								"cmd�������������"
								""
								"���磺"
								";(defun c:rjs()(av:load-fn-cmd \"RegFromApp64.exe\" nil))"
								";(defun c:vjs()(av:load-fn-cmd \"ϵͳ����������vjs.lsp\" \"vjs\"))"
								";(defun c:ls()(av:load-fn-cmd \"Lisp_v1.5.vlx\" \"lisp\"))"
								""
								"���⣺�����Զ����������ɰ�·��Avzztls\\Program\\Autoload�е��ļ���Ltoolsider.ini��"
								"���ƽ��Լ����Զ������ļ�����;"
								"��Щ�������Ա����´θ��¹�����ʱ������Ӱ�쵽�Զ���������"
								""
								"===========================================|;"
							))
		(setq file (strcat path "\\" fn))
		(av:newtxt file lst t) ;����λ��minitools��
		;(vla-activate (vla-add *docs* "")) ;�½��ĵ�
		(setq newdwg t)
	)
	(cond
		((wcmatch
			 (setq path (get_tile "path"))
			 (av:getenv "Autodir")
		 )
			(if (setq new (LM:DirectoryDialog "\n��ѡ�����Զ����ص�Ŀ¼" nil 512))
				(progn
					(setq new (av:setenv "Autodir" (strcase new t)))
					(set_tile "path" new)
					(av:AddSupportPath (list new))
					(av:RemoveSupportPath (list oldautodir))
					(autoloadcmd new)
					;(wyl:autoloadapp);��������
				)
			)
		)
		((findfile path)
			(av:setenv "Autodir" path)
			(av:AddSupportPath (list path))
			(av:RemoveSupportPath (list oldautodir))
			(autoloadcmd path)
			;(wyl:autoloadapp);��������
		)
		((wcmatch path "")
			(av:setenv "Autodir" "��ճ����ѡ��·��")
			(av:RemoveSupportPath (list oldautodir))
			(set_tile "path" (av:getenv "Autodir"))
			(alert "·���������")
		)
		(t (alert "·������ȷ��"))
	)
	;(wyl:autoloadapp);��������
)
;����ע�����ʱ�ļ��������ǿ�Ϊ���̣�����Ϊ����ָ�ȫ�ԣ�
(defun setupstatusbarnum (flag / app-key bar-key ls0 ls1)
	(setq bar-key (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles\\" (getvar "cprofile") "\\StatusBar"))
	(foreach l (list "DockAboveStatusbar" "ForcedDocAbove")
		(vl-registry-write bar-key l 0)
	)
	(setq ls1
		(list
			(list "CursorCoordinatesPane" 0);����
			(list "Paper/ModelPane" 1);ģ�Ϳռ�
			(list "GridPane" 1);��դ
			(list "SnapPane" 0);��׽ģʽ
			(list "InferRelationshipPane" 0);�ƶ�Լ��
			(list "DynInputPane" 1);��̬����
			(list "OrthoPane" 1);����ģʽ
			(list "PolarPane" 1);����׷��
			(list "IsoDraftPane" 0);������ͼ
			(list "OTrackPane" 1);����׽׷��
			(list "OSnapPane" 1);��ά����׽
			(list "LineWeight" 0);�߿�
			(list "TransparencyPane" 0);͸����
			(list "SelectionCycling" 0);ѡ��ѭ��
			(list "3DOSnapPane" 0);��ά��̬��׽
			(list "DynamicUCSPane" 0);��̬UCS
			(list "SelectionFilterPane" 0);ѡ�����
			(list "GizmoPane" 0);С�ؼ�
			(list "AnnotationVisibility" 0);ע�Ϳɼ���
			(list "AutoScale" 0);�Զ�����
			(list "AnnotationScales" 0);ע�ͱ���
			(list "WorkspaceSwitchingIconPane" 0);�л������ռ�
			(list "AnnoMonitorState" 0);ע�ͼ�����
			(list "UnitsPane" 0);��λ
			(list "QuickProperties" 0);�������
			(list "LockUIPane" 0);�����û�����
			(list "IsolateObjectPane" 0);�������
			(list "HardwareAccelerationPane" 0);ͼ������
			(list "CleanScreenPane" 0);ȫ����ʾ
		))
	(setq ls0 (list "CursorCoordinatesPane" "Paper/ModelPane" "GridPane" "SnapPane" "InferRelationshipPane" "DynInputPane" "OrthoPane" "PolarPane" "IsoDraftPane" "OTrackPane" "OSnapPane" "LineWeight" "TransparencyPane" "SelectionCycling" "3DOSnapPane" "DynamicUCSPane" "SelectionFilterPane" "GizmoPane" "AnnotationVisibility" "AutoScale" "AnnotationScales" "WorkspaceSwitchingIconPane" "AnnoMonitorState" "UnitsPane" "QuickProperties" "LockUIPane" "IsolateObjectPane" "HardwareAccelerationPane" "CleanScreenPane")
	)
	(setq app-key (strcat bar-key "\\Application"))
	(cond
		(flag
			(foreach l ls1 (vl-catch-all-apply 'vl-registry-write (cons app-key l)))
			(princ "\n��ʾ������״̬�����ܰ�ť����������AutoCAD�´�����ʱ��Ч��")
		)
		(t
			(foreach l ls0 (vl-registry-write app-key l 1))
			(princ "\n��ʾ��״̬�����ܰ�ť��ȫ���򿪣�����AutoCAD�´�����ʱ��Ч��")
		)
	)
)

;;;=================================================================
;ȷ���û��ļ���
(defun av:setautodir(/ autodir)
	(cond
		((null (setq autodir (av:getenv "Autodir")))
			(setq autodir (av:setenv "Autodir" "��ճ����ѡ��·��"))
		)
		((findfile autodir))
		(t (setq autodir (av:setenv "Autodir" "��ճ����ѡ��·��")))
	)
	autodir
)

;�Զ������ļ���ȫ��������Ҫ�Ƚ���������
(defun wyl:autoloadapp(/ ext inf lst_dir lst_fnm)
	(setq lst_dir
		(list
			(strcat *FSTL_DIR* "\\Program\\Autoload")
			(av:setautodir)
		)
	)
	(av:AddSupportPath lst_dir)
	(foreach l lst_dir
		(setq lst_fnm (append (mapcar '(lambda (x) (strcat l "\\" x))(vl-directory-files l nil 1)) lst_fnm))
	)
	(foreach x lst_fnm
		(setq ext (strcase (vl-filename-extension x) t))
		(cond
			((member ext (list ".vlx" ".fas" ".lsp"))
				(vl-catch-all-apply 'load (list x))
			)
			((wcmatch ext ".arx")
				(vl-catch-all-apply 'arxload (list x))
			)
			((wcmatch ext ".dll")
				(cond ((vl-cmdf "NETLOAD")) (t (princ (strcat "\n�ļ�" x "����ʧ�ܣ�"))))
			)
			(t nil)
		)
		(terpri)
	)
)
;�����Զ������ļ���,���ڴ˼��أ�ԭλ���ػᵼ������
(wyl:autoloadapp)

;===========================================================================
;ȡ���������ļ����ļ�·��
(defun av:getfile(fn / f lst)
	(foreach f (kk-getfiles (av:getenv "Autodir") "*.*" 1)
		(setq f (strcase f t))
		(setq lst (cons (list (av:getfname f) f) lst))
	)
	(cadr (assoc (strcase fn t) lst))
)
;�����������ļ��м������ļ����е��ļ�
;fn:�ļ����� cmd:��������
(defun av:load-fn-cmd(fn cmd / def ext file msg)
	(setvar "CMDECHO" 0)
	(setq file (av:getfile fn))
	(setq ext (strcase (vl-filename-extension file) t))
	(cond
		((member ext (list ".vlx" ".fas" ".lsp"))
			(cond ((setq def (eval (read (strcat "c:" cmd))))) (t (load file)))
			(def)
		)
		((wcmatch ext ".exe")(startapp file))
		((wcmatch ext ".dll")
			((if command-s command-s command) "netload" file)
			(command-s cmd)
		)
		((wcmatch ext ".arx")
			(if (vl-catch-all-error-p (setq msg (vl-catch-all-apply 'arxload (list file))))
				(princ (vl-catch-all-error-message msg))
				(command-s cmd)
			)
		)
		(t nil)
	)
	(setvar "CMDECHO" 1)
	(princ)
)

;;;===========================================================

;��ѯINI����ֵ
(defun getfkey(F)
	(cond
		((vl-ini-get (list (av:findfile "Info.ini") "Fkey" (strcase F))))
		(T "0")
	)
)
;;˵��:�����˵�
;;����:�б�
(defun createmnulist(/ isfkey ish)
	(defun isfkey(fkey)(wcmatch "1" (getfkey fkey)))
	(setq ish (getvar "ribbonstate"))
	(list
		"***MENUGROUP=FSTL"
		"***ACCELERATORS"
		(if (isfkey "f1") "[\"F1\"]'2view ")
		(if (isfkey "f3") "[\"F3\"]'zoom 0.4XP")
		(if (isfkey "f4") "[\"F4\"]'zoom extents")
		(if (isfkey "f6") "[\"F6\"] ^C^Cm&pspace")
		(if (isfkey "f7") "[\"F7\"] ^C^CPrONOFF")
		(if (isfkey "f8") "[\"F8\"] ^C^Copendir")
		(if (isfkey "f9") "[\"F9\"] ^C^Cbwl")
		(if (isfkey "f10") "[\"F10\"] ^C^Ctheui")
		(if (isfkey "f11") "[\"F11\"] ^C^CCleanScreen")
		(if (isfkey "f12") "[\"F12\"] ^C^CCleanRubbish")
		"[CONTROL+\"F\"] ^C^CFINDOUT"
		;"[Alt+\"W\"]^C^C_close"
		"***POP1"
		"[Ч�������]"
		"[cad-->txt\\tWB]^C^C_av-dcwb"
		"[cad-->xls\\tTTT]^C^C_TTT"
		"[ͳ���Ŵ���\\tWF]^C^C_windowsform"
		"[--]"
		"[�������ز���ͼ����\\tT3]^C^C_TSAVEASS"
		"[��ʱ�ļ�����\\t]^C^C_sv2dwg"
		"[��ͼ | ������������\\tFT]^C^C_ExportDwg"
		"[�ϲ�CADͼ���ļ�]^C^C_MergeDwg"
		"[PDF�ļ����嵼��]^C^C_PDFIMPORTS"
		"[--]"
		"[��ѹ��<��ά���ά>]^C^C_SimpleFlatten"
		"[��Ļ��ת��ָ�\\tZP]^C^C_CADZP"
		"[�ֲ�������\\tRR]^C^C_RR"
		;"[--]"
		;"[����ͼ����ǰ׺]^C^Cunxreflapre"
		;"[Ⱥ���ɢ\\t]^C^C_explodegroup"
		"[--]"
		"[���Ź��߼�...\\tLM]^C^C_cooltls"
		"[���˹�����...\\tY]^C^C_Y"
		"[�����ע��˵��...\\tNN]^C^C_Label-A"
		"[--]"
		"[����������...\\tKK]^C^C_KKfonts"
		"[����������ָ�ʽ]^C^C_CleanMtextFormat"
		"[����Ԫ������]^C^Cdwg-purge"
		"[--]"
		"[�����书������...\\tSET]^C^Cseting"
		"[��ͼ������ʼ��\\tDS]^C^C_REAVTLSENV"
		"[->��������]"
		"[��ʫCAD������...\\tFST]^C^CFSTL"
		"[ѡ����������\\tREOP]^C^C_REOP"
		"[���������������]^C^C_habitleft"
		"[��ʱ�ļ�������]^C^C_cleantemp"
		"[��ɫ����]^C^C_1colour"
		"[<-]"
		"[--]"
		"[��ͨ������]^c^cAvtlsWechat"
		;"[����ͨ��������]^C^C(AV:OPENWEB \"http://bbs.mjtd.com/thread-184305-1-1.html\")"
		;"[AVTLS����������]^C^C(AV:OPENWEB \"https://www.123pan.com/s/yOtA-BSM9d\")"
		"***POP2"
		"[�������޸�]"
		"[����ѡ��\\tSS]^C^Ceasyselect"
		"[�߳�ѡ��\\tSSL]^C^C_selectbycurvelength"
		"[����\"��\"����\\t0LL]^C^C_0LLine"
		"[����|����...\\tCTRL+F]^C^C_FINDOUT"
		"[--]"
		"[����ɾ��\\tREDEL]^C^Credel"
		"[--]"
		"[�����������\\tT]^C^C_T"
		"[���������ע\\tTT]^C^C_TT"
		"[���߲���ע\\t]^C^C_pline-dim"
		"[�Զ���������ע]^C^C_MarkPileNum"
		"[->�ı�ͳ�����޸�]"
		"[���������ͳ��\\tTS]^C^C_texttotal"
		"[��ĸ��Сдת��\\t]^C^C_TCASE"
		"[ͼ��ͳ�Ʊ�\\tKTJ]^C^C_blockcount"
		"[��������\\twbh]^C^C_nt2ot"
		"[��������ת����\\t]^C^C_nt2mt"
		"[�ı����\\t]^C^C_textID"
		"[�ı��ӿ�\\t]^C^C_TextBox"
		;"[����������ָ�ʽ]^C^C_CleanMtextFormat"
		"[<-]"
		;"[--]"
		;"[���򸲸�]^C^C_wipeout"
		"[--]"
		"[�ؽ����߽�\\tEF]^C^C_EF"
		"[���ɱ߽�����\\tEFF]^C^C_EFF"
		"[--]"
		"[��������\\tBOO]^C^C_BooleanRegion"
		"[����߱༭\\tDG]^C^C_pl_pt_ed"
		"[�����ģ������\\tPEE]^C^C_PEE"
		"[->���������]"
		"[���������\\tPE]^C^C_PE"
		"[�����޼�\\tTRR]^C^C_TRR"
		"[Ѱ�����򿪿�\\t]^C^C_CHKOPEN"
		"[������򿪿�\\t]^C^C_closepline"
		"[���ɶ���߱߿�]^C^C_mPolyOutline"
		"[���߿��\\tPLW]^C^C_plw"
		"[<-]"
		"[--]"
		"[���ֽܷ��\\tXX]^C^C_SMARTEXPL"
		"[һը���ס�Ƕ�׿顿]^C^C_ExplNestBlk"
		"[ը��һ��\\t]^C^C_BlastAll"
		"[����Ⱥ���ɢ\\t]^C^C_explodegroup"
		"[--]"
		"[������ת\\tRT]^C^C_RT"
		"[������ת\\tRTT]^C^C_RTT"
		"[--]"
		"[��ת����\\tRC]^C^C_YY-RO&CO"
		"[��̬��������\\tCCC]^C^C_CCC"
		"[--]"
		"[��̬˫��ƫ��\\tFSS]^C^C_FSS"
		"***POP3"
		"[ͼ�������]"
		"[���㲢ѡ��\\tNL]^C^C_putclayer"
		"[ɾָ����\\tDL]^C^C_DL"
		"[ɾ�رղ�\\t]^C^C_hh-delhidelayer"
		"[--]"
		"[��Ϊ���\\tSL]^C^C_put-color-bylayer"
		"[ָ����ǰ��\\t]^C^C_Activelayer"
		"[������ǰ\\tTDQ]^C^C_TDQ"
		"[ͼ��ˢ��\\tMAA]^C^C_LayerMatch"
		"[--]"
		"[->�������Ӵ�����]"
		"[���ٲ���\\t]^C^C_FastVports"
		"[�½��ӿ�\\tVS]^C^C_VportsPlus"
		"[--]"
		"[��ģ���벼���д�Խ]^C^C_ChangeSpace"
		"[--]"
		"[�Ӵ�|����\\tSK]^C^C_SK"
		"[�Ӵ�|����\\tKS]^C^C_KS"
		"[ģ�Ͳ����л�\\t1E]^C^C_1E"
		"[�ӿ��л�\\t2E]^C^C_M&Pspace"
		"[<-]"
		"[--]"
		"[�ر���ѡ\\tGG]^C^C_GG"
		"[�ر�����\\tZD]^C^C_ZD"
		"[����ر�\\tFH]^C^C_FH"
		"[ȫ����\\tQB]^C^C_QB"
		"[--]"
		"[�ͷ�ͼ�����\\t]^C^C_LayerAllF"
		"[--]"
		"[->ͼ�������붳��]"
		"[������ѡ\\tSD]^C^C_layerlock"
		"[������ѡ]^C^C_LayerUnlock"
		"[--]"
		"[������ѡ\\tDJ]^C^C_LayerFreeze"
		"[ȫ���ⶳ]^C^C_LayerFreeAll"
		"[���򶳽�]^C^C_LayerFreeSwap"
		"[<-]"
		"[->ͼ��Ԫ������]"
		"[������ѡ\\tHH]^C^C_Hideobj"
		"[��������]^C^C_isoobj"
		"[��������]^C^C_HideSwap"
		"[ȫ����ʾ]^C^C_Showall"
		"[<-]"
		"[--]"
		"[����ͼ����ǰ׺]^C^Cunxreflapre"
		"***TOOLBARS"
		"**TB_av_value"
		"[_Toolbar(\"AV_value\", _Top, _Show, 0, 0, 1)]"
		"[_Button(\"��������\", RCDATA_16_WALK-THROUGH_SAVE,)]^C^C_APIWINSET"
		"[_Button(\"�½�\", RCDATA_16_NEW,)]^C^C_qqnew"
		"[_Button(\"��\", RCDATA_16_OPEN,)]^C^C_open"
		"[_Button(\"����\", RCDATA_16_SAVE,)]^C^C_qsave"
		"             [--]"
		(strcat "[_Button(\"���ٴ�ӡ\"," (if ish "RCDATA_16_FRAME_DISPLAYPLOT" "RCDATA_16_PRINT") ",)]^C^C_fastprinter")
		(strcat "[_Button(\"��ͼ�벼�ֵ���FT\", " (if ish "RCDATA_16_ARRSCHANGE" "RCDATA_16_ARRREC") ",)]^C^C_ExportDwg")
		;"[_Button(\"��ͼ�벼�ֵ���FT\", RCDATA_16_MATERIALS_PAINTER,)]^C^C_ExportDwg"
		(strcat "[_Button(\"����ΪͼƬ\", " (if ish "RCDATA_16_WMFOUT" "RCDATA_16_FOG") ",)]^C^C_WMFPLUS")
		(strcat "[_Button(\"�ļ�������\", " (if ish "RCDATA_16_RENAME" "RCDATA_16_TOOMOD") ",)]^C^C_av:rename")
		"             [--]"
		(if dcl-project-load
			(strcat "[_Button(\"������忪��\", " (if ish "RCDATA_16_AUTHORPALETTE_NEW" "RCDATA_16_TPALETTE") ",)]^C^C_ltoolsswitch")
		)
		;(strcat "[_Button(\"�ӿ�ͬ��\", " (if ish "RCDATA_16_VPSYNC" "RCDATA_16_WINVRT") ",)]^C^C_AGDT")
		"[_Button(\"�ӿ�ͬ��\", RCDATA_16_VISUALCOMPARE,)]^C^C_AGDT"
		"[_Button(\"���ò���\", RCDATA_16_SET_VARIABLE,)]^C^Cseting"
		"[_Button(\"˫�������趨 F1\", RCDATA_16_BNGFWD,)]^C^C2viewset"
		"[_Button(\"��Ļ��תzp\", RCDATA_16_UCSZAR,)]^C^C_CADZP"
		"[_Button(\"��������KK\", RCDATA_16_SPELL,)]^C^C_KKfonts"
		;(strcat "[_Button(\"��������KK\", " (if ish "RCDATA_16_UPDFIELD" "RCDATA_16_SPELL") ",)]^C^C_KKfonts")
		"[_Button(\"��ѹ��\", RCDATA_16_SM_EXTEND,)]^C^C_SimpleFlatten"
		"             [--]"
		"[_Control(_Undo)]"
		"[_Control(_Redo)]"
		"             [--]"
		(strcat "[_Button(\"�������\", " (if ish "RCDATA_16_WALK_3DWALK" "RCDATA_16_3DWALK") ",)]^C^C_pt-pt-dis")
		(if ish "[_Button(\"���ٲ���\", RCDATA_16_QUICKMEASURE, RCDATA_32_QUICKMEASURE)]^C^C_MEASUREGEOM _M _Y")
		"[_Button(\"���߱곤\", RCDATA_16_DIST, RCDATA_16_DIVIDE)]^C^C_pline-dim"
		"[_Button(\"�����ע\", RCDATA_16_AREA, RCDATA_32_AREA)]^C^C_manuarea"
		;"[_Button(\"ͳ���Ŵ���WF\", RCDATA_16_TABLE,)]^C^C_windowsform"
		"             [--]"
		"[_Button(\"�����fg\", RCDATA_16_PLINE,)]^C^CFG"
		"[_Button(\"���ɾ���rec\", RCDATA_16_RECTAN,)]^C^CREC"
		(strcat "[_Button(\"��������ryy\", " (if ish "RCDATA_16_REVCLOUD_RECTANGLE" "RCDATA_16_REVCLOUD") ",)]^C^C_reccloud")
		"             [--]"
		;"[_Button(\"ը��һ��\", RCDATA_16_MOCORO,)]^C^C_BlastAll"
		"[_Button(\"���ֽܷ��xx\", RCDATA_16_EXPLODE,)]^C^C_smartexpl"
		(strcat "[_Button(\"��̬��������CCC\"," (if ish "RCDATA_16_COPYM" "RCDATA_16_COPYOB") ",)]^C^C_CCC")
		(if acet-sys-lmouse-down ;�ж��Ƿ�װ��ET
			"[_Button(\"����Ƕ��ͼԪ\", RCDATA_16_NCOPY,)]^C^C_NCOPY"
		)
		;(strcat "[_Button(\"�������\", " (if ish "RCDATA_16_UNGROUP" "RCDATA_16_SELGRO") ",)]^C^C_ExplodeGroup")
		"[_Button(\"�ߴ��\", RCDATA_16_BRE2PT, RCDATA_32_BRE2PT)]^C^C_dynbreak"
		"[_Button(\"���ߺϲ�pe\", RCDATA_16_JOIN,)]^C^C_PE"
		(strcat "[_Button(\"����al\", " (if ish "RCDATA_16_ALIGN_NEW" "RCDATA_16_ML_ALIGN") ",)]^C^C_align")
		;"[_Button(\"����al\", RCDATA_16_ML_ALIGN,)]^C^C_align"
		"[_Button(\"����ƥ��ma\", RCDATA_16_MATCH,)]'_matchprop"
		(strcat "[_Button(\"�����ظ�����ov\", " (if ish "RCDATA_16_OVERKILL" "RCDATA_16_PURGE") ",)]^C^C_ov")
		;(strcat "[_Button(\"ɾָ����dl\", " (if ish "RCDATA_16_LAYER_DELETE" "RCDATA_16_REFED_DISC") ",)]^C^Cdlayer")
		"             [--]"
		"[_Button(\"�ı����\", RCDATA_16_JUSTIFYTXT,)]^C^C_textID"
		(strcat "[_Button(\"�Զ��������ע\", " (if ish "RCDATA_16_TCOUNT" "RCDATA_16_POINT") ",)]^C^C_MarkPileNum")
		"             [--]"
		;"[_Button(\"��ע��ʽ������\", RCDATA_16_DIMSTY,)]'_dimstyle"
		;"[_Control(_dimstyle)]"
		;"             [--]"
		"[_Control(_Color)]"
		"[_Button(\"ͼ�����Թ�����\", RCDATA_16_LAYERS,)]'_layer"
		"[_Control(_Layer)]"
		
		"**TB_VIEWPORTS"
		"[_Toolbar(\"AV_�ӿ�\", _Floating, _Hide, 100,210, 1)]"
		(strcat "[_Button(\"���ٲ���\", " (if ish "RCDATA_16_ALIGN_SPACE" "RCDATA_16_VPDLG") ",)]^C^C_FastVports")
		"[_Button(\"�½��ӿ�VS\", RCDATA_16_VPONE,)]^C^C_VportsPlus"
		(strcat "[_Button(\"������ת��Ϊ�ӿ�\", " (if ish "RCDATA_16_VIEW_VP_OBJECT" "RCDATA_16_VPOBJ") ",)]$M=$(if,$(eq,$(getvar,tilemode),0),^C^C_-vports _o,^C^C^P(ai_viewports_alert)^P)")
		"[_Button(\"���������ӿ�\", RCDATA_16_VPCLIP, RCDATA_16_VPCLIP)]^C^C_vpclip"
		"[_Control(_ViewportScale)]"
		"[--]"
		(strcat "[_Button(\"���Ŀռ�\", " (if ish "RCDATA_16_CHANGE_SPACE" "RCDATA_16_TOLERA") ",)]^C^C_ChangeSpace")
		(if (>= *ver4* 2009)
			"[_Button(\"�����������ģ��\", RCDATA_16_EXPORT_LAYOUT,)]^C^C_EXPORTLAYOUT" ;RCDATA_16_SECTIONPLANETOBLOCK
		)
		(strcat "[_Button(\"�����ӿ�\", " (if ish "RCDATA_16_ISOLATEOBJECTS" "RCDATA_16_ONLAY") ",)]^C^C_actvp")
		"[--]"
		(strcat "[_Button(\"�����ӿ�SK\", " (if ish "RCDATA_16_VP_DISPLAY_LOCKED" "RCDATA_16_LCKLAY") ",)]^C^C_SK")
		(strcat "[_Button(\"�����ӿ�KS\", " (if ish "RCDATA_16_LAYOUT_VIEWPORT_UNLOCKED" "RCDATA_16_ULKLAY") ",)]^C^C_KS")
		(strcat "[_Button(\"��һ����\", " (if ish "RCDATA_16_VIEW_BACK" "RCDATA_16_HLNK_BACK") ",)]^C^C_LAYOUTUP")
		(strcat "[_Button(\"��һ����\", " (if ish "RCDATA_16_VIEW_FWD" "RCDATA_16_HLNK_FWD") ",)]^C^C_LAYOUTDOWN")
		(strcat "[_Button(\"�ӿڽ���2E\", " (if ish "RCDATA_16_VP_MINIMIZE" "RCDATA_16_REVOLV") ",)]^C^C_M&Pspace")
		
		;"**TB_LAYERS"
		;"[_Toolbar(\"AV_ͼ�����\", _Floating, _Hide, 100,150,1)]"
		;;"[_Button(\"ɾָ��ͼ��\", RCDATA_16_LAYER_DELETE,)]^C^C_11928511"
		;;"[--]"
		;"[_Button(\"�ͷ�ͼ�����\", RCDATA_16_LIGHT,)]^C^C_1111"
		;"[--]"
		;"[_Button(\"ͼ��ر�\", RCDATA_16_OFFLAY,)]^C^C_22"
		;"[_Button(\"ͼ��ȫ��\", RCDATA_16_ONLAY,)]^C^C_2112"
		;"[_Button(\"ͼ�����\", RCDATA_16_LAYISO,)]^C^C_29022"
		;"[--]"
		;"[_Button(\"ͼ������\", RCDATA_16_LCKLAY,)]^C^C_12323"
		;"[_Button(\"ͼ�����\", RCDATA_16_ULKLAY,)]^C^C_12344"
		;"[--]"
		;"[_Button(\"ͼ�㶳��\", RCDATA_16_FRZLAY,)]^C^C_1233"
		;"[_Button(\"ͼ��ⶳ\", RCDATA_16_LAYTHW,)]^C^C_124"
		;"[--]"
		;"[_Button(\"��������\", RCDATA_16_HIDEOBJECTS,)]^C^C_2224"
		;"[_Button(\"����ȫ��\", RCDATA_16_UNISOLATEOBJECTS,)]^C^C_28224"
		;"[--]"
		
		
	)
)

;============��ʱ���ӿڹ�����==============
(or *av:layoutswitchvpbar* ;���벼��ʱ���ӿڹ�����
	(setq *av:layoutswitchvpbar* (vlr-miscellaneous-reactor nil '((:vlr-layoutSwitched . av:layoutswitchvpbar))))
)
(defun av:layoutswitchvpbar(a b)
	(cond
		((null MJ:Toolbar))
		((= 0 (getvar "tilemode"))
			(MJ:Toolbar "FSTL" "AV_�ӿ�" 1)
		)
		(t (MJ:Toolbar "FSTL" "AV_�ӿ�" 0))
	)
)
(or *av:ifquitoffvpbar* ;��⵽�ض�����ʱ�Ķ���
	(setq *av:ifquitoffvpbar* (vlr-command-reactor nil '((:vlr-commandwillstart . av:ifquitoffvpbar))))
)
(defun av:ifquitoffvpbar (calling-reactor startcommandinfo)
	(cond
		((null MJ:Toolbar))
		((wcmatch (car startcommandinfo) "QUIT")
			(MJ:Toolbar "FSTL" "AV_�ӿ�" 0)
		)
		(t nil)
	)
)



(princ)





