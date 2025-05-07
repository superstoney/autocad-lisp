
;;�˳�����mnl�ļ��Զ�����

(defun c:ov()
	(princ "-->�����ظ����� ")
	(cond
		((>= *ver4* 2012) (command "overkill"))
		(T (if c:scty nil (load (av:findfile "scty.fas") "\n�ļ�scty.fas����ʧ��"))
			(c:scty)
		)
	)
)

;;;================================================================

;���Ʋ�����DUOTAB��ǩ���
(defun av:loadduotab(/ copyed f1 f2 f2n fn ls lst m n reg-key v val-data)
	(setvar "CMDECHO" 0)
	;;��ѯ�汾��,ȷ���ļ����
	(setq lst (list 2006 2007 2009 2013))
	(setq v t n (length lst))
	(while v
		(setq m (nth (setq n (1- n)) lst))
		(if (>= *ver4* m) (setq v nil))
	)
	;;�����ļ���
	(setq fn (strcat "AutoCAD DuoTab for " (itoa m) ".dll"))
	(setq f1 (av:findfile (strcat fn "������")))
	(setq f2n (strcat (getenv "temp") "\\" fn))
	;���踴���ļ�
	(cond ((setq f2 (findfile f2n)))
		(f1 (setq copyed (vl-file-copy f1 f2n)) (setq f2 (findfile f2n)));���Ʋ���ѯ
		(t (princ "\nû�з�����Ӧ��ǰCAD�汾��DuoTab��ǩ�����"))
	)
	;;����ע������ò���
	(setq reg-key (strcat "HKEY_CURRENT_USER\\"(vlax-product-key)"\\Applications\\AutoCAD DuoTab.dll"))
	(cond ((setq val-data (vl-registry-read reg-key "LOADER")))
		(f2 (setq ls (list
									 (list "SHOWTHUM" "0");����ʾ����ͼ
									 (list "HideExtension" "1");����ʾ��չ��
									 (list "UseFlowTab" "True");���б�ǩ��ʾ
									 (list "UseLockLauncher" "1");�����øð汾CAD���ļ�
									 (list "IMEC" "1");��������ͻ�����뷨���ƣ�ֱ������
									 (list "MenuShowed" "False,False,False,False");��ť����
								 )
				)
			(foreach l ls (vl-catch-all-apply 'vl-registry-write (cons reg-key l)))
		)
		(t
			(and (getvar "filetabstate") (command "Filetab"))
			(princ "û�з���duo��ǩ�壡")
		)
	)
	;;�������������ز��
	(cond
		(copyed (vl-cmdf "netload" f2)) ;������������
		((and f2 val-data (wcmatch (strcase f2) (strcase val-data)))) ;����������ظ�����
		(f2 ;���ΰ�װ��λ�ñ仯ʱ����
			(vl-cmdf "netload" f2)
			(and (getvar "filetabstate") (vl-cmdf "Filetabclose"))
		)
		(t nil)
	)
	(setvar "CMDECHO" 1)
	(princ)
)
(av:loadduotab)

;;;================================================================

;�ȼ�������
(defun av:loadhotkeys(/ cmd file fn loaded reg-key val-name vern vla-data)
	(setvar "cmdecho" 0)
	(cond
		((> *ver2* 17)
			(and
				(setq vern (atof (getvar "acadver")))
				(setq vern (cond ((>= vern 23) 23) ((>= vern 19) 19) (t nil)))
				(setq fn (strcat "hotKeys_r" (rtos vern) ".dll"))
				(setq file (av:findfile fn))
				(setq reg-key (strcat "HKEY_CURRENT_USER\\"(vlax-product-key)"\\Applications\\HotKeys.NET"))
				(setq val-name "LOADER")
				(setq vla-data (vl-registry-read reg-key val-name))
				(setq loaded (wcmatch (strcase file) (strcase vla-data)))
			)
			(cond
				(loaded)
				(file (setq loaded (vl-cmdf "NETLOAD" file)))
				(t nil)
			)
			(setq cmd "keyedit")
		)
		(t
			(and
				(setq fn (strcat "YJT_AdTool_r" (itoa *ver2*) ".arx"))
				(setq file (av:findfile fn))
				(setq loaded (member (strcase fn t) (arx)))
			)
			(cond
				(loaded)
				(file (setq loaded (arxload file)))
				(t nil)
			)
			(setq cmd "YJT_DJKJ")
		)
	)
	(setvar "cmdecho" 1)
	(cond
		((null file) (princ "\nû��ĿǰCAD�汾��Ӧ���ȼ��������������£�"))
		((null loaded) (princ "\n�ȼ��������޷����أ�"))
		(t cmd)
	)
)
;�����ȼ�������
(av:loadhotkeys)
;���ȼ�������
(defun c:hotkeys()
	(vl-cmdf (av:loadhotkeys))
)






(princ)




