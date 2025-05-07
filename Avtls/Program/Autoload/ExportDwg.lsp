
;;��ͼ�벼�ֵ���
(defun c:ExportDwg (/ *error* dwg file fn istim msexports name path psexport psexports ss)
	(princ "-->ͼֽ��ֵ���")
	(defun *error*(msg)
		(if (findfile file)
			(progn
				(av:opendir (vl-filename-directory path) t)
				(prompt "\n>>>��ֳ�����ͼֽ�ѱ����ڴ򿪵��ļ�����")
			)
		)
		(setvar "cmdecho" 1)
		(princ)
	)
	(defun msexports(ss / file i pts sdwgout time)
		(setq i 0)
		(while (or ss (setq ss (ssget)))
			(setq i (1+ i))
			(setq time (av:gettime "+hh-mm-ss-"))
			(setq sDwgOut (strcat name time (itoa i) ".dwg"))
			(setq pts (av:getboxpt ss))
			(vla-ZoomWindow *acad* (vlax-3D-point (car pts)) (vlax-3D-point (cadr pts)))
			(setq file (strcat path sDwgOut))
			(vla-WBlock *doc* file (ss->vlass ss))
			(av:sound (av:findfile "chimes.wav"))
			(vla-ZoomPrevious *acad*)
			(prompt (strcat "\n�ѱ��棺" sDwgOut))
			(setq ss nil)
		)
		file
	)
	(defun psexport(na / file sdwgout)
		(setq sdwgout (strcat name "_" na ".dwg"))
		(setq file (strcat path sdwgout))
		(if (vl-cmdf "exportlayout" file)
			(progn
				(prompt (strcat "\n�ѱ��棺" sDwgOut "\n"))
				(av:sound (av:findfile "chimes.wav"))
			)
		)
		file
	)
	(defun psexports(/ lst sdwgout stab)
		(setq stab (getvar "ctab"))
		(setq lst (lm:listbox "���������ģ���ļ����ɶ�ѡ��" (layoutlist) t))
		(foreach l lst (setvar "ctab" l) (setq file (psexport l)))
		(setvar "ctab" stab)
		file
	)
	;;��ʼ����
	(setvar "cmdecho" 0)
	(cond
		((setq dwg (cadr (av-mkdir-todwg t)))
			(setq fn (fnsplitl dwg) path (car fn) name (cadr fn))
			(prompt (strcat "\n�ļ�����λ��:" path))
		)
		(t nil)
	)
	(cond
		((null dwg))
		((and
			 (setq istim (= (getvar "tilemode") 1))
			 (princ "\n��ʾ���س������������ֵ�����")
			 (setq ss (ssget))
		 )
			(setq file (msexports ss))
		)
		(istim (setq file (psexports)))
		((progn
			 (if (/= 0 (vla-get-MSpace *doc*)) (vla-put-MSpace *doc* 0))
			 (princ "\n��ʾ���س��ɱ����ֵ������嵼����")
			 (setq ss (ssget))
		 )
			(setq file (msexports ss))
		)
		(t
			(setq file (psexport (getvar "ctab")))
			(princ "\n��ģ���п�ѡ�ɲ�������������")
		)
	)
	(*error* nil)
)
;�Զ������ļ�����
(defun c:sv2dwg(/ dwg extlst f2 flst i obj_wsh path savepath str winshell)
	(setq savepath (vl-filename-directory (getvar "savefilepath")))
	(setq extlst (list ".bak" ".dwg" ".sv$"))
	(setq flst
		(vl-remove-if-not
			(function
				(lambda(x / ext)
					(and
						(setq ext (vl-filename-extension x))
						(member (strcase ext t) extlst)
					)
				)
			)
			(kk-getfiles savepath "*.*" 0)
		)
	)
	(setq obj_wsh (vlax-create-object "wscript.shell"))
	(setq path (car (av-mkdir-todwg nil)))
	(setq i 0)
	(foreach f1 flst
		(setq dwg (strcat (cadr (fnsplitl f1)) ".dwg"))
		(setq f2 (strcat path "\\" dwg))
		(setq str (strcat "cmd /c copy " f1 " " f2))
		(vlax-invoke obj_wsh 'run str 0 0)
		(princ (strcat "\n���� " dwg))
		(setq i (1+ i))
	)
	(vlax-release-object obj_wsh)
	(cond
		((> i 0)
			(princ (strcat "\n��ʾ��������" (itoa i) "����ʱ�ļ���"))
		)
		(t
			(princ "\n��ʾ��û�з�����ʱ�ļ���")
		)
	)
	(cond
		(flst
			(av:opendir path t)
			;(textpage)
		)
	)
	(princ)
)

;��ǿ��wmfͼ�ε���
(defun c:wmfplus(/ *error* dwg file i ismax layout path pts ss stn time)
	(defun *error*(msg)
		;(setvar "lwdisplay" 0)
		(ShowPlotStyles :vlax-false)
		(cond
			(file
				(princ (strcat "\n���棺������" stn "��WMFͼƬ;"))
				(av:opendir path t)
			)
			(msg (princ "�����õ����ͼ��,����������WMFOUT��ȡ��"))
			(t nil)
		)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(princ "\n��������WMF��ʽ���ڲ����ĵ�ʱ��Ϊʸ��͸�������ͼ�����������ţ������ɫ��������䡣")
	;(setvar "lwdisplay" 1)
	(cond
		((= 1 (getvar "tilemode"))
			(if (< *ver4* 2019) (princ "\n���ѣ��Ͱ汾��CAD�����ڲ����в����Ե�ɫ��ʽ��ʾ��"))
		)
		((= 1 (getvar "cvport"))
			(princ "\n��ǰ״̬�£����谴��ǰ��ʾ��ͼ�����÷�ͼ���򵼳����ֺ���ٲ�����")
		)
		(t (command-s "vpmax") (setq ismax t))
	)
	(setq	layout (vla-get-activelayout *doc*))
	(vla-Put-StyleSheet layout "monochrome.ctb")
	(ShowPlotStyles :vlax-true)
	(setq path (car (av-mkdir-todwg nil)))
	(prompt (strcat "\n�ļ�����λ��:" path))
	(setq dwg (vl-filename-base (getvar "dwgname")))
	(setq i 1)
	(princ "\n��ѡ�񵼳�MWF�ĵ�1��ͼ��")
	(while (setq ss (ssget '((0 . "~VIEWPORT"))))
		(setq time (av:gettime "+hh-mm-ss-"))
		(setq stn (itoa i))
		(setq na (strcat dwg time stn ".wmf"))
		(setq file (strcat path "\\" na))
		(setq pts (av:getboxpt ss))
		(vla-ZoomWindow *acad* (vlax-3D-point (car pts)) (vlax-3D-point (cadr pts)))
		(while (= 1 (getvar "cmdactive")) (command pause))
		(command-s "wmfout" file ss "")
		(av:sound (av:findfile "snip.wav"))
		(vla-ZoomPrevious *acad*)
		(princ (strcat "\n�ѱ���:" na))
		(setq i (1+ i))
		(princ (strcat "\n��ѡ���" (itoa i) "��ͼ��"))
	)
	(cond
		(ismax
			(command-s "vpmin")
			(vla-put-MSpace *doc* :vlax-true)
		)
	)
  (*error* nil)
)

;��ʾ��ɫ����
(defun c:1colour(/ layout)
	(setq	layout (vla-get-activelayout *doc*))
	;(setq Background (getenv "Background"))
	;(setq Layoutbackground (getenv "Layout background"))
	(cond
		(
			isnip
			;(and
			;	Background
			;	Layoutbackground
			;	(wcmatch Background "16777215")
			;)
			(ShowPlotStyles :vlax-false)
			(Backgroundcolor Background Layoutbackground)
			(setq isnip nil)
			(princ "��ǰ��ͼ����")
		)
		(t
			(setq Background (getenv "Background"))
			(setq Layoutbackground (getenv "Layout background"))
			(vla-Put-StyleSheet layout "monochrome.ctb")
			(ShowPlotStyles :vlax-true)
			(Backgroundcolor 16777215 16777215)
			(setq isnip t)
			(princ "��ǰ��ӡ��ʾ")
		)
	)
	(princ)
)

(defun ShowPlotStyles(value)
	(vla-put-ShowPlotStyles layout value)
	(vla-regen *doc* AcAllViewPorts)
)
;;˵��:ȷ��·�����ļ���
;;����:flag:ȷ���Ƿ�����ѡ��·��
;;����:·�����ļ�ȫ���б�
(defun av-mkdir-todwg (flag / file folder path)
	(cond
		((and
			 (setq path (av:getenv "DwgExportPath"))
			 (findfile path)
		 )
		)
		(t
			(setq folder (strcat "��ͼ" (av:gettime "m-d")))
			(setq path (getenv "UserProfile"))
			(setq path (strcat path "\\" folder))
			(vl-mkdir path)
		)
	)
	(cond
		((and
			 flag
			 (setq path (strcat path "\\" (getvar "dwgname")))
			 (setq file (getfiled "ͼ���ļ���������" path "dwg;dwt" 1))
			 (setq path (vl-filename-directory file))
		 )
		)
		(t (setq file nil))
	)
	(av:setenv "DwgExportPath" path)
	(list path file)
)


