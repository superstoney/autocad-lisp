
;;��ͼ�Զ�����
((lambda()
	 (setvar "cmdecho" 0)
	 ;���ƿ����ڲ����ӿڡ�ҳ�沼�ֺʹ�ӡ�����ű������б��������ճ�������������������⣬�з�����Ϣ
	 (vl-catch-all-apply 'command-s (list "-scalelistedit" "r" "y" "e"))
	 ;��ͼ�ظ�դ
	 (cond
		 ((= 1 (getvar "ribbonstate"))(setvar "gridmode" 1))
		 (t (setvar "gridmode" 0))
	 )
 )
)

;;===================��ͼ������ʼ��=====================
(defun c:ds()(c:reavtlsenv))
(defun c:reavtlsenv (/ *error* acadpref ctb cursorsize d2b display dyn layout lst lts os1 os2 preferences right txt)
	(defun *error*(str)
		(foreach l (list preferences display AcadPref)
			(vlax-release-object l)
		)
		;(princ "������ɣ�")
		(setvar "cmdecho" 1)
		(princ)
	)
	(princ "-->��ͼ������ʼ�� ")
	(setvar "cmdecho" 0)
	(if (= 1 (getvar "nomutt")) (setvar "nomutt" 0));��ֹ��Ϣ����
	(setq preferences (vlax-get-property *acad* 'Preferences))
	(setq Display (vlax-get-property preferences 'Display))
	(setq AcadPref (vla-get-OpenSave preferences))
	(setq	layout (vla-get-activelayout *doc*))
	;=====================================================
	;���CAD��win11���޷����ü��±�������
	;(setvar "MTextEd" "notepad");�����ı�˫���ü��±��༭
	(setvar "MTextEd" ".");��Ĭ�ϱ༭��
	;=====================================================
	;�����ĵ��������(1=acmin,2=acnorm,3=acmax)
	(vla-put-windowstate *acad* acmax);���򴰿����
	(vla-put-windowstate *doc* acmax);�ĵ��������
	(setenv "ShowFullPathInTitle" "1");�ڱ�������ʾ����·��
	;=====================================================
	(setvar "proxynotice" 0);����ͼ�ζԻ��򣨲���ʾ��
	(setvar "proxyshow" 1);��ʾ����ͼ��
	(setvar "recoverymode" 1);��¼�޸���Ϣ������ϵͳ���ֹ��Ϻ��Զ���ʾ��ͼ���޸�������
	(setvar "dwgcheck" 2);�򿪴����ļ������о���
	(setvar "reporterror" 0);�����ʹ��󱨸�
	(and (getvar "qpmode")(setvar "qpmode" -1));����ʾ����������
	(setvar "tooltips" 1);��ʾ������ʾ����
	(setvar "acadlspasdoc" 0);���ļ�������lsp
	(and (getvar "rollovertips")(setvar "rollovertips" 0));��ʾ�����ͣ��ʾ����(������cad2009�����߰汾)
	(and (getvar "secureload")(setvar "secureload" 0));�κ�λ�ü��ز����棬��������trustedpaths�����������ļ���·��
	(setvar "expert" 1);��ֹ��ʾ��׼�������ɣ��Ƿ���������Լ����Ƿ�ȷʵҪ�رյ�ǰͼ�㣿��
	(setvar "filedia" 1);���ļ��Ի���
	;=====================================================
	(if (/= (getvar "sdi") 0)(setvar "sdi" 0));���ĵ�ģʽ���أ��˲���������ԭ����ǩ����
	;��ǩ������,�Ͱ�رգ��߰�򿪡���sdi�����й���
	(and (= 1 (getvar "filetabstate"))(command "filetabclose"))
	(command-s "taskbar" "0");��������ǩ�ϲ�,cmd�������ϲ�
	;=====================================================
	(and (getvar "startmode")(setvar "startmode" 0));�ر�"��ʼ"ѡ���
	(and (getvar "filetabpreview")(setvar "filetabpreview" 0));���ƽ������ͣ��ͼ���ļ�ѡ��Ϸ�ʱ�Ƿ���ʾ����ͼ
	;ָ�������������ͣ���ļ�ѡ�����ͼ��ʱ���Ƿ���ͼ�δ����м�����Ӧ��ģ�ͻ򲼾֡�
	(and (getvar "filetabthumbhover")(setvar "filetabthumbhover" 0))
	(vl-catch-all-apply 'vl-cmdf (list "-plotstamp" "l" "n" "" ""));��ӡ���Ǽ��߼�ѡ����ò�������־�ļ�
	;=====================================================
	(and (getvar "touchmode")(setvar "touchmode" 0));�رմ���ģʽ���������
	;(vla-put-DisplayScrollBars display :vlax-false);�رչ�����
	(setenv "Scrollbars" "0");�رչ�����
	(and (getvar "startup")(setvar "startup" 0));�رջ�ӭ����(2015������)
	(and (getvar "vpcontrol")(setvar "vpcontrol" 0));�ر����Ͻ���ͼ��ʾ
	(and (getvar "displayviewcubein2d")(setvar "displayviewcubein2d" 0));�رն�������
	(and (getvar "navbardisplay")(setvar "navbardisplay" 0));��ͼ�����������
	(and (getvar "gripmultifunctional")(setvar "gripmultifunctional" 1));��̬�е�˵��ر�(������cad2010�����߰汾)
	;(setvar "ucsicon" 0);�ر����½�ucsͼ��
	;�����ļ�����
	(setvar "isavebak" 0);���Ʊ����ļ� (bak) �Ĵ�����1-����, 0-������
	(setvar "isavepercent" 0);��ȫ����,ʹ�ļ���С
	(setvar "savetime" 10);�����Զ������ʱ��Ϊ10����
	;(setenv "DefaultFormatForSave" "36");����CADĬ�ϱ����ļ���ʽ2013(24-2004,36-2007,48-2010,60-2013)
  (vla-put-SaveAsType AcadPref ac2007_dwg);������ǰ�ĵ������ʽ
	(setvar "fontalt" "tssdeng.shx") ;ָ���Ҳ���ָ���������ļ�ʱҪʹ�õ��滻����(ѡ���е��滻�����ļ�)
	;;;=====================================================
	;ѡ��ʽ�趨
	(setvar "pickfirst" 1);ѡ��ģʽ(��ѡ���ִ��)����qaflags��ƥ��
	(and (getvar "qaflags")(setvar "qaflags" 0));�����㸴�ƣ���pickfirst��ƥ��
	;QAFLAGS�Ǹ�δ�����ı�������������LISP��ʹ��EXPLODEը��һ��ѡ�񼯵�ʱ��ı��֡�
	;���QAFLAGS��0����ô��LISPִ��(command "explode" ss "")��ʱ�������ը��ѡ�񼯵ĵ�һ��ʵ�壬����ʵ��ը����.
	;���QAFLAGS��1������ը��ѡ������ʵ�塣
	;���������Ӱ���׼��EXPLODE�����ִ�С�
	;�����������������ֱ����ñ���QAFLAGSΪ0����1ʱ��(command "explode" (ssget) "")
	(setvar "pickadd" 1);ѡ��ʽΪ��ͳ
	(and (getvar "selectioncycling")(setvar "selectioncycling" 1));�����ص�����ѡ��ѭ����ֻ��ʾ��ǣ��޶Ի���
	(setvar "selectionpreview" 3);ѡ��Ԥ��,1δ��������ʱ��2����ڻ״̬ʱ��3���߶�
  (setvar "previewfilter" 31);ѡ��Ԥ��ģʽ
  (setvar "xclipframe" 0);����ñ߽粻��ʾ����ԭͼ������Ϊ0��ִ��ʱ�������ɣ�
	(setvar "DRAGMODE" 2) ;���ƽ����϶��Ķ������ʾ��ʽ��0�أ�1����2�Զ�(����ͼ�ε�Ԥ��)��
	;;;=====================================================
	;������Ҽ��������
	(setenv "AutoSnapSize" "7");�Զ���׽��Ǵ�С
	(setvar "pickbox" 7);ʰȡ�������Ĭ��ֵ5
	(setvar "apbox" 0);�رհп�
	(setvar "gripsize" 6);���üе��Ĵ�С
	(setvar "gripobjlimit" 3000);ѡ�����ʱ������ʾ�ļе���
	(setvar "mbuttonpan" 1);�м�����
	(and ;��seting�����в�������
		(setq cursorsize (av:getenv "cursorsize"))
		(setvar "cursorsize" cursorsize);ʮ�ֹ���С
	)
	(and ;��seting�����в�������
		(setq right (av:getenv "right"))
		(if (= right 1)(setq right 2)(setq right 11))
		(setvar "shortcutmenu" right);�Ҽ�������ݲ˵�
	)
	;=====================================================
	(setvar "DRAGP1" 1);�����������϶�ģʽ�µ����������
	(setvar "DRAGP2" 1);���ÿ����϶�ģʽ�µ����������
	(setvar "TREEMAX" 1000000);ͨ�����ƿռ��������˲������еĽڵ���Ŀ���Ӷ�����������ͼ��ʱռ�õ��ڴ�
	;�����ٶ��������
	(setvar "regenmode" 1);����ͼ�ε��Զ������ɣ�0��ֹ������Զ������ɣ����磬�ڽⶳͼ��ʱ����1����ĳЩ������Զ�������
	(vl-catch-all-apply 'vl-cmdf (list "viewres" "y" "1000"));������ʾƽ���ȣ���ƽ˳ʱ��re������
	(setvar "vtenable" 0);���Ŷ������ƣ�0Ϊ�޶�����1Ϊ�ж���
	(and (getvar "hpmaxlines")(setvar "hpmaxlines" 100000));ansi������������ڴ˲���ʱ����solid��ʾ������ٶȣ�Ĭ��ֵ1000000.
	(setvar "vtfps" 4);vtfps 1~7 Ĭ��7 ����ƽ�����ŵ��ٶ�
	(setvar "zoomfactor" 60);��껬�����ű�����Ĭ��Ϊ60��
	(and (getvar "gfxdx12")(setvar "gfxdx12" 0));�ر�Ӳ�����٣��������������������ƶ������ͺ�����⣬������2022�����ϰ汾
	(and (getvar "blockmrulist")(setvar "blockmrulist" 5));�����ڡ��顱ѡ���ġ����ʹ�á�ѡ�����ʾ�����ʹ�ÿ������
	(setvar "fillmode" 1);ָ��ʵ����䡢������䡢��άʵ��Ϳ������Ƿ����
	(setvar "hpscale" 1);�趨���ͼ����������
	(setenv "maxhatch" "1000000");���ͼ���������Ŀ�����������֡�ͼ�������̫�ܣ���̻��ߴ�̫С��
	(setvar "HPQUICKPREVIEW" 0) ;�ر����ͼ����ʾԤ��
	(setvar "MAXACTVP" 30) ;��󼤻��ӿ���(ʵ��N-1��)��Ĭ��N=64
	;=====================================================
	(setvar "COORDS" 0);����״̬���ϵĹ��λ�����������и��»��ǽ����ض�ʱ����¡�0�أ�1����2����ʱ��ʱ��Լ�����
	(setvar "HPDLGMODE" 0);���ơ�ͼ�����ͽ���ɫHATCH���Ի����Լ���ͼ�����༭GRADIENT���Ի������ʾ��Ĭ��2
	;(setvar "SELECTIONEFFECT" 0);ָ��������ѡ��״̬ʱ��ʹ�õ��Ӿ�Ч����0����,1��Ӳ�����ٴ�������״̬ʱ������ʾ����������Ч��
	(setvar "TEMPOVERRIDES" 0);�򿪻�ر����ڻ�ͼ������F8��ʱ�������0�أ�1��
	;ϵͳ �� Ӳ������ �� ȡ����ѡ �Զ����֤�����
	(vl-registry-write
		(strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\3DGS Configuration")
		"AutoUpdateCertDB" 0
	)
	;=====================================================
	(setvar "dynprompt" 0);����ʮ�ֹ�긽����ʾ��ʾ����������
	(and ;��info�����в�������
		(setq dyn (av:getenv "dyn"))
		(if (= dyn 0)(setq dyn 0)(setq dyn 3))
		(setvar "dynmode" dyn);�رչ��ָ������
	)
	;��RIBBONģʽ�´򿪸�դ����������¹رո�դ
	(cond ((= 1 (getvar "ribbonstate"))(setvar "gridmode" 1))(t (setvar "gridmode" 0)))
	(setvar "snapmode" 0);�ر�դ��׽
	(setvar "autosnap" 63);�򿪶Խ�׷��(���������ع���)
	(setvar "polarmode" 2);�����м��������׷��
	(setenv "AutoSnapTrackPath" "0");��ʾȫ��������׷��ʸ��
	(setvar "polarang" (/ pi 4));���ü���׷��������Ϊ��45��
	(setvar "trackpath" 0);������ʾ����Ͷ���׽׷�ٵĶ���·��
	(setvar "polardist" 1);���ü��Ჶ׽����
	(and (getvar "3dosmode")(setvar "3dosmode" 11));�ر���ά����׽
	;=====================================================
	;(setvar "osmode" (logior (getvar "osmode") 16384));�ظ�׽
	(setvar "osmode" (logand (getvar "osmode") 49151));����׽
	(setq os1 (getvar "osmode"))
	;���ù����������еĶ���׽
	(setq os2 (cond ((setq os2 (av:getenv "osmode"))) (t 7223)))
	;ʮ����ת��Ϊ�����ƣ����Ҳ�ȫ 15 ��������λ
	;ʾ����(d2b 4133)������ (0 0 1 00 0 0 0 0 1 0 0 1 0 1)
	(defun d2b (i / s) (repeat 15 (setq s (cons (logand i 1) s) i (lsh i -1))) s)
	;��ʾ��ǰ��׽
	(setq txt '("��׽�ر�" "ƽ����" "�ӳ���" "��۽���" "��������" "�����"
							 "�е�" "����" "�����" "����" "���޵�" "�ڵ�" "Բ��" "�е�" "�˵�"))
	(setq lst (reverse (vl-remove nil (mapcar '(lambda (x y) (if (= 1 x) y)) (d2b os2) txt))))
	(cond ((= os1 os2))
		(t (princ "\n����׽������Ϊ��") (foreach l lst (princ (strcat l "��"))))
	)
	;=====================================================
	(setvar "blipmode" 0);ȡ����������׵���
	(and (getvar "layouttab") (setvar "layouttab" 1));��ʾ���ֺ�ģ��ѡ�
	(and (getvar "statusbarautowrap") (vl-catch-all-apply 'setvar (list "statusbarautowrap" "off")));�ص�״̬���Զ�����
	(setenv "CursorCoordinatesPane" "0");����ʾͼ������
	;(setvar "UCSICON" 2);�ر�������ʾ
	(setvar "lwdisplay" 0);�߿�ر�
	(setvar "traynotify" 0);�����Ƿ���״̬��ϵͳ��������ʾ����֪ͨ
	(and (getvar "sysmon")(setvar "sysmon" 0));�ر�ϵͳ�����������еı仯֪ͨ
	;=====================================================
	(vl-catch-all-apply 'vl-cmdf (list "commandline"));��������
	(setenv "CmdLine.FontFace" "consolas");������������ʽ
	(and (getvar "inputsearchoptionflags")(setvar "inputsearchoptionflags" 4));�ر������Զ���ɺ͸���(-inputsearchoptions)
	(and (getvar "clipromptlines")(setvar "clipromptlines" 3));��������ʾ��ʷ��¼������
	;(setenv "CmdHistLines" "500");������������ʷ��¼����
	(vla-put-historylines display 500);ͬ��
	(and (getvar "inputsearchdelay")(setvar "inputsearchdelay" 100));�����б��ӳ�ʱ��
	(setvar "lockui" 0)
	;=====================================================
	;��������������
	(setvar "layoutregenctl" 2);ѡ��л�ʱ�����ɿ��ƣ�0=��ǰ�������ɣ�1=��ǰ���ֽ�ֹ�����ɣ�2=��ǰ�״�������ֹ
	(setvar "ucsfollow" 0);ucs����ʱ���������ӿڵ�Ӱ�죻0=����1=�ɡ�
	;=====================================================
	(setvar "maxactvp" 64) ;��ʾ������ȫ���ӿڣ����ֵ
	;(vla-put-LayoutDisplayMargins display :vlax-false);�ڲ��ֲ���ʾ�ɴ�ӡ����
	;(vla-put-LayoutDisplayPaper display :vlax-false);�ڲ��ֲ���ʾͼֽ����
	;(vla-put-LayoutDisplayPaperShadow display :vlax-false);�ڲ��ֲ���ʾͼֽ��Ӱ
	(setenv "ShowPaperMargins" "0");����ʾ�ɴ�ӡ����
	(setenv "ShowPaperBackground" "0");����ʾ����ͼֽ��Ӱ
	(setenv "ShowPrintBorder" "0");����ʾ����ͼֽ����
	(cond ;�ر����²����д����ӿ�
		((getvar "layoutcreateviewport")(setvar "layoutcreateviewport" 0));�߰汾
		(t (setenv "CreateViewports" "0"));�Ͱ汾
	)
	(setvar "PSTYLEPOLICY" 1) ;��ͼ��Ĭ��ʹ����ɫ��صĴ�ӡ��ʽ
	(if (av:findfile (setq ctb "�ڰ�ϸ��.ctb"))
		(vla-Put-StyleSheet (vla-get-activelayout *doc*) ctb) ;ѡ����Ӧ�Ĵ�ӡ��ʽ
	)
	;=====================================================
	(setenv "BEditBackground" "0") ;���ÿ�༭������ɫΪ<��ɫ>
	;=====================================================
	;������������
	(setvar "cecolor" "bylayer");��ɫ���
	(setvar "celtype" "bylayer");�������
	(setvar "celweight" -1);�߿����
	(setvar "psltscale" 0);����ʱ��ʹ���ӿڱ���
	(setvar "celtscale" 1);���õ�ǰ�������Ϊ1
	(cond ;ȫ���޸��½������ж�������ͱ�������ʾ���ߣ�Ĭ��ֵ��1
		((= (getvar "ltscale") (setq lts 1000)))
		(t
			(setvar "ltscale" lts)
			(vla-Regen *doc* acActiveViewport)
		)
	)
	;���˽���
	(*error* nil)
)

;===============================================================

;ȡ��obj�������ض�en��VLA��
(defun MJ:GetToolVla(obj en / lst n sym val)
	(vlax-for l obj (setq lst (cons (vla-get-name l) lst)))
	(setq lst (reverse lst))
	(setq lst (mapcar 'strcase lst))
	(setq sym (strcase en))
	(setq n (vl-position sym lst))
	(cond (n (vla-Item obj n)) (t nil))
)
;;˵��:ȡ���Զ������й���������˵�����VLA��
;;����:mnugs:�Զ���˵�����
;;����:mb: 1=�˵�����  2=����������
;;����:�˵���򹤾������VLA��
(defun MJ:GetMenusOrBars (mnugs mb / gs obj)
	(setq gs (vla-get-menugroups *ACAD*))
	(setq obj (MJ:GetToolVla gs mnugs))
	(setq mb (cond ((= 1 mb) vla-get-Menus) ((= 2 mb) vla-get-toolbars) (t nil)))
	(cond ((and mb obj)(mb obj)) (t nil))
)
;����������
;ʾ��(MJ:Toolbar "FSTL" "av_value" v);�����
;���� mgroup:�˵��� tbname:�������� v:���ؿ���
;����V  -1��1��,0��,����=����ѭ��
(defun MJ:Toolbar (mgroup tbname v / lst n tb)
	(setq tb (MJ:GetToolVla (MJ:GetMenusOrBars mgroup 2) tbname))
	(cond
		((member v (list -1 0 1 :vlax-true :vlax-false)))
		(T (setq lst '((:vlax-true . 1)(:vlax-false . 0)))
			(setq n (cdr (assoc (vla-get-visible tb) lst)))
			(setq v (abs (1- n)))
		)
	)
	(vla-put-visible tb v)
)


;;ɾ������������˵�
;(foreach l (list "����(&P)" "����(&H)")
;	(vl-catch-all-apply 'vla-removefrommenubar (list (MJ:GetToolVla (MJ:GetMenusOrBars "ACAD" 1) l)))
;)

;vla-get-menus
;vla-get-onmenubar
;vla-insertinmenubar
;vla-removefrommenubar



;;=============��������==============
;CAD�߰澫�����
(defun cad-theui1(/ layout menu statu)
	(and (setq menu (getvar "menubar"))(if (/= menu 0)(setvar "menubar" 0)));�˵���
	(and (getvar "ribbonstate")(command-s "RibbonClose"))
	(and (setq layout (getvar "layouttab"))(if(/= layout 0)(setvar "layouttab" 0)));����ѡ�
	(and (setq statu (getvar "statusbar"))(if(/= statu 0)(setvar "statusbar" 0)));״̬���ر�
	(and (/= (getvar "gridmode") 0) (setvar "gridmode" 0));դ��ģ�Ϳ������ֹ�
	(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(MJ:Toolbar "FSTL" "av_value" 1)
	;(and (getvar "RIBBONSTATE")(Backgroundcolor 0 0));ģ�ͺڣ�������
	(Backgroundcolor 0 0);ģ�ͺڣ�������
	(setenv "LayoutXhairPickboxEtc" "16777215");���ֹ���ɫ
	;(setenv "XhairPickboxEtc" "65535");ʮ�ֹ����ɫ
	;(setenv "Model Xhair use tint" "1");ΪX��Y��Z��Ⱦɫ
	(av:setenv "theui" 1)
	(princ "+����")
)
;CAD�߰澭�����
(defun cad-theui2()
	(and (getvar "menubar")(if(/=(getvar "menubar")1)(setvar "menubar" 1)));�˵���
	(and (getvar "RIBBONSTATE")(command-s "RibbonClose"))
	(and (getvar "layouttab")(if(/=(getvar "layouttab")1)(setvar "layouttab" 1)));����ѡ�
	(and (getvar "statusbar")(if(/=(getvar "statusbar")1)(setvar "statusbar" 1)));״̬��
	(and (/= (getvar "gridmode") 0) (setvar "gridmode" 0));դ��ģ�Ϳ������ֹ�
	(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(MJ:Toolbar "FSTL" "av_value" 1)
	(MJ:Toolbar "ACAD" "��ͼ" 1)
	(MJ:Toolbar "ACAD" "�޸�" 1)
	;(and (getvar "RIBBONSTATE")(Backgroundcolor 0 0));ģ�ͺڣ�������
	(Backgroundcolor 0 0);ģ�ͺڣ�������
	(setenv "LayoutXhairPickboxEtc" "16777215");���ֹ���ɫ
	;(setenv "XhairPickboxEtc" "16777215");ʮ�ֹ����ɫ
	;(setenv "Model Xhair use tint" "0");ΪX��Y��Z��Ⱦɫ
	(av:setenv "theui" 2)
	(princ "+����")
)
;CAD�߰泣�����
(defun cad-theui3()
	(and (getvar "menubar")(if(/=(getvar "menubar")0)(setvar "menubar" 0)));�˵���
	(and (getvar "RIBBONSTATE")(command-s "Ribbon"));ribbon
	(and (getvar "layouttab")(if(/=(getvar "layouttab")1)(setvar "layouttab" 1)));����ѡ�
	(and (getvar "statusbar")(if(/=(getvar "statusbar")1)(setvar "statusbar" 1)));״̬��
	(cond
		((= (getvar "tilemode") 1) (setvar "gridmode" 1));դ��ģ�Ϳ�
		(t (setvar "gridmode" 0));դ�񣬲��ֹ�
	)
	(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(MJ:Toolbar "FSTL" "av_value" 0)
	;(and (getvar "RIBBONSTATE")(Backgroundcolor 3156001 16777215));ģ�ͻң�������
	(Backgroundcolor 3156001 16777215);ģ�ͻң�������
	(setenv "LayoutXhairPickboxEtc" "0");���ֹ���ɫ
	(av:setenv "theui" 3)
	(princ "+����")
)
;�״ΰ�װʱ��ʼ����ǰ����
(cond
	((av:getenv "theui"))
	(t
		(cad-theui2);�л����������
		(princ "\n��F10�����л���ͬ���棡")
		(c:reavtlsenv);������ʼ��
	)
)
;CAD�������
(defun c:theui(/ ct ui)
	(princ "-->�����л�")
	(setvar "cmdecho" 0)
	;(vlax-for l (MJ:GetMenusOrBars "ACAD" 2) (vla-put-visible l 0))
	(setq ui (av:getenv "theui"))
	(setq ct (av:getenv "theui1"))
	(if (null ct) (setq ct 0))
	(if (getvar "ribbonstate")
		(cond
			((= 1 ui)(cad-theui2))
			((= 2 ui)(cad-theui3))
			((= 1 ct)(cad-theui1))
			((= 0 ct)(cad-theui2))
			(t nil)
		)
		(cond
			((= 1 ui)(cad-theui2))
			((= 2 ui)(cad-theui1))
			((= 3 ui)(cad-theui1))
			(t nil)
		)
	)
	(if (= 1 (getvar "cleanscreenstate"))
		(setvar "cleanscreenstate" 0)
	)
	(command "commandline")
	(setvar "cmdecho" 1)
	(princ)
)


;==============ȫ���л�=================
(defun c:CleanScreen(/ cs-cad-theui2-0 cs-cad-theui2-1 cs-cad-theui3-0 cs-cad-theui3-1)
	;�������ȫ��
	(defun cs-cad-theui2-0()
		(and (getvar "layouttab")(setvar "layouttab" 0));ģ�Ͳ��ֱ�ǩ��
		(and (getvar "statusbar")(setvar "statusbar" 0));״̬��
		(and (getvar "menubar")(setvar "menubar" 0));�˵���
		(command "cleanscreenon")
	)
	;�����������
	(defun cs-cad-theui2-1()
		(and (getvar "layouttab")(setvar "layouttab" 1))
		(and (getvar "statusbar")(setvar "statusbar" 1))
		(and (getvar "menubar")(setvar "menubar" 1))
		(command "cleanscreenoff")
	)
	;�߰����ȫ��
	(defun cs-cad-theui3-0()
		(and (getvar "layouttab")(setvar "layouttab" 0))
		(and (getvar "statusbar")(setvar "statusbar" 0))
		(command "cleanscreenon")
		(MJ:Toolbar "FSTL" "av_value" :vlax-true)
	)
	
	;�ر�ȫ��������
	;	(foreach l (mapcar '(lambda(x) (MJ:GetMenusOrBars x 2)) (list "ACAD" "FSTL"))
	;	(vlax-for s l (vla-put-visible s 0))
	;)
	
	;�߰��������
	(defun cs-cad-theui3-1()
		(and (getvar "layouttab")(setvar "layouttab" 1))
		(and (getvar "statusbar")(setvar "statusbar" 1))
		(command "cleanscreenoff")
		(MJ:Toolbar "FSTL" "av_value" :vlax-false)
	)
	;������ʼ
	(princ "-->ȫ���л�")
	(setvar "cmdecho" 0)
	(cond
		((and (= 1 (av:getenv "theui")))
			(MJ:Toolbar "FSTL" "av_value" "����")
		)
		((and
			 (= 2 (av:getenv "theui"))
			 (= 0 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui2-0)
		)
		((and
			 (= 2 (av:getenv "theui"))
			 (= 1 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui2-1)
		)
		((and
			 (= 3 (av:getenv "theui"))
			 (= 0 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui3-0)
		)
		((and
			 (= 3 (av:getenv "theui"))
			 (= 1 (getvar "cleanscreenstate"))
		 )
			(cs-cad-theui3-1)
		)
		(t nil)
	)
	(setvar "cmdecho" 1)
	(princ)
)



;=================��ԭ��ǰ����================

(defun c:reop(/ info)
	(setq info "��������ʼ��ȫ��������Ϣ��\n\n�Ƿ������")
	(if (= 1 (api-msgbox "������ʾ" info (+ 1 48 256 0 262144)))
		(progn
			(vlax-Invoke-Method
				(vla-get-Profiles (vlax-get-Property *ACAD* 'Preferences))
				'ResetProfile
				(getvar "cprofile")
			)
			(and (getvar "RIBBONSTATE")(setvar "gridmode" 1));դ��
			(and (getvar "secureload")(setvar "secureload" 0));��������λ�ò������ʾ
			(av:AddSupportPath (list (strcat *fstl_dir* "\\support")))
			(if (av:getenv "cursorsize")
				(vl-catch-all-apply 'vl-registry-delete (list *avzzts-reg-key* "cursorsize"))
			)
			(if av:unpgp (av:unpgp))
		)
	)
	(princ)
)

;===========���������===================
(princ)




