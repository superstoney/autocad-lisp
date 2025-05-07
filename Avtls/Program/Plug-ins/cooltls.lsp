;ͼֽ�������Ź��߼�
(defun c:cooltls (/ *error* dclid dcls key b)
	(defun *error* (msg)(setvar "cmdecho" 1)(princ))
	(setvar "cmdecho" 0)
	(setq dcls (list
							 "cooltls:dialog {"
							 "    initial_focus = \"cancel\" ;"
							 "    label = \"��Aά���졿ͼֽ�������Ź��߼�\" ;"
							 "    :boxed_column {label = \"1.��������\" ;"
							 "        :row {"
							 "            :button {key = \"delrecover\" ;label = \"��Recover(&F)\" ;width = 16 ;}"
							 "            :button {key = \"linkclip\" ;label = \"ͨճ����(&B)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"2.��������\" ;"
							 "        :row {"
							 "            :button {key = \"EPS\" ;label = \"���δ���(&C)\" ;width = 16 ;}"
							 "            :button {key = \"delstamp\" ;label = \"�ļ�ȥ��(&W)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"3.�汾ת��\" ;"
							 "        :row {"
							 "            :button {key = \"GetHverLst\" ;label = \"�߰��ѯ(&G)\" ;width = 16 ;}"
							 "            :button {key = \"DwgConvert\" ;label = \"DWG ת��(&Z)\" ;width = 16 ;}"
							 "        }"
							 "        :row {"
							 "            :button {key = \"AcmeCAD\" ;label = \"Acme CAD Converter 2022 (&A)\" ;}"
							 "        }}"
							 "    :boxed_column {label = \"4.ͼ�ε���\" ;"
							 "        :row {"
							 "            :button {key = \"loadtch\" ;label = \"������ͼ(&T)\" ;width = 16 ;}"
							 "            :button {label = \"�رձ����\" ; is_cancel = true ; width = 16 ;}"
							 ;"            :button {key = \"BClipbrd\" ;label = \"BetterWmf(&E)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"5.ɱ������\" ;"
							 "        :row {"
							 "            :button {key = \"kill-hy\" ;label = \"��ҵɱ��(&K)\" ;width = 16 ;}"
							 "            :button {key = \"dwg-purge-batch\" ;label = \"��������(&L)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"6.��ͼ����\" ;"
							 "        :row {"
							 "            :button {key = \"sync\" ;label = \"�ӿ�ͬ��(&D)\" ;width = 16 ;}"
							 "            :button {key = \"compare\" ;label = \"DWG �Ƚ�(&Q)\" ;width = 16 ;}"
							 "        }}"
							 "    :boxed_column {label = \"7.��������\" ;"
							 "        :row {"
							 "            :button {key = \"batchprint\" ;label = \"������ӡ(&P)\" ;width = 16 ;}"
							 "            :button {key = \"jpgtodxf\" ;label = \"λͼʸ��(&S)\" ;width = 16 ;}"
							 "        }}"
							 ;"cancel_button;"
							 ;;":button {is_cancel = true ;"
							 ;;"is_enabled = false ;"
							 ;;"label = \"ȡ���Ի���\" ;}"
							 ;;":button {fixed_width = true ; is_cancel = true ;width = 5 ;}"
							 "}"
						 )
	)
	(setq dclid (av:loaddialog dcls));�½���ʱDCL���ز�ɾ��
	(new_dialog "cooltls" dclid)
	
	(action_tile "delrecover" "(setq b \"delrecover\")(done_dialog 1)");����recover���Ƶ���
	(action_tile "linkclip" "(setq b \"linkclip\")(done_dialog 1)");�����޷����Ƶ�ճ����ĵ���
	
	(action_tile "EPS" "(setq b \"EPS\")(done_dialog 1)");������δ��ǵ���
	(action_tile "delstamp" "(setq b \"delstamp\")(done_dialog 1)");�ļ�����ȥ����
	
	(action_tile "GetHverLst" "(setq b \"GetHverLst\")(done_dialog 1)");��ѯCAD�޷��򿪵�DWG�߰��ļ�
	(action_tile "DwgConvert" "(setq b \"DwgConvert\")(done_dialog 1)");DWGת��
	(action_tile "AcmeCAD" "(setq b \"AcmeCAD\")(done_dialog 1)");DWG�汾ת����
	
	(action_tile "loadtch" "(setq b \"loadtch\")(done_dialog 1)");����
	
	(action_tile "kill-hy" "(setq b \"kill-hy\")(done_dialog 1)");��������
	(action_tile "dwg-purge-batch" "(setq b \"dwg-purge-batch\")(done_dialog 1)");���������ļ�����
	
	(action_tile "sync" "(setq b \"sync\")(done_dialog 1)");ͬ����ͼ
	(action_tile "compare" "(setq b \"compare\")(done_dialog 1)");��ͼ�Ƚ�
	
	(action_tile "batchprint" "(setq b \"batchprint\")(done_dialog 1)");������ӡ
	(action_tile "jpgtodxf" "(setq b \"jpgtodxf\")(done_dialog 1)");λͼʸ����
	
	(action_tile "cancel" "(done_dialog 0)");ȡ����ť˵��
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((null b))
		((wcmatch b "sync")(av:SyncDwgs));�ӿ�ͬ����ͼ
		((wcmatch b "compare")
			(princ "\n˵����ѡ��ͼ���뵱ǰͼ�ζԱȣ�������������ʾͬһͼ�ε������޶���ͬͼ��֮��Ĳ���")
			(vla-SendCommand *doc* "compare ")
		)
		((wcmatch b "loadtch")
			(princ "˵���������������T3��ֽ����")
			(vla-SendCommand *doc* "loadtchdcl ")
		)
		((wcmatch b "delrecover")
			(princ "˵�������ͼ�θ���ʱ��Recover����")
			(av:delrecover)
		)
		((wcmatch b "linkclip")
			(princ "˵��������\"�޷����Ƶ�������\"����")
			(terpri)
			(av:linkclip)
		)
		((wcmatch b "delstamp")
			;�ҵ�������DXF��ʽȥ���������
			(av:delstamp)
		)
		((wcmatch b "EPS")
			(princ "˵����EPS2.0��ӡ���ǲ����������У��������ڵ������½�")
			(startapp (av:findfile "EPS2.0.exe"))
		)
		((wcmatch b "GetHverLst")
			(princ "˵������ѯ�޷��򿪵�DWG�߰��ļ�")
			(vla-SendCommand *doc* "GetHverLst ")
		)
		((wcmatch b "DwgConvert")
			(princ "˵����DWG�汾ת��")
			(vla-SendCommand *doc* "DwgConvert ")
		)
		((wcmatch b "AcmeCAD")
			(princ "˵����CAD�ļ��汾ת��������ͬʱͼ�β鿴��תPDF��תλͼ��ȥ��������ǡ�ȥ���Ƶ������ļ��޸���ѹ��")
			(startapp (av:findfile "AcmeCAD.exe"))
		)
		((wcmatch b "batchprint")
			(princ "˵����������ӡͼֽ(langjs)")
			(cond (c:pldy) (t (vl-load-all (av:findfile "PLDY������ӡ.vlx"))))
			(c:pldy)
		)
		((wcmatch b "jpgtodxf")
			(princ "\n˵����λͼʸ����")
			(startapp (av:findfile "DocRdr.exe"))
		)
		((wcmatch b "dwg-purge-batch")
			(princ "˵��������ͼ��������DWG�ļ�����")
			(vla-SendCommand *doc* "dwg-purge-batch ")
		)
		((wcmatch b "kill-hy")
			(princ "˵������ҵCAD������ɱ����")
			(startapp (av:findfile "CAD������ɱ����.exe"))
		)
		(t nil)
	)
	(*error* nil);�ָ�ԭ�в���
)
;;=========================================================
;ͬ����ͼ
(defun av:SyncDwgs(/ agdt)
	(cond
		((and
			 (>= *ver4* 2013)
			 (setq agdt (av:findfile "AganVportSyn2013.dll"))
		 )
			(command-s "netload" agdt)
			(command-s "agdt")
			(princ "\nҲ��������agdt��ִ��")
		)
		(t (princ "û�д�CAD�汾��ͬ�������"))
	)
)
;;���浱ǰ�հ��ĵ���ɾ�����ļ�
(defun av:saveasdraw(file suf / ext fullname name path)
	(vla-zoomextents *acad*)
	(setq path (vl-filename-directory file))
	(setq name (vl-filename-base file))
	(setq ext (vl-filename-extension file))
	(setq fname (strcat path "\\" name suf ".dwg"))
	(vla-SaveAs *doc* fname ac2004_DWG)
	(command-s "qsave")
	(while (null (vl-file-delete file))
		(vla-close (vla-item *docs* (strcat name ext)) :vlax-false)
	)
)
;��������ʱ��RECOVER����
(defun av:delrecover (/ *error* av:xref file1 file2 noexp path)
	;�ж��Ƿ�ֽ�ɹ�
	(defun noexp (name / lst xrefs)
		(vlax-for b (vla-get-Blocks *doc*)
			(if (= (vla-get-IsXRef b) :vlax-true)
				(setq xrefs (cons (list b (vla-get-name b)) xrefs))
			)
		)
		(setq lst (vl-remove-if '(lambda (x) (/= (cadr x) name)) xrefs))
		(princ (caar lst))
	)
	;��cmd�������޷��÷�Ӧ������ɣ�
	(defun av:xref (file / e name s ss)
		;�ⲿ���ղ���󶨲��ֽ�
		(command-s "-xref" "A" file "0,0,0" "1" "" "0")
		(and (getvar "bindtype") (setvar "bindtype" 1));ͼ�㲻��ǰ׺
		(setq name (vl-filename-base file))
		(command-s "-xref" "Bind" name)
		(if (setq s (ssget "X" (list'(0 . "insert")(cons 2 name))))
			(while (setq e (ssname s 0))
				(ssdel e s)
				(vla-Explode (Vlax-Ename->Vla-Object e))
				(entdel e)
			)
		)
		;�ж��Ƿ���Ҫɾ�������ĵ�
		(if (noexp file)
			(progn
				(alert "RECOVER����δ����������������תT3��ʵ�֣�")
				(command-s "close" "y")
			)
			(progn
				(av:saveasdraw file "-QC")
				(princ "\n>>>����ʱ���ֵ�RECOVER�����������ϣ�")
				(princ "�����Ȼ���ڵ�������תT3��ʽ��")
				;ԭ�����ɾ���ֵ��е�������ʶ
				;(dictremove (namedobjdict) "TCH_DBCONFIG")
			)
		)
	)
	(defun *error*(str)(setvar "cmdecho" 1)(princ))
	;���ܰɣ����꣡
	(setvar "cmdecho" 0)
	(cond
		((setq file1 (findfile (vla-get-FullName *doc*)))
			(command-s "qsave")
			(vl-bb-set "pathforxref" file1)
			(alert "���ܣ���������ʱ��RECOVER������\n\nԭ�������汾�����ݣ�\n\n���������ڼ����򿪵Ŀհ��ĵ����ٴ����д��������")
			(vla-activate (vla-add *docs* ""))
		)
		((setq file2 (vl-bb-ref "pathforxref"))
			(vl-bb-set "pathforxref" nil)
		)
		(t (setq path (vla-item (vlax-get (vlax-create-object "wscript.shell") 'specialfolders) "desktop"))
			(setq file2 (getfiled "ѡ����Ҫ�����������ļ�" (strcat path "\\") "dwg" 0))
			;(setq file file2)
		)
	)
	(if file2 (av:xref file2))
	(*error* nil)
)
;��������ʱ\"�޷����Ƶ�������\"����
(defun av:linkclip(/ *error* file)
	(defun *error*(str)(setvar "cmdecho" 1)(princ))
	;���ܰɣ����꣡
	(setvar "cmdecho" 0)
	(cond
		((setq file (findfile (vla-get-FullName *doc*)))
			(command-s "qsave")
			(vl-bb-set "linkclip" file)
			(command-s "copylink")
			(alert
				"���ܣ���������ʱ\"�޷����Ƶ�������\"������
\n\n���������ڼ����򿪵Ŀհ��ĵ��У�ֱ��\"CTRL+V\"ճ�������ٴ����д��������"
			)
			(vla-activate (vla-add *docs* ""))
		)
		((setq file (vl-bb-ref "linkclip"))
			(vl-bb-set "linkclip" nil)
			(command-s "pasteclip" "0,0")
			(av:saveasdraw file "-QC")
			(princ "\n>>>����ʱ���ֵ�\"�޷����Ƶ�������\"������������")
		)
		(t (princ "������\"�޷����Ƶ�������\"�������ĵ������б����"))
	)
	(*error* nil)
)
;�������������
(defun av:delstamp (/ bit dwgname dxf file fn)
	(setq bit (cdr (assoc *acadlen* '((39 . "") (47 . "_X64")))))
	(setq fn (strcat "BladeR" (itoa *ver2*) bit ".arx"))
	(cond
		((setq file (av:findfile fn))
			(if (null (member (strcase fn t) (arx))) (arxload file))
		)
		((null (setq dwgname (findfile (vla-get-FullName *doc*))))
			(princ "\n���ѣ��ļ���δ���棬��������Ҫȥ��")
		)
		(t (setq dxf (vl-filename-mktemp nil nil nil))
			(vla-SaveAs *doc* dxf ac2004_dxf)
			(av:saveasdraw dwgname "-QC")
			(princ "\n>>>��ͨ��DXF��ʽ������������ǣ�������Ϊ2004��ʽ��")
		)
		(t nil)
	)
)

;=====================================================================================
;��ѯCAD�޷��򿪵�DWG�߰��ļ�
(defun c:GetHverLst(/ acmecad av:getdwgver dir f file key len lst lst-all lst-all-msg lst-high lst-high-msg msg n)
	(defun av:getdwgver(file / cadver dwgver handle header lst)
		(setq handle (open file "r"))
		(setq header (substr (read-line Handle) 1 6))
		(close handle)
		(setq lst
			(list
				;(list "AC1018" 3000) ;��Ӧ���ԣ���������
				(list "AC1032" 2018)
				(list "AC1027" 2013)
				(list "AC1024" 2010)
				(list "AC1021" 2007)
				(list "AC1018" 2004)
				(list "AC1015" 2000)
				(list "AC1014" 1998)
			)
		)
		(cadr (assoc header lst))
	)
	(setvar "cmdecho" 0)
	;���ض�ѡ�ļ��Ի���
	;ȡ���ļ��б�
	(cond
		((progn
			 (setq lst (av:getexpdirlst) len (length lst))
			 (setq dir (cond (lst (nth (1- len) lst)) (t nil)))
			 (setq lst-all (try-getfiles "��ѡ��DWG�ļ���ȡ��ѡ��ᴩ͸��ǰ�򿪵��ļ��С�" dir nil "*.dwg"))
		 )
		)
		(T
			(setq  n 0)
			;ȷ�ϴ�͸�ļ���
			(cond
				((null lst))
				((= len 1)(setq dir (car lst)))
				((> len 1)
					(princ "\nû��ѡ���ļ�������ѯ����͸�򿪵��ļ���")
					(princ "\n��ǰ�򿪵��������ļ���")
					(foreach dir lst (princ (strcat "\n�ļ���" (itoa (setq n (1+ n))) ": " dir)))
					(setq key (getint (strcat "\n��ѡ���ѯ�ļ���:<Ĭ��" (itoa len) ">")))
					(setq dir (nth (1- (cond (key) (t len))) lst))
				)
			)
			;��ʾ��ǰ�ļ���
			(if dir
				(progn
					(princ (strcat "\n��ѯ����͸�ļ��У�" dir))
					;ȷ���ļ��б�
					(setq lst-all (vl-catch-all-apply 'kk-getfiles (list dir "*.dwg" 1)))
					;ȷ���ļ��в㼶����
					(if (vl-catch-all-error-p lst-all) (setq msg (vl-catch-all-error-message lst-all)))
					;���ݴ��ļ������ͷֱ���ʾ��Ϣ
					(cond
						((and msg (wcmatch msg "�ļ����Ͷ���̫��"))
							(princ "\n�ļ��в㼶̫�࣬��������ļ��й���Ŀ¼")
							(setq lst-all nil) ;ȷ��û�к�����Ϣ��ʾ
						)
						(T nil)
					)
				)
				(princ "\n��ǰû�д򿪵��ļ���")
			)
		)
	)
	(cond ;ȡ�ø߰��ļ��б�
		(lst-all
			(setq lst-all-msg (strcat "����" (itoa (length lst-all)) "��dwg�ļ�"))
			(setq lst-high
				(mapcar '(lambda(f) (if (> (av:getdwgver f) *ver4*) f)) lst-all)
			)
			(setq lst-high (vl-remove-if '(lambda (l) (null l)) lst-high))
		)
		(t nil)
	)
	(cond ;�ڼ��±����г��ļ��嵥
		(lst-high
			(setq lst-high-msg (strcat "��ѯ��" (itoa (length lst-high)) "���߰��ļ�"))
			(setq msg (strcat lst-all-msg "��" lst-high-msg "����ǰCAD" (itoa *ver4*) "�򲻿�������а汾ת�����ø߰�CAD��"))
			(setq lst (append (list msg "") lst-high))
			(setq file	(vl-filename-mktemp nil nil ".txt"))
			(setq f (open file "a"))
			(foreach l lst (write-line l f))
			(close f)
			(startapp "notepad" file)
			(and lst-high (setq AcmeCAD (av:findfile "AcmeCAD.exe"))
				(progn(getint "\n�س������汾ת����")(startapp AcmeCAD))
			)
		)
		((and dir lst-all)
			(princ (strcat "\n>>>�ڵ�ǰ�򿪵��ļ��м������ļ����У�" lst-all-msg "��û�е�ǰCAD�򲻿��ĸ߰汾dwg�ļ�"))
		)
		(lst-all
			(princ (strcat "\n>>>ѡ����ļ��У�" lst-all-msg "��û�е�ǰCAD�򲻿��ĸ߰汾dwg�ļ�"))
		)
		(msg nil) ;�ļ����޷���͸
		(dir (princ "\n>>>�ļ��м������ļ�����û�з���dwg�ļ�"))
		(T nil)
	)
	(setvar "cmdecho" 1)
	(princ)
)
;=====================================================================================

;����ҵCAD����ת������
(defun av:transcad (/ fn)
	(setq fn "TransCADFile.exe")
	(cond
		((and AV-EXE-IsRun-Kill (AV-EXE-IsRun-Kill fn nil))
			(princ "\n��ʾ��CAD�Ʒ������ں�̨���У��������½Ǵ򿪣�")
		)
		(t (startapp (av:findfile fn))
			(princ "\n��ʾ����ҵCAD�Ʒ���ת�������ļ�Ч���Ƚ���ǿ������ѡ���������������")
		)
	)
	(princ)
)
;�������������岢����
(defun c:loadtchdcl (/ b cmdtch dclid dcls fn key)
	(setq dcls
		(list
			"tchkernal:dialog {"
			"    initial_focus = \"13\" ;"
			"    label = \"��������T3��ʽ\" ;"
			"    :boxed_column {"
			"        label = \"�������\" ;"
			"        :radio_button {"
			"            key = \"11\" ;"
			"            label = \"�ֽ����\" ;"
			"        }"
			"        :radio_button {"
			"            key = \"12\" ;"
			"            label = \"��ͼ����(t3)\" ;"
			"        }"
			"        :radio_button {"
			"            key = \"13\" ;"
			"            label = \"��������\" ;"
			"        }"
			"        :radio_button {"
			"            key = \"14\" ;"
			"            label = \"ͼֽ����\" ;"
			"        }"
			"    }"
			"    :boxed_column {"
			"        label = \"��ҵCAD\" ;"
			"        :radio_button {"
			"            key = \"21\" ;"
			"            label = \"�Ʒ�����������\" ;"
			"        }"
			"    }"
			"    :row {"
			"        :button {"
			"            is_default = true ;"
			"            key = \"accept\" ;"
			"            label = \"ȷ��\" ;"
			"        }"
			"        :button {"
			"            is_cancel = true ;"
			"            key = \"cancel\" ;"
			"            label = \"ȡ��\" ;"
			"        }"
			"    }"
			"}"
		)
	)
	(defun cmdtch(b)
		(cond
			((= b 11)
				(command (cond ((> *ver4* 2012) "texplode") (t "T81_TExplode")))
			)
			((= b 12)
				(command (cond ((> *ver4* 2012) "tsaveas") (t "T81_TSaveAs")))
			)
			((= b 13)
				(cond ((> *ver4* 2012) (command "tbatsave")) (t (princ "�Ͱ汾�޴�����")))
			)
			((= b 14)
				(getint "����������ɲ��ֽܷ�ʹ�ӡ��TCH_PROTECT_ENTITYͼ�飬�Һ����޷��ָ������������ <�س�����>")
				(command "tprotect")
			)
			((= b 21) (av:transcad))
			(t nil)
		)
	)
	(cond
		((av:tcharx))
		((setq fn (av:tchfile)) (arxload fn "\n���棺δ�ܳɹ������������!"))
		(t (av:transcad))
	)
	(cond
		((av:tcharx)
			(setq dclid (av:loaddialog dcls));�½���ʱDCL���ز�ɾ��
			(new_dialog "tchkernal" dclid)
			(action_tile "11" "(setq b 11)")
			(action_tile "12" "(setq b 12)")
			(action_tile "13" "(setq b 13)")
			(action_tile "14" "(setq b 14)")
			(action_tile "21" "(setq b 21)")
			(action_tile "accept" "(done_dialog 1)")
			(setq key (start_dialog))
			(unload_dialog dclid)
			(if (= 1 key)(cmdtch b))
			(setq b nil)
		)
		(t nil)
	)
	(princ)
)
;=========================================================
;�ж��Ƿ����ĳ���ʵ�
(defun av:indict (key)
	(member key
		(mapcar 'cdr
			(vl-remove-if
				'(lambda (a) (/= 3 (car a)))
				(entget (namedobjdict))
			)
		)
	)
)
;�ж��Ƿ��д���,����(av:inproxy "AcDbZombieEntity")
(defun av:inproxy (proxy / i j)
  (vl-load-com)
  (setq i 0 j 0)
  (vlax-for o1 (vla-get-blocks *doc*)
    (if (= proxy (vla-get-objectname o1))
      (setq i (1+ i))
    )
    (vlax-for o2 o1
      (if (= proxy (vla-get-objectname o2))
        (setq j (1+ j))
      )
    )
  )
	(or (> i 0)(> j 0))
)

;ɾ���ֵ��е�������ʶ
;(dictremove (namedobjdict) "TCH_DBCONFIG")

;ͼֽ��ʱ���ж��Ƿ�Ϊ�����ļ���������ز��
(defun loadtchkernal ()
	(cond
		((av:tcharx))
		((or (av:indict "TCH_DBCONFIG") (av:inproxy "AcDbZombieEntity"))
			(if (arxload (av:tchfile) "\n���棺��������޷��������أ�������ͼԪ�޷�������ʾ��")
				(princ "\n>>>��������Ѽ��أ�����ͼ���Ƿ���ʾ������")
			)
		)
		(t nil)
	)
	(vl-acad-undefun 'loadtchkernal)
)
(loadtchkernal)
;����ֱ��������ļ��������������λ�ڲ˵���
(defun c:tsaveass (/ key loadtcharx msg tchfile)
	(defun loadtcharx(/ key msg)
		(initget "Y N")
		(setq msg "\n�Ƿ�ǿ�Ƽ����������[��(Y)/��(N)]��<Ĭ��Ϊ��>")
		(setq key (getkword msg))
		(and
			(= key "Y")
			(arxload (av:tchfile))
			(princ "\n>>>��������Ѿ�������ɣ����赼�������ٴ����б����")
		)
	)
	;������ʼ
	(setvar "cmdecho" 0)
	(setq tchfile (av:tchfile))
	(cond
		((av:tcharx)
			;(princ "-->������ͼ����T3��ʽ")
			(command (cond ((> *ver4* 2012) "tsaveas") (t "T81_TSaveAs")))
		)
		((and
			 (or (av:indict "TCH_DBCONFIG") (av:inproxy "AcDbZombieEntity"))
			 tchfile
		 )
			;(princ "-->����������ز���ͼ����T3��ʽ")
			(arxload tchfile "\n���棺δ�ܳɹ������������!")
			(command (cond ((> *ver4* 2012) "tsaveas") (t "T81_TSaveAs")))
		)
		(tchfile
			(princ "\n��ʾ����ǰ�ļ������������ɣ���������������")
			(loadtcharx)
		)
		(T
			(getint "\nû�д˰汾����Ӧ������������س���������������ҵCAD����ת������")
			(av:transcad)
		)
	)
	(if av:initotittle (av:initotittle))
	(setvar "CMDECHO" 1)
	(princ)
)
(princ)












