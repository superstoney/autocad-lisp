;�����������ļ���������
(defun c:dwg-purge-batch();����������
	(princ ">>>CAD������������������")
	(av:cmddrawpurge "dwg-purge-batch")
	(princ)
)
(defun c:dwg-purge();���ļ�����
	(princ "\n>>>CAD�������ֵ��ļ�����")
	(av:cmddrawpurge "dwg-purge")
	(princ)
)
(defun av:cmddrawpurge (cmd / av:drawpurgetips loaddrawpurge regsetdir)
	(defun av:drawpurgetips ()
		(prompt "
������˵��:��
Drawing Purge��һ��ǿ�������õ�cad���������ߡ�
���ɽ����������:��
(1)���ļ��俽��ʱ��\"dgn����\"�ᵼ���ļ��޴������������ݣ�
(2)�޷����ƺ�ճ��ͼԪ��
(3)��ʱ��������ѡ���ı���\"����\"ѡ����ϣ�
(4)ͼ���ļ���ʱ�����;
(5)��������п��ٸС�
")
	)
	;д��ע���·����Ϣ
	(defun regsetdir (path / folderloc reg-key)
		(setq reg-key
			(strcat "HKEY_CURRENT_USER\\"(vlax-product-key) "\\Profiles\\" (getvar "cprofile") "\\AVVADrawingPurge-072")
		)
		;д������·��
		(if (setq folderloc (findfile (strcat path "\\DwgPurgeSettings")))
			(progn
				(setq folderloc (strcat folderloc "\\"))
				(vl-registry-write reg-key "AVVADwgPurgeFolderLoc" folderloc)
				(vl-registry-write reg-key "AVVALanguage" "Chinese")
			)
			(progn
				(setq folderloc nil)
				(princ "\n�Ҳ����趨�����ļ��У�")
			)
		)
		folderloc
	)
	;����������DLL�ļ�
	(defun loaddrawpurge(/ file name path)
		(setq file (av:findfile "AVVADrawingPurge2018.dll"))
		(setq path (vl-filename-directory file))
		(if file
			(and (regsetdir path) (command-s "netload" file))
			(prompt "û���ҵ���������ļ���")
		)
	)
	;��������������
	(cond
		((>= *ver4* 2018)
			(if (vl-cmdf cmd) (progn (loaddrawpurge) (av:drawpurgetips)))
		)
		(t (c:cleanrubbish))
	)
)

;=================================================================
;�ļ����������嵥
(defun c:cleanrubbish (/ av:get-file-size av:try-time-len av:try-time-start clean1 clean2 dicts>n f fn llsheng:mpurge m n rep size0 size1 size2 time)
	;��������ض������������ʵ�
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
	;�������
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
	;��1��������
	(defun clean1 (/ n)
		;(repeat 1 (command "-purge" "a" "*" "n"));�˰취����Ч���Ժã����д�������
		(vla-PurgeAll *doc*);ɾ����������һ�ְ취��û�л���
		;ɾ�����鼰����Ϊ1���鶨��
		(vlax-for obj (vla-get-groups *doc*)
			(cond
				((< (vla-get-count obj)2) (vla-delete obj));ɾ�����鼰����Ϊ1���鶨��
				((wcmatch (vla-get-name obj) "'**") (vla-delete obj));ɾ��������
			)
		)
		;ͼ���д�������
		;(princ "+��������")
		;(command "_audit" "y");������⵽���κδ���
	)
	;��2��������,��ɾ������
	(defun clean2 (/ n)
		(llsheng:mpurge
			'((blocks);����δ���ÿ�
				 (blocks t);�����ͼ��
				 (groups);��ɢ����
				 (groups t);�������
				 (layers);����ͼ��
				 (Linetypes);��������
				 (textstyles);����������ʽ
				 (dimstyles);�����ע��ʽ
				 ;(Layouts);ɾ������
			 )
		)
	)
	;��ѯ�ļ���С����λm
	(defun av:get-file-size(fn)(rtos (/ (vl-file-size fn) 1e6) 2 2))
	;��ʼ����ʱ�䣬��λ�룬��������Ϊav:try-time-len
	(defun av:try-time-start()(getvar "millisecs"))
	;��������ʱ�䣬��λ�룬��ʼ����Ϊav:try-time-start
	(defun av:try-time-len (t1)(rtos (/ (- (getvar "millisecs") t1) 1000.000) 2 3))
	;��ʼ����
	(vl-load-com)
	(setvar "cmdecho" 0)
	;(textscr);չʾ�ı���
	(setq time (av:try-time-start));��ʼ����ʱ��
	(setq fn (strcat (getvar "dwgprefix") (getvar "dwgname")))
	(setq f (findfile fn))
	(if f (setq size0 (av:get-file-size fn)));ͳ������ǰ��С
	;���½������ͼ��
	(dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
	;��ʼ����
	(setq rep t size1 size0 m 0)
	(while rep
		(princ (strcat "\r����ִ�е�" (itoa (setq m (1+ m))) "������..."))
		(clean1);��1�ֱ�Դ������
		;(clean2);��2�ֿ�Դ������
		(and
			(setq n 100);���ִ���1000�Ķ�������
			(dicts>n n);�����ض������Ĵʵ�
		)
		(cond
			(f
				(vla-Save *doc*)
				(setq size2 (av:get-file-size fn));ͳ��������С
				(if (equal size1 size2 0.05) (setq rep nil))
				(setq size1 size2)
			)
			(t (setq rep nil))
		)
	)
	(princ (strcat
					 "\n>>>������ɣ���ʱ" (av:try-time-len time) "��"
					 (cond
						 (size2
							 (strcat "��֮ǰ" size0 "m��֮��" size2 "m��")
						 )
						 (t "���ļ���δ���棻")
					 )
				 )
	)
	(princ "����DICTS�ɶ������ֵ��������")
	(setvar "cmdecho" 1)
	(princ)
)



; �Ե�ǰͼ����ĳ���ʵ�����б�
(defun c:Dicts (/ *error* dictslst i k:adddel4str k:catchapply pos tmppos tmpvar)
	(defun *error* (x) ;������
		;(Graphscr);�˳��ı���Ļ
		(vla-endundomark *DOC*) ;��������
		;(setvar "CMDECHO" 1)
		(princ)
	)
	;������ɹ���ץȡ��������ɹ���ִ�г���
	(defun K:CatchApply (fun args / result)
		(setq result (vl-catch-all-apply (if (= 'SYM (type fun)) fun (function fun)) args))
		(cond
			((vl-catch-all-error-p result)
				(setq result nil)
			)
			(t result)
		)
	)
	;��������Ų����ַ���,ǿ������λ����FlagΪTʱ����ǰ��
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
  (vla-startundomark *DOC*) ;��¼����
	(textscr);�л����ı���Ļ
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
	(princ (strcat "\n\n�������� ��ǰ�ļ���ɾ���Ĵʵ�����Ϊ<" (rtos (length DictsLst) 2 0) "> �����"))
	;(setvar "CMDECHO" 0)
	(while
		(progn
			(initget (+ 2 4)) ;����Ǹ�
			(setq TmpPos
				(cond
					((getint (strcat "\n�������������,����ɾ���ĸ��ֵ�<" (rtos Pos 2 0)">: ")))
					((fix Pos))
				)
			)
			(cond
				((<= TmpPos (length DictsLst))
					(setq Pos TmpPos)
					(dictremove (namedobjdict) (nth (1- Pos) DictsLst));ɾ��ָ�����ֵ�
					;(dictremove (namedobjdict) "ACAD_DGNLineStyleCOMP");ɾ���ֵ��е�DGN����
					;(command "_.-scalelistedit" "_R" "_Y" "_E");�����ӿڱ���
					;(command "_.-PUrge" "_R" "*" "_N");ɾ��ע�����
					(vla-PurgeAll *DOC*);����ȫ��
					;(while (> (getvar "CMDACTIVE") 0) (command PAUSE));�ȴ�ǰ����������
					;(princ (strcat "\n�������� ɾ���ֵ���Kucha�Ż�����������<DICTS>���ٴ����г��� �����"))
					;(Graphscr);�˳��ı���Ļ
					Nil ;�˳�ѭ��
				)
				((> TmpPos (length DictsLst))
					(princ (strcat "\n�������� �����б�Χ,���������룡 �����"))
					T ;����ѭ��
				)
			)
		)
	)
  (*error* nil)
)


(princ)
