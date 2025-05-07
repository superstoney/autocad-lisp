(vl-load-com)

;������ؿ�ݼ�������unpgp����Ϊȫ�ֺ�����������
(defun av:acad-pgp(/ av:acad-pgp-autodesk)
	(defun av:acad-pgp-autodesk(/ lst p runcmd spl)
		(defun spl(sym n)(terpri)(repeat n (princ sym)))
		(defun runcmd(lst s / a1 a2 a3 cmd)
			(setq a1 (car lst) a2 (cadr lst) a3 (caddr lst))
			(setq cmd (strcat "(defun c:" a1 "()(princ \"" a2 "\")(command " a3 ")(princ))"))
			(eval (read cmd)) (terpri)
			(and s (princ (strcat a1 " " a2)))
		)
		(setq lst
			(list
				(list "RA" "-->���� " "\"ray\"")
				
				(list "FV" "-->�ƶ� " "\"move\"")
				(list "CC" "-->�������� " "\"copy\" (ssget) \"\" \"m\"")
				(list "WW" "-->���� " "\"mirror\"")
				(list "FS" "-->ͨ��ƫ�� " "\"offset\" \"t\"")
				(list "SC" "-->�������� " "\"scale\" (ssget) \"\" pause \"r\" pause pause pause")
				(list "MA" "-->����ƥ�� " "\"matchprop\"")
				
				(list "DY" "-->��̬���� " "\"lengthen\" \"dy\"")
				(list "3R" "-->��ά��̬�۲��� " "\"3dorbit\"")
				
				(list "TMP" "-->��ʱ�ļ��� " "(av:opendir (getenv \"tmp\") nil)")
			)
		)
		(setq p (/= 1 (av:getenv "mypgp")))
		(and p (spl "=" 40))
		(foreach l lst (runcmd l p))
		(and p (spl "=" 40))
		(av:setenv "mypgp" 1)
	)
	;��ʼ����������
	(av:acad-pgp-autodesk)
	;�˺�������seting��ʹ��
	(defun c:unpgp()
		(foreach l
			(mapcar '(lambda(x) (eval (read (strcat "'c:" x))))
				(list "fg" "ra" "fv" "cc" "ww" "fs" "sc" "dy" "3r" "tmp" "unpgp" "ar")
			)
			(vl-acad-undefun l)
		)
		(av:setenv "mypgp" 0)
		(princ)
	)
	(princ)
)
;��ͼ�Զ��������ּ�
(and (= 1 (av:getenv "mypgp"))(av:acad-pgp))


;��������(�жԻ���)
(defun c:ar(/ cmd)
	(setq cmd
		(cond
			((>= *ver4* 2012) "arrayclassic")
			(t "array")
		)
	)
	(command-s cmd)
)

;;==============================================================
;��VBA�½�ģ��
(defun c:qqnew()
	(vla-activate (vla-add *docs* (findfile "acadiso.dwt")))
)
;���Կ���
(defun c:PrONOFF ()
	(IF (= (getvar "opmstate") 1)(vl-cmdf "propertiesclose")(vl-cmdf "properties"))
	(princ)
)
;�ļ�������
(defun c:av:rename(/ rename1 rename2)
	;����������������
	(defun rename1(/ dclid dcls fname1 fname2 key name1 path)
		(setq dcls
			(list
				"RENAME:dialog {"
				"initial_focus = \"name\" ;"
				"    label = \"�޸��ļ�����\" ;"
				"    :row {"
				"        children_fixed_height = true ;"
				"        :edit_box {"
				"            key = \"name\" ;"
				"        }"
				"        ok_only;"
				"    }"
				"    :button {"
				"        is_cancel = true ;"
				"        is_enabled = false ;"
				"        label = \"ԭ���ļ��������������ԭ·������ɾ��ԭ�ļ�!\" ;"
				"    }"
				"}"
			))
		(setq fname1 (vla-get-FullName *doc*))
		(if (wcmatch "" fname1)
			(vla-SendCommand *doc* "qsave ")
			(progn
				(setq path (vl-filename-directory fname1))
				(setq name1 (vl-filename-base fname1))
				(setq dclid (av:loaddialog dcls));�½���ʱDCL���ز�ɾ��
				(new_dialog "RENAME" dclid)
				(set_tile "name" name1)
				(action_tile "accept"
					"(setq name2 (get_tile \"name\"))(done_dialog 1)"
				)
				(setq key (start_dialog))
				(unload_dialog dclid)
			)
		)
		(cond
			((/= key 1))
			((equal name2 name1)
				(princ ">>>�ļ���û�б仯��")
			)
			(name2
				(setq fname2 (strcat path "\\" name2 ".dwg"))
				(vla-SaveAs *doc* fname2 ac2004_DWG)
				(princ (strcat ">>>�ļ����Ѹ�Ϊ:" name2))
				(command-s "qsave")
				(vl-file-delete fname1)
			)
			(t nil)
		)
	)
	;���������������ʱ���׺
	(defun rename2(/ dd file1 hh id mm mo n name1 name2 path pre ss suf time yy)
		(setq file1 (vla-get-FullName *doc*))
		(cond
			((wcmatch "" file1)
				(vla-SendCommand *doc* "qsave ")
			)
			(t
				(setq path (vl-filename-directory file1))
				(setq name1 (vl-filename-base file1))
				(setq id " - ");ǰ���ʶ��
				(setq n (vl-string-search id name1))
				(setq pre (if n (substr name1 1 n) name1))
				(setq time (av:gettime "YYMODDHHMMSS"))
				(setq suf (strcat id time))
				(setq name2 (strcat path "\\" pre suf ".dwg"))
			)
		)
		(cond
			((null name2))
			((findfile name2))
			(name2
				(vla-SaveAs *doc* name2 ac2004_DWG)
				(command-s "qsave")
				(vl-file-delete file1)
				(setq yy (substr time 1 2))
				(setq mo (substr time 3 2))
				(setq dd (substr time 5 2))
				(setq hh (substr time 7 2))
				(setq mm (substr time 9 2))
				(setq ss (substr time 11 2))
				(setq time (strcat yy "��" mo "��" dd "��" hh "ʱ" mm "��" ss "��"))
				(princ (strcat ">>>�ļ���ʱ���׺���޸�Ϊ:" time))
			)
			(t nil)
		)
	)
	;���ܰɣ����꣡
	(setvar "CMDECHO" 0)
	(cond
		((av:is2cmd 60)(rename1))
		(t (rename2))
	)
	(setvar "CMDECHO" 1)
	(princ)
)

;;==============================================================

;���ļ���
(defun c:Opendir(/ fname path winshell)
	(princ "-->�򿪵�ǰͼֽ�����ļ���")
	(setvar "cmdecho" 0)
	(setq fname (vla-get-FullName *doc*))
	(cond
		((wcmatch fname "")
			(vla-SendCommand *doc* "qsave ")
		)
		(t (setq path (vl-filename-directory fname))
			(av:opendir path nil)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

;���ı���û�����½�
;txt:�ı��ļ���strlst:д���ı�����Ϣ�б�v:�Ƿ���ı�
(defun av:newtxt(txt strlst v / f)
	(cond
		((findfile txt))
		(t (setq f (open txt "a"))
			(foreach str strlst (write-line str f))
			(close f)
		)
	)
	(and v (startapp "notepad" txt))
)

;���̱���¼
(defun c:bwl (/ file fname path strlst)
	(princ "-->�򿪹��̱���¼���±�")
	(setvar "cmdecho" 0)
	(setq fname (vla-get-FullName *doc*))
	(setq path (vl-filename-directory fname))
	(setq file (strcat path "\\���̱���¼.txt"))
	(cond
		((wcmatch fname "")
			(vla-SendCommand *doc* "qsave ")
		)
		(t (setq strlst (list
											"�˼��±�Ϊ�����̼�������е�ע�������¼��"
											"����¼�ļ��빤��ͼֽ������ͬһĿ¼�£�"
											"���ڴ˼�¼���ɡ�"
											"======================================================="
											" "
										))
			(av:newtxt file strlst t)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

;��ת��Ļ
(defun c:cadzp (/ *error* ang ang1 ang2 enterview islocked ismode msg1 msg2 pointnil pt1 pviewportobj rotatevp tips w)
	;�ָ������ͼֽ�Ƕ�
	(defun pointnil(/ view)
		;�ָ�targetֻ��������ʼֵ(�洢Ŀ����UCS���������ڵ�ǰ�ӿ��е�͸��ͶӰ)
		(cond
			((equal (getvar "TARGET") '(0.0 0.0 0.0)))
			(t (and
					 (setq view (vla-item (vla-get-Viewports *doc*) 0))
					 (vla-put-target view (vlax-3d-point 0 0 0))
					 (vla-put-ActiveViewport *doc* view)
					 (princ "�ָ�target��ԭ��")
				 )
			)
		)
		(command-s "ucs" "w");������ǰ����ϵ
		;(vl-cmdf "-view" "top" "shademode" "2")
	)
	;�����ض��ӿ�
	(defun enterview(/ e obj)
		(cond
			((and
				 (setq e (car (entsel "\nѡȡ�ӿڽ���: ")))
				 (= "VIEWPORT" (cdr (assoc 0 (entget e))))
			 )
				(vla-put-mspace *doc* :vlax-true);�����ӿ�
				(setq obj (vlax-ename->vla-object e))
				(vla-put-activepviewport *doc* obj);����ѡ���ӿ�
			)
			(t (exit))
		)
	)
	;��ת�������ӿ�
	(defun rotatevp(/ $screen atio ce ch ch2 hh hh2 pt1 pt2)
		(setq $screen (getvar "SCREENSIZE")) 
		(setq ch (getvar "viewsize")) 
		(setq ch2 (/ ch 2)) 
		(setq ce (getvar "viewctr")) 
		(setq atio (/ (car $screen) (cadr $screen))) 
		(setq hh (* atio ch)) 
		(setq hh2 (/ hh 2))
		(setq pt1 (polar (polar ce 0 hh2) (* 1.5 pi) ch2))
		(setq pt2 (polar (polar ce pi hh2) (* 0.5 pi) ch2))
		(command-s "plan" "c")
		(command-s "zoom" "w" pt1 pt2);���Ա��ⷵ�ش�λ
		;(vla-zoomwindow *acad* (vlax-3d-point pt1) (vlax-3d-point pt2))
	)
	(defun *error* (str)
		(and islocked (vla-put-DisplayLocked pviewportObj :vlax-true))
		(setvar "cmdecho" 1)(princ)
	)
	;��ʼ����
	(vl-load-com)
	(setvar "cmdecho" 0)
	(setq ismode (= (getvar "tilemode") 1))
	(cond ;�ӿڲ���
		(ismode) ;ģ��״̬�޶���
		((= (getvar ' CVPORT) 1)
			(command-s "ucs" "w");������ǰ����ϵ
			(enterview);�����ض��ӿ�
		)
		(t nil)
	)
	(and ;������ǰ�ӿ�
		(null ismode)
		(setq pviewportObj (vla-get-ActivePViewport *doc*))
		(setq islocked (= (vla-get-DisplayLocked pviewportObj) :vlax-true))
		(vla-put-DisplayLocked pviewportObj :vlax-false)
	)
	;ȷ����ת�Ƕ�
	(setq msg1 (strcat "\nָ�����������Ƕ�:" (if islocked "<�س���������>" "<�س��ָ���ͼ>")))
	(setq msg2 "\nָ����һ��:<�س��ָ���ͼ> ")
	(initget 128)
	(setq pt1 (getpoint msg1))
	(cond ;ȷ�ϽǶ�
		((= (type pt1) 'list) ;��ǰ��ѡ
			(and
				(setq ang (getangle pt1 msg2))
				(setq ang (angtos ang 0 10))
				(setq ang1 (rtos (atof ang) 2 2))
			)
		)
		((= (type pt1) 'str) ;���ֹ�����Ƕ�
			(setq ang (rtos (* -1 (atof pt1)) 2))
			(setq ang2 (rtos (atof pt1) 2 2))
		)
		(t nil)
	)
	(cond ;��ת����ϵ
		(ang (command-s "ucs" "z" ang))
		(pt1 (pointnil))
		(islocked (setq w t))
		(t (pointnil))
	)
	(rotatevp);��ת�������ӿ�
	;��ʾ��תЧ��
	(setq tips (cond
							 (ang1 (strcat ">>>ͼ����������ת" ang1 "��"))
							 (ang2 (strcat ">>>ͼ����������ת" ang2 "��"))
							 (t ">>>ͼ���ѻָ�������״̬")
						 )
	)
	(and (null w) (princ tips))
	;��ͣ�����Ž���
	(and islocked pt1
		(getint "\n��ʾ����ǰ���������ţ��س��������")
	)
	(*error* nil)
)




;�������������ֿ���
(defun c:habitleft(/ *error*)
	(princ "-->�������������������")
	(defun *error*(msg)
		(startapp "explorer" "shell:::{80F3F1D5-FECA-45F3-BC32-752C152E456E}")
		(princ)
	)
	(textpage)
	(princ "\n�����������������ֿ��ҵ����������\"ƽ���������\"�У�ѡ��\"��������\"���ָ���������롣")
	(princ "\n���޴�ѡ����п�����Ӳ������δ����ȷ��װ���¡�")
	(getint "\n�س�����...")
	(*error* nil)
)

;=============================================================

;ѭ���л���ǩ
(defun c:tabup()(tab-loop 1))
(defun c:tabdown()(tab-loop -1))
(defun tab-loop (dir / len n obj tab tablist)
	(vlax-for layout *docs*
		(setq tablist (cons (list (vla-get-Name layout) layout) tablist))
	)
	(setq tablist (reverse tablist))
	(setq tab (assoc (vla-get-Name *doc*) tablist))
	(setq n (vl-position tab tablist))
	(setq len (length tablist))
	(setq n
		(cond
			((= dir 1)
				(if (= n 0)(setq n len))
				(1- n)
			)
			((= dir -1)
				(if (= n (1- len))(setq n -1))
				(1+ n)
			)
			(t nil)
		)
	)
	(setq tab (cadr (nth n tablist)))
	(vla-activate tab)
)
;ѭ���л�����
(defun c:layoutup()(layout-loop 1))
(defun c:layoutdown()(layout-loop -1))
(defun layout-loop (dir / count layouts n tab tablist)
	(setq layouts (vla-get-layouts *doc*))
	(vlax-for layout layouts
		(setq tablist (cons (vla-get-Name layout) tablist))
	)
	(setq tablist (reverse tablist))
	(setq n (vl-position (getvar "ctab") tablist))
	(setq count (vla-get-count layouts))
	(cond
		((= dir 1)
			(if (= n 0)(setq n count))
			(setq tab (nth (1- n) tablist))
		)
		((= dir -1)
			(if (= n (1- count))(setq n -1))
			(setq tab (nth (1+ n) tablist))
		)
	)
	(setvar "CTAB" tab)
)

;=============================================

;ɾ��DWG�ļ��������ļ�
(defun cleanviru (/ file)
	(foreach f
		(list ;����ɾ���ļ�
			"acad.lsp" "acad.vlx" "acad.fas" "acaddoc.lsp" "acaddoc.fas"
			"acadapp.lsp" "acadapq.lsp" "logo.gif" "plot.txt"
		)
		(setq file (findfile (strcat (getvar "dwgprefix") f)))
		(if (null file) nil
			(progn
				(if (= (av:get-att-RO file) 1)(av:put-att-RO file 0))
				(if (vl-file-delete file)
					(princ (strcat "\nɾ���ļ�:" f))
					(princ (strcat "\n�޷�ɾ��:" f))
				)
			)
		)
	)
)
(cleanviru)


;;˵��:����ϵͳ��ʱ�ļ���
(defun c:cleantemp(/ fn fso l msg na tmp)
	(princ "-->��ʱ�ļ�������")
  (setq fso (vlax-create-object "scripting.FileSystemObject"))
	(setq tmp (getenv "tmp"))
	(foreach l (kk-getFolds tmp 0)
		(setq msg (vl-catch-all-apply 'vlax-invoke (list fso 'DeleteFolder l :vlax-true)))
		(cond
			((vl-catch-all-error-p msg))
			(t
				(setq na (cadr (fnsplitl l)))
				(princ (strcat "\n�������ļ��У�" na))
			)
		)
	)
	(foreach l (kk-getfiles tmp "*.*" 0)
		(setq msg (vl-catch-all-apply 'vlax-invoke (list fso 'deletefile l :vlax-true)))
		(cond
			((vl-catch-all-error-p msg))
			(t
				(setq l (fnsplitl l))
				(setq fn (strcat (cadr l) (caddr l)))
				(princ (strcat "\n�������ļ���" fn))
			)
		)
	)
  (vlax-release-object fso)
  (princ)
)

;;˵��:����ɾ��
(defun c:redel(/ lst ss sslst)
	(princ "\nѡ��Ҫ������ͼԪ��δѡ��Ľ��ᱻɾ����")
	(cond
		((setq ss (ssget))
			(setq sslst (av:ss->ssnlst ss))
			(setq lst (vl-remove-if
									(function (lambda(x) (member x sslst)))
									(av:ss->ssnlst (ssget "A"))
								)
			)
			(foreach l lst (entdel l))
			(vla-ZoomExtents *acad*)
		)
		(t nil)
	)
	(princ)
)

;;=====================================================================
;;��������
(defun c:PasteToBlock(/ pt ss)
	(setvar "CMDECHO" 0)
	(vla-StartUndoMark *doc*)
	(while (setq ss (ssget))
		(setq pt (av:getboxpt ss))
		(command-s "cutclip" ss "")
		(command-s "pasteblock" (car pt))
	)
	(vla-EndUndoMark *doc*)
	(setvar "CMDECHO" 1)
	(princ)
)

;;128.1 [����] ѡ��->������
;;ע�� ������ѡ���д��ھ������Ե�ͼ�鼰���Ӷ�������Ч
;(defun MJ:BLK-MakeUnNameBlock (/ count entlist ent blk pt)
;	(setq ss (ssget))
;  (setq pt (car (av:getboxpt ss)))
;  (entmake (list
;						 '(0 . "BLOCK")
;						 '(2 . "*U")
;						 '(70 . 1)
;						 (cons 10 pt)
;					 )
;  )
;  (setq count 0)
;  (repeat (sslength ss)
;		(setq ent (ssname ss count))
;    (setq entlist (entget ent))
;    (setq count (1+ count))
;    (entmake entlist)
;  )
;  (setq count 0)
;  (repeat (sslength ss)
;    (setq ent (ssname ss count))
;    (setq count (1+ count))
;    (entdel ent)
;  )
;  (setq blk (entmake '((0 . "ENDBLK"))))
;  (if (princ blk)
;    (entmake (list
;							 (cons 0 "INSERT")
;							 (cons 2 blk)
;							 (cons 10 pt)
;						 )
;    )
;  )
;  blk
;)

;;=====================================================================






(princ)
