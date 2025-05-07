
;;��ȡ��������ķ�Χ�Խǵ�
(defun av:GgetSSBoxPoint(ss 3d / maxp maxx maxy minp minx miny obj pt1 pt2 sslst x1 x2 y1 y2)
	(setq sslst (av:ss->ssnlst ss))
	(foreach l sslst
		(setq obj (vlax-ename->vla-object l))
		(vla-GetBoundingBox obj 'minp 'maxp)
		(setq
			minp (vlax-safearray->list minp)
			maxp (vlax-safearray->list maxp)
			x1 (car minp)
			y1 (cadr minp)
			x2 (car maxp)
			y2 (cadr maxp)
		)
		(if minx nil (setq minx x1))
		(if miny nil (setq miny y1))
		(if maxx nil (setq maxx x2))
		(if maxy nil (setq maxy y2))
		(setq
			minx (min minx x1)
			miny (min miny y1)
		)
		(setq
			maxx (max maxx x2)
			maxy (max maxy y2)
		)
	)
	(setq
		pt1 (list minx miny 0.0)
		pt2 (list maxx maxy 0.0)
	)
	(cond
		(3d
			(setq pt1 (vlax-3D-point pt1))
			(setq pt2 (vlax-3D-point pt2))
		)
	)
	(list pt1 pt2)
)


;;ը��ͼ�μ�����
(defun c:BlastAll(/ fn ismax n obj pt12 ss ssn)
	(setvar "cmdecho" 0)
	(cond
		((and (= 0 (getvar "tilemode")) (/= 1 (getvar "cvport")))
			(command "vpmax")
			(setq ismax t)
		)
	)
	(while (setq ss (ssget '((0 . "~VIEWPORT"))))
		;(setq lst (av:GgetSSBoxPoint ss t))
		;(setq pt1 (car lst))
		;(setq pt2 (cadr lst))
		;(Vlax-Invoke-Method *acad* 'ZoomWindow pt1 pt2)
		(command "zoom" "o" ss "")
		(setq pt12 (cadddr (av:getscr4pt)))
		(setq fn (vl-filename-mktemp nil nil ".wmf"))
		;(while (= 1 (getvar "cmdactive")) (command pause))
		(command "wmfout" fn ss "")
		(repeat (setq n (sslength ss))
			(setq ssn (ssname ss (setq n (1- n))))
			(setq obj (vlax-ename->vla-object ssn))
			(vla-Erase obj)
		)
		(command "wmfin" fn pt12 "2" "" "")
		(setq obj (vlax-ename->vla-object (entlast)))
		(vla-Explode obj)
		(vla-Erase obj)
		(vl-file-delete fn)
		(Vlax-Invoke-Method *acad* 'ZoomPrevious)
	)
	(cond
		(ismax
			(command "vpmin")
			(vla-put-MSpace *doc* :vlax-true)
		)
	)
	(if (and ss acet-sys-lmouse-down) ;�ж��Ƿ�װ��ET
		(princ "\n��ʾ��������ʵ������û��ը�飬�ɳ�������TXTEXP�����ַֽ����")
	)
	(setvar "cmdecho" 1)
	(princ)
)

;======================================================================================
;һը����
(defun c:ExplNestBlk(/ fil ss)
	(setvar "cmdecho" 0)
	(setvar "qaflags" 1)
	(setq fil (list (cons 0 "insert")))
	(setq ss (ssget fil))
	(while (setq ss (ssget "p" fil))
		(command-s ".explode" ss "")
	)
	(setvar "qaflags" 0)
	(setvar "cmdecho" 1)
	(princ)
)
;======================================================================================
;����ը��
(defun c:smartexpl(/ *error* *sbar attlst blolst cutlst data enlst filter inslst m msg n na obj os reflst ss1 ss2 ssn thco total ty)
	(defun *error*(str)
		(vla-EndUndoMark *doc*)
		(setvar "osmode" os)
		(setvar "cmdecho" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(vla-StartUndoMark *doc*)
	(setq filter (list (cons 0 "INSERT,TCH_OPENING")))
	(setq
		ss1 (ssget filter)
		ss2 ss1
		thco 0
	)
	(repeat (setq n (sslength ss1))
		(setq ssn (ssname ss1 (setq n (1- n))))
		(setq ty (cdr (assoc 0 (entget ssn))))
		(cond
			((wcmatch ty "TCH_OPENING")
				(command "explode" ssn)
				(ssdel ssn ss2)
				(setq thco (1+ thco))
			)
		)
	)
	(setq enlst (av:ss->ssnlst ss2))
	(foreach l enlst
		(setq obj (vlax-ename->vla-object l))
		(cond
			((and
				 (setq msg (vl-catch-all-apply 'vla-getobject (list (vla-getextensiondictionary obj) "ACAD_FILTER")))
				 (null (vl-catch-all-error-p msg))
			 )
				(setq cutlst (cons l cutlst))
			)
			((vlax-property-available-p obj 'path)
				(setq reflst (cons l reflst))
			)
			((equal (vla-get-ObjectName obj) "AcDbMInsertBlock")
				(setq inslst (cons l inslst))
			)
			;((equal (cdr (assoc 66 (entget l))) 1)
			;	(setq attlst (cons l attlst))
			;)
			((equal (vla-Get-HasAttributes obj) :vlax-true)
				(setq attlst (cons l attlst))
			)
			(t (setq blolst (cons l blolst)))
		)
	)
	(setq n 0 m (itoa (length enlst)))
	(foreach x cutlst
		(princ (strcat "\r���ڷֽ�(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(and
			(vlax-property-available-p (vlax-ename->vla-object x) 'path)
			(setq data (entget x))
			(setq na (cdr (assoc 2 data)))
			(setq obj (vla-Item (vla-get-Blocks *doc*) na))
			(vl-catch-all-apply 'vla-Bind (list obj :vlax-false))
		)
		(exblkcut x)
	)
	(foreach x attlst
		(princ (strcat "\r���ڷֽ�(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(exblkatt x)
	)
	(foreach x reflst
		(princ (strcat "\r���ڷֽ�(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(exblkref x)
	)
	(foreach x inslst
		(princ (strcat "\r���ڷֽ�(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(exblkins x)
	)
	(foreach x blolst
		(princ (strcat "\r���ڷֽ�(" (itoa (setq n (1+ n))) "/" m ")..." (setq *sbar (Spinbar *sbar))))
		(vla-Explode (vlax-ename->vla-object x))
		(entdel x)
	)
	(if enlst
		(setq total (+ thco (length enlst)))
		(setq total thco)
	)
	(and (null (zerop total)) (princ (strcat "\n���ֽ�" (itoa total) "����������У�")))
	(and (null (zerop thco)) (princ (strcat "�����T��" (itoa thco) "����")))
	(and reflst (princ (strcat "�ⲿ����" (itoa (length reflst)) "����")))
	(and inslst (princ (strcat "���ز���" (itoa (length inslst)) "����")))
	(and cutlst (princ (strcat "�ü���" (itoa (length cutlst)) "����")))
	(and attlst (princ (strcat "���Կ�" (itoa (length attlst)) "����")))
	(and blolst (princ (strcat "��ͨ��" (itoa (length blolst)) "����")))
	(*error* nil)
)

(defun exblkatt (x / attb attnam da ent h k n obj p)
	(setq
		attnam (entnext x)
		da (entget attnam)
		k (cdr (assoc 1 da))
	)
	(while (= "ATTRIB" (cdr (assoc 0 da)))
		(cond
			((= "STAR" (cdr (assoc 2 da))))
			(t
				(entmakex
					(list
						'(0 . "TEXT")
						(cons 1 k) ;�ı�
						(cons 10 (cdr (assoc 10 da))) ;����
						(cons 40 (cdr (assoc 40 da))) ;�ָ�
						(cons 50 (cdr (assoc 50 da))) ;�Ƕ�
						(cons 41 (cdr (assoc 41 da))) ;��߱�
						(cons 7 (cdr (assoc 7 da))) ;������ʽ
						(cons 8 (cdr (assoc 8 da))) ;ͼ��
					)
				)
			)
		)
		(setq
			attnam (entnext attnam)
			da (entget attnam)
			k (cdr (assoc 1 da))
		)
	)
	(setq obj (vlax-ename->vla-object x))
	(vla-Explode obj)
	(vla-Erase obj)
	(cond
		((setq ent (ssget "x" (list (cons 0 "ATTDEF"))))
			(repeat (setq n (sslength ent))
				(entdel (ssname ent (setq n (1- n))))
			)
		)
		(t nil)
	)
)

(defun exblkins (x / ent1 ent2 obj)
	(setq ent1 (entget x))
	(setq ent2 (entmakex (list '(0 . "INSERT") (assoc 2 ent1) (assoc 10 ent1))))
	(entdel x)
	(setq obj (vlax-ename->vla-object ent2))
	(vla-Explode obj)
	(entdel ent2)
)

(defun exblkref(x / data msg na obj)
	(setq data (entget x))
	(setq na (cdr (assoc 2 data)))
	(setq obj (vla-Item (vla-get-Blocks *doc*) na))
	(vl-catch-all-apply 'vla-Bind (list obj :vlax-false))
	(setq msg (vl-catch-all-apply 'vla-Explode (list obj)))
	(if (vl-catch-all-error-p msg)
		(vl-cmdf "explode" x)
		(vla-Erase obj)
	)
)

;;�ֽ����ͼ��
(defun exblkcut(enl / filter-list in-ss limits lst maxlst minlst n auxptlst limptlst auxl obj pt pt1 pt-list ssn)
	;;    (setq enl (car (entsel)))
	;;���ɸ�����Χ��
	(vl-cmdf "xclip" (ssadd enl) "" "p")
	(setq limits (entlast))
	(setq limptlst (av:getptn (entget limits)))
	;;�ֽ��뷶Χ���ཻ��ͼ������
	(setq pt1 (nth 0 limptlst))
	(setq pt-list (append limptlst (list pt1)))
	(setq filter-list '((0 . "INSERT,HATCH,REGION")))
	(cond
		((setq in-ss (ssget "f" pt-list filter-list))
			(setq n (sslength in-ss))
			(while (setq ssn (ssname in-ss (setq n (1- n))))
				(vl-cmdf "explode" ssn)
			)
		)
		(t nil)
	)
	;;��ǰѡ��ͼԪ�б�
	(setq maxlst (av:ss->ssnlst (ssget "p")))
	;;���ɲü���Χ������
	;(vl-cmdf "offset" 1 limits "-1000,-1000" "")
	(vla-Offset (vlax-ename->vla-object limits) 1)
	(setq auxl (entlast))
	(setq auxptlst (av:getptn (entget auxl)))
	(entdel auxl) ;ɾ��������
	;;�ü���Χ�ཻ��
	(setq auxptlst (append auxptlst (list pt1 "" "")))
	(setq n 0)
	(vl-cmdf "trim" limits "" "f")
	(while (setq pt (nth (setq n (1+ n)) auxptlst))
		(vl-cmdf pt)
	)
	(entdel limits)
	;;ɾ����Χ��ͼԪ
	(setq minlst (av:ss->ssnlst (ssget "cp" limptlst)))
	(setq lst
		(vl-remove-if
			(function (lambda(x) (member x minlst)))
			maxlst
		)
	)
	(foreach l lst
		(setq obj (vlax-ename->vla-object l))
		(if (vlax-erased-p obj) nil (entdel l))
	)
)

;======================================================================================
;;��ɢȺ��
(defun c:ExplodeGroup(/ *error* ent group grouplst handlelst mp:cdrs obj objlst ss ss-to-objlst)
	(defun *error*(str) (princ))
	(defun ss-to-objlst (ss / n obj objlst ssn)
		(repeat (setq n (sslength ss))
			(setq ssn (ssname ss (setq n (1- n))))
			(setq obj (vlax-ename->vla-object ssn))
			(setq objlst (cons obj objlst))
		)
	)
	;;���������б��ǰֵɸѡ�б��ֵ
	(defun mp:cdrs (DxfKey ImpLst / TmpLst OutLst)
		(while (setq TmpLst (assoc DxfKey ImpLst))
			(setq
				OutLst (cons (cdr TmpLst) OutLst)
				ImpLst (cdr (member TmpLst ImpLst))
			)
		)
		(reverse OutLst)
	)
	(setq ss (ssget))
	(setq objlst (ss-to-objlst ss))
	(foreach obj objlst
    (setq handlelst
			(cons (vla-get-handle obj) handlelst)
		)
	)
	(setq ent (dictsearch (namedobjdict) "ACAD_GROUP"))
  (setq grouplst (mp:cdrs 350 ent))
  (foreach l grouplst
    (setq group (mp:cdrs 340 (entget l)))
    (foreach grl group
      (setq obj (vlax-ename->vla-object grl))
      (if (vl-position (vla-get-handle obj) handlelst) (entdel l))
		)
	)
	(*error* nil)
)

;======================================================================================
;AutoCAD�ϲ�ͼ�����laymrg��
;���ͼֽ�ò��շ�������ͼԪ��ͼ�����ƻ��С�$��������������ӣ������ͼ��ܶ�������
;����ѡ�$������������ͬ��ͼ��ϲ����ֶ��Ļ��ͱȽ��鷳��
;Ҫ�ϲ���ͬ��׺��ͼ�㣬����ʹ�ó��������.
;һ������ͼ��ȥ����#������$��ǰ׺������
(defun c:unxreflapre(/ ocl ss5)
	;(princ "-->�����ⲿ����ͼ����ǰ׺")
	(setvar "cmdecho" 0)
	(vla-StartUndoMark *doc*)
	(princ "\n�������ⲿ���գ���ͼ�����������γɵġ�$�������������")
	(setvar "blipmode" 0)
	(if (null vlax-dump-object)(vl-load-com))
	(setq ocl (getvar "clayer"))
	(setq ss5 (x1812031));���ͼ���Ƿ��ж��ᣬ�������߹ر�
	(if (car ss5) (x1812032 ss5));��ԭͼ��
	(setvar "clayer" ocl)
	(if (s1811301) ;�޸�ͼ������
		(progn(terpri)(princ ">>>������ͼ��ǰ׺����ɺϲ�"))
		(princ "\n��ʾ��û���ⲿ���հ󶨺��ͼ��ǰ׺")
	)
	(vla-EndUndoMark *doc*)
	(setvar "cmdecho" 1)
	(princ)
)

;ȡ������ͼ������
(defun w1810232 (doc / doc lay obj ss tc)
	(setq lay (vla-get-layers doc) ss '())
	(vlax-for obj lay
		(setq tc (vla-get-name obj));ȡ��ͼ������
		(setq ss (cons (list tc obj) ss))
	)
	ss
)
;�޸�ͼ������
(defun s1811301 (/ doc n obj s2 ss2 ss3 tc1 tc2 x)
	(setq
		doc (vla-get-activedocument (vlax-get-acad-object));ȡ�õ�ǰ���ж��󼯺�
		ss2 (w1810232 doc);ͼ�㼯��
		ss3 (mapcar 'strcase (mapcar 'car ss2));ͼ�����Ƽ���
		;tc1 (getvar "clayer");ȡ�õ�ǰͼ������
	)
	;(if (or (vl-string-search "\#" tc1 0)(vl-string-search "$" tc1 0))(setvar "clayer" "0"));�����ǰͼ����Ҫ�޸ģ���ת��ͼ��Ϊ"0"
	(setvar "clayer" "0")
	(while (setq s2 (car ss2));����ͼ��
		(setq ss2 (cdr ss2) tc1 (car s2) tc2 tc1 obj (cadr s2))
		(while (vl-string-search "\#" tc2 0)(setq tc2 (vl-string-subst "" "\#" tc2)));������#��ͼ������
		(while (setq n (vl-string-search "$" tc2 0))(setq tc2 (substr tc2 (+ 2 n))));������$��ͼ������
		;(while (setq n (vl-string-search "A-" tc2 0))(setq tc2 (substr tc2 (+ 3 n))));������A-������
		(if (= tc2 "")(setq tc2 "0"));����ǿվ��޸�ͼ��Ϊ"0"
		(if (/= tc2 tc1);������Ʒ����仯
			(progn
				(if (member (strcase tc2) ss3);2;����Ѿ������ͼ������
					(progn
						(if (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list obj)));ɾ�����ͼ��
							(progn
								(vl-catch-all-apply 'vl-cmdf (list "laymrg" "N" tc1 "" "N" tc2 "Y"))
								;(Command "laymrg" "N" tc1 "" "N" tc2 "Y");���ͼ�㲻��ɾ���ͺϲ�
							)
						)
					)
					(progn
						(if ;���û����ͬ������ͼ��͸ı�ͼ������
							(null (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-name (list obj tc2))))
							(setq ss3 (cons (strcase tc2) ss3))
						)
					)
				)
				(setq x t)
			)
		)
	)
	x
)
;��ԭͼ��״̬
(defun x1812032 (ss / ss x y)
	(setq ss (vl-remove-if '(lambda (x)(vlax-erased-p (cadr x))) ss));�ų��Ѿ�ɾ����ͼ��
	(mapcar '(lambda (y)(vla-put-lock y :vlax-true));����
		(mapcar 'cadr (vl-remove-if-not '(lambda (x)(= (car x) 1)) ss))
	)
	(mapcar '(lambda (y)(vla-put-Freeze y :vlax-true));����
		(mapcar 'cadr (vl-remove-if-not '(lambda (x)(= (car x) 2)) ss))
	)
	(mapcar '(lambda (y)(vla-put-LayerOn y :vlax-false));�ر�
		(mapcar 'cadr (vl-remove-if-not '(lambda (x)(= (car x) 3)) ss))
	)
)
;;��ȡͼ��״̬
(defun x1812031 ( / lay ss)
	(setq ss '())
	(vlax-for lay (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
		(if (= (vla-get-lock lay) :vlax-true);���ͼ������
			(progn
				(vla-put-lock lay :vlax-false) ;����
				(setq ss (cons (list 1 lay) ss))
			)
		)
		(if (= (vlax-get-property lay "Freeze") :vlax-true);����
			(progn
				(vla-put-Freeze lay :vlax-False);�ⶳ
				(setq ss (cons (list 2 lay) ss))
			)
		)
		(if (= (vlax-get-property lay "LayerOn") :vlax-false);�ر�
			(progn
				(vla-put-LayerOn lay :vlax-true);��
				(setq ss (cons (list 3 lay) ss))
			)
		)
	)
	ss
)
;======================================================================================









(princ)
