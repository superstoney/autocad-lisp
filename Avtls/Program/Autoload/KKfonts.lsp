;�����������
;================================================================
(defun c:kkfonts (/ $fontlist$ $sydzt$ addlist av:changefontsdcl dclid desetpopx-0 desetpopx-1 deshow fname getpop key setcusfontstype setpop1-0 setpop1-1 setpop2-0 setpop2-1 ss)
  (defun av:changefontsdcl (fname / dcls fn)
    (setq dcls
			(list
				"fonts:dialog {"
				"    initial_focus = \"211\" ;"
				"    key = \"k00\" ;"
				"    label = \"��Aά���졿������������������\" ;"
				"    :boxed_column {"
				"        label = \"1.��������\" ;"
				"        :row {"
				"            :button {"
				"                key = \"111\" ;"
				"                label = \"�������(&N)\" ;"
				"            }"
				"            :button {"
				"                key = \"112\" ;"
				"                label = \"��ϸ����(&F)\" ;"
				"            }"
				"        }"
				"        :row {"
				"            :button {"
				"                key = \"121\" ;"
				"                label = \"��ѡ����(&E)\" ;"
				"            }"
				"            :button {"
				"                is_cancel = true ;"
				;"                is_enabled = false ;"
				"                key = \"122\" ;"
				"                label = \"�ش���(ESC)\" ;"
				"                width = 15 ;"
				"            }"
				"        }"
				"    }"
				"    :boxed_column {"
				"        label = \"2.��һ����\" ;"
				"        :row {"
				"            :button {"
				"                key = \"211\" ;"
				"                label = \"�Ƴ�����(&H)\" ;"
				"            }"
				"            :button {"
				"                key = \"212\" ;"
				"                label = \"��������(&G)\" ;"
				"            }"
				"        }"
				"        :row {"
				"            :button {"
				"                key = \"221\" ;"
				"                label = \"��������(&Z)\" ;"
				"            }"
				"            :button {"
				"                key = \"222\" ;"
				"                label = \"˫�߷���(&D)\" ;"
				"            }"
				"        }"
				"    }"
				"    spacer;"
				"    :boxed_column {"
				"        label = \"3.��ѡ����\" ;"
				"        :row {"
				"            :column {"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop1-t\" ;"
				"                        label = \"����1:\" ;"
				"                        width = 8 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop1\" ;"
				"                        width = 21 ;"
				"                    }"
				"                }"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop2-t\" ;"
				"                        label = \"����2:\" ;"
				"                        width = 8 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop2\" ;"
				"                        width = 21 ;"
				"                    }"
				"                }"
				"            }"
				"            :column {"
				"                fixed_width = true ;"
				"                width = 10 ;"
				"                spacer;"
				"                :toggle {"
				"                    key = \"sydzt\" ;"
				"                    label = \"������(&J)\" ;"
				"                }"
				"                :button {"
				"                    key = \"ok\" ;"
				"                    label = \"�޸�(&K)\" ;"
				"                }"
				"            }"
				"        }"
				"    }"
				"    :boxed_column {"
				"        label = \"4.��������\" ;"
				"        :button {"
				"            key = \"411\" ;"
				"            label = \"ȫ�������ı����͸������ƽ��\" ;"
				"        }"
				"    }"
				"}"
			)
		)
		(if	(setq fn (open fname "w"))
			(progn
				(foreach dcl dcls (write-line dcl fn))
				(close fn)
				fname
			)
		)
	)
	;AutoCAD-86 bigfont 1.0 ������
	;AutoCAD-86 unifont 1.0 ��������
	;AutoCAD-86 shapes 1.0 ���ļ�
	;ȡ���ض����������б�
	(defun $fontlist$ (txt / $fonttype$ dir1 dirlist fontlist fonts getfontlist)
		;�ж���������
		(defun $fonttype$ (fn txt / cnt dv fh fonttype inp)
			(setq cnt 22 dv "")
			(if	(setq fh (open fn "r"))
				(progn
					(while (and (> (setq cnt (1- cnt)) 0) (setq inp (read-char fh)) (> inp 0))
						(setq dv (strcat dv (chr inp)))
					)
					(close fh)
					(and dv (setq fonttype (WCMATCH (strcase dv t) txt)))
				)
			)
			fonttype
		)
		;�õ��ļ����ڶ�Ӧ�����б�
		(defun getfontlist(dir txt / files fontlist)
			(setq files (vl-directory-files dir "*.shx"))
			(setq fontlist
				(vl-remove-if-not
					(function (lambda(a)($fonttype$ (strcat dir "\\" a) txt)))
					files
				)
			)
		)
		;��ѯ����ļ����������б�
		(and *fstl_dir* (setq dir1 (findfile (strcat *fstl_dir* "\\support"))))
		(setq dirlist (list dir1 (findfile "fonts")))
		(foreach dir dirlist
			(setq fonts (getfontlist dir txt))
			(setq fontlist (append fontlist fonts))
		)
		(setq fontlist (av:delstsame fontlist));�б�ȥ��
		(setq fontlist (acad_strlsort fontlist));�б�����
	)
	;�Ի���ؼ����
	(defun AddList (key lst)
		(IF	(AND key lst)
			(PROGN
				(if (= (type lst) 'str)(setq lst (list lst)))
				(start_list key)
				(foreach x lst (AND X (= (type x) 'str) (add_list x)))
				(end_list)
			)
			(PROGN (start_list key) (end_list))
		)
		lst
	)
	;д���б�ļ��д��
	;(defun setpop(fonts)
	;	(start_list "pop1")
	;	(mapcar 'add_list fonts)
	;	(end_list)
	;)
	;����SHX��������
	(defun setpop1-1(/ npop1 shx shx1 shx2)
		(setq shx1 ($fontlist$ "*unifont*"));��������
		(setq shx2 ($fontlist$ "*shapes*"));������
		(setq shx (acad_strlsort (append shx1 shx2)))
		(mode_tile "pop1" 0)
		(AddList "pop1" shx)
		(setq npop1 (vl-bb-ref "pop1-1"))
		(setq npop1 (vl-position (if npop1 npop1 "txt.shx") shx))
		(and npop1 (set_tile "pop1" (itoa npop1)))
		(set_tile "pop1-t" "SHX����:")
		shx
	)
	;���ô���������
	(defun setpop2-1(/ bigs npop2)
		(setq bigs ($fontlist$ "*bigfont*"));������
		(mode_tile "pop2" 0)
		(AddList "pop2" bigs)
		(setq npop2 (vl-bb-ref "pop2-1"))
		(setq npop2 (vl-position (if npop2 npop2 "hztxt.shx") bigs))
		(and npop2 (set_tile "pop2" (itoa npop2)))
		(set_tile "pop2-t" "������:")
		bigs
	)
	;����ttf��������
	(defun setpop1-0(/ npop1 ttfs)
		(set_tile "pop1-t" "������:")
		(setq ttfs (list "΢���ź�" "����" "����" "����" "����" "������"))
		(AddList "pop1" ttfs)
		(setq npop1 (vl-bb-ref "pop1-0"))
		(setq npop1 (vl-position (if npop1 npop1 "����") ttfs))
		(and npop1 (set_tile "pop1" (itoa npop1)))
		ttfs
	)
	;������ʽ��ʾ����
	(defun setpop2-0()
		(set_tile "pop2-t" "��ʽ��:")
		(mode_tile "pop2" 1)
		(AddList "pop2" nil)
	)
	;����Ĭ��SHX������ʽ
	(defun desetpopx-1() ;ȫ�ֱ���
		(setq unis (setpop1-1))
		(setq bigs (setpop2-1))
	)
	;����Ĭ��ttf������ʽ
	(defun desetpopx-0() ;ȫ�ֱ���
		(setq ttfs (setpop1-0))
		(setpop2-0)
	)
	;������ѡ�����
	(defun $sydzt$ (/ pick)
		(setq pick (get_tile "sydzt"))
		(if (= pick "1")
			(desetpopx-1) ;shx����
			(desetpopx-0) ;ttf����
		)
	)
	;����Ĭ����ʾ���
	(defun deshow ()
		(cond
			((= (vl-bb-ref "sydzt") "1")
				(set_tile "sydzt" "1")
				(desetpopx-1) ;ϸ����
			)
			(t (set_tile "sydzt" "0")
				(desetpopx-0) ;������
			)
		)
	)
	;ȡ���б�����
	(defun getpop (pop fontlist / font pick)
		(setq pick (atoi (get_tile pop)))
		(if fontlist (setq font (nth pick fontlist)))
		font
	)
	;��������ѡ�������������
	(defun setcusfontstype(/ pop1-0 pop1-1 pop2-1 sydzt)
		(setq pop1-1 (getpop "pop1" unis))
		(setq pop1-0 (getpop "pop1" ttfs))
		(setq pop2-1 (getpop "pop2" bigs))
		(setq sydzt (get_tile "sydzt"))
		(vl-bb-set "sydzt" sydzt)
		(cond
			((= sydzt "0")
				(av:fontstottf pop1-0)
				(vl-bb-set "pop1-0" pop1-0)
			)
			((= sydzt "1")
				(av:fontstoshx pop1-1 pop2-1)
				(vl-bb-set "pop1-1" pop1-1)
				(vl-bb-set "pop2-1" pop2-1)
			)
			(t nil)
		)
	)
	;��ʼ����
	(vl-load-com)
	(setvar "cmdecho" 0)
	(and
		(setq fname (vl-filename-mktemp nil nil ".dcl"))
		(av:changefontsdcl fname)
		(setq dclid (load_dialog fname))
	)
	;��DCL�ļ�
	;(vlax-invoke (vlax-get-or-create-object "Wscript.Shell") 'RUN fname)
	(vl-file-delete fname)
	(and dclid (new_dialog "fonts" dclid ""))
	(deshow);����Ĭ����ʾ���
	(action_tile "111" "(done_dialog 111)")
	(action_tile "112" "(done_dialog 112)")
	(action_tile "121" "(done_dialog 121)")
	(action_tile "211" "(done_dialog 211)")
	(action_tile "212" "(done_dialog 212)")
	(action_tile "221" "(done_dialog 221)")
	(action_tile "222" "(done_dialog 222)")
	(action_tile "sydzt" "($sydzt$)")
	(action_tile "ok" "(setcusfontstype)(done_dialog 300)")
	(action_tile "411" "(done_dialog 411)")
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((= key 111)(av:nulltofill "txt.shx" "hztxt.shx" "����"))
		((= key 112)(av:fontstofh "txt.shx" "hztxt.shx"))
		((= key 121)(av:entsel))
		((= key 211)(av:fontstoshx "AllInOneUni" "AllInOneBig"))
		((= key 212)(av:fontstoshx "gbenor.shx" "gbcbig.shx"))
		((= key 221)(av:fontstoshx "tssdeng.shx" "tssdchn.shx"))
		((= key 222)(av:fontstoshx "tssdeng2.shx" "hzfs.shx"))
		((= key 411)
			(AV:CleanMtextFormat
				(ssget "X" '((0 . "MTEXT")))
				'(
					 ("(\\\\F|\\\\f)(.[^;]*);" . "") ;������������Ȩ��
					 ;("(\\\\W)(.[^;]*);" . "") ;����߿��
					 ;("({|})" . "") ;����ǰ������
				 )
			)
		)
		(t nil)
	)
	;�����������ӿ�
	;(vlax-invoke-method *doc* 'regen acallviewports)
	
	;��ˢ������
	(cond
		((= key 0))
		((setq ss (ssget "x" '((0 . "*TEXT,INSERT"))))
			(av:regen ss)
		)
		(t nil)
	)
	(princ)
)
;================================================================
;��ѡ��������
(defun av:entsel(/ change en sm st)
	(defun change(/ g1 g2 mouse ss)
		(setq key (strcase (getstring ">>>ѡ������[��������<A>/��������<S>/�Ƴ�����<D>/ʵ�����<F>](Ĭ��ΪA):")))
		(cond
			((or (wcmatch key "") (wcmatch key "A")) (av:toshx x "tssdeng" "tssdchn"))
			((wcmatch key "S") (av:toshx x "gbenor.shx" "gbcbig.shx"))
			((wcmatch key "D") (av:toshx x "AllInOneUni" "AllInOneBig"))
			((wcmatch key "F") (av:tofft x "����"))
			(t (princ "�밴��ʾ����"))
		)
	)
	(while (setq en (entsel "\n���ѡ����"))
		(setq st (cdr (assoc 7 (entget (car en)))))
		(vlax-for x *sts*
			(if (wcmatch st (vla-get-name x)) (change))
			(av:regen (ssget "x" (list '(0 . "*text")(cons 7 st))))
		)
	)
)
;����shx����
(defun av:toshx (x shxx shxb)(vla-put-fontfile x shxx)(vla-put-bigfontfile x shxb))
(defun av:fontstoshx(shxx shxb)
	(vlax-for x *sts* (av:toshx x shxx shxb)
		;(princ (entmod (entget (tblobjname "style" (vla-get-name x)))))
	)
	(princ (strcat "\n>>>�����滻Ϊ" shxx "��" shxb))
)
;����������
(defun av:tofft(x ttf)
	(vla-getfont x 'a 'b 'c 'd 'e)
	(vla-setfont x ttf b c d e)
	;����һ������ˢ�·�ʽ
	;(vlax-invoke-method x 'setfont ttf :vlax-false :vlax-false 0 0)
)
(defun av:fontstottf(ttf)
	(vlax-for x *sts* (av:tofft x ttf))
	(princ (strcat "\n>>>�����滻Ϊ" ttf))
)
;������������
(defun av:fontstofh(shxx shxb / xn)
	(vlax-for x *sts*
		(vla-getfont x 'a 'b 'c 'd 'e)
		(setq xn (vla-get-name x))
		(cond
			((wcmatch xn "*����*") (vla-setfont x "����" b c d e))
			((wcmatch xn "*����*") (vla-setfont x "����" b c d e))
			((wcmatch xn "*����*") (vla-setfont x "����" b c d e))
			((wcmatch xn "*����*") (vla-setfont x "����" b c d e))
			(t (av:toshx x shxx shxb))
		)
	)
	(princ (strcat "\n>>>�����滻Ϊ" shxx "��" shxb "������ttf������"))
)
;�������滻��ԭ���Ͽ�ͼ�Զ����У������ֶ���
(defun av:nulltofill	(shxx shxb ttf / big err shx)
	(vlax-for x	*sts*
		(vla-getfont x 'a 'b 'c 'd 'e)
		(if (= a "")
			(progn
				(cond
					((findfile (setq shx (vla-get-fontfile x))))
					((findfile (setq shx (strcat shx ".shx"))))
					(t (vla-put-fontfile x shxx))
				)
				(cond
					((findfile (setq big (vla-get-bigfontfile x))))
					((findfile (setq big (strcat big ".shx"))))
					(t (vla-put-bigfontfile x shxb))
				)
			)
			(progn
				(setq err	(vl-catch-all-apply 'vla-setfont (list x a b c d e)))
				(if (vl-catch-all-error-p err) (vla-setfont x ttf b c d e))
			)
		)
	)
	(princ (strcat "\n>>>�����ͷֱ��滻Ϊ" shxx "��" shxb "��" ttf))
)
;================================================================
(defun av:regen (ss / msg n pt ssn x)
	(repeat (setq n (sslength ss))
		(setq n (1- n))
		(setq ssn (ssname ss n))
		(setq x (vl-catch-all-apply 'vlax-ename->vla-object (list ssn)))
		(cond
			((vl-catch-all-error-p x))
			(t
				(setq pt (vlax-3D-point 0 0))
				(setq msg (vl-catch-all-apply 'vla-Move (list x pt pt)))
				(if (vl-catch-all-error-p msg) (entupd ssn))
			)
		)
	)
)

(defun c:rr(/ 4pt pt11 pt22 ss)
	(setq
		4pt (av:getscr4pt)
		pt11 (car 4pt)
		pt22 (caddr 4pt)
	)
	(cond
		((setq ss (ssget "C" pt11 pt22))
			(princ ">>>��ǰ����ֲ�������")
			(av:regen ss)
			;(setvar "cmdecho" 0)
			;(command "move" ss "" "0,0,0" "0,0,0")
			;(setvar "cmdecho" 1)
		)
		(t (princ ">>>��ǰ������ͼԪ"))
	)
	(princ)
)
;�ֽ⵱ǰ��Ļ��������ı�
;(defun explode-screen-mtext (/ ss)
;	(setvar "qaflags" 1);����cmdը��ѡ������ʵ��
;	(and
;		(setq ss (ssscreen))
;		(setq ss (ssget "p" '((0 . "mtext"))))
;		(command "explode" ss "")
;	)
;	(setvar "qaflags" 0)
;)


;;˵��:�����Զ��滻����
(defun ChangeFonts (/ file fn fn1 fn2)
	(setvar "cmdecho" 0)
	(cond
		((and
			 (>= *ver4* 2012)
			 (setq file (av:findfile "ChangeFonts2012.dll"))
		 )
			(command-s "netload" file)
		)
		(t
			(setvar "fontalt" "tssdeng.shx")
			(av:nulltofill "AllInOneUni" "AllInOneBig" "����")
			(av:regen (ssget "x" '((0 . "*TEXT,INSERT"))))
		)
	)
	(setvar "cmdecho" 1)
	(vl-acad-undefun 'ChangeFonts)
	(princ)
)
(ChangeFonts)
;;;==========================================================================
;;˵��:����������ָ�ʽ
(defun c:CleanMtextFormat()
	(AV:CleanMtextFormat
		(ssget '((0 . "MTEXT")))
		'(
			 ;("\\\\P|\\n|\\t" . "")
			 ("\\\\\\\\" . "\001")
			 ("\\\\{" . "\002")
			 ("\\\\}" . "\003")
			 ("\\\\p(.[^;]*);" . "")
			 ("\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);" . "")
			 ("(\\\\F|\\\\f|\\\\C|\\\\H|\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);" . "")
			 ("(\\\\L|\\\\O|\\\\l|\\\\o)" . "")
			 ("\\\\~" . "")
			 ("({|})" . "")
			 ("\\x01" . "")
			 ("\\x02" . "")
			 ("\\x03" . "\\")
		 )
	)
)
(defun AV:CleanMtextFormat(ss lst / rx st)
	(if (and ss
				(setq rx (vlax-create-object "Vbscript.RegExp")) ;����������ʽ
			)
		(progn
			(vlax-put-property rx "IgnoreCase" 0)
			(vlax-put-property rx "Global" 1)
			(vlax-for e (vla-get-activeselectionset *doc*)
				(setq st (vla-get-TextString e))
				(foreach x lst
					(vlax-put-property rx "Pattern" (car x))
					(setq st (vlax-invoke-method rx "Replace" st (cdr x)))
				)
				(vl-catch-all-apply 'vla-put-TextString (list e st))
			)
			(vlax-release-object rx)
			(princ)
		)
	)
)
(princ)
