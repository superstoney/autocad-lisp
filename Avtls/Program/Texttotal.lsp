;�ı������ͳ��
(defun c:texttotal (/ *error* key ss textlst)
	(vl-load-com)
	(defun *error*(str)(if(or(= str "quit / exit abort")(= str "������ȡ��"))nil)(setvar "cmdecho" 1)(princ))
	;(princ "-->�ı������ͳ��")
	(setvar "cmdecho" 0)
	(princ "\n��ѡ���ı����ͣ��ı�+���֣��Է�����͡�<ֱ�ӻس��л�Ϊ������ͻ��ı�ͳ��>")
	(if (setq ss (ssget ":L" (list '(0 . "text"))))
		(if (setq textlst (texttotal1 ss))
			(opennotepad textlst)
			(princ "������ѡ�񣬵����ı�+���֣��磺��ҶŮ��C 7����ҶŮ��B 8 ��")
		)
		(if
			(progn
				(setq key (getint "\n��ѡ�񷽰�[(1)�������/(2)�ı�ͳ��]��<Ĭ��Ϊ2>"))
				(setq ss (ssget ":L" (list '(0 . "text"))))
			)
			(cond
				((= 1 key) (textsum ss))
				((or (null key) (= 2 key))
					(setq textlst (texttotal2 ss))
					(opennotepad textlst)
				)
				(t nil)
			)
		)
	)
	(*error* nil);�ָ�ԭ�в���
)
;�ı��������
(defun c:textsum(/ ss)
	(setq ss (ssget ":L" (list '(0 . "text"))))
	(textsum ss)
	(princ)
)
;�ı�ͳ��
(defun c:texttotal2(/ ss)
	(setq ss (ssget ":L" (list '(0 . "text"))))
	(opennotepad (texttotal2 ss))
	(princ)
)

;ͳ�ƽ����¼��չʾ
(defun opennotepad(textlst / f fn)
	(setq fn (vl-filename-mktemp nil nil ".txt"))
	(setq f (open fn "w"))
	(foreach text textlst 
		(write-line (if (listp text)(apply 'strcat text) text) f)
	)
	(close f)
	(startapp "notepad.exe" fn)
	(and (vl-cmdf "delay" 1000) (vl-file-delete fn))
)

;=================================================================================
;���ַ�������������ʽ���Լ��滻
(defun av:xxexp (pat str key / *xxvbsexp end keys matches)
	(vl-load-com)
	(if (not *xxvbsexp)
		(setq *xxvbsexp (vlax-get-or-create-object "VBScript.RegExp"))
	)
	(vlax-put *xxvbsexp 'Pattern pat)
	(if (not key)(setq key ""))  
	(setq key (strcase key))
	(setq keys '(("I"  "IgnoreCase")("G"  "Global")("M"  "Multiline")))
	(mapcar '(lambda(x)
						 (if (wcmatch key (strcat "*" (car x) "*"))
							 (vlax-put *xxvbsexp (read(cadr x)) 0)
							 (vlax-put *xxvbsexp (read(cadr x)) -1)
						 ))
		keys)
	(setq matches (vlax-invoke *xxvbsexp 'Execute str))
	(vlax-for x matches (setq end (cons (vla-get-value x) end)))
	(reverse end)
)


;�ı����ַ������
(defun texttotal1(ss / ent n namelst newtext num text textlst txth)
	(vla-StartUndoMark *doc*)
	(setq namelst '() textlst '())
	(setq ent (cdr (entget (ssname  ss 0))))
	(setq txth (cdr (assoc 40 ent)))
	(repeat (setq n (sslength ss))
		(setq namelst (cons (vlax-ename->vla-object(ssname  ss (setq n (1- n)))) namelst))
	)
	(setq namelst (mapcar '(lambda (x) (av:xxexp "[\\-,\\d,\\.]+|[^\\-,\\d,\\.]+" (vla-get-TextString x) "")) namelst))
	;�ϲ�ͬ����
	(foreach name namelst
		(if (setq text (assoc (car name) textlst))
			(progn
				(if (and (= 2 (vl-list-length name)) (numberp (setq num (read (cadr name)))))
					(progn
						(setq num (+ num (read (cadr text))))
						(setq newtext (list (car text) (vl-princ-to-string num)))
						(setq textlst (subst newtext text textlst))
					)
				)
			)
			(progn
				(if (and (= 2 (vl-list-length name)) (numberp (setq num (read (cadr name)))))
					(setq textlst (cons name  textlst))
				)
			)
		)
	)
	(vla-EndUndoMark *doc*)
	textlst
)


;�ı�ͳ��
(defun texttotal2(ss / ename endate f i j k textlst trtj trtn tsort txt)
	(setq i 0)
	(setq tsort '())
	(setq textlst '())
	(repeat (sslength ss)
		(setq ename (ssname ss i))
		(setq endate (entget ename))
		(setq txt (cdr (assoc 1 endate)))
		(setq tsort (cons txt tsort))
		(setq i (+ i 1))
	)
	(setq tsort (vl-sort tsort '<))
	(setq j 0)
	(setq k 1)
	(repeat (length tsort)
		(setq trtj (nth j tsort))
		(if	(/= j (length tsort))
			(setq trtn (nth (+ j 1) tsort))
		)
		(if	(= trtj trtn)
			(setq k (+ k 1))
			(progn
				(setq textlst (append textlst (list (strcat trtj "=" (rtos k 2 0)))))
				(setq k 1)
			)
		)
		(setq j (+ j 1))
	)
	(setq textlst (append textlst (list (strcat "\nͳ��������" (rtos j 2 0)))))
)

;==========================================================================
;�ı��������
;�ı��������֧��һ��text,mtext���ж�������ַ���,֧���ַ�����С��,����;
(defun textsum ( ss / *error* d draw getnumb str strs sum)
	;(princ "-->�������")
	(defun Draw (ss key) (foreach e (fsxm-ss->enlist ss)(redraw e key)))
	(defun GetNumb (str / filter)
		(setq filter (vl-string->list "0123456789.-+"))
		(setq 
			str (vl-list->string (mapcar '(lambda (a) (if (vl-position a filter) a 32)) (vl-string->list str))) 
			str (vl-string-trim " " str)
		)
		(strcat "0" (fsxm-replace (fsxm-replace str "+" " +" ) "-" " -"))
	)
	(defun *error*(msg) (if ss (Draw ss 4)))
	;(princ "\nѡ��Ҫ������ı�(֧��*TEXTѡ��):")
	;(setq ss (ssget '((0 . "*TEXT"))))
	(setq strs " ")
	(or ss (fsxm-silenceexit))
	(Draw ss 3)
	(foreach e (fsxm-ss->enlist ss)
		(setq d (entget e))
		(setq str (cdr (assoc 1 d)))
		(if	 (= "MTEXT" (cdr (assoc 0 d)))
			(setq str (fsxm-MStr2Str str))
		)
		(setq strs (strcat strs (GetNumb str) " "))
	)
	(if (apply '= (cons 32 (vl-string->list strs)))
		(progn
			(Draw ss 4)
			(princ "\n!û����ѡȡ�ı��ҵ���Ч����!\n")
			(fsxm-silenceexit)
		)
	)
	(setq sum (read (strcat "(" strs ")")))
	(setq sum (vl-remove-if-not '(lambda (a) (numberp a)) sum))
	(setq sum (fsxm-tostring (apply '+ sum)))
	(princ "\n�ı������ܺ�Ϊ: ")
	(princ sum)
	(Draw ss 4)
	(princ)
)


;(DEFUN FSXM-SS->ENLIST( SS  / LST N EN )  (setq N -1)  (while (and (setq  EN (SSNAME SS (setq  N (1+ N )) ))  ) (setq  LST (cons EN LST)) )  )
;(DEFUN FSXM-REPLACE( STR OLD NEW  / LEN PO START )  (setq LEN (STRLEN NEW )) (setq START 0) (while (and (setq  PO (VL-STRING-SEARCH OLD STR START ))  ) (setq STR (VL-STRING-SUBST NEW OLD STR PO )) (setq START (+ PO LEN )) ) STR  )
;(DEFUN FSXM-SILENCEEXIT( / *ERROR* )  (T (setq  *ERROR* STRCAT) )  )
;(DEFUN FSXM-MSTR2STR( S  / REGEX )  (setq REGEX (vlax-create-object "Vbscript.RegExp" )) (vlax-put-property REGEX "IgnoreCase" 0 ) (vlax-put-property REGEX "Global" 1 ) (vlax-put-property REGEX "Pattern" "\\\\\\\\" ) (setq S (vlax-invoke-method REGEX "Replace" S (CHR 1 ) )) (vlax-put-property REGEX "Pattern" "\\\\{" ) (setq S (vlax-invoke-method REGEX "Replace" S (CHR 2 ) )) (vlax-put-property REGEX "Pattern" "\\\\}" ) (setq S (vlax-invoke-method REGEX "Replace" S (CHR 3 ) )) (vlax-put-property REGEX "Pattern" "\\\\pi(.[^;]*);" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "\\\\pt(.[^;]*);" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "\\\\~" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "\\\\P" ) (setq S (vlax-invoke-method REGEX "Replace" S "\n" )) (vlax-put-property REGEX "Pattern" "({|})" ) (setq S (vlax-invoke-method REGEX "Replace" S "" )) (vlax-put-property REGEX "Pattern" "\\x01" ) (setq S (vlax-invoke-method REGEX "Replace" S "\\" )) (vlax-put-property REGEX "Pattern" "\\x02" ) (setq S (vlax-invoke-method REGEX "Replace" S "{" )) (vlax-put-property REGEX "Pattern" "\\x03" ) (setq S (vlax-invoke-method REGEX "Replace" S "}" )) (vlax-release-object REGEX ) S  )
;(DEFUN FSXM-TOSTRING( A  / TOSTR )  (DEFUN TOSTR( A  / TP )  (setq TP (TYPE A )) (COND ((= TP 'REAL ) (FSXM-NUMBER->STRING A ) ) ((= TP 'STR ) (VL-PRIN1-TO-STRING A ) ) ((LISTP A )  (if (LISTP (cdr A ) ) (PROGN   (MAPCAR 'TOSTR A ) )(PROGN   (cons (TOSTR (car A ) ) (TOSTR (cdr A ) )) )) ) (T A ) )  ) (VL-PRINC-TO-STRING (TOSTR A ) )  )


(princ)




