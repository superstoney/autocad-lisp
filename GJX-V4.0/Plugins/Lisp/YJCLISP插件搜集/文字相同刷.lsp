
(setq source_text "") ; ��Դ����Ϊȫ�ֱ���
(defun c:fa (/ en en_dxf en1 en1_dxf ent entype entype_source i ob pt source_text2 ss ss_data str txtst)
  (defun *error* (x) ;������
		(if en (redraw en 4))
		(setvar "ErrNo" 0)
		(setvar "cmdecho" 1)
  )
  (setvar "cmdecho" 0)
  (setvar "ErrNo" 0)
  (if (= source_text "")
		(setq str "\n��ѡ��Դ����(�Ҽ��˳�):")
		(setq str (strcat "\n��ѡ��Դ����: Ĭ��:" source_text))
	)
	(if (and (not (setq en (car (nentsel str)))) (= (getvar "ErrNo") 52))
		(progn
			(setvar "ErrNo" 0)
			(if (= source_text "")
				(setq txtst nil)
				(setq txtst T)
			)
		)
		(if en
			(progn
				(setq en_dxf (entget en))
				(setq entype_source (cdr (assoc 0 en_dxf)))
				(if (or (setq txtst (cdr (assoc 7 en_dxf)))(= entype_source "TCH_DRAWINGNAME")(= entype_source "MULTILEADER"))
					(progn
						(redraw en 3)
						(setq source_text (cdr (assoc 1 en_dxf)))
						(setq source_text2 "")
						(cond
							((= entype_source "ATTDEF") ;����������֣���ȡ����ǡ�ΪԴ����
								(setq source_text (cdr (assoc 2 en_dxf)))
							)
							((= entype_source "TCH_MULTILEADER") ;����������ע
								(setq source_text2 (cdr (assoc 2 en_dxf)))
							)
							((= entype_source "TCH_ARROW") ;������ͷ��ע
								(setq source_text2 (vlax-get-property (vlax-ename->vla-object en) 'Text2))
							)
							((= entype_source "MULTILEADER") ;CAD������ע
								(setq source_text (cdr (assoc 304 en_dxf)))
							)
						)
					)
				)
			)
			(setvar "ErrNo" 52)
		)
	)
  (if (or txtst (= entype_source "TCH_DRAWINGNAME")(= entype_source "MULTILEADER"))
		(progn
			(prompt "\n��ѡ��Ҫ�޸����ݵ�����:")
			(while (/= (getvar "ErrNo") 52)
				(prompt (strcat "\n�������ݽ���ˢ��:" source_text))
				(if (and (setq ss (ssget ":S" '((0 . "*TEXT,TCH_DRAWINGNAME,TCH_ELEVATION,INSERT,ATTDEF,ATTRIB,TCH_MULTILEADER,TCH_ARROW,MULTILEADER")))) source_text)
					(progn
						(if (= (caar (setq ss_data (ssnamex ss 0))) 1)
							(progn  ;��ѡʱ
								(setq ent (ssname ss 0)
									pt (trans (CADr (last (car ss_data))) 0 1)
									en1 (car (nentselp pt))
									en1_dxf (entget en1)
									entype (cdr (assoc 0 en1_dxf))
									ob (vlax-ename->vla-object en1)
								)
								(wenzishua entype entype_source ob source_text en1 ent source_text2)
							)
							(progn  ;��ѡʱ
								(setq i 0)
								(repeat (sslength ss)
									(setq en1 (ssname ss i)
										en1_dxf (entget en1)
										entype (cdr (assoc 0 en1_dxf))
										ob (vlax-ename->vla-object en1)
									)
									(wenzishua entype entype_source ob source_text en1 en1 source_text2)
									(setq i (1+ i))
								)
							)
						)
					)
				)
			);end while
		)
	)
  (if en (redraw en 4))
  (setvar "ErrNo" 0)
  (setvar "cmdecho" 1)
  (princ)
)

;����ˢ�ӳ���
(defun wenzishua (entype entype_source ob source_text en1 ent source_text2)
	;ȥ�������������ø�ʽ����
	(if (= entype_source "MTEXT")
		(setq source_text (mtext2text source_text))
	)	
  (cond
		;CAD��������
		((= entype "MTEXT")
			(vla-put-TextString ob source_text)
			(entupd en1)
			(entupd ent)
		)
		;CAD�������֡�������ע
		((or (= entype "TEXT") (= entype "MULTILEADER")) 
			(vla-put-TextString ob source_text)
			(entupd en1)
			(entupd ent)
		)
		;�������֡����
		((or (= entype "TCH_TEXT")(= entype "TCH_ELEVATION"))
			(vlax-put-property ob 'Text source_text)
			(entupd en1)
			(entupd ent)
		)   
		;����ͼ��
		((= entype "TCH_DRAWINGNAME")
			(progn
				(vlax-put-property ob 'NameText source_text)
				(entupd en1)
				(entupd ent)
			)
		)
		;�������� ֻ��"���"
		((= entype "ATTDEF")
			(vla-put-TagString ob source_text);�ı��
			(entupd en1)
			(entupd ent)
		)
		;������������ ֻ��"Ĭ��"
		((= entype "ATTRIB")
			(vla-put-TextString ob source_text);��Ĭ��
			(entupd en1)
			(entupd ent)
		)
		;����������ע
		((= entype "TCH_MULTILEADER")
			(progn
				(vlax-put-property ob 'UpText source_text)   ;�ϱ�
				(vlax-put-property ob 'DownText source_text2);�±�
				(entupd en1)
				(entupd ent)
			)
		)
		;������ͷ��ע
		((= entype "TCH_ARROW")
			(vlax-put-property ob 'Text source_text)  ;�ϱ�    
			(vlax-put-property ob 'Text2 source_text2);�±�
			(entupd en1)
			(entupd ent)
		)
	)
)

;��ȡ��������,ȥ�����ø�ʽ����--��������
(defun mtext2text(MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
  (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
  (setq s MTextString)
	;�滻\\�ַ�
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
	;�滻\{�ַ�
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
	;�滻\}�ַ�
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
	;ɾ������������ʽ
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ���Ʊ����ʽ
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ���ѵ���ʽ
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ�����塢��ɫ���ָߡ��־ࡢ��б���ֿ������ʽ
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ���»��ߡ�ɾ���߸�ʽ
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ������Ͽո��ʽ
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ�����з���ʽ
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ�����з���ʽ(���Shift+Enter��ʽ)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	;ɾ��{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
	
	;�滻��\\,\{,\}�ַ�
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
	
  (vlax-release-object regex)
  s
)