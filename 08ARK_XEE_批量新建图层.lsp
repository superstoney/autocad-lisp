(defun c:ARK_XEE ()
  (vl-load-com)
  (if (null csvpath)
    (setq csvpath "")
  )
  (setq	file (getfiled "�����ݱ�"
		       csvpath
		       "xlsx;xls"
		       16
	     )
  )
  (if file
    (progn
      (setq csvpath (strcat (vl-filename-directory file) "\\"))
      (setq LnWtlist '(000   005   009	 013   015   018   020	 025
		       030   035   040	 050   053   060   070	 080
		       090   100   106	 120   140   158   200	 211
		      )
      )
      (setq appsession (vlax-get-or-create-object "Excel.Application"))
      (vlxls-app-Init)
      (if (setq *xlapp* (vlxls-app-open file nil))
	(setq Range (vlxls-sheet-get-usedrange *xlapp* nil)
	      value (vlax-get-property Range "Value2")
	      data  (vlxls-variant->list value)
	)
      )
      (vlax-release-object range)
      (vlax-release-object *xlapp*)
      (cond
	((not (vlax-object-released-p appsession))
	 (vlax-invoke-method appsession 'quit)
	 (vlax-release-object appsession)
	)
      )
      (if data
	(progn
	  (setq num (length data))
	  (setq n -1)
	  (while (> num 0)
	    (setq layer-data (nth (setq n (1+ n)) data))
	    (if	(and (not (vl-string-search "<" (nth 0 layer-data) 0))
		     (/= "" (nth 0 layer-data))
		)
	      (progn
		(if
		  (not
		    (tblsearch "ltype" (setq LnType (nth 1 layer-data)))
		  )
		   (progn
		     (setq Ark-acadobj	  (vlax-get-acad-object)
			   Ark-acaddocobj (vla-get-activedocument
					    Ark-acadobj
					  )
			   Ark-linetypes  (vla-get-linetypes Ark-acaddocobj)
		     )
		     (if
		       (or
			 (not (vl-catch-all-apply
				'vla-load
				(list Ark-linetypes
				      LnType
				      "acadiso.lin"
				)
			      )
			 )
			 (not (vl-catch-all-apply
				'vla-load
				(list Ark-linetypes
				      LnType
				      "acad.lin"
				)
			      )
			 )
		       )
			(princ (strcat "\n����" LnType "�ɹ�����"))
			(progn
			  (if (= "" (nth 1 layer-data))
			    (setq LnType "continuous")
			    (progn
			      (princ
				(strcat
				  "\n����"
				  LnType
				  "����ʧ��,�Ѿ������͡�continuous�����."
				)
			      )
			      (setq LnType "continuous")
			    )
			  )
			)
		     )
		   )
		)
		(if (= (tblobjname "LAYER" (nth 0 layer-data)) nil)
		  (progn
		    (entmake
		      (list
			'(0 . "LAYER")
			'(100 . "AcDbSymbolTableRecord")
			'(100 . "AcDbLayerTableRecord")
			(cons 2 (nth 0 layer-data)) ;����
			(cons 6 LnType)	;����
			(if (= "" (nth 3 layer-data))
			  (cons 370 -3)	;-1 ��� -2 ��� -3Ĭ��
			  (cons	370	;�߿�(50����0.5MM)
				(Matching-LnWt
				  (fix (* (atof (nth 3 layer-data)) 100))
				  LnWtlist
				)
			  )
			)
			(if (= "" (nth 2 layer-data))
			  (cons 62 7)	;��ɫ
			  (cons 62 (atoi (nth 2 layer-data))) ;��ɫ
			)
			'(70 . 0)
			(cons 290	;1��ӡ��0����ӡ
			      (if
				(OR
				  (= "Y" (nth 4 layer-data))
				  (= "y" (nth 4 layer-data))
				  (= "" (nth 4 layer-data))
				)
				 1
				 0
			      )
			)
		      )
		    )
		    (setq AcadObject   (vlax-get-acad-object)
			  AcadDocument (vla-get-ActiveDocument Acadobject)
			  mSpace       (vla-get-ModelSpace Acaddocument)
		    )
		    (setq layers (vla-get-Layers AcadDocument))
		    (setq lya_data (vla-add layers (nth 0 layer-data)))
		    (vla-put-Description lya_data (nth 5 layer-data))
		  )
		  (progn
		    (setq AcadObject   (vlax-get-acad-object)
			  AcadDocument (vla-get-ActiveDocument Acadobject)
			  mSpace       (vla-get-ModelSpace Acaddocument)
		    )
		    (setq layers (vla-get-Layers AcadDocument))
		    (setq lya_data (vla-add layers (nth 0 layer-data)))
		    (vla-put-linetype lya_data LnType) ;�����޸�
		    (if	(/= "" (nth 3 layer-data)) ;�߿��޸�
		      (Modify-LnWt
			(nth 0 layer-data)
			(fix (* (atof (nth 3 layer-data)) 100))
		      )
		      (vla-put-lineweight
			(vla-item
			  (vla-get-layers
			    (vla-get-activedocument
			      (vlax-get-acad-object)
			    )
			  )
			  (nth 0 layer-data)
			)
			-3
		      )
		    )
		    (if	(/= "" (nth 2 layer-data)) ;��ɫ�޸�
		      (vla-put-Color lya_data (atoi (nth 2 layer-data)))
		      (vla-put-Color lya_data 7)
		    )
		    (if			;��ӡ�޸�
		      (OR
			(= "Y" (nth 4 layer-data))
			(= "y" (nth 4 layer-data))
			(= "" (nth 4 layer-data))
		      )
		       (vla-put-Plottable lya_data :vlax-true)
		       (vla-put-Plottable lya_data :vlax-false)
		    )
		    (if	(nth 5 layer-data) ;˵���޸�
		      (vla-put-Description lya_data (nth 5 layer-data))
		    )
		  )
		)
	      )
	    )
	    (setq num (1- num))
	  )
	)
      )
      (princ "\n����(���޸�)ͼ�����")
    )
    (princ "\nͼ��δ�ܴ���(���޸�)")
  )
  (princ)
)
 ;|
Excel Sheet Progress Function
Name	(vlxls-sheet-get-usedrange ExcelSessionVLA-OBJECT SheetName)
Usage	Get all used range of certain Excel sheet. If sheet name not exist, return NIL.
Input	VLOBJ 	Excel session vla-object
STRING	Excel sheet name string, NIL for current active sheet.
RetVal	True	VLOBJ	Excel Range vla-object
Fail	BOOLE	NIL
Examples:
(vlxls-sheet-get-usedrange *xlapp* "Sheet1")   T
(vlxls-sheet- get-usedrange *xlapp* "NewSheet")   T
|;
(defun vlxls-sheet-get-UsedRange (xlapp Name / sh Rtn)
  (if (null Name)
    (setq Name (cadr (XD::Excel:getactiveSheet)))
  )
  ;|
	(if (null Name)
	(setq Name (vlax-get-property (msxl-get-ActiveSheet Xlapp) 'Name));msxl-get-ActiveSheet������ʱ�޷����ص���
)
	|;
  (vlax-for sh (vlax-get-property Xlapp "sheets")
    (if	(= (vlax-get-property sh "Name") Name)
      (setq Rtn (vlax-get-property sh "UsedRange"))
    )
  )
  Rtn
)
 ;|
�������ԡ�����CAD��
��������:	XD::Excel:getactiveSheet
���ø�ʽ:	(XD::Excel:getactiveSheet)
����˵��:	��
����ֵ:	�ɹ�����(������ ������),���򷵻�nil��
ע�������Ŵ�0��ʼ
�������:	��ȡ��ǰ���񣬳ɹ�����(������ ������),
������Դ:	ԭ��
��������:
���ð汾:	����
������ʱ��:	2014-11-11
��ע:	��ȡ��ǰ����ɹ�����(������ ������),���򷵻�nil��ע�������Ŵ�0��ʼ
|;
(defun XD::Excel:getactiveSheet	(/ sh)
  (or
    **XD::Excel**
    (setq **XD::Excel** (vlax-get-or-create-object "Excel.Application"))
  )
  (setq sh (vlax-get-property **XD::Excel** 'activesheet))
  (list	(1- (vlax-get-property sh 'index))
	(vlax-get-property sh 'name)
  )
)
 ;|
Public Function
Name	(vlxls-variant->list VariantValue)
Usage	Convert a variant into normal Visual LISP LIST data, nested Variant and safearray will also be converted.
Input	VARIANT	Input Variant
RetVal	True	LIST	Valid Visual LISP variable value
Fail	STR	""
|;
(Defun vlxls-variant->list (VarX / Run Item Rtn)
  (setq Run T)
  (while
    Run
     (cond ((= (type VarX) 'SAFEARRAY)
	    (setq VarX (vlax-safearray->list VarX))
	   )
	   ((= (type VarX) 'VARIANT)
	    (if	(member (vlax-variant-type VarX) (list 5 4 3 2))
	      (setq VarX (vlax-variant-change-type Varx vlax-vbString))
	    )
	    (setq VarX (vlax-variant-value VarX))
	   )
	   (t (setq Run nil))
     )
  )
  (cond	((= (type VarX) 'LIST)
	 (foreach Item VarX
	   (setq Item (vlxls-variant->list Item)
		 Rtn  (append Rtn (list Item))
	   )
	 )
	)
	((= VarX nil) (setq Rtn ""))
	(t (setq Rtn VarX))
  )
  Rtn
)
 ;|
Excel Application Session Progress Function
Name	(vlxls-app-open XLSfilename ShowExcelFlag)
Usage	Open a new Excel session to start existing XLS file.
Input	STR	XLS file name with full path, ".XLS" not needed.
BOOLE	T for display, nil for hide
RetVal	True	VLOBJ	Excel Session vla-object
Fail	BOOLE	NIL
Examples:
(setq *xlapp* (vlxls-app-open "C:/test.XLS" T))   #<VLA-OBJECT _Application 001efd2c>
|;
(Defun vlxls-app-open
       (XLSFile UnHide / ExcelApp WorkSheet Sheets ActiveSheet Rtn)
  (setq XLSFile (strcase XLSFile))
  ;|
	(if (null (wcmatch XLSFile "*.XLSX"))
	(setq XLSFile (strcat XLSFile ".XLSX"))
)
	|;
  (if (and (findfile XLSFile)
	   (setq Rtn (vlax-get-or-create-object "Excel.Application"))
      )
    (progn
      (vlax-invoke-method
	(vlax-get-property Rtn 'WorkBooks)
	'Open
	XLSFile
      )
      (if UnHide
	(vla-put-visible Rtn 1)
	(vla-put-visible Rtn 0)
      )
    )
  )
  Rtn
)
 ;|
Excel Application Session Progress Function
Name	(vlxls-app-init)
Usage	Import Microsoft Excel Type Library, set prefix of "msxl-" for all of the :methods-prefix; :properties-prefix & :constants-prefix. This function can detect Excel's installation path automatically from Windows registry so that it can run smoothly on any language platform of Windows and Office.
Input	NONE	No Arguments
RetVal	True	BOOLE	msxl-xl24HourClock
Fail	BOOLE	NIL
Examples:
(vlxls-app-init)  33
|;
(Defun vlxls-app-Init
       (/ OSVar GGG Olb8 Olb9 Olb10 TLB Out msg msg1 msg2)
  (if *Chinese*
    (setq msg  "\n ��ʼ��΢��Excel "
	  msg1 "\042��ʼ��Excel����\042"
	  msg2 (strcat
		 "\042 ����"
		 "\n ===="
		 "\n �޷������ļ�����ϼ�⵽΢��Excel���"
		 "\n �����ȷ���Ѿ���װExcel, �뷢�͵�����"
		 "\n ����kozmosovia@hotmail.com��ȡ����Ľ������\042"
		)
    )
    (setq msg  "\n Initializing Microsoft Excel "
	  msg1 "\042Initialization Error\042"
	  msg2 (strcat
		 "\042 WARNING"	"\n ======="
		 "\n Can NOT detect Excel97/200X/XP in your computer"
		 "\n If you already have Excel installed, please email"
		 "\n us to get more solution via GuXiaolin@hxch.com.cn\042")
    )
  )
  (if (null msxlc-xl24HourClock)
    (progn
      (if (and (setq GGG
		      (vl-registry-read
			"HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Excel.EXE"
			"Path"
		      )
	       )
	       (setq GGG (strcase (strcat GGG "Excel.EXE")))
	  )
	(progn
	  (foreach OSVar (list "SYSTEMROOT"	 "WINDIR"
			       "WINBOOTDIR"	 "SYSTEMDRIVE"
			       "USERNAME"	 "COMPUTERNAME"
			       "HOMEDRIVE"	 "HOMEPATH"
			       "PROGRAMFILES"
			      )
	    (if	(vl-string-search (strcat "%" OSVar "%") GGG)
	      (setq GGG	(vl-string-subst
			  (strcase (getenv OSVar))
			  (strcat "%" OSVar "%")
			  GGG
			)
	      )
	    )
	  )
	  (setq	Olb8  (findfile
			(vl-string-subst "EXCEL8.OLB" "EXCEL.EXE" GGG)
		      )
		Olb9  (findfile
			(vl-string-subst "EXCEL9.OLB" "EXCEL.EXE" GGG)
		      )
		Olb10 (findfile	(vl-string-subst
				  "EXCEL10.OLB"
				  "EXCEL.EXE"
				  GGG
				)
		      )
	  )
	  (cond
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE15"
	     )
	     (setq TLB GGG
		   Out "2013"
	     )
	    )
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE14"
	     )
	     (setq TLB GGG
		   Out "2010"
	     )
	    )
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE12"
	     )
	     (setq TLB GGG
		   Out "2007"
	     )
	    )
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE11"
	     )
	     (setq TLB GGG
		   Out "2003"
	     )
	    )
	    ((=	(vl-filename-base (vl-filename-directory GGG))
		"OFFICE10"
	     )
	     (setq TLB GGG
		   Out "XP"
	     )
	    )
	    (Olb9
	     (setq TLB Olb9
		   Out "2000"
	     )
	    )
	    (Olb8
	     (setq TLB Olb8
		   Out "97"
	     )
	    )
	    (t
	     (setq TLB GGG
		   Out "Version Unknown"
	     )
	    )
	  )
	  (if TLB
	    (progn
	      (princ (strcat MSG Out "..."))
	      (vlax-import-type-library
		:tlb-filename	   TLB
		:methods-prefix	   "msxl-"
		:properties-prefix "msxlp-"
		:constants-prefix  "msxlc-"
	       )
	    )
	  )
	)
	(progn
	 ;|(if vldcl-msgbox
					(vldcl-msgbox "x" msg1 msg2)
					(alert (read msg2))
				)|;
	  (alert msg2)
	  (exit)
	)
      )
    )
  )
  msxlc-xl24HourClock
)
;;����
;;���ر�����Сֵ
;;lstҪ����ı�
(defun Min-lst (lst)
  (last
    (vl-sort
      lst
      (function
	(lambda	(e1 e2)
	  (> e1 e2)
	)
      )
    )
  )
)
;;�߿�ƥ��
;;�ӱ��з������û�������ӽ����߿�
;;width �û������߿�
;;acLnWtlist �߿��
(defun Matching-LnWt (width acLnWtlist)
  (setq	LnWtlist '(000	 005   009   013   015	 018   020   025
		   030	 035   040   050   053	 060   070   080
		   090	 100   106   120   140	 158   200   211
		  )
  )
  (setq LnWtlen (length LnWtlist))
  (setq LnWtlist (mapcar '(lambda (x) (abs (- width x))) LnWtlist))
  (setq
    LnWtnth (- LnWtlen (length (member (Min-lst LnWtlist) LnWtlist)))
  )
  (setq LnWt (nth LnWtnth acLnWtlist))
  LnWt
)
;;�޸�ͼ���߿�
;;layname ͼ������
;;acwidth Ҫ���õ��߿�
(defun Modify-LnWt (layname acwidth)
  (setq	acLnWtlist
	 (list acLnWt000     acLnWt005	   acLnWt009	 acLnWt013
	       acLnWt015     acLnWt018	   acLnWt020	 acLnWt025
	       acLnWt030     acLnWt035	   acLnWt040	 acLnWt050
	       acLnWt053     acLnWt060	   acLnWt070	 acLnWt080
	       acLnWt090     acLnWt100	   acLnWt106	 acLnWt120
	       acLnWt140     acLnWt158	   acLnWt200	 acLnWt211
	      )
  )
  (vla-put-lineweight
    (vla-item (vla-get-layers
		(vla-get-activedocument (vlax-get-acad-object))
	      )
	      layname
    )
    (Matching-LnWt acwidth acLnWtlist)
  )
)
;;������
(if (not (or (getcname "XEE") c:XEE))
  (progn
    (defun c:XEE ()
      (c:ARK_XEE)
    )
    (princ "\n>>>>��������ͼ��,��������XEE��ARK_XEE<<<<")
    (princ)
  )
  (progn
    (princ "\n>>>>��������ͼ��,��������ARK_XEE<<<<")
    (princ)
  )
)