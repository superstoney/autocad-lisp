;在图中进行序号递增标注，有多种形状及方式的切换。
(defun c:numinc
	(/	 *error* _alignment _attachment
		_blocks _layers _scalevars _styles a
		acspc	 alignment arr-end	 arr-qty arr-qty#
		arr-rot arr-rot# arr-typ	 arr-typ-fun
		arr-use arr-use-fun		 att-nme attachment
		attrib	 attribs b	 blk-nme blk-scl
		blk-scl-fun	 blk-scl#	 block	 blocks
		bor	 bor-enc bor-enc-fun	 bor-lay
		bor-rot bor-shp bor-shp-fun	 bor-sid
		bor-sid# bor-typ bor-typ-fun	 cfgfname
		create-bor create-obj dclflag	 dclfname dclid
		deg	 dyn-flg elst	 ent	 file
		fix-ed1 fix-ed1# fix-ed2	 fix-ed2# g1
		g2	 gr	 i	 inc-sec inc-str
		mid-str mode_color mode_image msg	 msk-col
		msk-off msk-off# msk-trn	 msk-trn-fun
		msk-use msk-use-fun		 mtw	 mtx-bak
		nm	 oba	 obj	 obj-typ obj-typ-fun
		off-ed1 off-ed1# p1	 p2	 pre-str
		prop	 pt	 r1	 savepath scalevars
		scl-pop scl-var ss	 string style
		suf-str symb symlist	 table	 tile
		tmp	 tog-cnt txt-aln	 txt-bst txt-lay
		txt-rot txt-sty txt-sty-fun	 txt-sze
		txt-sze# v1	 vallst	 varlst x
		xa);end /	
	(defun *error* (msg)
		(if
			(and
				(= 1 dclflag)
				(= 'str (type cfgfname))
				symlist);end and
			(numinc:writeconfig
				cfgfname
				(mapcar 'eval (mapcar 'car symlist))));end if
		(if
			(and
				(= 'vla-object (type numinc:wshobject))
				(not (vlax-object-released-p numinc:wshobject)));end and
			(progn
				(vlax-release-object numinc:wshobject)
				(setq numinc:wshobject nil)));end if
		(if
			(and
				(= 'ename (type mtw))
				(entget mtw));end and
			(entdel mtw));end if
		(if (= "1" dyn-flg)
			(foreach obj (list obj bor)
				(if (and (= 'vla-object (type obj))
							(not (vlax-erased-p obj))
							(vlax-write-enabled-p obj));end and
					(vla-delete obj))));end if
		(mapcar 'setvar varlst vallst)
		(if (= 'file (type file))
			(close file));end if
		(if (< 0 dclid)
			(unload_dialog dclid));end if
		(if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
			(princ (strcat "\n错误:" msg)));end if
		(princ));end defun
	(setq	varlst '(dimzin modemacro)
		vallst (mapcar 'getvar varlst));end setq	varlst
	(cond
		((=	4
			 (logand	4
				 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))));end =	4
			(princ "\n当前图层锁定。"));end "("
		((not
			 (vl-file-directory-p (setq savepath (numinc:getsavepath))));end not
			(numinc:popup
				"Save Path Invalid" 
				16
				(princ
					(strcat
						"下列路径不存在或无效:\n\n" 
						savepath))));end "("
		((progn
			 (setq dclfname (strcat savepath "\\NumInc.dcl")
				 cfgfname (strcat savepath "\\NumInc.cfg"));end setq
			 (not (numinc:writedcl dclfname)));end progn
			(numinc:popup
				"DCL File could not be Written" 
				16
				(princ
					(strcat
						"The DCL file required by this application was unable to be written to the following location:\n\n" 
						dclfname
						"\n\nPlease ensure that you have write permissions for this directory."))));end "("
		((<= (setq dclID (load_dialog dclfname)) 0)
			(numinc:popup
				"DCL File could not be Loaded" 
				16
				(princ
					(strcat
						"The following DCL file could not be loaded:\n\n" 
						dclfname
						"\n\nPlease check the integrity of this file."))));end "("
		(t
			(setq symlist
				(list
					(cons 'arr-use "0")
					(cons 'arr-qty "5")
					(cons 'arr-typ "arr-aln")
					(cons 'arr-rot "0.0")
					(cons 'arr-end nil)
					(cons 'crv-per (/ pi 2.0))
					(cons 'crv-off 0.0)
					(cons 'txt-rot 0.0)
					(cons 'bor-rot nil)
					(cons 'tog-cnt t)
					(cons 'dyn-flg "1")
					(cons 'pre-str "")
					(cons 'mid-str "1")
					(cons 'suf-str "")
					(cons 'inc-str "1")
					(cons 'inc-sec 2)
					(cons 'obj-typ "obj-txt")
					(cons 'blk-nme "")
					(cons 'att-nme "")
					(cons 'blk-scl "1.0")
					(cons 'scl-var "0")
					(cons 'scl-pop "DIMSCALE")
					(cons 'bor-enc "0")
					(cons 'bor-shp "0")
					(cons 'bor-sid "6")
					(cons 'bor-lay (getvar 'clayer))
					(cons 'bor-typ "bor-off")
					(cons 'off-ed1 "1.0")
					(cons 'fix-ed1 "1.0")
					(cons 'fix-ed2 "1.0")
					(cons 'txt-lay (getvar 'clayer))
					(cons 'txt-sty (getvar 'textstyle))
					(cons 'txt-aln "正中")
					(cons 'txt-bst "1")
					(cons 'txt-sze
						(rtos
							(if
								(zerop
									(cdr
										(assoc 40
											(setq style
												(tblsearch "style" (getvar 'textstyle))))));end zerop
								(cdr (assoc 42 style))
								(cdr (assoc 40 style)))));end cons
					(cons 'msk-use "0")
					(cons 'msk-off "1.5")
					(cons 'msk-trn "0")
					(cons 'msk-col '((62 . 1)))));end setq
			(if (null (findfile cfgfname))
				(numinc:writeconfig cfgfname (mapcar 'cdr symlist)));end if
			(numinc:readconfig cfgfname (mapcar 'car symlist))
			(foreach x	SymList
				(if (null (boundp (car x)))
					(set (car x) (cdr x))));end foreach
			(setq _layers (numinc:gettableitems "layer")
				_styles (numinc:gettableitems "style")
				_blocks (numinc:getblockdata));end setq
			(setq Alignment
				(list
					(cons "左" acAlignmentLeft)
					(cons "中间" acAlignmentCenter)
					(cons "右" acAlignmentRight)
					(cons "中心" acAlignmentMiddle)
					(cons "左上" acAlignmentTopLeft)
					(cons "中上" acAlignmentTopCenter)
					(cons "右上" acAlignmentTopRight)
					(cons "左中" acAlignmentMiddleLeft)
					(cons "正中" acAlignmentMiddleCenter)
					(cons "右中" acAlignmentMiddleRight)
					(cons "左下" acAlignmentBottomLeft)
					(cons "中下" acAlignmentBottomCenter)
					(cons "右下" acAlignmentBottomRight)));end setq
			(setq Attachment
				(list
					(cons "左上" acAttachmentPointTopLeft)
					(cons "中上" acAttachmentPointTopCenter)
					(cons "右上" acAttachmentPointTopRight)
					(cons "左中" acAttachmentPointMiddleLeft)
					(cons "正中" acAttachmentPointMiddleCenter)
					(cons "右中" acAttachmentPointMiddleRight)
					(cons "左下" acAttachmentPointBottomLeft)
					(cons "中下" acAttachmentPointBottomCenter)
					(cons "右下" acAttachmentPointBottomRight)));end setq
			(setq _Alignment (mapcar 'car Alignment))
			(setq _Attachment (mapcar 'car Attachment))
			(setq ScaleVars
				(vl-remove-if
					'null
					(mapcar
						(function
							(lambda (var / value)
								(if
									(and
										(setq value (getvar var))
										(< 0.0 value));end and
									(if (= "CANNOSCALEVALUE" (strcase var))
										(cons var (rtos (/ 1.0 value)))
										(cons var (rtos value))))));end function
						(acad_strlsort
							'( "CANNOSCALEVALUE" "CELTSCALE" 
								 "DIMLFAC" "DIMSCALE" 	 "DIMTFAC" 
								 "DIMTXT" "HPSCALE" 	 "LTSCALE" 
								 "MLEADERSCALE" "MSOLESCALE" "TEXTSIZE")))));end setq
			(setq _ScaleVars (mapcar 'car ScaleVars))
			((lambda (/ i j x y)
				 (repeat (setq i 20)
					 (setq j 1)
					 (repeat 20
						 (setq x (cons j x)
							 y (cons i y)
							 j (1+ j)));end repeat
					 (setq i (1- i)));end repeat
				 (setq mode_image
					 (eval
						 (list
							 'lambda
							 '(key mode)
							 (list
								 'cond
								 '((= 1 mode)
										(start_image key)
										(fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
										(end_image)
										(mode_tile key mode));end "("
								 (list 't
									 '(start_image key)
									 '(fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
									 (list 'mapcar
										 ''vector_image
										 (list 'quote x)
										 (list 'quote y)
										 (list 'quote x)
										 (list 'quote y)
										 '(cond
												((member
													 key
													 '( "scl-pik" "arr-pik" "txt-pik" "msk-pik"));end member
													'(-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 095 096 096 096 096 096 096 096 096 095 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 096 254 254 254 254 254 254 254 254 096 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 096 063 063 -15 063 063 063 063 063 096 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 096 063 -15 250 -15 063 063 063 063 096 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 096 -15 250 250 -15 063 063 063 063 096 -15 -15 -15 -15
														 -15 -15 -15 -15 -15 -15 254 250 -15 250 -15 063 063 063 063 096 -15 -15 -15 -15
														 254 254 254 254 254 254 250 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
														 254 254 254 254 254 250 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
														 254 254 254 254 250 -15 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
														 254 254 254 250 250 250 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
														 254 254 254 254 254 250 -15 250 -15 250 -15 063 063 063 063 096 254 254 254 254
														 254 254 254 254 254 250 -15 250 250 250 -15 063 063 063 063 096 254 254 254 254
														 254 254 254 254 250 -15 250 254 254 250 254 096 096 096 096 095 254 254 254 254
														 254 254 254 254 250 -15 250 254 254 254 254 254 254 254 254 254 254 254 254 254
														 254 254 254 254 254 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254
														 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
														 -15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15
													 ));end "("
												('(
														-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 095 096 096 096 096 096 095 -15 -15 -15 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 096 063 063 063 063 063 096 -15 -15 -15 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 096 254 254 063 063 063 096 -15 -15 -15 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 253 250 254 063 063 063 096 254 008 254 -15 -15 -15
														-15 -15 -15 -15 -15 -15 -15 250 250 254 063 063 063 096 -15 252 251 254 -15 -15
														-15 -15 -15 -15 -15 -15 250 -15 250 254 063 063 063 096 -15 254 252 008 -15 -15
														254 254 254 254 254 250 -15 -15 250 253 096 096 096 095 -15 254 254 149 254 254
														254 254 254 254 250 -15 -15 -15 250 -15 254 -15 -15 -15 -15 254 254 149 254 254
														254 254 254 250 -15 -15 -15 -15 250 -15 008 253 -15 -15 -15 -15 253 008 254 254
														254 254 250 250 250 -15 -15 -15 250 254 254 251 253 -15 -15 253 251 254 254 254
														254 254 254 254 250 -15 250 -15 250 254 254 254 008 149 149 008 254 254 254 254
														254 254 254 254 250 -15 250 250 250 254 254 254 254 254 254 254 254 254 254 254
														254 254 254 250 -15 250 254 254 250 254 254 254 254 254 254 254 254 254 254 254
														254 254 254 250 -15 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254
														254 254 254 254 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
														254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
														-15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15
													))));end list
									 '(end_image)
									 '(mode_tile key mode)));end list
							 'mode)))));end "("
			(setq mode_color
				(lambda (key col)
					(start_image key)
					(fill_image 0 0 (dimx_tile key) (dimy_tile key) col)
					(end_image)
					(if
						(or
							(= 0 col)
							(= -15 col));end or
						(mode_tile key 1)
						(mode_tile key 0))));end setq
			(while (not (member dclflag '(1 0)))
				(cond
					((not (new_dialog "numinc" dclID))
						(numinc:popup
							"NumInc Dialog could not be Loaded" 
							16
							(princ
								(strcat
									"The Incremental Numbering Suite dialog could not be loaded. " 
									"The DCL file required by the application resides in the following location:\n\n" 
									dclfname
									"\n\nPlease check the integrity of this file."))));end "("
					((eval
						 (read (vl-list->string
										 '(40 110 117 109 105 110 99 58 109 105 115 99 41))));end eval
						(set_tile "dyn-flg" dyn-flg)
						(action_tile "dyn-flg" "(setq dyn-flg $value)")
						(foreach symb	'(pre-str mid-str suf-str inc-str)
							(setq tile (strcase (vl-symbol-name symb) t))
							(set_tile tile (eval symb))
							(action_tile tile (strcat "(setq " tile " $value)")));end foreach
						((lambda (/ bit)
							 (setq bit 1)
							 (foreach tile '( "inc-pre" "inc-mid" "inc-suf")
								 (if (= bit (logand bit inc-sec))
									 (set_tile tile "1"));end if
								 (action_tile
									 tile
									 (strcat
										 "(setq inc-sec (boole (if (eq \"1\" $value) 7 4) " 
										 (itoa bit)
										 " inc-sec))"));end action_tile
								 (setq bit (lsh bit 1)))));end "("
						(numinc:makelist "txt-lay" _layers)
						(set_tile
							"txt-lay" 
							(itoa
								(cond
									((vl-position txt-lay _layers))
									((vl-position (setq txt-lay (getvar 'clayer)) _layers)))));end set_tile
						(action_tile
							"txt-lay" 
							"(setq txt-lay (nth (atoi $value) _layers))");end action_tile
						(numinc:MakeList "txt-sty" _styles)
						(set_tile "txt-sty" 
							(itoa
								(cond
									((vl-position txt-sty _styles))
									((vl-position
										 (setq txt-sty (getvar 'textstyle))
										 _styles)))));end set_tile
						((setq txt-sty-fun
							 (lambda (style / tmp)
								 (if
									 (zerop
										 (setq tmp (cdr (assoc 40 (tblsearch "style" style)))));end zerop
									 (progn
										 (set_tile "txt-bst" (setq txt-bst "0"))
										 (mode_tile "txt-bst" 1)
										 (mode_tile "txt-sze" 0));end progn
									 (progn
										 (mode_tile "txt-bst" 0)
										 (if (= "1" txt-bst)
											 (set_tile "txt-sze" (setq txt-sze (rtos tmp))))))));end setq
							txt-sty);end "("
						(action_tile
							"txt-sty" 
							"(txt-sty-fun (setq txt-sty (nth (atoi $value) _styles)))");end action_tile
						(numinc:MakeList
							"txt-aln" 
							(if (= "obj-mtx" obj-typ)
								_Attachment
								_Alignment));end numinc:MakeList
						(set_tile "txt-aln" 
							(itoa
								(cond
									((vl-position
										 txt-aln
										 (if (= "obj-mtx" obj-typ)
											 _Attachment
											 _Alignment)));end "("
									((setq txt-aln
										 (car
											 (if (= "obj-mtx" obj-typ)
												 _Attachment
												 _Alignment)));end setq
										0))));end set_tile
						(action_tile
							"txt-aln" 
							(vl-prin1-to-string
								(quote
									(setq txt-aln
										(nth (atoi $value)
											(if (= "obj-mtx" obj-typ)
												_Attachment
												_Alignment))))));end action_tile
						(set_tile "txt-sze" txt-sze)
						(action_tile "txt-sze" "(setq txt-sze $value)")
						(if (= "1" txt-bst)
							(if
								(zerop
									(setq tmp (cdr (assoc 40 (tblsearch "style" txt-sty)))));end zerop
								(progn
									(set_tile "txt-bst" (setq txt-bst "0"))
									(mode_tile "txt-bst" 1));end progn
								(progn
									(set_tile "txt-bst" txt-bst)
									(set_tile "txt-sze" (setq txt-sze (rtos tmp))))));end if
						(mode_tile "txt-sze" (atoi txt-bst))
						(mode_image "txt-pik" (atoi txt-bst))
						(action_tile
							"txt-bst" 
							(vl-prin1-to-string
								(quote
									(progn
										(mode_tile "txt-sze" (atoi (setq txt-bst $value)))
										(mode_image "txt-pik" (atoi txt-bst))
										(if (= "1" $value)
											(set_tile
												"txt-sze" 
												(rtos (cdr (assoc 40 (tblsearch "style" txt-sty))))))))));end action_tile
						(action_tile "txt-pik" "(done_dialog 4)")
						(set_tile "msk-trn" msk-trn)
						((setq msk-trn-fun
							 (lambda (value)
								 (if (= "1" value)
									 (mode_color "msk-col" 0)
									 (mode_color "msk-col" (cdr (assoc 62 msk-col))))));end setq
							msk-trn);end "("
						(action_tile
							"msk-trn" 
							"(msk-trn-fun (setq msk-trn $value))");end action_tile
						(action_tile
							"msk-col" 
							(vl-prin1-to-string
								'((lambda (/ tmp)
										(if
											(setq
												tmp
												(acad_truecolordlg
													(vl-some
														(function
															(lambda (x) (assoc x msk-col)));end function
														'(430 420 62));end vl-some
													nil));end setq
											(mode_color
												"msk-col" 
												(cdr (assoc 62 (setq msk-col tmp)))))))));end action_tile
						(set_tile "msk-off" msk-off)
						(action_tile "msk-off" "(setq msk-off $value)")
						(action_tile "msk-pik" "(done_dialog 7)")
						(set_tile "msk-use" msk-use)
						((setq msk-use-fun
							 (lambda (value)
								 (if (= "1" value)
									 (progn
										 (mode_tile "msk-off" 0)
										 (mode_image "msk-pik" 0)
										 (mode_tile "msk-trn" 0)
										 (msk-trn-fun msk-trn));end progn
									 (progn
										 (mode_tile "msk-off" 1)
										 (mode_image "msk-pik" 1)
										 (mode_tile "msk-trn" 1)
										 (mode_color "msk-col" -15)))));end setq
							msk-use);end "("
						(action_tile
							"msk-use" 
							"(msk-use-fun (setq msk-use $value))");end action_tile
						(set_tile "bor-enc" bor-enc)
						((setq bor-enc-fun
							 (lambda (value)
								 (if (= "1" value)
									 (progn
										 (mode_tile "bor-shp" 0)
										 (if (= "3" bor-shp)
											 (mode_tile "bor-sid" 0));end if
										 (mode_tile "bor-lay" 0)
										 (mode_tile "bor-off" 0)
										 (mode_tile "bor-fix" 0)
										 (mode_tile "bor-pik" 0)
										 (mode_tile "bor-ltx" 0)
										 (if (= "bor-off" bor-typ)
											 (progn
												 (mode_tile "off-ed1" 0)
												 (mode_tile "fix-ed1" 1)
												 (mode_tile "fix-txt" 1)
												 (mode_tile "fix-ed2" 1));end progn
											 (progn
												 (mode_tile "off-ed1" 1)
												 (mode_tile "fix-ed1" 0)
												 (if (member bor-shp '( "1" "2"))
													 (progn
														 (mode_tile "fix-txt" 0)
														 (mode_tile "fix-ed2" 0));end progn
													 (progn
														 (mode_tile "fix-txt" 1)
														 (mode_tile "fix-ed2" 1))))));end progn
									 (foreach tile
										 '( "bor-shp" "bor-sid" 
												"bor-lay" "bor-off" 
												"bor-fix" "off-ed1" 
												"fix-ed1" "fix-ed2" 
												"bor-pik" "fix-txt" 
												"bor-ltx");end 
										 (mode_tile tile 1)))));end setq
							bor-enc);end "("
						(action_tile
							"bor-enc" 
							"(bor-enc-fun (setq bor-enc $value))");end action_tile
						(numinc:makelist
							"bor-shp" 
							'( "圆形" "四方形" "扁圆形" "多边形"));end numinc:makelist
						(set_tile "bor-shp" bor-shp)
						((setq bor-shp-fun
							 (lambda (value)
								 (if (= "bor-fix" bor-typ)
									 (mapcar 'mode_tile
										 '( "bor-sid" "fix-txt" "fix-ed2")
										 (cond
											 ((= value "0")
												 '(1 1 1));end "("
											 ((member value '( "1" "2"))
												 '(1 0 0));end "("
											 ('(0 1 1))));end mapcar
									 (mapcar 'mode_tile
										 '( "bor-sid" "fix-txt" "fix-ed2")
										 (cond
											 ((member value '( "0" "1" "2"))
												 '(1 1 1));end "("
											 ('(0 1 1)))))));end setq
							bor-shp);end "("
						(action_tile
							"bor-shp" 
							"(bor-shp-fun (setq bor-shp $value))");end action_tile
						(set_tile "bor-sid" bor-sid)
						(action_tile "bor-sid" "(setq bor-sid $value)")
						(numinc:makelist "bor-lay" _layers)
						(set_tile
							"bor-lay" 
							(itoa
								(cond
									((vl-position bor-lay _layers))
									((vl-position (setq bor-lay (getvar 'clayer)) _layers)))));end set_tile
						(action_tile
							"bor-lay" 
							"(setq bor-lay (nth (atoi $value) _layers))");end action_tile
						(set_tile bor-typ "1")
						((setq bor-typ-fun
							 (lambda (typ)
								 (if (= "1" bor-enc)
									 (if (= "bor-off" typ)
										 (mapcar	'mode_tile
											 '( "off-ed1" "fix-ed1" "fix-ed2" "fix-txt")
											 '(0 1 1 1));end mapcar	'mode_tile
										 (progn
											 (mode_tile "off-ed1" 1)
											 (mode_tile "fix-ed1" 0)
											 (if (member bor-shp '( "1" "2"))
												 (progn
													 (mode_tile "fix-ed2" 0)
													 (mode_tile "fix-txt" 0))))))));end setq
							bor-typ);end "("
						(action_tile "bor-off" "(bor-typ-fun (setq bor-typ $key))")
						(action_tile "bor-fix" "(bor-typ-fun (setq bor-typ $key))")
						(foreach symb	'(off-ed1 fix-ed1 fix-ed2)
							(setq tile (strcase (vl-symbol-name symb) t))
							(set_tile tile (eval symb))
							(action_tile tile (strcat "(setq " tile " $value)")));end foreach
						(action_tile "bor-pik" "(done_dialog 3)")
						(set_tile "arr-use" arr-use)
						((setq arr-use-fun
							 (lambda (value)
								 (if (= "1" value)
									 (progn
										 (foreach tile
											 '( "arr-qty" 
													"arr-aln" 
													"arr-per" 
													"arr-oth");end 
											 (mode_tile tile 0));end foreach
										 (if (= "arr-oth" arr-typ)
											 (progn
												 (mode_tile "arr-rot" 0)
												 (mode_image "arr-pik" 0));end progn
											 (progn
												 (mode_tile "arr-rot" 1)
												 (mode_image "arr-pik" 1))));end progn
									 (progn
										 (foreach tile
											 '( "arr-qty" "arr-aln" 
													"arr-per" "arr-oth" 
													"arr-rot");end 
											 (mode_tile tile 1));end foreach
										 (mode_image "arr-pik" 1)))));end setq
							arr-use);end "("
						(action_tile
							"arr-use" 
							"(arr-use-fun (setq arr-use $value))");end action_tile
						(set_tile "arr-qty" arr-qty)
						(action_tile "arr-qty" "(setq arr-qty $value)")
						(set_tile arr-typ "1")
						((setq arr-typ-fun
							 (lambda (typ)
								 (foreach tile '( "arr-aln" "arr-per" "arr-oth")
									 (if (/= typ tile)
										 (set_tile tile "0")));end foreach
								 (if (= "arr-oth" arr-typ)
									 (progn
										 (mode_tile "arr-rot" 0)
										 (mode_image "arr-pik" 0));end progn
									 (progn
										 (mode_tile "arr-rot" 1)
										 (mode_image "arr-pik" 1)))));end setq
							arr-typ);end "("
						(foreach tile
							'( "arr-aln" 
								 "arr-per" 
								 "arr-oth");end 
							(action_tile tile "(arr-typ-fun (setq arr-typ $key))"));end foreach
						(set_tile "arr-rot" arr-rot)
						(action_tile "arr-rot" "(setq arr-rot $value)")
						(action_tile "arr-pik" "(done_dialog 5)")
						(set_tile "blk-scl" blk-scl)
						(action_tile "blk-scl" "(setq blk-scl $value)")
						(numinc:makelist "scl-pop" _ScaleVars)
						(set_tile "scl-pop" 
							(itoa
								(cond
									((vl-position scl-pop _ScaleVars))
									((setq scl-pop (car _ScaleVars))
										0))));end set_tile
						((setq blk-scl-fun
							 (lambda (value)
								 (if (= "1" value)
									 (progn
										 (mode_tile "blk-scl" 1)
										 (mode_tile "scl-pop" 0)
										 (mode_image "scl-pik" 1)
										 (set_tile
											 "blk-scl" 
											 (setq blk-scl (cdr (assoc scl-pop ScaleVars)))));end progn
									 (progn
										 (mode_tile "blk-scl" 0)
										 (mode_tile "scl-pop" 1)
										 (mode_image "scl-pik" 0)))));end setq
							scl-var);end "("
						(action_tile
							"scl-var" 
							"(blk-scl-fun (setq scl-var $value))");end action_tile
						(action_tile
							"scl-pop" 
							(vl-prin1-to-string
								'(set_tile
									 "blk-scl" 
									 (setq
										 blk-scl
										 (cdr
											 (assoc
												 (setq scl-pop (nth (atoi $value) _ScaleVars))
												 ScaleVars))))));end action_tile
						(action_tile "scl-pik" "(done_dialog 6)")
						(if (and (= "obj-blk" obj-typ) (null _blocks))
							(setq obj-typ "obj-txt"));end if
						(set_tile obj-typ "1")
						((setq obj-typ-fun
							 (lambda (typ)
								 (if (= typ "obj-blk")
									 (progn
										 (set_tile "lay-txt" "图块图层: ")
										 (foreach pair
											 '(( "blk-nme" 0)
													( "blk-txt" 0)
													( "att-txt" 0)
													( "att-nme" 0)
													( "scl-var" 0)
													( "sty-txt" 1)
													( "txt-sty" 1)
													( "aln-txt" 1)
													( "txt-aln" 1)
													( "txt-bst" 1)
													( "txt-sze" 1)
													( "bor-enc" 1)
													( "bor-shp" 1)
													( "bor-sid" 1)
													( "bor-ltx" 1)
													( "bor-lay" 1)
													( "bor-off" 1)
													( "bor-fix" 1)
													( "off-ed1" 1)
													( "fix-ed1" 1)
													( "fix-ed2" 1)
													( "bor-pik" 1)
													( "msk-use" 1)
													( "msk-off" 1)
													( "msk-trn" 1));end "("
											 (apply 'mode_tile pair));end foreach
										 (mode_image "blk-pik" 0)
										 (mode_image "msk-pik" 1)
										 (mode_color "msk-col" -15)
										 (blk-scl-fun scl-var));end progn
									 (progn
										 (set_tile "lay-txt" "文字图层: ")
										 (foreach pair
											 '(( "blk-txt" 1)
													( "blk-nme" 1)
													( "att-txt" 1)
													( "att-nme" 1)
													( "blk-scl" 1)
													( "scl-var" 1)
													( "scl-pop" 1)
													( "sty-txt" 0)
													( "txt-sty" 0)
													( "aln-txt" 0)
													( "txt-aln" 0)
													( "bor-enc" 0));end "("
											 (apply 'mode_tile pair));end foreach
										 (mode_image "blk-pik" 1)
										 (mode_image "scl-pik" 1)
										 (bor-enc-fun bor-enc)
										 (txt-sty-fun txt-sty)
										 (numinc:makelist
											 "txt-aln" 
											 (if (= "obj-mtx" obj-typ)
												 _Attachment
												 _Alignment));end numinc:makelist
										 (set_tile "txt-aln" 
											 (itoa
												 (cond
													 ((vl-position
															txt-aln
															(if (= "obj-mtx" obj-typ)
																_Attachment
																_Alignment)));end "("
													 ((setq txt-aln
															(car
																(if (= "obj-mtx" obj-typ)
																	_Attachment
																	_Alignment)));end setq
														 0))));end set_tile
										 (if (= "obj-mtx" typ)
											 (progn
												 (mode_tile "msk-use" 0)
												 (msk-use-fun msk-use));end progn
											 (progn
												 (msk-use-fun "0")
												 (mode_tile "msk-use" 1)))))));end setq
							obj-typ);end "("
						(foreach tile
							'( "obj-txt" 
								 "obj-mtx" 
								 "obj-blk");end 
							(action_tile tile "(obj-typ-fun (setq obj-typ $key))"));end foreach
						(if _blocks
							(progn
								(numinc:makelist
									"blk-nme" 
									(setq blocks (mapcar 'car _blocks)));end numinc:makelist
								(set_tile	 "blk-nme" 
									(setq block
										(itoa
											(cond
												((vl-position blk-nme blocks))
												((setq blk-nme (car blocks))
													0)))));end set_tile	
								(numinc:makelist
									"att-nme" 
									(setq attribs (cdr (nth (atoi block) _blocks)))
								);end numinc:makelist
								(set_tile	 "att-nme" 
									(setq attrib
										(itoa
											(cond
												((vl-position att-nme attribs))
												((setq att-nme (car attribs))
													0))))));end progn
							(mode_tile "obj-blk" 1));end if
						(action_tile
							"blk-nme" 
							(vl-prin1-to-string
								(quote
									(progn
										(setq	blk-itm (nth (atoi (setq block $value)) _blocks)
											blk-nme (car blk-itm));end setq	blk-itm
										(numinc:makelist
											"att-nme" 
											(setq attribs (cdr blk-itm)));end numinc:makelist
										(set_tile "att-nme" 
											(setq attrib
												(itoa
													(cond
														((vl-position att-nme attribs))
														((setq att-nme (car attribs))
															0)))))))));end action_tile
						(action_tile "blk-pik" "(done_dialog 2)")
						(action_tile
							"att-nme" 
							"(setq attrib $value att-nme (nth (atoi $value) attribs))");end action_tile
						(action_tile "about" "(numinc:about dclid)")
						(action_tile
							"accept" 
							(vl-prin1-to-string
								(quote
									(progn
										(if (= "" inc-str)
											(setq inc-str "0"));end if
										(if (= "" txt-sze)
											(setq txt-sze (rtos (getvar 'textsize))));end if
										(cond
											((and
												 (/= "obj-blk" obj-typ)
												 (= "1" bor-enc)
												 (= "bor-off" bor-typ)
												 (not (setq off-ed1# (distof off-ed1))));end and
												(numinc:popup
													"Information" 
													48
													"边框偏移系数必须是数值。");end numinc:popup
												(mode_tile "off-ed1" 2));end "("
											((and
												 (/= "obj-blk" obj-typ)
												 (= "1" bor-enc)
												 (= "bor-off" bor-typ)
												 (< off-ed1# 1.0));end and
												(numinc:popup
													"Information" 
													48
													"边界偏移系数必须大于或等于一。");end numinc:popup
												(mode_tile "off-ed1" 2));end "("
											((and
												 (/= "obj-blk" obj-typ)
												 (= "1" bor-enc)
												 (= "bor-fix" bor-typ)
												 (or (not (setq fix-ed1# (distof fix-ed1)))
													 (and
														 (member bor-shp '( "1" "2"))
														 (not (setq fix-ed2# (distof fix-ed2))))));end and
												(numinc:popup
													"Information" 
													48
													"边框大小必须是数值。");end numinc:popup
												(mode_tile "fix-ed1" 2));end "("
											((and
												 (/= "obj-blk" obj-typ)
												 (= "1" bor-enc)
												 (= "bor-fix" bor-typ)
												 (or (<= fix-ed1# 0.0)
													 (and
														 (member bor-shp '( "1" "2"))
														 (<= fix-ed2# 0.0))));end and
												(numinc:popup
													"Information" 
													48
													"边界大小必须大于零。");end numinc:popup
												(mode_tile "fix-ed1" 2));end "("
											((and
												 (= "1" arr-use)
												 (< (setq arr-qty# (atoi arr-qty)) 1));end and
												(numinc:popup
													"Information" 
													48
													"阵列项目的数量必须大于或等于一。");end numinc:popup
												(mode_tile "arr-qty" 2));end "("
											((and
												 (= "1" arr-use)
												 (= "arr-oth" arr-typ)
												 (not (setq arr-rot# (angtof arr-rot))));end and
												(numinc:popup
													"Information" 
													48
													"阵列对象必须是数值。");end numinc:popup
												(mode_tile "arr-rot" 2));end "("
											((and
												 (= "obj-mtx" obj-typ)
												 (= "1" msk-use)
												 (not (setq msk-off# (distof msk-off))));end and
												(numinc:popup
													"Information" 
													48
													"背景遮蔽范围系数必须是数值。");end numinc:popup
												(mode_tile "msk-off" 2));end "("
											((and
												 (= "obj-mtx" obj-typ)
												 (= "1" msk-use)
												 (or (< 5.0 msk-off#)
													 (< msk-off# 1.0)));end and
												(numinc:popup
													"Information" 
													48
													"背景遮蔽范围系数必须在数值1-5之间。");end numinc:popup
												(mode_tile "msk-off" 2));end "("
											((and
												 (= "obj-blk" obj-typ)
												 (not (setq blk-scl# (distof blk-scl))));end and
												(numinc:popup
													"Information" 
													48
													"图块比例必须是数值。");end numinc:popup
												(mode_tile "blk-scl" 2));end "("
											((and
												 (= "obj-blk" obj-typ)
												 (<= blk-scl# 0.0));end and
												(numinc:popup
													"Information" 
													48
													"图块比例尺度必须大于零。");end numinc:popup
												(mode_tile "blk-scl" 2));end "("
											((not (distof inc-str 2))
												(numinc:popup
													"Information" 
													48
													"增量必须是数值。");end numinc:popup
												(mode_tile "inc-str" 2));end "("
											((and
												 (/= "obj-blk" obj-typ)
												 (not (setq txt-sze# (distof txt-sze))));end and
												(numinc:popup
													"Information" 
													48
													"文字高度必须是数值。");end numinc:popup
												(if (= "0" txt-bst)
													(mode_tile "txt-sze" 2)));end "("
											((and
												 (/= "obj-blk" obj-typ)
												 (<= txt-sze# 0.0));end and
												(numinc:Popup
													"Information" 
													48
													"文字高度必须大于零。");end numinc:Popup
												(if (= "0" txt-bst)
													(mode_tile "txt-sze" 2)));end "("
											((and
												 (/= "obj-blk" obj-typ)
												 (= "1" bor-enc)
												 (= "3" bor-shp)
												 (< (setq bor-sid# (atoi bor-sid)) 3));end and
												(numinc:popup
													"Information" 
													48
													"多边形边数必须是大于2的数值和。");end numinc:popup
												(mode_tile "bor-sid" 2));end "("
											(t
												(done_dialog 1)))))));end action_tile
						(setq dclflag (start_dialog))));end cond
				(cond
					((= 2 dclflag)
						(while
							(progn (setvar 'errno 0)
								(setq ent (car (entsel "\n选择图块: ")))
								(cond
									((= 7 (getvar 'errno))
										(princ "\n没有选中，再选择。"));end "("
									((= 'ename (type ent))
										(if
											(and
												(= "INSERT" 
													(cdr (assoc 0 (setq elst (entget ent)))));end =
												(= 1 (cdr (assoc 66 elst))));end and
											(progn
												(setq blk-nme
													(if (vlax-property-available-p
																(setq obj (vlax-ename->vla-object ent))
																'effectivename);end vlax-property-available-p
														(vla-get-effectivename obj)
														(vla-get-name obj)));end setq
												nil);end progn
											(princ "\n请选择一个图块。")))))));end "("
					((= 3 dclflag)
						(cond
							((= "bor-off" bor-typ)
								(while
									(and
										(progn
											(initget 6)
											(setq tmp
												(getdist
													(strcat "\n指定边界偏移系数 <" 
														off-ed1
														">: "))));end progn
										(< tmp 1.0));end and
									(princ
										"\n请提供一个值大于或等于一。"));end while
								(if tmp
									(setq off-ed1 (rtos tmp))));end "("
							(t
								(cond
									((member bor-shp '( "0" "3"))
										(setq fix-ed1
											(cond
												((setq	tmp
													 (getdist
														 (strcat "\n指定边界半径 <" 
															 fix-ed1
															 ">: ")));end setq	tmp
													(rtos tmp));end "("
												(fix-ed1))));end "("
									(t
										(if
											(and
												(setq p1 (getpoint "\n指定第一点: "))
												(setq
													p2 (getcorner p1 "\nSpecify opposite corner: ")));end and
											(setq fix-ed1 (rtos (abs (- (car p2) (car p1))))
												fix-ed2 (rtos (abs (- (cadr p2) (cadr p1)))))))))));end "("
					((= 4 dclflag)
						(initget 6)
						(setq	txt-sze
							(cond
								((setq tmp
									 (getdist
										 (strcat "\n指定文字大小 <" txt-sze ">: ")));end setq
									(rtos tmp));end "("
								(txt-sze))));end "("
					((= 5 dclflag)
						(setq	arr-rot
							(cond
								((setq tmp
									 (getangle
										 (strcat "\n指定对象的角度 <" arr-rot ">: ")));end setq
									(angtos tmp));end "("
								(arr-rot))));end "("
					((= 6 dclflag)
						(initget 6)
						(setq	blk-scl
							(cond
								((setq tmp
									 (getdist
										 (strcat "\n指定图块的比例 <" blk-scl ">: ")));end setq
									(rtos tmp));end "("
								(blk-scl))));end "("
					((= 7 dclflag)
						(while
							(and
								(progn
									(initget 6)
									(setq tmp
										(getdist
											(strcat "\n指定背景遮蔽的范围系数 <" 
												msk-off
												">: "))));end progn
								(or
									(< 5.0 tmp)
									(< tmp 1.0)));end and
							(princ "\n请提供1和5之间的值。"));end while
						(if tmp
							(setq msk-off (rtos tmp))))));end while
			(if (= 1 dclflag)
				(progn
					(if
						(setq ss
							(ssget "_X" 
								(list '(0 . "ACAD_TABLE")
									(if (= 1 (getvar 'cvport))
										(cons 410 (getvar 'ctab))
										'(410 . "Model")))));end setq
						(repeat (setq i (sslength ss))
							(setq table (cons (vlax-ename->vla-object
																	(ssname ss (setq i (1- i))));end vlax-ename->vla-object
														table))));end if
					(setq acspc (vlax-get-property
												(numinc:acdoc)
												(if (= 1 (getvar 'cvport))
													'paperspace
													'modelspace));end vlax-get-property
						nm (trans '(0.0 0.0 1.0) 1 0 t)
						xa (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 nm t)));end setq
					(if (/= "obj-blk" obj-typ)
						(progn
							(if (numinc:annotative-p txt-sty)
								(setq txt-sze# (/ txt-sze#
																 (cond ((getvar 'cannoscalevalue))
																	 (1.0)))));end if
							(setq oba (cdr (assoc 50 (tblsearch "style" txt-sty))))));end if
					(setq symb
						(mapcar	'cdr
							(vl-remove-if
								(function
									(lambda (pair)
										(zerop (logand (car pair) inc-sec))));end function
								'((1 . pre-str)
									 (2 . mid-str)
									 (4 . suf-str)))));end setq
					(setq prop
						(if
							(and
								(= "obj-txt" obj-typ)
								(/= "Left" txt-aln));end and
							'textalignmentpoint
							'insertionpoint));end setq
					(if (= "1" msk-use)
						(setq mtx-bak :vlax-true)
						(setq mtx-bak :vlax-false));end if
					(setq create-obj
						(cond
							((= "obj-txt" obj-typ)
								(lambda (point string / obj)
									(setq point (vlax-3D-point (trans point 1 0))
										obj (vla-addtext acspc string point txt-sze#));end setq
									(vla-put-stylename obj txt-sty)
									(vla-put-layer obj txt-lay)
									(vla-put-alignment obj (cdr (assoc txt-aln Alignment)))
									(if (= "Left" txt-aln)
										(vla-put-insertionpoint obj point)
										(vla-put-textalignmentpoint obj point));end if
									(vla-put-obliqueangle obj oba)
									(vla-put-rotation obj (+ xa txt-rot))
									obj));end "("
							((= "obj-mtx" obj-typ)
								(lambda (point string / obj)
									(setq point (vlax-3D-point (trans point 1 0)))
									(setq obj
										(vla-addmtext
											acspc
											point
											(numinc:mtextwidth string txt-sty txt-sze#)
											string));end setq
									(vla-put-stylename obj txt-sty)
									(vla-put-layer obj txt-lay)
									(vla-put-height obj txt-sze#)
									(vla-put-attachmentpoint
										obj
										(cdr (assoc txt-aln Attachment)));end vla-put-attachmentpoint
									(vla-put-insertionpoint obj point)
									(vla-put-rotation obj txt-rot)
									(if (= "1" msk-use)
										(entmod
											(append
												(vl-remove-if
													(function
														(lambda (pair)
															(member (car pair) '(45 63 90 421 431 441))));end function
													(entget (vlax-vla-object->ename obj)));end vl-remove-if
												(if (= "1" msk-trn)
													'((90 . 3))
													'((90 . 1)));end if
												(if (= "1" msk-trn)
													'((63 . 256))
													(mapcar '(lambda (x) (cons (1+ (car x)) (cdr x)))
														msk-col));end if
												(list
													(cons 45 msk-off#)
													'(441 . 0)))));end if
									(vla-put-backgroundfill obj mtx-bak)
									obj));end "("
							((= "obj-blk" obj-typ)
								(lambda (point string / obj)
									(setq point (vlax-3D-point (trans point 1 0))
										obj (vla-insertblock
													acspc
													point
													blk-nme
													blk-scl#
													blk-scl#
													blk-scl#
													(+ xa txt-rot)));end setq
									(vl-some
										(function
											(lambda (attrib)
												(if
													(= (strcase (vla-get-tagstring attrib)) att-nme)
													(null (vla-put-textstring attrib string)))));end function
										(vlax-invoke obj 'getattributes));end vl-some
									(vla-put-layer obj txt-lay)
									obj))));end setq
					(if
						(and
							(/= "obj-blk" obj-typ)
							(= "1" bor-enc)
							(= "bor-off" bor-typ)
							off-ed1#);end and
						(setq off-ed1# (* txt-sze# (1- off-ed1#))));end if
					(setq create-bor
						(lambda (obj prop / bor)
							(setq	bor
								(vlax-ename->vla-object
									(numinc:createtextborder
										(vlax-vla-object->ename obj)
										bor-shp
										(cond (off-ed1#)
											(0.0));end cond
										fix-ed1#
										fix-ed2#
										bor-sid#)));end setq	bor
							(vla-put-layer bor bor-lay)
							(if (and (= "3" bor-shp) bor-rot)
								(vla-rotate
									bor
									(vlax-3D-point (numinc:polygoncentroid bor))
									(/ pi bor-sid#)));end if
							bor));end setq
					(cond
						((= "1" arr-use)
							(if (setq p1 (getpoint "\n指定阵列的基准点: "))
								(progn
									(while
										(progn
											(if	arr-end
												(progn
													(initget "Spacing")
													(setq
														p2 (getpoint
																 "\n指定阵列的端点 [间隔]: " 
																 p1)));end progn
												(progn
													(initget "Endpoint")
													(setq p2
														(getpoint
															"\n指定阵列间距向量 [结束点]:  " 
															p1))));end if	arr-end
											(cond
												((null p2)
													nil);end "("
												((= "Endpoint" p2)
													(setq arr-end t));end "("
												((= "Spacing" p2)
													(setq arr-end nil)
													t);end "("
												((and
													 (listp p2)
													 (equal p1 p2 1e-8));end and
													(princ "\n选择点必须是不同的。")))));end while
									(if (and arr-end (< 1 arr-qty#))
										(setq
											v1 (mapcar '(lambda (a b)
																		(/ (- a b) (float (1- arr-qty#))));end lambda
													 p2
													 p1));end setq
										(setq v1 (mapcar '- p2 p1)));end if
									(cond
										((= "arr-aln" arr-typ)
											(setq r1 (numinc:makereadable (angle p1 p2))));end "("
										((= "arr-per" arr-typ)
											(setq r1 (numinc:makereadable
																 (+ (angle p1 p2) (/ pi 2.0)))));end "("
										((setq r1 arr-rot#)));end cond
									(if (/= "obj-mtx" obj-typ)
										(setq r1 (+ r1 xa)));end if
									(repeat	arr-qty#
										(setq	obj
											(create-obj p1 (strcat pre-str mid-str suf-str)));end setq	obj
										(vla-put-rotation obj r1)
										(if
											(and
												(/= "obj-blk" obj-typ)
												(= "1" bor-enc));end and
											(create-bor obj prop));end if
										(numinc:increment symb inc-str)
										(setq p1 (mapcar '+ p1 v1))))));end "("
						((= "1" dyn-flg)
							(while (/= 5 (car (setq gr (grread t 13 0)))))
							(setq obj (create-obj
													(cadr gr)
													(strcat pre-str mid-str suf-str)));end setq
							(if
								(and
									(/= "obj-blk" obj-typ)
									(= "1" bor-enc));end and
								(setq bor (create-bor obj prop)));end if
							(princ
								(setq msg
									(strcat
										"\n[C]曲线对齐, [R]替换, [o]旋转文字[<]逆 / [>]顺, [T]递增开关, [I]增量\n" 
										(if
											(and
												(/= "obj-blk" obj-typ)
												(= "1" bor-enc)
												(= "3" bor-shp));end and
											"[B]旋转边框, " 
											"");end if
										"[Tab]=旋转 90°" 
										", [M]镜像边框" 
										(if (= "obj-mtx" obj-typ)
											", [a]背景遮蔽" 
											"");end if
										" <退出>")));end princ
							(setvar
								'modemacro
								(strcat
									"旋转角度: " 
									(rtos (rem (+ 360.0 (* 180.0 (/ txt-rot pi))) 360) 2 2)
									"°"));end setvar
							(while
								(progn
									(setq gr (grread t 15 0)
										g1 (car gr)
										g2 (cadr gr));end setq
									(cond
										((member g1 '(3 5))
											(setq p1 (vlax-3D-point (trans g2 1 0)))
											(if bor
												(vla-move bor (vlax-get-property obj prop) p1));end if
											(vlax-put-property obj prop p1)
											(if (= 3 g1)
												(progn
													(if (and	table
																(numinc:textincell
																	table
																	p1
																	(strcat pre-str mid-str suf-str)));end and	table
														(progn
															(vla-delete obj)
															(if bor
																(vla-delete bor))));end if
													(if tog-cnt
														(numinc:increment symb inc-str));end if
													(setq obj (create-obj
																			g2
																			(strcat pre-str mid-str suf-str)));end setq
													(if
														(and
															(/= "obj-blk" obj-typ)
															(= "1" bor-enc));end and
														(setq bor (create-bor obj prop)));end if
													(redraw)));end if
											t);end "("
										((= 25 g1)
											(vla-delete obj)
											(if bor
												(vla-delete bor));end if
											nil);end "("
										((= 2 g1)
											(cond
												((member g2 '(67 99))
													(vla-delete obj)
													(if bor
														(vla-delete bor));end if
													(while
														(setq ent
															(numinc:selectif
																"\n选择曲线 <退出>: " 
																(function
																	(lambda (x)
																		(not
																			(vl-catch-all-error-p
																				(vl-catch-all-apply
																					'vlax-curve-getendparam
																					(list x))))));end function
																entsel));end setq
														(if (numinc:aligntocurve
																	(setq
																		obj (create-obj
																					(cadr ent)
																					(strcat pre-str mid-str suf-str)));end setq
																	prop
																	(car ent)
																	(if
																		(and
																			(/= "obj-blk" obj-typ)
																			(= "1" bor-enc));end and
																		(setq bor (create-bor obj prop))));end numinc:aligntocurve
															(if tog-cnt
																(numinc:increment symb inc-str));end if
															(progn
																(vla-delete obj)
																(if bor
																	(vla-delete bor)))));end while
													(setq obj (create-obj
																			(cadr (grread t 13 0))
																			(strcat pre-str mid-str suf-str)));end setq
													(if
														(and
															(/= "obj-blk" obj-typ)
															(= "1" bor-enc));end and
														(setq bor (create-bor obj prop)));end if
													(princ msg));end "("
												((member g2 '(44 46 60 62))
													(if (member g2 '(44 60))
														(setq deg (/ pi 180.0))
														(setq deg (/ pi -180.0)));end if
													(setvar
														'modemacro
														(strcat
															"旋转角度: " 
															(rtos
																(rem
																	(+ 360.0
																		(* 180.0
																			(/ (setq txt-rot (+ txt-rot deg)) pi)));end +
																	360);end rem
																2
																2);end rtos
															"°"));end setvar
													(vla-put-rotation
														obj
														(+ (vla-get-rotation obj) deg));end vla-put-rotation
													(if bor
														(vla-rotate
															bor
															(vlax-get-property obj prop)
															deg));end if
													t);end "("
												((member g2 '(79 111))
													(setq txt-rot
														(cond
															((getangle
																 (strcat "\n指定 " 
																	 (cdr
																		 (assoc obj-typ
																			 '(( "obj-txt" . "text")
																					( "obj-mtx" . "mtext")
																					( "obj-blk" . "block"))));end cdr
																	 " 旋转角度 <" 
																	 (angtos txt-rot)
																	 ">: ")));end "("
															(txt-rot)));end setq
													(setvar
														'modemacro
														(strcat	 "旋转角度: " 
															(rtos
																(rem
																	(+ 360.0 (* 180.0 (/ txt-rot pi)))
																	360);end rem
																2
																2);end rtos
															"°"));end setvar
													(if bor
														(vla-rotate
															bor
															(vlax-get-property obj prop)
															(- txt-rot
																(if (= "obj-mtx" obj-typ)
																	(vla-get-rotation obj)
																	(- (vla-get-rotation obj) xa)))));end if
													(vla-put-rotation
														obj
														((lambda (a)
															 (if (= "obj-mtx" obj-typ)
																 a
																 (+ a xa)));end lambda
															txt-rot));end vla-put-rotation
													(princ msg));end "("
												((member g2 '(84 116))
													(if (setq tog-cnt (not tog-cnt))
														(princ "\n<递增打开>")
														(princ "\n<递增关闭>"));end if
													(princ msg));end "("
												((member g2 '(73 105))
													(vla-delete obj)
													(if bor
														(vla-delete bor));end if
													(numinc:increment symb inc-str)
													(setq obj (create-obj
																			(cadr (grread t 13 0))
																			(strcat pre-str mid-str suf-str)));end setq
													(if
														(and
															(/= "obj-blk" obj-typ)
															(= "1" bor-enc));end and
														(setq bor (create-bor obj prop)));end if
													t);end "("
												((member g2 '(66 98))
													(if
														(and
															(/= "obj-blk" obj-typ)
															(= "1" bor-enc)
															(= "3" bor-shp)
															bor);end and
														(progn
															(setq bor-rot (not bor-rot))
															(vla-rotate
																bor
																(vlax-3D-point (numinc:PolygonCentroid bor))
																(/ pi bor-sid#)));end progn
														(princ (strcat "\n无效按键。" msg)));end if
													t);end "("
												((= g2 9)
													(setq txt-rot (rem (+ pi pi txt-rot) (+ pi pi)))
													(if
														(vl-some
															(function
																(lambda (x)
																	(equal txt-rot x 1e-6)));end function
															(list (* pi 0.0)
																(* pi 0.5)
																(* pi 1.0)
																(* pi 1.5)));end vl-some
														(setq txt-rot
															(rem (+ txt-rot (/ pi 2.0)) (+ pi pi)));end setq
														(setq
															txt-rot (numinc:roundto txt-rot (/ pi 2.0))));end if
													(setvar
														'modemacro
														(strcat	 "旋转角度: " 
															(rtos
																(rem
																	(+ 360.0 (* 180.0 (/ txt-rot pi)))
																	360);end rem
																2
																2);end rtos
															"°"));end setvar
													(if bor
														(vla-rotate
															bor
															(vlax-get-property obj prop)
															(- txt-rot
																(if (= "obj-mtx" obj-typ)
																	(vla-get-rotation obj)
																	(- (vla-get-rotation obj) xa)))));end if
													(vla-put-rotation
														obj
														((lambda (a)
															 (if (= "obj-mtx" obj-typ)
																 a
																 (+ a xa)));end lambda
															txt-rot));end vla-put-rotation
													t);end "("
												((member g2 '(77 109))
													(setq txt-rot (rem (+ pi pi (* -1.0 txt-rot))
																					(+ pi pi)));end setq
													(setvar
														'modemacro
														(strcat	 "旋转角度: " 
															(rtos
																(rem
																	(+ 360.0 (* 180.0 (/ txt-rot pi)))
																	360);end rem
																2
																2);end rtos
															"°"));end setvar
													(if bor
														(vla-rotate
															bor
															(vlax-get-property obj prop)
															(- txt-rot
																(if (= "obj-mtx" obj-typ)
																	(vla-get-rotation obj)
																	(- (vla-get-rotation obj) xa)))));end if
													(vla-put-rotation
														obj
														((lambda (a)
															 (if (= "obj-mtx" obj-typ)
																 a
																 (+ a xa)));end lambda
															txt-rot));end vla-put-rotation
													t);end "("
												((member g2 '(82 114))
													(vla-delete obj)
													(if bor
														(vla-delete bor));end if
													(while (numinc:replace
																	 (strcat pre-str mid-str suf-str));end numinc:replace
														(if tog-cnt
															(numinc:increment symb inc-str)));end while
													(setq obj (create-obj
																			(cadr (grread t 13 0))
																			(strcat pre-str mid-str suf-str)));end setq
													(if
														(and
															(/= "obj-blk" obj-typ)
															(= "1" bor-enc));end and
														(setq bor (create-bor obj prop)));end if
													(princ msg));end "("
												((member g2 '(65 97))
													(if (= "obj-mtx" obj-typ)
														(progn
															(vlax-put obj
																'backgroundfill
																(setq mtx-bak
																	(~ (vlax-get obj 'backgroundfill))));end vlax-put
															(if (zerop mtx-bak)
																(princ "\n<关闭背景遮蔽>")
																(princ "\n<打开背景遮蔽>")));end progn
														(princ "\n无效按键。"));end if
													(princ msg));end "("
												((member g2 '(13 32))
													(vla-delete obj)
													(if bor
														(vla-delete bor));end if
													nil);end "("
												((princ (strcat "\n无效按键。" msg)))));end "("
										(t
											(vla-delete obj)
											(if bor
												(vla-delete bor));end if
											nil)))));end "("
						(t
							(setq msg
								(strcat
									"\n选择点 或[C]曲线对齐, [R]替换, [o]旋转文字[<]逆 / [>]顺, [T]递增开关, [I]增加量\n" 
									(if
										(and
											(/= "obj-blk" obj-typ)
											(= "1" bor-enc)
											(= "3" bor-shp));end and
										"[B]旋转边框, " 
										"");end if
									"[R]旋转 90°" 
									", [M]镜像" 
									(if (= "obj-mtx" obj-typ)
										",[a]背景遮蔽" 
										"");end if
									" <退出>: "));end setq
							(setvar
								'modemacro
								(strcat
									"旋转角度: " 
									(rtos (rem (+ 360.0 (* 180.0 (/ txt-rot pi))) 360) 2 2)
									"°"));end setvar
							(while
								(progn
									(initget
										(strcat
											"Curve Replace rOtation Toggle Increment ROtate" 
											(if
												(and
													(/= "obj-blk" obj-typ)
													(= "1" bor-enc)
													(= "3" bor-shp));end and
												" Border" 
												"");end if
											" Mirror" 
											(if (= "obj-mtx" obj-typ)
												" bAckground" 
												"")));end initget
									(setq pt (getpoint msg))
									(cond
										((null pt)
											nil);end "("
										((listp pt)
											(if
												(null
													(and table
														(numinc:textincell
															table
															(vlax-3D-point (trans pt 1 0))
															(strcat pre-str mid-str suf-str))));end null
												(progn
													(setq obj (create-obj
																			pt
																			(strcat pre-str mid-str suf-str)));end setq
													(if
														(and
															(/= "obj-blk" obj-typ)
															(= "1" bor-enc));end and
														(setq bor (create-bor obj prop)))));end if
											(if tog-cnt
												(numinc:increment symb inc-str));end if
											(princ "\n--------------------")
											t);end "("
										((= "Curve" pt)
											(while
												(setq ent
													(numinc:selectif
														"\n选择曲线 <退出>: " 
														(function
															(lambda (x)
																(not
																	(vl-catch-all-error-p
																		(vl-catch-all-apply
																			'vlax-curve-getendparam
																			(list x))))));end function
														entsel));end setq
												(if (numinc:aligntocurve
															(setq obj (create-obj
																					(cadr ent)
																					(strcat pre-str mid-str suf-str)));end setq
															prop
															(car ent)
															(if
																(and
																	(/= "obj-blk" obj-typ)
																	(= "1" bor-enc));end and
																(setq bor (create-bor obj prop))));end numinc:aligntocurve
													(if tog-cnt
														(numinc:increment symb inc-str));end if
													(progn
														(vla-delete obj)
														(if bor
															(vla-delete bor)))));end while
											t);end "("
										((= "Replace" pt)
											(while
												(numinc:replace (strcat pre-str mid-str suf-str))
												(if tog-cnt
													(numinc:increment symb inc-str)));end while
											t);end "("
										((= "rOtation" pt)
											(setq txt-rot
												(cond
													((getangle
														 (strcat "\n指定 " 
															 (cdr
																 (assoc	obj-typ
																	 '(
																			( "obj-txt" . "text")
																			( "obj-mtx" . "mtext")
																			( "obj-blk" . "block"))));end cdr
															 " 旋转角度 <" 
															 (angtos txt-rot)
															 ">: ")));end "("
													(txt-rot)));end setq
											(setvar 'modemacro
												(strcat "旋转角度: " 
													(rtos
														(rem
															(+ 360.0 (* 180.0 (/ txt-rot pi)))
															360);end rem
														2
														2);end rtos
													"°"));end setvar
											t);end "("
										((= "Toggle" pt)
											(if (setq tog-cnt (not tog-cnt))
												(princ "\n<递增打开>")
												(princ "\n<递增关闭>>"));end if
											t);end "("
										((= "Increment" pt)
											(numinc:increment symb inc-str)
											t);end "("
										((= "Border" pt)
											(princ "\n<边框旋转>")
											(setq bor-rot (not bor-rot))
											t);end "("
										((= "ROtate" pt)
											(setq txt-rot (rem (+ pi pi txt-rot) (+ pi pi)))
											(if
												(vl-some
													(function
														(lambda (x)
															(equal txt-rot x 1e-6)));end function
													(list (* pi 0.0)
														(* pi 0.5)
														(* pi 1.0)
														(* pi 1.5)));end vl-some
												(setq
													txt-rot (rem (+ txt-rot (/ pi 2.0)) (+ pi pi)));end setq
												(setq txt-rot (numinc:roundto txt-rot (/ pi 2.0))));end if
											(princ
												(strcat
													"\n" 
													(setvar
														'modemacro
														(strcat "旋转角度: " 
															(rtos
																(rem
																	(+ 360.0 (* 180.0 (/ txt-rot pi)))
																	360);end rem
																2
																2);end rtos
															"°"))));end princ
											t);end "("
										((= "Mirror" pt)
											(setq txt-rot
												(rem (+ pi pi (* -1.0 txt-rot)) (+ pi pi)));end setq
											(princ
												(strcat
													"\n" 
													(setvar
														'modemacro
														(strcat "旋转角度: " 
															(rtos
																(rem
																	(+ 360.0 (* 180.0 (/ txt-rot pi)))
																	360);end rem
																2
																2);end rtos
															"°"))));end princ
											t);end "("
										((= "bAckground" pt)
											(if (zerop (setq mtx-bak (~ mtx-bak)))
												(princ "\n<关闭背景遮蔽>")
												(princ "\n<打开背景遮蔽>"));end if
											t))))));end cond
					(numinc:writeconfig
						cfgfname
						(mapcar 'eval (mapcar 'car symlist))));end progn
				(princ "\n*Cancel*"))));end cond
	(if (< 0 dclid)
		(setq dclid (unload_dialog dclid)));end if
	(if
		(and
			(= 'vla-object (type numinc:wshobject))
			(not (vlax-object-released-p numinc:wshobject)));end and
		(progn
			(vlax-release-object numinc:wshobject)
			(setq numinc:wshobject nil)));end if
	(mapcar 'setvar varlst vallst)
	(princ));end defun
(defun numinc:selectif (msg pred func / ent)
	(setq pred (eval pred))
	(while
		(progn (setvar 'errno 0)
			(setq ent (func msg))
			(cond
				((= 7 (getvar 'errno))
					(princ "\n没有选中，再选择。"));end "("
				((= 'ename (type (car ent)))
					(if (and pred (null (pred (car ent))))
						(princ "\n无效对象。"))))));end while
	ent);end defun
(defun numinc:replace (str / aid enx fun obj obl par rtn sel tmp)
	(while
		(progn
			(setvar 'errno 0)
			(setq sel (nentsel "\n选择要替换的文字 <退出>: "))
			(cond
				((= 7 (getvar 'errno))
					(princ "\n未选中，再试一次。"));end "("
				((null sel)
					(setq rtn nil));end "("
				((progn
					 (setq enx (entget (car sel))
						 par (cadddr sel));end setq
					 (cond
						 ((= "ATTRIB" (cdr (assoc 0 enx)))
							 (setq obl (list (vlax-ename->vla-object (car sel)))
								 fun	vla-put-textstring));end "("
						 ((and par
								(wcmatch (cdr (assoc 0 (entget (car par))))
									"*DIMENSION"));end and
							 (setq obl (list (vlax-ename->vla-object (car par)))
								 fun	vla-put-textoverride));end "("
						 ((wcmatch (cdr (assoc 0 enx)) "TEXT,MTEXT")
							 (setq obl (list (vlax-ename->vla-object (car sel)))
								 fun	vla-put-textstring));end "("
						 ((= "MULTILEADER" (cdr (assoc 0 enx)))
							 (setq obl (list (vlax-ename->vla-object (car sel))))
							 (cond
								 ((= acblockcontent (vla-get-contenttype (car obl)))
									 (vlax-for sub
										 (vla-item
											 (vla-get-blocks (numinc:acdoc))
											 (vla-get-contentblockname (car obl)));end vla-item
										 (if (= "AcDbAttributeDefinition" 
													 (vla-get-objectname sub));end =
											 (progn
												 (setq tmp (cons sub tmp))
												 (if (vlax-property-available-p sub 'objectid32)
													 (setq aid (cons (vla-get-objectid32 sub) aid))
													 (setq aid (cons (vla-get-objectid sub) aid))))));end vlax-for
									 (setq tmp (reverse tmp)
										 aid (reverse aid));end setq
									 (if
										 (or (not (cdr aid))
											 (setq aid
												 (mapcar '(lambda (n) (nth n aid))
													 (numinc:listbox
														 "Select Attribute(s) to Replace" 
														 (mapcar 'vla-get-tagstring tmp)))));end or
										 (if (vlax-method-applicable-p
													 (car obl)
													 'setblockattributevalue32);end vlax-method-applicable-p
											 (setq fun
												 (lambda (obj idx str)
													 (vla-setblockattributevalue32 obj idx str)));end setq
											 (setq
												 fun (lambda (obj idx str)
															 (vla-setblockattributevalue obj idx str))));end if
										 t));end "("
								 ((= acmtextcontent (vla-get-contenttype (car obl)))
									 (setq fun vla-put-textstring));end "("
								 ((princ "\nSelected multileader has no annotation."))));end "("
						 ((and par
								(= "INSERT" (cdr (assoc 0 (entget (last par)))))
								(setq obl (vlax-invoke
														(vlax-ename->vla-object (last par))
														'getattributes)));end and
							 (if
								 (or (not (cdr obl))
									 (setq obl
										 (mapcar '(lambda (n) (nth n obl))
											 (numinc:listbox
												 "Select Attribute(s) to Replace" 
												 (mapcar 'vla-get-tagstring obl)))));end or
								 (setq fun vla-put-textstring)
								 t));end "("
						 ((princ "\n慖掕懳徾澷澚丅")));end cond
					 (not (and obl fun)));end progn
					t);end "("
				((vl-some '(lambda (x) (not (vlax-write-enabled-p x))) obl)
					(if (cdr obl)
						(princ
							"\nOne or more of the selected objects is on a locked layer or is write-protected.");end princ
						(princ
							"\nThe selected object is on a locked layer or is write-protected.")));end "("
				((setq rtn t)
					(if aid
						(foreach idx aid (fun (car obl) idx str))
						(foreach obj obl (fun obj str)));end if
					(if par
						(entupd (last par)));end if
					nil))));end while
	rtn);end defun
(defun numinc:annotative-p (sty)
	(and
		(setq sty (tblobjname "style" sty))
		(setq sty (cadr (assoc -3 (entget sty '( "AcadAnnotative")))))
		(= 1 (cdr (assoc 1070 (reverse sty))))));end defun
(defun numinc:getblockdata (/ a b c)
	(while (setq a (tblnext "block" (null a)))
		(if
			(and
				(null (wcmatch (cdr (assoc 02 a)) "`**,*|*"))
				(= 2 (logand 2 (cdr (assoc 70 a))))
				(setq c
					((lambda (c / d e)
						 (while (setq c (entnext c))
							 (if (= "ATTDEF" (cdr (assoc 0 (setq d (entget c)))))
								 (setq e (cons (strcase (cdr (assoc 2 d))) e))));end while
						 (vl-sort e '<));end lambda
						(tblobjname "block" (cdr (assoc 2 a))))));end and
			(setq b (cons (cons (cdr (assoc 2 a)) c) b))));end while
	(vl-sort b '(lambda (a b) (< (car a) (car b)))));end defun
(defun numinc:gettableitems (table / a b)
	(while (setq a (tblnext table (null a)))
		(if
			(not
				(or (wcmatch (cdr (assoc 2 a)) "`**,*|*")
					(and (= "layer" (strcase table t))
						(= 4 (logand 4 (cdr (assoc 70 a)))))));end not
			(setq b (cons (cdr (assoc 2 a)) b))));end while
	(acad_strlsort b));end defun
(defun numinc:roundto (a b)
	(* b
		(fix (/ (+	a
							(* b
								(if (minusp a)
									-0.5
									0.5)));end +	a
					 b))));end defun
(defun numinc:listbox (msg lst / rtn)
	(cond
		((or (null dclid) (not (new_dialog "listbox" dclid)))
			(numinc:popup
				"About Dialog could not be Loaded" 
				16
				(princ
					(strcat
						"The Incremental Numbering Suite List Box dialog could not be loaded. " 
						"The DCL file required by the application resides in the following location:\n\n" 
						dclfname
						"\n\nPlease check the integrity of this file.")));end numinc:popup
			(princ));end "("
		(t
			(set_tile "dcl" msg)
			(start_list "lst")
			(foreach itm lst (add_list itm))
			(end_list)
			(setq rtn (set_tile "lst" "0"))
			(action_tile "lst" "(setq rtn $value)")
			(setq rtn
				(if (= 1 (start_dialog))
					(read (strcat "(" rtn ")"))))));end cond
	rtn);end defun
(defun numinc:about (id / _dialogtext _displaybitmap i j x y)
	(defun _dialogtext (key str)
		(set_tile key str)
		(start_image key)
		(vector_image
			0
			(1- (dimy_tile key))
			(dimx_tile key)
			(1- (dimy_tile key))
			0);end vector_image
		(end_image));end defun
	(cond
		((not (new_dialog "about" id))
			(numinc:popup
				"About Dialog could not be Loaded" 
				16
				(princ
					(strcat
						"The Incremental Numbering Suite About dialog could not be loaded. " 
						"The DCL file required by the application resides in the following location:\n\n" 
						dclfname
						"\n\n请检查该文件的完整性。")));end numinc:popup
			(princ));end "("
		(t
			(repeat (setq i 32)
				(setq j 1)
				(repeat 32
					(setq x (cons j x)
						y (cons i y)
						j (1+ j)));end repeat
				(setq i (1- i)));end repeat
			(foreach pair
				'(( "title1" "递增编号套件")
					 ( "title2" "位置放置控制")
					 ( "title3" "曲线线形控制"));end "("
				(apply '_dialogtext pair));end foreach
			(start_dialog))));end defun
(defun numinc:fixdir (dir)
	(vl-string-right-trim
		"\\" 
		(vl-string-translate "/" "\\"  dir)));end defun
(defun numinc:getsavepath (/ tmp)
	(cond
		((setq tmp (getvar 'roamablerootprefix))
			(strcat (numinc:fixdir tmp) "\\Support"));end "("
		((setq tmp (findfile "acad.pat"))
			(numinc:fixdir (vl-filename-directory tmp)));end "("
		((numinc:fixdir (vl-filename-directory (vl-filename-mktemp))))));end defun
(defun numinc:writedcl (dcl / file)
	(cond
		((findfile dcl))
		((setq file (open dcl "w"))
			(foreach line
				'( "edit:edit_box{edit_width=5; alignment=centered;fixed_width=true;}" 
					 "but1:button{width=12; fixed_width=true; alignment=centered;}" 
					 "but2:button{width=19.5; fixed_width=true; alignment=centered;}" 
					 "but3:button{width=15.0; fixed_width=true; alignment=centered; fixed_height=true; height=2.2;}" 
					 "rad1:radio_button{alignment=centered;}" 
					 "txt1:text{alignment=centered; fixed_width=false;}" 
					 "txt2:text{alignment=centered; fixed_width=true; width=8.8;}" 
					 "txt3:text{alignment=left; fixed_width=false;}" 
					 "btxt:image{fixed_width=true;height=1.5; fixed_height=true; color=dialog_background;}" 
					 "ctxt:text{width=30; fixed_width=true; alignment=centered;}" 
					 "pop1:popup_list{width=28; fixed_width=true;}" 
					 "spc1:spacer{height=0.1; fixed_height=true; width=0.1; fixed_width=true;}" 
					 "imgbox:image_button{alignment=centered; height=1.5; aspect_ratio=1; fixed_width=true; fixed_height=true; color=1;}" 
					 "img20b:image_button{fixed_width=true; fixed_height=true; width=3.5; aspect_ratio=1.0;}" 
					 "numinc:dialog{key=\"dcl\";" 
					 " spacer;" 
					 ":row{" 
					 ":column{" 
					 ":toggle{alignment=left; key=\"dyn-flg\"; label=\"文字跟随光标\";}}}" 
					 "spacer;" 
					 ":row{" 
					 ":column{" 
					 ":boxed_column{label=\"递增格式\"; width=37; fixed_width=true;" 
					 ":row{alignment=centered; fixed_width=true;" 
					 ":column{" 
					 ":edit{key= \"pre-str\";}" 
					 ":txt2{label=\"字首\";}}" 
					 ":column{" 
					 ":edit{key= \"mid-str\";}" 
					 ":txt2{label=\"中间\";}}" 
					 ":column{" 
					 ":edit{key= \"suf-str\";}" 
					 ":txt2{label=\"字尾\";}}}" 
					 "spacer;" 
					 ":edit{label=\"递增: \"; key=\"inc-str\";}" 
					 "spacer;" 
					 ":boxed_column{label=\"递增部分\";" 
					 ":row{alignment=centered; fixed_width=true;" 
					 ":toggle{label=\"字首\"; key=\"inc-pre\";}" 
					 ":toggle{label=\"中间\"; key=\"inc-mid\";}" 
					 ":toggle{label=\"字尾\"; key=\"inc-suf\";}}" 
					 " spacer;" 
					 "}" 
					 "spacer;" 
					 "}" 
					 ":boxed_column{label=\"边框选项\";" 
					 "spacer;" 
					 ":toggle{key=\"bor-enc\"; label=\"附上外框:\";}" 
					 ":row{" 
					 ":popup_list{key=\"bor-shp\"; width=18; fixed_width=true; }" 
					 ":column{" 
					 "spc1;" 
					 ":edit_box{edit_width=5; fixed_width=true; key=\"bor-sid\"; label=\"边数:\";}" 
					 "spc1;" 
					 "}}" 
					 ":text{label=\"图层:\"; key=\"bor-ltx\";}" 
					 ":popup_list{key=\"bor-lay\";}" 
					 "spacer;" 
					 ":boxed_column{label=\"外框尺寸\";" 
					 ":row{fixed_width=true; alignment=centered;" 
					 ":radio_column{" 
					 ":radio_button{key=\"bor-off\"; label=\"偏  移:\";}" 
					 ":radio_button{key=\"bor-fix\"; label=\"固  定:\";}}" 
					 ":column{" 
					 ":edit{key=\"off-ed1\" ;}" 
					 ":edit{key=\"fix-ed1\" ;}}" 
					 ":column{fixed_width=true;" 
					 " spacer_1;" 
					 ":text{label=\" x\"; key=\"fix-txt\";}}" 
					 ":column{" 
					 "spacer_1;" 
					 ":edit{key=\"fix-ed2\";}}}" 
					 ":row{fixed_width=true; alignment=centered;" 
					 ":spacer" 
					 "{height=0.1; fixed_height=true; width=11; fixed_width=true;}" 
					 ":but2{key=\"bor-pik\"; label=\"拾取\";}}" 
					 " spacer;" 
					 "}" 
					 "spacer;" 
					 "}}" 
					 ":column{alignment=top;" 
					 ":boxed_column{label=\"对象属性\"; width=37; fixed_width=true; fixed_height=true;" 
					 ":radio_row{alignment=centered; fixed_width=true;" 
					 ":rad1{label=\"单行文本\"; key=\"obj-txt\";}" 
					 ":rad1{label=\"多行文本\"; key=\"obj-mtx\";}" 
					 ":rad1{label=\"图 块\"; key=\"obj-blk\";}}" 
					 ":text{label=\"图 块:\"; key=\"blk-txt\";}" 
					 ":row{fixed_width=true;" 
					 ":pop1{key=\"blk-nme\";}" 
					 ":column{" 
					 "spc1;" 
					 ":img20b{key=\"blk-pik\"; alignment=top;}}}" 
					 "spacer;" 
					 ":text{label=\"属性文字:\"; key=\"att-txt\";}" 
					 ":popup_list{key=\"att-nme\";}" 
					 "spacer;" 
					 ":boxed_column{label=\"图块比例\";" 
					 ":row{fixed_width=true; alignment=centered;" 
					 ":edit{label=\"比  例:\"; key=\"blk-scl\";}" 
					 ":column{" 
					 "spc1;" 
					 ":img20b{key=\"scl-pik\"; alignment=top;}}" 
					 "}" 
					 "spacer;" 
					 ":toggle{key=\"scl-var\"; label=\"使用系统变量:\";}" 
					 ":popup_list{key=\"scl-pop\";}" 
					 "spacer;" 
					 "}" 
					 "spacer;" 
					 "}" 
					 ":boxed_column{label=\"阵列选项\"; width=37; fixed_width=true;" 
					 ":row{fixed_width=true;" 
					 ":toggle{label=\"递增阵列\"; key=\"arr-use\";}" 
					 ":edit{label=\"数量:\"; key=\"arr-qty\";}}" 
					 "spacer;" 
					 ":boxed_column{label=\"旋转对象\";" 
					 ":row{" 
					 ":radio_button{key=\"arr-aln\"; label=\"平  行\";}" 
					 ":radio_button{key=\"arr-per\"; label=\"垂  直\";}}" 
					 ":row{fixed_width=true;" 
					 ":radio_button{key=\"arr-oth\"; label=\"角  度:\";}" 
					 ":edit{key=\"arr-rot\";}" 
					 ":column{" 
					 "spc1;" 
					 ":img20b{key=\"arr-pik\"; alignment=top;}}" 
					 "}" 
					 "spacer;" 
					 "}" 
					 "spacer;" 
					 "}}" 
					 ":column{" 
					 ":boxed_column{label=\"格  式\";" 
					 ":text{label=\"文字图层: \"; key=\"lay-txt\";}" 
					 ":popup_list{key=\"txt-lay\";}" 
					 "spacer;" 
					 ":text{label=\"文字样式: \"; key=\"sty-txt\";}" 
					 ":popup_list{key=\"txt-sty\";}" 
					 "spacer;" 
					 ":text{label=\"对齐方式:\"; key=\"aln-txt\";}" 
					 ":popup_list{key=\"txt-aln\";}" 
					 "spacer;" 
					 ":boxed_column{" 
					 "label=\"文字高度\";" 
					 ":row{" 
					 "fixed_width=true;" 
					 "alignment=centered;" 
					 ":toggle{key=\"txt-bst\"; label=\"使用样式高度\";}" 
					 ":edit{key=\"txt-sze\";}" 
					 ":column{" 
					 "spc1;" 
					 ":img20b{key=\"txt-pik\"; alignment=top;}}}" 
					 "spacer;" 
					 "}" 
					 "spacer;" 
					 ":boxed_column{" 
					 " label=\"背景遮蔽\";" 
					 " width=37;" 
					 " fixed_width=true;" 
					 " fixed_height=true;" 
					 ":toggle{label=\"使用背景遮蔽\"; key=\"msk-use\";}" 
					 " spacer;" 
					 ":boxed_column{" 
					 "label=\"遮蔽范围\";" 
					 ":row{" 
					 "alignment=centered;" 
					 "fixed_width=true;" 
					 ":edit{label=\"偏移系数:\"; key=\"msk-off\";}" 
					 ":column" 
					 "{" 
					 "spc1;" 
					 ":img20b{key=\"msk-pik\"; alignment=top;}}}" 
					 "spacer;" 
					 "}" 
					 ":boxed_column{" 
					 "label=\"填充颜色\";" 
					 ":row{alignment=centered; fixed_width=true;" 
					 ":toggle{key=\"msk-trn\"; label=\"使用透明\";}" 
					 ":imgbox{key=\"msk-col\";}" 
					 "}" 
					 "spacer;" 
					 "}" 
					 " spacer;" 
					 "}" 
					 "spacer;" 
					 "}}}" 
					 "spacer;" 
					 ":row{fixed_width=true; alignment=centered;" 
					 "spacer;" 
					 ":but1{key=\"about\";  label=\"帮助\";}" 
					 "spacer_1;" 
					 ":but3{key=\"accept\"; label=\"确定\"; is_default=true;}" 
					 "spacer_1;" 
					 ":but1{key=\"cancel\"; label=\"取消\"; is_cancel=true;}" 
					 "spacer;" 
					 "}" 
					 "spacer;" 
					 "}" 
					 "about:dialog{" 
					 "label=\"帮助\";" 
					 "spacer;" 
					 ":btxt{key  =\"title1\"; alignment=centered; width=15;}" 
					 ":row" 
					 "{" 
					 "fixed_width=true;" 
					 "alignment=left;" 
					 "spacer;" 
					 ":btxt {width=10; key=\"title2\"; alignment=centered;}" 
					 "}" 
					 "spacer;" 
					 ":row{fixed_width=true; alignment=centered;" 
					 "spacer;" 
					 ":column" 
					 "{" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 "}" 
					 ":column" 
					 "{" 
					 ":txt1{label=\"回车\" ;}" 
					 ":txt1{label=\"点击\" ;}" 
					 ":txt1{label=\"<\"  ;}" 
					 ":txt1{label=\">\"  ;}" 
					 ":txt1{label=\"O\"  ;}" 
					 ":txt1{label=\"Tab\";}" 
					 ":txt1{label=\"M\"  ;}" 
					 ":txt1{label=\"C\"  ;}" 
					 ":txt1{label=\"R\"  ;}" 
					 ":txt1{label=\"T\"  ;}" 
					 ":txt1{label=\"I\"  ;}" 
					 ":txt1{label=\"B\"  ;}" 
					 ":txt1{label=\"A\"  ;}" 
					 "}" 
					 ":column" 
					 "{" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 "}" 
					 "spacer;" 
					 ":column" 
					 "{" 
					 ":txt3{label=\"(或空格键/右击) 退出程序\" ;}" 
					 ":txt3{label=\"选择位置\";}" 
					 ":txt3{label=\"逆时针旋转对象\";}" 
					 ":txt3{label=\"顺时针旋转对象\";}" 
					 ":txt3{label=\"指定角度旋转\";}" 
					 ":txt3{label=\"90°旋转对象\" ;}" 
					 ":txt3{label=\"镜像物体\" ;}" 
					 ":txt3{label=\"对齐曲线\"  ;}" 
					 ":txt3{label=\"替换现有文本/属性字符串\" ;}" 
					 ":txt3{label=\"切换递增计数器开关\";}" 
					 ":txt3{label=\"数值增量\";}" 
					 ":txt3{label=\"旋转多边形边框\";}" 
					 ":txt3{label=\"切换文字背景遮蔽\";}" 
					 "}}" 
					 " spacer;" 
					 ":row" 
					 "{" 
					 "fixed_width=true;" 
					 "alignment=left;" 
					 "spacer;" 
					 ":btxt {width=15; key=\"title3\"; alignment=centered;}" 
					 "}" 
					 " spacer;" 
					 ":row" 
					 "{" 
					 "fixed_width=true;" 
					 "alignment=centered;" 
					 "spacer;" 
					 ":column" 
					 "{" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 ":txt1{label=\"[\"  ;}" 
					 "}" 
					 ":column" 
					 "{" 
					 ":txt1{label=\"回车\" ;}" 
					 ":txt1{label=\"点击\" ;}" 
					 ":txt1{label=\"+/-\";}" 
					 ":txt1{label=\"O\"  ;}" 
					 ":txt1{label=\"P\"  ;}" 
					 ":txt1{label=\"B\"  ;}" 
					 ":txt1{label=\"A\"  ;}" 
					 "}" 
					 ":column" 
					 "{" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 ":txt1{label=\"]\"  ;}" 
					 "}" 
					 "spacer;" 
					 ":column" 
					 "{" 
					 ":txt3{label=\"(或空格键/右击) 退出程序\";}" 
					 ":txt3{label=\"选择位置\";}" 
					 ":txt3{label=\"增加 / 减少偏移量\";}" 
					 ":txt3{label=\"选择偏移对象\"  ;}" 
					 ":txt3{label=\"切换对象的垂直度\";}" 
					 ":txt3{label=\"旋转多边形边界\";}" 
					 ":txt3{label=\"切换文字背景遮蔽\";}" 
					 "}}" 
					 " spacer_1;" 
					 "ok_only;" 
					 "}");end 
				(write-line line file));end foreach
			(setq file (close file))
			(while (not (findfile dcl)))
			dcl)));end defun
(defun numinc:misc ()
	(mapcar 'set_tile
		(mapcar 'vl-list->string '((100 99 108) (97 117 116)))
		(list
			(strcat
				"递增编号套件  V3.9--" 
				(menucmd "m=$(edtime,0,yyyy)"));end strcat
			(strcat
				(vl-list->string
					'(067	111 112 121	 114	105 103 104	 116
						 032	040 099 041	 032	076 101 101	 032
						 077	097 099 032));end vl-list->string
				(menucmd "m=$(edtime,0,yyyy)")))));end defun
(defun numinc:tostring (arg / dim)
	(cond
		((= 'int (type arg))
			(itoa arg));end "("
		((= 'real (type arg))
			(setq dim (getvar 'dimzin))
			(setvar 'dimzin 8)
			(setq arg (rtos arg 2 15))
			(setvar 'dimzin dim)
			arg);end "("
		((vl-prin1-to-string arg))));end defun
(defun numinc:writeconfig (name lst / file)
	(if (setq file (open name "w"))
		(progn
			(foreach x lst (write-line (numinc:tostring x) file))
			(setq file (close file))
			t)));end defun
(defun numinc:readconfig (name lst / file line)
	(if
		(and
			(setq name (findfile name))
			(setq file (open name "r")));end and
		(progn
			(foreach	x lst
				(if (setq line (read-line file))
					(set x (read line))));end foreach	x
			(setq file (close file))
			t)));end defun
(defun numinc:popup (title flags msg / err)
	(setq	err (vl-catch-all-apply
							'vlax-invoke-method
							(list (numinc:wsh) 'popup msg 0 title flags)));end setq	err
	(if (null (vl-catch-all-error-p err))
		err));end defun
(defun numinc:wsh nil
	(cond (numinc:wshobject)
		((setq numinc:wshobject (vlax-create-object "wscript.shell")))));end defun
(defun numinc:makelist (key lst)
	(start_list key)
	(foreach x lst (add_list x))
	(end_list));end defun
(defun numinc:textincell (lst pnt str / data dir)
	(setq dir (vlax-3D-point (trans (getvar 'viewdir) 1 0)))
	(if
		(setq data
			(vl-some
				(function
					(lambda (table / row col)
						(if (= :vlax-true (vla-hittest table pnt dir 'row 'col))
							(list table row col))));end function
				lst));end setq
		(not (apply 'vla-settext (append data (list str))))));end defun
(defun numinc:increment (lst inc)
	(foreach sym lst
		(if (distof (eval sym) 2)
			(set sym (numinc:incrementnumba (eval sym) inc))
			(set sym
				(numinc:incrementalpha (eval sym) (fix (abs (distof inc))))))));end defun
(defun numinc:incrementnumba
	(str inc / _rtos _decimalplaces incd maxd num slen strd)
	(defun _rtos (real prec / dimzin result)
		(setq dimzin (getvar 'dimzin))
		(setvar 'dimzin 0)
		(setq result (rtos real 2 prec))
		(setvar 'dimzin dimzin)
		result);end defun
	(defun _decimalplaces (string / pos)
		(if (setq pos (vl-string-position 46 string))
			(- (strlen string) pos 1)
			0));end defun
	(setq num (+ (distof str) (distof inc)))
	(if (minusp (distof str))
		(setq str (substr str 2)));end if
	(if (minusp (distof inc))
		(setq inc (substr inc 2)));end if
	(setq	incd (_decimalplaces inc)
		strd (_decimalplaces str)
		maxd (max incd strd)
		slen (strlen str));end setq	incd
	(cond
		((and (< 0 strd) (< 0 incd))
			(setq slen (+ (- slen strd) maxd)));end "("
		((and (= 0 strd) (< 0 incd))
			(setq slen (+ incd slen 1))));end cond
	(setq str (_rtos num maxd))
	(if (minusp num)
		(setq str (substr str 2)));end if
	(while (< (strlen str) slen)
		(setq str (strcat "0" str)));end while
	(if (minusp num)
		(strcat "-" str)
		str));end defun
(defun numinc:incrementalpha (str inc / _incrementalpha a)
	(defun _incrementalpha (a b / c d e)
		(cond
			((cond
				 ((< 47 (setq c (car a)) 58)
					 (setq	d 48
						 e 10));end "("
				 ((< 64 c 91)
					 (setq	d 65
						 e 26));end "("
				 ((< 96 c 123)
					 (setq	d 97
						 e 26)));end cond
				(setq c (+ (- c d) b)
					b (/ c e));end setq
				(cons (+ d (rem c e))
					(if (zerop b)
						(cdr a)
						(if (cdr a)
							(_incrementalpha (cdr a) b)
							(_incrementalpha
								(list d)
								(if (= 10 e)
									b
									(1- b)))))));end "("
			((cons c
				 (if (cdr a)
					 (_incrementalpha (cdr a) b)
					 (_incrementalpha (list 65) (1- b)))))));end defun
	(vl-list->string
		(reverse
			(if (setq a (reverse (vl-string->list str)))
				(_incrementalpha a inc)
				(_incrementalpha '(65) (1- inc))))));end defun
(defun numinc:aligntocurve
	(obj prp ent bor / a1 fac fl g1 g2 gr ll msg mtx p1 ur xa)
	(setq	fac
		(if (= "AcDbBlockReference" (vla-get-objectname obj))
			(progn
				(vla-getboundingbox obj 'll 'ur)
				(/
					(-
						(cadr (vlax-safearray->list ur))
						(cadr (vlax-safearray->list ll)));end -
					2.0));end progn
			(vla-get-height obj)));end setq	fac
	(setq	msg
		(princ
			(strcat
				"\n点击对齐 <退出>: [+/-] for [O]偏移, [P]垂直" 
				(if (and bor (= "3" bor-shp))
					", [B]旋转边框" 
					"");end if
				(if (setq mtx (= "AcDbMText" (vla-get-objectname obj)))
					", [a]背景遮蔽" 
					""))));end setq	msg
	(setq	xa
		(angle	'(0.0 0.0 0.0)
			(trans
				(getvar 'ucsxdir)
				0
				(trans '(0.0 0.0 1.0) 1 0 t))));end setq	xa
	(while
		(progn
			(setq gr (grread t 15 0)
				g1 (car gr)
				g2 (cadr gr));end setq
			(cond
				((member g1 '(5 3))
					(setq p1 (vlax-curve-getclosestpointto
										 ent
										 (setq g2 (trans g2 1 0)));end vlax-curve-getclosestpointto
						a1 (angle p1 g2)
						p1 (vlax-3D-point (polar p1 a1 (* fac crv-off)))
						a1 (numinc:makereadable (+ a1 crv-per)));end setq
					(if bor
						(vla-move bor (vlax-get-property obj prp) p1));end if
					(vlax-put-property obj prp p1)
					(if bor
						(vla-rotate
							bor
							p1
							(-	a1
								(if mtx
									(+ (vla-get-rotation obj) xa)
									(vla-get-rotation obj)))));end if
					(vla-put-rotation
						obj
						((lambda (a)
							 (if mtx
								 (- a xa)
								 a));end lambda
							a1));end vla-put-rotation
					(null (setq fl (= g1 3))));end "("
				((= 25 g1)
					nil);end "("
				((= 02 g1)
					(cond
						((member g2 '(80 112))
							(setq crv-per (- (/ pi 2.0) crv-per)));end "("
						((member g2 '(45 95))
							(setq crv-off (- crv-off 0.1)));end "("
						((member g2 '(43 61))
							(setq crv-off (+ crv-off 0.1)));end "("
						((member g2 '(13 32))
							nil);end "("
						((member g2 '(79 111))
							(setq crv-off
								(/
									(cond
										((getdist (strcat "\nSpecify offset <" 
																(rtos (* fac crv-off))
																">: ")));end "("
										((* fac crv-off)));end cond
									fac));end setq
							(princ msg));end "("
						((and (member g2 '(65 97)) mtx)
							(vlax-put obj
								'backgroundfill
								(setq mtx-bak (~ (vlax-get obj 'backgroundfill))));end vlax-put
							(if (zerop mtx-bak)
								(princ "\n<关闭背景遮蔽>")
								(princ "\n<打开背景遮蔽>"));end if
							(princ msg));end "("
						((member g2 '(66 98))
							(if (and bor (= "3" bor-shp))
								(progn
									(setq bor-rot (not bor-rot))
									(vla-rotate
										bor
										(vlax-3D-point (numinc:polygoncentroid bor))
										(/ pi bor-sid#)));end progn
								(princ (strcat "\n无效按键。" msg)));end if
							t);end "("
						((princ (strcat "\n无效按键。" msg))))))));end while
	(redraw)
	fl);end defun
(defun numinc:mtextwidth (str sty hgt / box mtw)
	(cond
		((setq mtw
			 (entmakex
				 (list
					 '(000 . "MTEXT")
					 '(100 . "AcDbEntity")
					 '(100 . "AcDbMText")
					 '(10 0.0 0.0 0.0)
					 (cons 01 str)
					 (cons 07 sty)
					 (cons 40 hgt))));end setq
			(setq box (numinc:gettextbox (entget mtw) 0.0))
			(entdel mtw)
			(* 1.01 (- (caadr box) (caar box))));end "("
		(((lambda (box)
				(if box
					(* 1.01 (- (caadr box) (caar box)))));end lambda
			 (textbox
				 (list
					 (cons 01 str)
					 (cons 40 hgt)
					 (cons 07 sty)))));end "("
		(0.0)));end defun
(defun numinc:makereadable (a)
	((lambda (a)
		 (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
			 (numinc:makereadable (+ a pi))
			 a));end lambda
		(rem (+ a pi pi) (+ pi pi))));end defun
(defun numinc:polygoncentroid (obj / _group)
	(defun _group (lst)
		(if	lst
			(cons (list (car lst) (cadr lst)) (_group (cddr lst)))));end defun
	((lambda (lst)
		 ((lambda (len)
				(mapcar '/ (apply 'mapcar (cons '+ lst)) (list len len)));end lambda
			 (length lst)));end lambda
		(_group (vlax-get obj 'coordinates))));end defun
(defun numinc:createtextborder (ent typ off fx1 fx2	 sid /	 cen
																 enx i	 inc lst mat	 pts rad rot
																 vec);end ent
	(setq enx (entget ent))
	(cond
		((setq lst (numinc:gettextbox enx off))
			(setq cen (mapcar '(lambda (a b) (/ (+ a b) 2.0))
									(car lst)
									(caddr lst));end mapcar
				rot (if (= "MTEXT" (cdr (assoc 0 enx)))
							(angle	'(0. 0. 0.)
								(trans (cdr (assoc 11 enx)) 0 (cdr (assoc 210 enx))));end "("
							(cdr (assoc 50 enx))));end setq
			(cond
				((= "0" typ)
					(entmakex
						(list
							'(0 . "CIRCLE")
							(cons 010 cen)
							(cons 040
								(cond (fx1)
									((distance cen (car lst)))));end cons
							(assoc 210 enx))));end "("
				((member typ '( "1" "2"))
					(if (and fx1 fx2)
						(progn
							(setq fx1 (/ fx1 2.0)
								fx2 (/ fx2 2.0));end setq
							(setq mat
								(list
									(list (cos rot) (- (sin rot)) 0.0)
									(list (sin rot) (cos rot) 0.0)
									(list 0.0 0.0 1.0)));end setq
							(setq vec (mapcar '- cen (mxv mat cen)))
							(setq lst
								(list
									(list (- (car cen) fx1) (- (cadr cen) fx2) (caddr cen))
									(list (+ (car cen) fx1) (- (cadr cen) fx2) (caddr cen))
									(list (+ (car cen) fx1) (+ (cadr cen) fx2) (caddr cen))
									(list (- (car cen) fx1) (+ (cadr cen) fx2) (caddr cen))));end setq
							(entmakex
								(append
									'(
										 (000 . "LWPOLYLINE")
										 (100 . "AcDbEntity")
										 (100 . "AcDbPolyline")
										 (090 . 4)
										 (070 . 1));end 
									(list (cons 38 (caddar lst)))
									(apply 'append
										(mapcar
											(function
												(lambda (a b)
													(list
														(cons 10 (mapcar '+ (mxv mat a) vec))
														(cons 42 b))));end function
											lst
											(if (= "1" typ)
												'(0.0 0.0 0.0 0.0)
												'(0.0 1.0 0.0 1.0))));end apply
									(list (assoc 210 enx)))));end progn
						(entmakex
							(append
								'((000 . "LWPOLYLINE")
									 (100 . "AcDbEntity")
									 (100 . "AcDbPolyline")
									 (090 . 4)
									 (070 . 1));end "("
								(list (cons 38 (caddar lst)))
								(apply 'append
									(mapcar
										(function
											(lambda (a b) (list (cons 10 a) (cons 42 b))));end function
										lst
										(if (= "1" typ)
											'(0.0 0.0 0.0 0.0)
											'(0.0 1.0 0.0 1.0))));end apply
								(list (assoc 210 enx))))));end "("
				(t
					(setq inc (/ (+ pi pi) sid)
						rad (cond (fx1)
									((/ (distance cen (car lst)) (cos (/ inc 2.0)))));end cond
						i	 -1);end setq
					(if (= 1 (logand 1 sid))
						(setq rot (+ rot (/ pi 2.))));end if
					(repeat	sid
						(setq	pts
							(cons
								(cons 10
									(polar cen (+ rot (* (setq i (1+ i)) inc)) rad));end cons
								pts)));end repeat	sid
					(entmakex
						(append
							(list
								'(000 . "LWPOLYLINE")
								'(100 . "AcDbEntity")
								'(100 . "AcDbPolyline")
								(cons 90 (length pts))
								'(070 . 1));end list
							(list (cons 38 (caddar lst)))
							(reverse pts)
							(list (assoc 210 enx)))))))));end defun
(defun numinc:gettextbox (enx off / b h j l m n o p r w)
	(if
		(setq l
			(cond
				((= "TEXT" (cdr (assoc 0 enx)))
					(setq b (cdr (assoc 10 enx))
						r (cdr (assoc 50 enx))
						l (textbox enx));end setq
					(list
						(list (- (caar l) off) (- (cadar l) off))
						(list (+ (caadr l) off) (- (cadar l) off))
						(list (+ (caadr l) off) (+ (cadadr l) off))
						(list (- (caar l) off) (+ (cadadr l) off))));end "("
				((= "MTEXT" (cdr (assoc 0 enx)))
					(setq n (cdr (assoc 210 enx))
						b (trans (cdr (assoc 10 enx)) 0 n)
						r (angle '(0.0 0.0 0.0) (trans (cdr (assoc 11 enx)) 0 n))
						w (cdr (assoc 42 enx))
						h (cdr (assoc 43 enx))
						j (cdr (assoc 71 enx))
						o (list
								(cond
									((member j '(2 5 8)) (/ w -2.0))
									((member j '(3 6 9)) (- w))
									(0.0));end cond
								(cond
									((member j '(1 2 3)) (- h))
									((member j '(4 5 6)) (/ h -2.0))
									(0.0))));end setq
					(list
						(list (- (car o) off) (- (cadr o) off))
						(list (+ (car o) w off) (- (cadr o) off))
						(list (+ (car o) w off) (+ (cadr o) h off))
						(list (- (car o) off) (+ (cadr o) h off))))));end setq
		((lambda (m)
			 (mapcar '(lambda (p) (mapcar '+ (mxv m p) b)) l));end lambda
			(list
				(list (cos r) (sin (- r)) 0.0)
				(list (sin r) (cos r) 0.0)
				'(0.0 0.0 1.0)))));end defun
(defun mxv (m v)
	(mapcar (function (lambda (r) (apply '+ (mapcar '* r v))))
		m));end defun
(defun numinc:acdoc nil
	(eval (list 'defun
					'numinc:acdoc
					'nil
					(vla-get-activedocument (vlax-get-acad-object))));end eval
	(numinc:acdoc)
);end defun