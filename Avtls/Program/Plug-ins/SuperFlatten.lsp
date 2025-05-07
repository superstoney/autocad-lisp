(vl-load-com)
;;SuperFlatten 2.0c 超级压缩：修正中文版CAD拍平三维对象的VIEW问题
(defun c:SuperFlatten (/ *error* *expans* *overkillans* *proxyans* *sbar actlayout applyprops attributestotext blkdef blknamelst blocks checkblock checkrename cnt commandexplode deleteblockproxies elevms elevps expblkcnt expblklst expblockmethod expm flatace flatarc flatcircle flatcoordinates flatdimension flatellipse flathatch flatleader flatline flatminsert flatmleader flatmline flatmtext flatpline flatpointobj flatpolyfacemesh flatrayorxline flatregion flatshape flatsolid flatspline flattable flattext flattolerance flattrace flatwipeoutorraster flatxref getblock getnestednames hpa i inoutlst istop layoutblk layouts locklst lst lstacadpat modblockscale mspace mspacecnt n name newname newnamelst notrenamedlst obj objname optans orig pksty pointlist pos prefixsuffix presufstr processlist proxylst proxyreport pt pt11 pt22 relocklayers renameans renameflag rotatetonormal round sf:getfields sf:makelwpolyline sf:symbolstring sf:traceobject slu ss ssafterent ssall sscol ssvlalist templayout testlst testnormal testznormal ucsflag ucsfol unlocklayers validitem validlst version views wmfflag wmfoutin zzerocoord zzeropoint)
  (defun *error* (msg / i)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*")
        (if blknamelst
          (princ "\n ** CANCELLED - UNDO RECOMMENED ** \n")
        )
      )
			(t nil)
      ;(T (princ (strcat "\n错误信息: " msg)))
    )
    (foreach x expblklst (vlax-put x 'Explodable acFalse))
    (RelockLayers locklst)
    (if hpa (setvar "hpassoc" hpa))
    (if slu (setvar "showlayerusage" slu))
    (if elevms (vlax-put *doc* 'ElevationModelSpace elevms))
    (if elevps (vlax-put *doc* 'ElevationPaperSpace elevps))
    (if expm (setvar "explmode" expm))
    (if pksty (setvar "pickstyle" pksty))
    (if
      (and
        templayout
        (not (vlax-erased-p templayout))
      )
      (vla-delete templayout)
    )
    (if ucsfol (setvar "ucsfollow" ucsfol))
    (if (= 0 (vlax-get *doc* 'ActiveSpace))
      (vlax-put *doc* 'ActiveSpace 1)
    )
		;返回平面
		(cond
			((null ssall))
			((> n 0)
				;(getpoint "\n回车返回平面")
				(command-s "view" "top")
				(vla-zoomwindow *acad* pt11 pt22)
			)
			(istop)
			(t (princ "如需返回平面，请运行屏幕旋转命令ZP。"))
		)
    (vla-EndUndoMark *doc*)
		;(setvar "nomutt" 0)
		(setvar "cmdecho" 1)
    (princ)
  )
  (defun WMFOutIn (obj / space tmp tmpwmf blkref space lay clr lt
										explst pts newobj UpperLeft ActiveXSS 2DPoints)
    (setq *doc* (vla-get-activedocument (vlax-get-acad-object))
			space (vlax-get (vla-get-ActiveLayout *doc*) 'Block)
    )
    (defun UpperLeft ( / scrn ang vsiz vcen pt d)
      (setq scrn (getvar "screensize")
				ang (angle (list (car scrn) 0.0 0.0) (list 0.0 (cadr scrn) 0.0))
				vsiz (/ (getvar "viewsize") 2.0)
				vcen (getvar "viewctr")
				;; View top middle point.
				pt (polar vcen (/ pi 2.0) vsiz)
				d (distance pt vcen)
				;; Do the triangle math. Get the distance from view center
				;; to upper left corner.
				d (/ d (sin (- pi ang)))
      )
      ;; Point at upper left of screen.
      (polar vcen ang d)
    ) ; end UpperLeft
    ;; Argument: vla-object
    ;; Returns: an ActiveX selection set object.
    (defun ActiveXSS (obj / ssobj i)
      (if (setq i (ValidItem sscol "tempss"))
        (vla-delete i)
      )
      (setq ssobj (vlax-invoke sscol 'Add "tempss"))
      (vlax-invoke ssobj 'AddItems (list obj))
      (vla-item sscol "tempss")
    ) ; end ActiveXSS
    ;; Remove every third element from flat list of 3D points.
    (defun 2DPoints (coord / lst)
      (repeat (/ (length coord) 3)
        (setq lst (cons (car coord) lst)
					lst (cons (cadr coord) lst)
					coord (cdddr coord)
        )
      )
      (reverse lst)
    )
    (if (= 1 (vlax-get *doc* 'ActiveSpace))
      (command-s "-view" "o" "T")
    )
    (if
      (and
        ;; get a temp filename
        (setq tmp (vl-filename-mktemp nil nil nil))
        (setq tmpwmf (strcat tmp ".WMF"))
      )
      (progn
        ;; WMFout
        (setq lay (vlax-get obj 'Layer)
					clr (vlax-get obj 'Color)
					lt (vlax-get obj 'Linetype)
        )
        ;; Requires 2005 or later for the zoom object command-s.
        (command-s "._zoom" "_object" (vlax-vla-object->ename obj) "")
        (vla-update obj)
        (vlax-invoke *doc* 'Export tmp "WMF" (ActiveXSS obj))
        ;; WMFin
        (if (not (vl-catch-all-error-p
									 (setq blkref (vl-catch-all-apply 'vlax-invoke
																	(list *doc* 'Import tmpwmf (UpperLeft) 2.0)))))
          (progn
            (princ "Flattening solid objects, please do not Cancel... ")
            (print)
            (setq name (vlax-get blkref 'Name))
            (setq explst (vlax-invoke blkref 'Explode))
            (vla-delete blkref)
            (command-s "._purge" "_blocks" name "_no")
            (foreach x explst
              ;; Convert heavy plines from WMFin to lines or lightweight plines.
              (setq pts (vlax-get x 'Coordinates))
              (vla-delete x)
              (if
                (or
                  ;; Convert two point object to a line.
                  (= 6 (length pts))
                  ;; Given a three point object and the first and third points
                  ;; are the same, convert to a line.
                  (and
                    (= 9 (length pts))
                    (equal (car pts) (nth 6 pts) 1e-8)
                    (equal (cadr pts) (nth 7 pts) 1e-8)
                  )
                )
                (setq newobj (vlax-invoke space 'AddLine
															 (list (car pts) (cadr pts) 0.0)
															 (list (nth 3 pts) (nth 4 pts) 0.0)
														 ))
                ;; Else convert to lwpline.
                (setq newobj (vlax-invoke space 'AddLightWeightPolyline (2DPoints pts)))
              )
              (vlax-put newobj 'Layer lay)
              (vlax-put newobj 'Color clr)
              (vlax-put newobj 'Linetype lt)
            )
            (vla-delete obj)
            (if
              (and
                (= 1 (vlax-get *doc* 'ActiveSpace)) ;; model space
                (tblobjname "view" "SFview")
              )
              (command-s "-view" "_restore" "SFview")
            )
          )
          ;; else
          (CommandExplode obj)
        )
        (vl-file-delete tmpwmf)
      )
    )
  )
  (defun TestNormal (obj / n)
    (if (= (type obj) 'VLA-object)
      (setq n (vlax-get obj 'Normal))
      (setq n (cdr (assoc 210 (entget obj))))
    )
    (or
      (equal n '(0.0 0.0 1.0) 1e-8)
      (equal n '(0.0 0.0 -1.0) 1e-8)
    )
  )
  (defun TestZNormal (obj / n mn mx newobj)
    (if (= (type obj) 'VLA-object)
      (setq n (vlax-get obj 'Normal))
      (setq n (cdr (assoc 210 (entget obj)))
				obj (vlax-ename->vla-object obj)
      )
    )
    (if (equal 0.0 (caddr n) 1e-8)
      (progn
        (vla-GetBoundingBox obj 'mn 'mx)
        (setq mn (ZZeroPoint (vlax-safearray->list mn))
					mx (ZZeroPoint (vlax-safearray->list mx))
        )
        (cond
          ((or
             ;; Back (0.0 1.0 0.0) or Right (1.0 0.0 0.0).
             (equal 1.0 (apply '+ n) 1e-8)
             ;; Includes Left (-1.0 0.0 0.0).
             (and
               (minusp (car n))
               (not (minusp (cadr n)))
             )
             ;; Includes Front (0.0 -1.0 0.0).
             (and
               (not (minusp (car n)))
               (minusp (cadr n))
             )
					 )
            (setq newobj (vlax-invoke (GetBlock) 'AddLine mn mx))
          )
          ;; Or use the other two corners of the bounding box.
          (T
            (setq newobj
              (vlax-invoke (GetBlock) 'AddLine
                (list (car mx) (cadr mn) 0.0)
                (list (car mn) (cadr mx) 0.0)
              )
            )
          )
        )
        (ApplyProps obj newobj)
        ;; Return T to condition calls.
        (setq renameflag T)
      ) ;progn
    ) ;if
  ) ;end
  ;Argument: a single 3D point list.
  (defun ZZeroPoint (lst)
    (list (car lst) (cadr lst) 0.0)
  )
  (defun ZZeroCoord (coord / lst)
    (repeat (/ (length coord) 3)
      (setq lst (cons (car coord) lst)
				lst (cons (cadr coord) lst)
				lst (cons 0.0 lst)
				coord (cdddr coord)
      )
    )
    (reverse lst)
  ) ;end
  (defun GetBlock ()
    (vlax-get (vla-get-ActiveLayout *doc*) 'Block)
  ) ;end
  (defun CheckRename (exval testval)
    (if (and renameans presufstr)
      (or
        (equal exval testval 1e-6)
        (setq renameflag T)
      )
    )
  )
  (defun ValidItem (collection item / res)
    (vl-catch-all-apply
      '(lambda ()
				 (setq res (vla-item collection item))))
    res
  )
  (defun PrefixSuffix (argstr / str StripSpaces)
    ;Remove leading and trailing spaces for snvalid check.
    (defun StripSpaces (str)
      (vl-string-right-trim " " (vl-string-left-trim " " str))
    )
    (setq str (getstring T (strcat "\nBlock name " argstr ": ")))
    (if (eq argstr "prefix")
      (setq str (vl-string-left-trim " " str))
      (setq str (vl-string-right-trim " " str))
    )
    (cond
      ((eq "" str)
        (princ "\nBlocks will not be renamed. ")
      )
      ((not (snvalid (StripSpaces str) 0))
        (while
          (and
            (not (eq "" str))
            (not
              (snvalid
                (setq str (StripSpaces (getstring T (strcat "\nInvalid " argstr ": ")))) 0
              )
            )
          )
        )
      )
    )
    (if (not (eq "" str))
      str
    )
  )
  (defun SF:MakeLWPolyline (ptlst width)
    (if
      (and
        (> (length ptlst) 1)
        (apply 'and ptlst)
      )
      (if (entmake
            (append
              (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
								(cons 90 (length ptlst))
								(cons 43 width)
              )
              (mapcar '(lambda (x) (cons 10 x)) ptlst)
            )
          )
        (progn
          (setq renameflag T)
          (vlax-ename->vla-object (entlast))
        )
      )
    )
  )
  (defun ApplyProps (old new)
    (if
      (and
        old
        new
        (not (vlax-erased-p old))
        (not (vlax-erased-p new))
      )
      (progn
        (mapcar '(lambda (x) (vlax-put new x (vlax-get old x)))
          '("Color" "Layer" "Linetype" "LinetypeScale" "Lineweight")
        )
        (vl-catch-all-apply
          '(lambda ()
						 (vlax-put new 'LinetypeGeneration (vlax-get old 'LinetypeGeneration))
					 )
        )
        (vla-delete old)
        (setq renameflag T)
      )
    )
  ) ;end
  ;; Returns a nested point list from a flat point list.
  (defun PointList (obj / coord lst)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((eq "AcDbPolyline" (vlax-get obj 'ObjectName))
        (repeat (/ (length coord) 2)
          (setq lst (cons (list (car coord) (cadr coord)) lst)
						coord (cddr coord)
          )
        )
      )
      (T
        (repeat (/ (length coord) 3)
          (setq lst (cons (list (car coord) (cadr coord) (caddr coord)) lst)
						coord (cdddr coord)
          )
        )
      )
    )
    (reverse lst)
  )
  (defun AttributesToText (attlst / n elst str obj res AlignMtext UCSAng)
    (defun AlignMtext (obj / align)
      (setq align (vlax-get obj 'Alignment))
      (cond
        ((<= 0 align 2) (1+ align))
        ((<= 3 align 5) 1)
        (T (- align 5))
      )
    )
    (defun UCSAng (ang)
      (angle
        (trans '(0 0 0) 0 1)
        (trans (polar '(0 0 0) ang 1) 0 1)
      )
    )
    (foreach attobj attlst
      (setq n (vlax-get attobj 'Normal))
      (setq elst (entget (vlax-vla-object->ename attobj)))
      (setq str (SF:GetFields attobj))
      (if
        (and
          (vlax-property-available-p attobj 'MTextAttribute)
          (= -1 (vlax-get attobj 'MTextAttribute))
        )
        ;; multiline attribute
        (progn
          (if
            (entmake
              (list
                '(0 . "MTEXT")
                '(100 . "AcDbEntity")
                '(100 . "AcDbMText")
                (cons 1 (vlax-get attobj 'TextString))
                ;(cons 1 str)
                (cons 7 (vlax-get attobj 'StyleName))
                (cons 8 (vlax-get attobj 'Layer))
                ;(cons 10 (vlax-get x 'InsertionPoint))
                (cons 10 (vlax-get attobj 'TextAlignmentPoint))
                ;; this is AttachmnetPoint property
                (cons 71 (AlignMtext attobj))
                (cons 40 (vlax-get attobj 'Height))
                (cons 50 (UCSAng (vlax-get attobj 'Rotation)))
                ;(cons 50 (vlax-get attobj 'Rotation))
                (cons 62 (vlax-get attobj 'Color))
                ;; Added in 2.3 12/10/2009 mtext width
                (cons 41 (vlax-get attobj 'MTextBoundaryWidth))
                ;(cons 210 (vlax-get attobj 'Normal))
                (assoc 410 elst)
              )
            ) ;make
            (progn
              (if (assoc 90 elst)
                (entmod
                  (append
                    (entget (entlast))
                    (vl-member-if '(lambda (x) (= 90 (car x))) elst)
                  )
                )
              )
              (setq obj (vlax-ename->vla-object (entlast)))
              (vlax-put obj 'Normal n)
              (setq res (cons (FlatMText obj) res))
            )
          )
        ) ; progn
        ;; standard text attribute
        (progn
          (if
            (entmake
              (list
                '(0 . "TEXT")
                (cons 1 (vlax-get attobj 'TextString))
                (cons 7 (vlax-get attobj 'StyleName))
                (cons 8 (vlax-get attobj 'Layer))
                (cons 10 (vlax-get attobj 'InsertionPoint))
                (cons 11 (vlax-get attobj 'TextAlignmentPoint))
                (cons 40 (vlax-get attobj 'Height))
                (cons 41 (vlax-get attobj 'ScaleFactor))
                (cons 50 (vlax-get attobj 'Rotation))
                (cons 51 (vlax-get attobj 'ObliqueAngle))
                (cons 62 (vlax-get attobj 'Color))
                ;(cons 210 (vlax-get attobj 'Normal))
                (cons 67 (cdr (assoc 67 elst)))
                (cons 71 (cdr (assoc 71 elst)))
                (cons 72 (cdr (assoc 72 elst)))
                (cons 73 (cdr (assoc 74 elst)))
                (assoc 410 elst)
              )
            ) ; make
            (progn
              (setq obj (vlax-ename->vla-object (entlast)))
              (vlax-put obj 'Normal n)
              (if (= 0 (vlax-get obj 'Alignment))
                (vlax-put obj 'InsertionPoint
                  (vlax-get attobj 'InsertionPoint)
                )
                (vlax-put obj 'TextAlignmentPoint
                  (vlax-get attobj 'TextAlignmentPoint)
                )
              )
              (setq res (cons (FlatText obj) res))
            )
          )
        )
      )
      ;; Preserve symbols and fields.
      (if (and str obj) (vlax-put obj 'TextString str))
    ) ;foreach
    res
  )
  (defun SF:SymbolString (obj / e typ str name String blocks)
    (defun String (ename / str lst)
      (setq str "")
      (setq lst
        (vl-remove-if-not
          '(lambda (x) (or (= 3 (car x)) (= 1 (car x)))) (entget ename)
        )
      )
      (if (and (< 1 (length lst)) (= 1 (caar lst)))
        (setq lst (cdr lst))
      )
      (foreach x lst
        (setq str (strcat str (cdr x)))
      )
    ) ; end String
    (if (= (type obj) 'VLA-OBJECT)
      (setq e (vlax-vla-object->ename obj))
      (progn
        (setq e obj)
        (setq obj (vlax-ename->vla-object obj))
      )
    )
    (setq typ (vlax-get obj 'ObjectName))
    (cond
      ((or
         (eq typ "AcDbMText")
         (eq typ "AcDbAttribute")
			 )
        (setq str (String e))
      )
      ((eq typ "AcDbMLeader")
        (setq str (cdr (assoc 304 (entget e))))
      )
      ((and
         (wcmatch typ "*Dimension*")
         (setq name (cdr (assoc 2 (entget e))))
         (wcmatch name "`*D*")
         (setq blocks
           (vla-get-blocks
             (vla-get-activedocument
               (vlax-get-acad-object)
             )
           )
         )
       )
				(vlax-for x (vla-item blocks name)
					(if (eq "AcDbMText" (vlax-get x 'ObjectName))
						(progn
							(setq str (String (vlax-vla-object->ename x)))
							(vlax-put x 'TextString str)
							(setq str (vlax-invoke x 'FieldCode))
						)
					)
				)
			)
    )
    str
  )
  (defun SF:GetFields (obj / srcdict srcdictename srcTEXTdict
												srcfieldename targdict targdictename
												fieldelst fielddict dicts actlay
												tempobj lockflag res)
    (cond
      ((or
         (= 0 (vlax-get obj 'HasExtensionDictionary))
         (not
           (vl-catch-all-error-p
             (vl-catch-all-apply 'vlax-invoke
               (list (vlax-invoke obj 'GetExtensionDictionary) 'Delete)
             )
           )
         )
       )
				(setq res (SF:SymbolString obj))
      )
      ((and
				 (= -1 (vlax-get obj 'HasExtensionDictionary))
				 (setq srcdict (vlax-invoke obj 'GetExtensionDictionary))
				 (setq srcdictename (vlax-vla-object->ename srcdict))
				 (setq srcTEXTdict (dictsearch srcdictename "ACAD_FIELD"))
				 (setq srcfieldename (cdr (assoc 360 srcTEXTdict)))
       )
        ;; Check for active layer locked.
        (setq actlay (vlax-get *doc* 'ActiveLayer))
        (if (= -1 (vlax-get actlay 'Lock))
          (progn
            (vlax-put actlay 'Lock 0)
            (setq lockflag T)
          )
        )
        (setq tempobj (vlax-invoke (GetBlock) 'AddMText '(0.0 0.0 0.0) 0.0 "xx"))
        (setq targdict (vlax-invoke tempobj 'GetExtensionDictionary)
					targdictename (vlax-vla-object->ename targdict)
					fieldelst (entget srcfieldename)
					;; not sure about the need for these
					fieldelst (vl-remove (assoc 5 fieldelst) fieldelst)
					fieldelst (vl-remove (assoc -1 fieldelst) fieldelst)
					fieldelst (vl-remove (assoc 102 fieldelst) fieldelst)
					fieldelst (vl-remove-if '(lambda (x) (= 330 (car x))) fieldelst)
        )
        (foreach x fieldelst
          (if (= 360 (car x))
            (progn
              (setq dicts (cons (cdr x) dicts))
            )
          )
        )
        ;; remove all 360s from fieldelst
        (setq fieldelst (vl-remove-if '(lambda (x) (= 360 (car x))) fieldelst))
        (foreach x (reverse dicts)
          (setq fieldelst (append fieldelst (list (cons 360 (entmakex (entget x))))))
        )
        (setq fielddict
          (dictadd targdictename "ACAD_FIELD"
            (entmakex
              '(
								 (0 . "DICTIONARY")
								 (100 . "AcDbDictionary")
								 (280 . 1)
								 (281 . 1)
							 )
            )
          )
        )
        (dictadd fielddict "TEXT"
          (entmakex fieldelst)
        )
        (vlax-put tempobj 'TextString (SF:SymbolString tempobj))
        (setq res (vlax-invoke tempobj 'FieldCode))
        (vla-delete tempobj)
        (if lockflag (vlax-put actlay 'Lock -1))
      )
      ;; This really isn't needed given first conditon, but leave it.
      (T (setq res (SF:SymbolString obj)))
    )
    res
  )
  (defun ModBlockScale (blk / xsf ysf zsf)
    (setq xsf (vlax-get blk 'XScaleFactor)
			ysf (vlax-get blk 'YScaleFactor)
			zsf (vlax-get blk 'ZScaleFactor)
    )
    (if
      (and
        (or
          (equal xsf ysf 1e-2)
          (equal (- xsf) ysf 1e-2)
        )
        (equal ysf zsf 1e-2)
      )
      (progn
        (vlax-put blk 'XScaleFactor (Round xsf 1e-2))
        (vlax-put blk 'YScaleFactor (Round ysf 1e-2))
        (vlax-put blk 'ZScaleFactor (Round zsf 1e-2))
        T
      )
    )
  )
  (defun ExpBlockMethod (blkref / ip blkdef flag lay attlst exlst)
    (setq blkdef (vla-item blocks (vlax-get blkref 'Name)))
    (if
      (or
        (not (vlax-property-available-p blkdef 'Explodable))
        (eq acTrue (vlax-get blkdef 'Explodable))
      )
      (setq flag T)
    )
    (cond
      ((TestNormal blkref)
        (setq ip (vlax-get blkref 'InsertionPoint))
        (CheckRename ip (ZZeroPoint ip))
        (vlax-put blkref 'InsertionPoint (ZZeroPoint ip))
        (setq attlst (vlax-invoke blkref 'GetAttributes))
        (foreach x attlst (FlatText x))
      )
      ((and flag (ModBlockScale blkref))
        (setq lay (vlax-get blkref 'Layer)
					attlst (vlax-invoke blkref 'GetAttributes)
					exlst (vlax-invoke blkref 'Explode)
        )
        (if exlst
          (progn
            (setq renameflag T)
            (setq expblkcnt (1+ expblkcnt))
            (AttributesToText attlst)
            (vla-delete blkref)
            (foreach x exlst
              (if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
                (vla-delete x)
              )
            )
            (setq exlst (vl-remove-if 'vlax-erased-p exlst))
            (foreach x exlst
              (if (eq "0" (vlax-get x 'Layer))
                (vlax-put x 'Layer lay)
              )
              (if (zerop (vlax-get x 'Color))
                (vlax-put x 'Color 256)
              )
            )
            (ProcessList exlst)
          )
          (progn
            (setq cnt (1+ cnt))
            (if (not (vl-position "AcDbBlockReference" notflatlst))
              (setq notflatlst (cons "AcDbBlockReference" notflatlst))
            )
          )
        )
      )
      (T (CommandExplode blkref))
    ) ;cond
  )
  (defun CommandExplode (obj / lay mark objname attlst name exlst)
    (setq mark (entlast)
			objname (vlax-get obj 'ObjectName)
    )
    (cond
      ((or
         (eq "AcDb3dSolid" objname)
         (eq "AcDbSurface" objname)
         (eq "AcDbHatch" objname)
         (eq "AcDbHelix" objname)
         (eq "AcDbZombieEntity" objname)
         (eq "AcDbPlaneSurface" objname)
			 )
        (command-s "._explode" (vlax-vla-object->ename obj))
        (if (not (eq mark (entlast)))
          (setq exlst (SSVLAList (ssget "_p")))
        )
      )
      ;; Added 1/11/2010.
      ((wcmatch objname "*Dimension*")
        (setq str (SF:SymbolString obj))
        (command-s "._explode" (vlax-vla-object->ename obj))
        (if
          (and
            (not (eq mark (entlast)))
            (setq exlst (SSVLAList (ssget "_p")))
          )
          (foreach x exlst
            (if (eq "AcDbMText" (vlax-get x 'ObjectName))
              (vlax-put x 'TextString str)
            )
          )
        )
      )
      ;; Added 1/12/2010.
      ((eq "AcDbMLeader" objname)
        (command-s "._explode" (vlax-vla-object->ename obj))
        (if
          (and
            (not (eq mark (entlast)))
            (setq exlst (SSVLAList (ssget "_p")))
          )
          (foreach x exlst
            (if (eq "AcDbSolid" (vlax-get x 'ObjectName))
              (progn
                (vla-delete x)
                (setq exlst (vl-remove x exlst))
              )
            )
          )
        )
      )
      ((eq "AcDbBlockReference" objname)
				(setq lay (vlax-get obj 'Layer)
					attlst (vlax-invoke obj 'GetAttributes)
				)
				(command-s "._explode" (vlax-vla-object->ename obj))
				;; Had some problems here with blocks which cannot be exploded.
				;; The following test seems to fix it.
				(if
					(and
						(not (eq mark (entlast)))
						(setq exlst (SSVLAList (ssget "_p")))
					)
					(progn
						(setq expblkcnt (1+ expblkcnt))
						(AttributesToText attlst)
						(foreach x exlst
							(if (eq "AcDbAttributeDefinition" (vlax-get x 'ObjectName))
								(vla-delete x)
							)
						)
						(setq exlst (vl-remove-if 'vlax-erased-p exlst))
						;If an exlpoded object is on layer 0, put it on the
						;layer of the exploded object. If its color is byBlock,
						;change color to byLayer.
						(foreach x exlst
							(if (eq "0" (vlax-get x 'Layer))
								(vlax-put x 'Layer lay)
							)
							(if (zerop (vlax-get x 'Color))
								(vlax-put x 'Color 256)
							)
						)
					)
				)
			)
    ) ;cond
    (cond
      (exlst
        (setq renameflag T)
        (ProcessList exlst)
      )
      ((not (vlax-erased-p obj))
        (setq cnt (1+ cnt))
        (if (eq "AcDbZombieEntity" objname)
          (setq objname "DbProxy")
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
  )
  (defun RotateToNormal (obj n)
    (if
      (and
        (not (equal 1.0 (caddr n) 1e-5))
        (not (equal -1.0 (caddr n) 1e-5))
      )
      (vlax-put obj 'Rotation
        (+ (vlax-get obj 'Rotation) (+ (* pi 0.5) (angle '(0 0) n)))
      )
    )
  ) ;end
  ;;; TRACE ;;;
  (defun SF:TraceObject (obj / typlst typ ZZeroList TracePline TraceACE
													TraceType1Pline TraceType23Pline)
    (defun ZZeroList (lst)
      (mapcar '(lambda (p) (list (car p) (cadr p) 0.0)) lst)
    )
    (defun TracePline (obj / param endparam anginc tparam pt blg
												ptlst delta inc arcparam flag)
      (setq param (vlax-curve-getStartParam obj)
				endparam (vlax-curve-getEndParam obj)
				anginc (* pi (/ 7.5 180.0))
      )
      (while (<= param endparam)
        (setq pt (vlax-curve-getPointAtParam obj param))
        (if (not (equal pt (car ptlst) 1e-12))
          (setq ptlst (cons pt ptlst))
        )
        (if
          (and
            (/= param endparam)
            (setq blg (abs (vlax-invoke obj 'GetBulge param)))
            (/= 0 blg)
          )
          (progn
            (setq delta (* 4 (atan blg)) ;included angle
							inc (/ 1.0 (1+ (fix (/ delta anginc))))
							arcparam (+ param inc)
            )
            (while (< arcparam (1+ param))
              (setq pt (vlax-curve-getPointAtParam obj arcparam)
								ptlst (cons pt ptlst)
								arcparam (+ inc arcparam)
              )
            )
          )
        )
        (setq param (1+ param))
      ) ;while
      (if (> (length ptlst) 1)
        (progn
          (setq ptlst (vl-remove nil ptlst))
          (ZZeroList (reverse ptlst))
        )
      )
    ) ;end
    (defun TraceACE (obj / startparam endparam anginc
											delta div inc pt ptlst)
      (setq startparam (vlax-curve-getStartParam obj)
				endparam (vlax-curve-getEndParam obj)
				anginc (* pi (/ 2.5 180.0))
      )
      (if (equal endparam (* pi 2) 1e-6)
        (setq delta endparam)
        (setq delta (abs (- endparam startparam)))
      )
      (setq div (1+ (fix (/ delta anginc)))
				inc (/ delta div)
      )
      (while
        (or
          (< startparam endparam)
          (equal startparam endparam 1e-12)
        )
        (setq pt (vlax-curve-getPointAtParam obj startparam)
					ptlst (cons pt ptlst)
					startparam (+ inc startparam)
        )
      )
      (ZZeroList (reverse ptlst))
    )
    (defun TraceType1Pline (obj / ptlst objlst lst)
      (setq ptlst (list (vlax-curve-getStartPoint obj))
				objlst (vlax-invoke obj 'Explode)
      )
      (foreach x objlst
        (setq lst (TraceACE x))
        (if (not (equal (car lst) (last ptlst) 1e-8))
          (setq lst (reverse lst))
        )
        (setq ptlst (append ptlst (cdr lst)))
        (vla-delete x)
      )
      (ZZeroList ptlst)
    )
    (defun TraceType23Pline (obj / objlst ptlst lastpt)
      (setq objlst (vlax-invoke obj 'Explode)
				lastpt (vlax-get (last objlst) 'EndPoint)
      )
      (foreach x objlst
        (setq ptlst (cons (vlax-get x 'StartPoint) ptlst))
        (vla-delete x)
      )
      (ZZeroList (reverse (cons lastpt ptlst)))
    )
    (setq typlst '("AcDb2dPolyline" "AcDbPolyline"
										"AcDbCircle" "AcDbArc" "AcDbEllipse")
    )
    (or
      (eq (type obj) 'VLA-OBJECT)
      (setq obj (vlax-ename->vla-object obj))
    )
    (setq typ (vlax-get obj 'ObjectName))
    (if (vl-position typ typlst)
      (cond
				((or (eq typ "AcDb2dPolyline") (eq typ "AcDbPolyline"))
					(cond
						((or
							 (not (vlax-property-available-p obj 'Type))
							 (= 0 (vlax-get obj 'Type))
						 )
							(TracePline obj)
						)
						((or (= 3 (vlax-get obj 'Type)) (= 2 (vlax-get obj 'Type)))
							(TraceType23Pline obj)
						)
						((= 1 (vlax-get obj 'Type))
							(TraceType1Pline obj)
						)
					)
				)
				((or (eq typ "AcDbCircle") (eq typ "AcDbArc") (eq typ "AcDbEllipse"))
					(TraceACE obj)
				)
      )
    )
  )
  (defun LstACADPAT ( / file line tmp lst )
    (setq file (open (findfile "acad.pat") "r"))
    (while (setq line (read-line file))
      (setq tmp (cons line tmp))
    )
    (close file)
    (setq tmp (reverse tmp))
    (setq lst (vl-remove-if-not
								'(lambda (string)
									 (if (eq (substr string 1 1) "*") string)) tmp))
    (mapcar
      '(lambda (string)
				 (substr string 2 (- (vl-string-search "," string) 1))) lst)
  ) ;end
  (defun UnlockLayers (*doc* / laylst)
    (vlax-for x (vla-get-Layers *doc*)
      ;filter out xref layers
      (if
        (and
          (not (vl-string-search "|" (vlax-get x 'Name)))
          (eq :vlax-true (vla-get-lock x))
        )
        (progn
          (setq laylst (cons x laylst))
          (vla-put-lock x :vlax-false)
        )
      )
    )
    laylst
  ) ;end
  (defun RelockLayers (lst)
    (foreach x lst
      (vl-catch-all-apply 'vla-put-lock (list x :vlax-true))
    )
  ) ;end
  (defun SSVLAList (ss / obj lst i)
    (setq i 0)
    (if ss
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
					lst (cons obj lst)
					i (1+ i)
        )
      )
    )
    (reverse lst)
  )
  (defun SSAfterEnt (ent / ss entlst)
    (and
      (setq ss (ssadd))
      (while (setq ent (entnext ent))
        (setq entlst (entget ent))
        (if
          (and
            (not (wcmatch (cdr (assoc 0 entlst)) "ATTRIB,VERTEX,SEQEND"))
            (eq (cdr (assoc 410 entlst)) (getvar "ctab"))
          )
          (ssadd ent ss)
				)
			)
		)
    (if (> (sslength ss) 0) ss)
  )
  (defun GetNestedNames (blkcol blkname / name namelst temp1 temp2)
    ;first nested level
    (vlax-for x (vla-item blkcol blkname)
      (if
        (and
          (= "AcDbBlockReference" (vlax-get x 'ObjectName))
          (not (vl-position (setq name (vlax-get x 'Name)) namelst))
        )
        (setq namelst (cons name namelst))
      )
    )
    ;nested deeper
    (setq temp1 namelst)
    (while temp1
      (foreach x temp1
        (vlax-for x (vla-item blkcol x)
          (if
            (and
              (= "AcDbBlockReference" (vlax-get x 'ObjectName))
              (not (vl-position (setq name (vlax-get x 'Name)) namelst))
            )
            (setq namelst (cons name namelst)
							temp2 (cons name temp2)
            )
          )
        )
      )
      (setq temp1 temp2 temp2 nil)
    )
    (reverse namelst)
  ) ;end
  ;; Joe Burke 2/23/03
  (defun Round (value to)
    (if (zerop to) value
      (* (atoi (rtos (/ (float value) to) 2 0)) to)
		)
	)
  (defun CheckBlock (blkname objtyp / flag)
    (setq objtyp (strcase objtyp))
    (vlax-for x (vla-item blocks blkname)
      (if (eq objtyp (strcase (vlax-get x 'ObjectName)))
        (setq flag T)
      )
    )
    flag
  )
  (defun DeleteBlockProxies (blkname / blkdef tempname org copylst tempblk)
    (setq blkdef (vla-item blocks blkname))
    (vlax-for x blkdef (setq copylst (cons x copylst)))
    (cond
      ((vl-every
				 '(lambda (x)
						(eq "AcDbZombieEntity" (vlax-get x 'ObjectName))
					)
         copylst
			 )
        (vlax-for x blocks
          (if (eq acFalse (vlax-get x 'IsXref))
            (vlax-for i x
              (if
                (and
                  (eq "AcDbBlockReference" (vlax-get i 'ObjectName))
                  (eq (strcase blkname) (strcase (vlax-get i 'Name)))
                )
                (vla-delete i)
              )
            )
          )
        )
        (if (vl-catch-all-error-p
              (vl-catch-all-apply 'vla-delete (list blkdef))
            )
          (setq proxyerror T)
        )
      )
      (T
        (if
          (and
            (setq tempname (strcat "%%%" blkname))
            (not (ValidItem blocks tempname))
            (setq org (vlax-get blkdef 'Origin))
            (setq tempblk (vlax-invoke blocks 'Add org tempname))
          )
          (progn
            (vlax-invoke *doc* 'CopyObjects (reverse copylst) tempblk)
            (vlax-for x blocks
              (if (eq acFalse (vlax-get x 'IsXref))
                (vlax-for i x
                  (if
                    (and
                      (eq "AcDbBlockReference" (vlax-get i 'ObjectName))
                      (eq (strcase blkname) (strcase (vlax-get i 'Name)))
                    )
                    (vlax-put i 'Name tempname)
                  )
                )
              )
            )
            (if
              (not
                (vl-catch-all-error-p
                  (vl-catch-all-apply 'vla-delete (list blkdef))
                )
              )
              (vlax-put tempblk 'Name blkname)
              (setq proxyerror T)
            )
          )
        ) ;if
      ) ;cond T
    ) ;cond
  ) ;end
  ;;;;;;;; START FLATTEN SUB-FUNCTIONS ;;;;;;;;;
  (defun FlatPointObj (obj / coord)
    (if (not (TestNormal obj))
      (vlax-put obj 'Normal '(0.0 0.0 1.0))
    )
    (setq coord (vlax-get obj 'Coordinates))
    (CheckRename coord (ZZeroPoint coord))
    (vlax-put obj 'Coordinates (ZZeroPoint coord))
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
  ) ;end
  (defun FlatLine (obj / stpt enpt)
    (if (not (TestNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq renameflag T)
      )
    )
    (setq stpt (vlax-get obj 'StartPoint))
    (CheckRename stpt (ZZeroPoint stpt))
    (vlax-put obj 'StartPoint (ZZeroPoint stpt))
    (setq enpt (vlax-get obj 'EndPoint))
    (CheckRename enpt (ZZeroPoint enpt))
    (vlax-put obj 'EndPoint (ZZeroPoint enpt))
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    ;; If flattening made the length very short, delete line.
    (if (equal 0.0 (vlax-get obj 'Length) 1e-6)
      (progn
        (vla-delete obj)
        (setq renameflag T)
      )
    )
  ) ;end
  ;; Revised 8/19/2007.
  (defun FlatMText (obj / ip apt ang ip1 ip2)
    (setq ip (vlax-get obj 'InsertionPoint))
    (CheckRename ip (ZZeroPoint ip))
    (if (TestNormal obj)
      (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
      (progn
        (setq apt (vlax-get obj 'AttachmentPoint))
        (vlax-put obj 'AttachmentPoint 1)
        (setq ip1 (vlax-get obj 'InsertionPoint))
        (vlax-put obj 'AttachmentPoint 2)
        (setq ip2 (vlax-get obj 'InsertionPoint))
        (setq ang (angle ip1 ip2))
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (vlax-put obj 'Rotation ang)
        (vlax-put obj 'AttachmentPoint apt)
        (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        (setq renameflag T)
      )
    )
  ) ;end
  ;; Revised 8/19/2007.
  (defun FlatText (obj / pt ip ap algn ang)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (if (TestNormal obj)
      (if (= 0 (vlax-get obj 'Alignment))
        (progn
          (setq ip (vlax-get obj 'InsertionPoint))
          (CheckRename ip (ZZeroPoint ip))
          (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        )
        (progn
          (setq ap (vlax-get obj 'TextAlignmentPoint))
          (CheckRename ap (ZZeroPoint ap))
          (vlax-put obj 'TextAlignmentPoint (ZZeroPoint ap))
        )
      )
      ;; If the text object has an odd normal.
      (progn
        (setq algn (vlax-get obj 'Alignment))
        (if (= 0 algn)
          (setq pt (vlax-get obj 'InsertionPoint))
          (setq pt (vlax-get obj 'TextAlignmentPoint))
        )
        ; Center alignment to get the angle.
        (vlax-put obj 'Alignment 1)
        (setq ang
          (angle
            (vlax-get obj 'InsertionPoint)
            (vlax-get obj 'TextAlignmentPoint)
          )
        )
        ; Restore previous alignment.
        (vlax-put obj 'Alignment algn)
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (if (= 0 algn)
          (vlax-put obj 'InsertionPoint (ZZeroPoint pt))
          (vlax-put obj 'TextAlignmentPoint (ZZeroPoint pt))
        )
        (vlax-put obj 'Rotation ang)
        (setq renameflag T)
      ) ;progn odd normal
    ) ;if
    ;; Revised to return the result object 9/3/2007.
    obj
  ) ;end
  ;; Convert a circle with odd normal to an ellipse or otherwise.
  (defun FlatCircle (obj / ratio cen pt rad newobj)
    (cond
      ((TestNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestZNormal obj))
      (T
        (setq ratio (abs (caddr (vlax-get obj 'Normal)))
					cen (ZZeroPoint (vlax-get obj 'Center))
					pt (ZZeroPoint (vlax-curve-getPointAtParam obj 0))
					rad (vlax-get obj 'Radius)
        )
        (cond
          ((equal ratio 0.0 1e-4)
            (FlatACE obj)
          )
          ((equal ratio 1.0 1e-4)
            (if (setq newobj (vlax-invoke (GetBlock) 'AddCircle cen rad))
              (ApplyProps obj newobj)
            )
          )
          (T
            (setq newobj (vlax-invoke (GetBlock)
													 'AddEllipse cen (mapcar '- cen pt) (abs ratio))
            )
            (ApplyProps obj newobj)
          )
        )
      )
    )
  )
  (defun FlatArc (obj / ratio cen pt stpt enpt pt rad newobj stparam
									 enparam flag oldang newang oldstang oldenang)
    ;; Added per comments above 12/2/2011.
    (setq oldang (vlax-get obj 'TotalAngle))
    (cond
      ((TestNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestZNormal obj))
      (T
        (setq ratio (caddr (vlax-get obj 'Normal))
					cen (ZZeroPoint (vlax-get obj 'Center))
					stpt (ZZeroPoint (vlax-get obj 'StartPoint))
					enpt (ZZeroPoint (vlax-get obj 'EndPoint))
					rad (vlax-get obj 'Radius)
        )
        (if (minusp ratio)
          (setq ratio (abs ratio) flag T)
        )
        (cond
          ((< ratio 1e-4)
            (FlatACE obj)
          )
          ((equal ratio 1.0 1e-4)
            (if
              (setq newobj (vlax-invoke (GetBlock)
														 'AddArc cen rad (angle cen stpt) (angle cen enpt))
              )
              (progn
                (setq newang (vlax-get newobj 'TotalAngle))
                (if (equal newang (- (* pi 2) oldang) 0.0174533)
                  (progn
                    (setq oldstang (vlax-get newobj 'StartAngle))
                    (setq oldenang (vlax-get newobj 'EndAngle))
                    (vlax-put newobj 'EndAngle oldstang)
                    (vlax-put newobj 'StartAngle oldenang)
                  )
                )
                (ApplyProps obj newobj)
              )
            )
          )
          (T
            (vlax-put obj 'StartAngle 0.0)
            (setq pt (ZZeroPoint (vlax-curve-getStartPoint obj)))
            (setq newobj (vlax-invoke (GetBlock)
													 'AddEllipse cen (mapcar '- cen pt) ratio)
            )
            ;; This idea from BreakMethod seems to do the trick.
            (setq pt (vlax-curve-getClosestPointTo newobj stpt)
							stparam (vlax-curve-getParamAtPoint newobj pt)
							pt (vlax-curve-getClosestPointTo newobj enpt)
							enparam (vlax-curve-getParamAtPoint newobj pt)
            )
            ;; If the ratio (last value of normal)
            ;; was negative which param goes where is reversed.
            (if flag
              (progn
                (vlax-put newobj 'StartParameter enparam)
                (vlax-put newobj 'EndParameter stparam)
              )
              (progn
                (vlax-put newobj 'StartParameter stparam)
                (vlax-put newobj 'EndParameter enparam)
              )
            )
            (ApplyProps obj newobj)
          )
        ) ;cond
      ) ;progn
    ) ;if
  ) ;end
  (defun FlatEllipse (obj / cen)
    (cond
      ((TestNormal obj)
        (setq cen (vlax-get obj 'Center))
        (CheckRename cen (ZZeroPoint cen))
        (vlax-put obj 'Center (ZZeroPoint cen))
      )
      ((TestZNormal obj))
      (T (FlatACE obj))
    )
  )
  (defun FlatACE (obj / ptlst newobj objname)
    (setq ptlst (SF:TraceObject obj))
    (if (setq newobj (SF:MakeLWpolyline ptlst 0.0))
      (ApplyProps obj newobj)
      (progn
        (setq objname (vlax-get obj 'ObjectName)
					cnt (1+ cnt)
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
  )
  (defun FlatPline (obj / width ptlst newobj)
    (cond
      ((TestNormal obj)
        (CheckRename (vlax-get obj 'Elevation) 0)
        (vlax-put obj 'Elevation 0.0)
        (CheckRename (vlax-get obj 'Thickness) 0)
        (vlax-put obj 'Thickness 0.0)
      )
      ((TestZNormal obj))
      (T
        (if
          (vl-catch-all-error-p
            (setq width
              (vl-catch-all-apply 'vlax-get (list obj 'ConstantWidth))
            )
          )
          (setq width 0.0)
        )
        (setq ptlst (SF:TraceObject obj))
        (if (setq newobj (SF:MakeLWpolyline ptlst width))
          (ApplyProps obj newobj)
        )
      )
    )
  )
  (defun FlatPolyFaceMesh (obj / mark)
    (if (/= 1 (vlax-get obj 'NumberOfFaces))
      (FlatCoordinates obj)
      (progn
        (setq mark (entlast))
        (command-s "._explode" (vlax-vla-object->ename obj))
        (if (not (eq mark (entlast)))
          (FlatCoordinates (vlax-ename->vla-object (entlast)))
        )
      )
    )
  )
  (defun FlatMLeader (obj / mn mx n1 n2)
    (vla-GetBoundingBox obj 'mn 'mx)
    (setq n1 (caddr (vlax-safearray->list mn)))
    (setq n2 (caddr (vlax-safearray->list mx)))
    (cond
      ;; Doesn't need to be flattened.
      ((and
         (equal 0 n1 1e-8)
         (equal 0 n2 1e-8)
       )
      )
      ;; Move if it's parallel to WCS.
      ((equal n1 n2 1e-8)
        (vlax-invoke obj 'Move (list 0.0 0.0 n1) '(0.0 0.0 0.0))
      )
      (T (CommandExplode obj))
    )
  )
  (defun FlatLeader (obj / coord zlst n)
    (setq coord (vlax-get obj 'Coordinates))
    (repeat (/ (length coord) 3)
      (setq zlst (cons (caddr coord) zlst)
				coord (cdddr coord)
      )
    )
    (setq n (car zlst))
    (cond
      ((vl-every '(lambda (z) (equal 0.0 z 1e-6)) zlst))
      ((vl-every '(lambda (z) (equal n z 1e-6)) (cdr zlst))
        (vlax-invoke obj 'Move (list 0.0 0.0 n) '(0.0 0.0 0.0))
      )
      (T
        (FlatCoordinates obj)
      )
    )
  )
  (defun FlatCoordinates (obj / coord objname)
    (setq coord (vlax-get obj 'Coordinates))
    (if
      (vl-catch-all-error-p
        (vl-catch-all-apply
          '(lambda () (vlax-put obj 'Coordinates (ZZeroCoord coord)))
        )
      )
      (progn
        (setq cnt (1+ cnt)
					objname (vlax-get obj 'ObjectName)
        )
        (if (not (vl-position objname notflatlst))
          (setq notflatlst (cons objname notflatlst))
        )
      )
    )
    (CheckRename (vlax-get obj 'Coordinates) coord)
  )
  (defun FlatSpline (obj / ctrlpts testpts kts)
    (setq ctrlpts (vlax-get obj 'ControlPoints)
			testpts (ZZeroCoord ctrlpts)
    )
    ;; Revised 8/17/2007 - bug fix.
    (if
      (or
        (eq acFalse (vlax-get obj 'IsPlanar))
        (not (equal ctrlpts testpts 1e-8))
      )
      (progn
        (setq kts (vlax-get obj 'Knots))
        (vlax-put obj 'ControlPoints testpts)
        (vlax-put obj 'Knots kts)
        (setq renameflag T)
      )
    )
  )
  (defun FlatDimension (obj / z pt proplst e elst dxf13 dxf14)
    (if (TestNormal obj)
      (progn
        ;; Added 1/8/2010.
        (command-s "._dimdisassociate" (vlax-vla-object->ename obj) "")
        (setq z (caddr (vlax-get obj 'TextPosition)))
        (CheckRename z 0)
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
        ;; Revised 9/22/2007. Condensed if statements.
        (setq proplst '("ExtLine1Point" "ExtLine2Point" "ExtLine1StartPoint"
												 "ExtLine2StartPoint" "ExtLine1EndPoint" "ExtLine2EndPoint")
        )
        (foreach p proplst
          (if (vlax-property-available-p obj p)
            (progn
              (setq pt (vlax-get obj p))
              (CheckRename pt (ZZeroPoint pt))
              (vlax-put obj p (ZZeroPoint pt))
            )
          )
        )
        ;; Revised 1/2/2010. Ensure points in the dimension are at Z zero.
        (setq e (vlax-vla-object->ename obj))
        (setq elst (entget e))
        (if (setq dxf13 (assoc 13 elst))
          (progn
            (setq dxf13 (list 13 (cadr dxf13) (caddr dxf13) 0.0))
            (entmod (subst dxf13 (assoc 13 elst) elst))
            (setq elst (entget e))
          )
        )
        (if (setq dxf14 (assoc 14 elst))
          (progn
            (setq dxf14 (list 14 (cadr dxf14) (caddr dxf14) 0.0))
            (entmod (subst dxf14 (assoc 14 elst) elst))
          )
        )
      )
      (CommandExplode obj)
    )
  )
  ;; Change the normal first and then the IP.
  (defun FlatXref (obj / ip nrml)
    (setq ip (vlax-get obj 'InsertionPoint)
			nrml (vlax-get obj 'Normal)
    )
    (if (not (TestNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (RotateToNormal obj nrml)
      )
    )
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end
  (defun FlatTolerance (obj / ip nrml)
    (setq ip (vlax-get obj 'InsertionPoint)
			nrml (vlax-get obj 'Normal)
    )
    (if (not (TestNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  )
  (defun FlatMInsert (obj / ip attlst colcnt rowcnt colspc rowspc collst
											 rowlst xfac yfac inspt rot bname blknum index
											 inslst attlst txtlst vlaip newip nrml newblk
											 newobj blklst)
    (if (TestNormal obj)
      (progn
        (setq ip (vlax-get obj 'InsertionPoint))
        (CheckRename ip (ZZeroPoint ip))
        (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
        (setq attlst (vlax-invoke obj 'GetAttributes))
        (foreach x attlst
          (CheckRename (vlax-get x 'Thickness) 0)
          (vlax-put x 'Thickness 0.0)
          ;; FlatText checks for rename.
          (FlatText x)
        )
      )
      (progn
        (setq colcnt (vlax-get obj 'Columns)
					rowcnt (vlax-get obj 'Rows)
        )
        (if (or (> colcnt 1) (> rowcnt 1))
          (progn
            (setq bname (vlax-get obj 'Name)
							lay (vlax-get obj 'Layer)
							xfac (vlax-get obj 'XScaleFactor)
							yfac (vlax-get obj 'YScaleFactor)
							rot (vlax-get obj 'Rotation)
							colspc (vlax-get obj 'ColumnSpacing)
							rowspc (vlax-get obj 'RowSpacing)
							nrml (vlax-get obj 'Normal)
							blknum (* rowcnt colcnt)
            )
            (if (setq attlst (vlax-invoke obj 'GetAttributes))
              (setq txtlst (AttributesToText attlst))
            )
            (setq inspt (cdr (assoc 10 (entget (vlax-vla-object->ename obj))))
							vlaip (ZZeroPoint (vlax-get obj 'InsertionPoint))
            )
            (setq index 1)
            (repeat (1- colcnt)
              (setq collst (cons (polar inspt rot (* index colspc)) collst))
              (setq index (1+ index))
            )
            (setq collst (cons inspt collst))
            (setq index 1)
            (foreach x collst
              (progn
                (repeat (1- rowcnt)
                  (progn
                    (setq rowlst (cons (polar x (+ rot (* pi 0.5)) (* index rowspc)) rowlst))
                    (setq index (1+ index))
                  )
                )
                (setq index 1)
              )
            )
            (foreach x rowlst (setq inslst (cons x inslst)))
            (foreach x collst (setq inslst (cons x inslst)))
            (setq index 0)
            (repeat blknum
              (entmake (list '(0 . "INSERT")
												 '(100 . "AcDbEntity")
												 (cons 8 lay)
												 '(100 . "AcDbBlockReference")
												 (cons 2 bname)
												 (cons 10 (nth index inslst))
												 (cons 41 xfac)
												 (cons 42 yfac)
												 (cons 50 rot)
												 (cons 210 nrml)
                       )
              )
              (setq newblk (vlax-ename->vla-object (entlast))
								blklst (cons newblk blklst)
								newip (ZZeroPoint (vlax-get newblk 'InsertionPoint))
              )
              (foreach i txtlst
                (setq newobj (vlax-invoke i 'Copy))
                (vlax-invoke newobj 'Move vlaip newip)
              )
              (setq index (1+ index))
            ) ;repeat
            ;; Delete the minsert object.
            (vla-delete obj)
            ;; Delete the text objects initially created.
            (mapcar 'vla-delete txtlst)
            (setq renameflag T)
            (ProcessList blklst)
          ) ;progn
        ) ;progn
      ) ;if
    ) ;if
  ) ;end
  (defun FlatHatch (obj / rtd patname mark ss sset newobj
										 mn mx scale GetHatchScale)
    ;radians to degrees
    (defun rtd (radians)
			(/ (* radians 180.0) pi)
    )
    (defun GetHatchScale (obj / stdoffsetlst patname elst scl ang
													 xoff yoff yspacing offset)
      (setq stdoffsetlst
        '(("AR-SAND" 1.567 39.8018)
					 ("AR-CONC" -5.89789472 -149.807)
					 ("ANSI31" 0.125 3.175)
					 ("ANSI32" 0.375 9.525)
					 ("ANSI33" 0.25 6.35)
					 ("ANSI34" 0.75 19.05)
					 ("ANSI35" 0.25 6.35)
					 ("ANSI36" 0.125 3.175)
					 ("ANSI37" 0.125 3.175)
					 ("ANSI38" 0.125 3.175))
      )
      (setq patname (vlax-get obj 'PatternName))
      (if (vl-position patname (mapcar 'car stdoffsetlst))
        (progn
          (setq
            scl (vlax-get obj 'PatternScale)
            elst (entget (vlax-vla-object->ename obj))
            ang (cdr (assoc 53 elst))
            xoff (cdr (assoc 45 elst))
            yoff (cdr (assoc 46 elst))
            yspacing (/ (- (* yoff (cos ang)) (* xoff (sin ang))) scl)
          )
          (if (zerop (getvar "measurement"))
            (setq offset (car (cdr (assoc patname stdoffsetlst))))
            (setq offset (cadr (cdr (assoc patname stdoffsetlst))))
          )
          (abs (* scl (/ yspacing offset)))
        )
      )
    ) ;end
    (cond
      ((TestNormal obj)
        (CheckRename (vlax-get obj 'Elevation) 0)
        (vlax-put obj 'Elevation 0.0)
      )
      ((TestZNormal obj))
      ;; Added 7/21/2007.
      ;; A gradient hatch can be changed to a solid.
      ((and
         (vlax-property-available-p obj 'HatchObjectType)
         (= 1 (vlax-get obj 'HatchObjectType))
			 )
        (vlax-put obj 'HatchObjectType 0)
        (ProcessList (list obj))
      )
      ((and
         ;; Recreate boundary introduced at 2006.
         (>= (atof (getvar "AcadVer")) 16.2)
         (or patlst (setq patlst (LstACADPAT)))
         (setq patname (vlax-get obj 'PatternName))
         (vl-position patname patlst)
         (or
           (if (eq "SOLID" patname) (setq scale 1.0))
           (setq scale (GetHatchScale obj))
         )
         ;; Avoid the hatch boundary associativity removed message.
         (not (vlax-put obj 'AssociativeHatch 0))
         (setq mark (entlast))
         (not (command-s "._hatchedit" (vlax-vla-object->ename obj) "_b" "_p" "_n"))
         ;; Selection set of the boundary object(s).
         (setq sset (SSAfterEnt mark))
         (not (command-s "._zoom" "_object" sset ""))
         (if hpa (setvar "hpassoc" 0))
         (not (command-s "._hatch" patname scale
                (rtd (vlax-get obj 'PatternAngle)) "_s" sset ""
              )
         )
         ;; Restore previous zoom.
         (not (command-s "._zoom" "_previous"))
         ;; Delete boundary objects here rather than later.
         (if sset
           (mapcar 'vla-delete (SSVLAList sset))
         )
         (setq newobj (vlax-ename->vla-object (entlast)))
         (eq "AcDbHatch" (vlax-get newobj 'ObjectName))
         ;; Updates the hatch.
         (not (vl-catch-all-error-p
								(vl-catch-all-apply 'vlax-invoke
									(list newobj 'Evaluate))))
       ) ;and
        (vlax-put newobj 'HatchStyle (vlax-get obj 'HatchStyle))
        ;; Should not be needed.
        ;(vlax-put newobj 'AssociativeHatch 0)
        (ApplyProps obj newobj)
      )
      (T (CommandExplode obj))
    ) ;cond
  ) ;end
  (defun FlatSolid (obj / coord)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((TestNormal obj)
        (CheckRename coord (ZZeroCoord coord))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
      )
      ((TestZNormal obj))
      (T
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
        (setq renameflag T)
      )
    )
  )
  (defun FlatTrace (obj / coord)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq coord (vlax-get obj 'Coordinates))
    (cond
      ((TestNormal obj)
        (CheckRename coord (ZZeroCoord coord))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
      )
      (T
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (vlax-put obj 'Coordinates (ZZeroCoord coord))
        (setq renameflag T)
      )
    )
  ) ;end
  (defun FlatShape (obj / ip nrml)
    (CheckRename (vlax-get obj 'Thickness) 0)
    (vlax-put obj 'Thickness 0.0)
    (setq ip (vlax-get obj 'InsertionPoint)
			nrml (vlax-get obj 'Normal)
    )
    (if (not (TestNormal obj))
      (progn
        (vlax-put obj 'Normal '(0.0 0.0 1.0))
        (RotateToNormal obj nrml)
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
  ) ;end
  (defun FlatRayOrXline (obj / bp sp dv)
    (setq bp (vlax-get obj 'BasePoint))
    (CheckRename bp (ZZeroPoint bp))
    (vlax-put obj 'BasePoint (ZZeroPoint bp))
    (setq sp (vlax-get obj 'SecondPoint))
    (CheckRename sp (ZZeroPoint sp))
    (vlax-put obj 'SecondPoint (ZZeroPoint sp))
    (setq dv (vlax-get obj 'DirectionVector))
    (CheckRename dv (ZZeroPoint dv))
    (vlax-put obj 'DirectionVector (ZZeroPoint dv))
  )
  (defun FlatWipeoutOrRaster (obj / org)
    (vlax-put obj 'Rotation (vlax-get obj 'Rotation))
    (setq org (vlax-get obj 'Origin))
    (CheckRename org (ZZeroPoint org))
    (vlax-put obj 'Origin (ZZeroPoint org))
  )
  (defun FlatMline (obj / ename elst mark lst ptlst pts z line)
    (setq ename (vlax-vla-object->ename obj))
    (cond
      ((TestZNormal ename))
      ;; Flatten mline with an odd normal.
      ((not (TestNormal ename))
        (setq elst (entget ename))
        (entmod (subst (cons 210 '(0.0 0.0 1.0)) (assoc 210 elst) elst))
        ;; This is needed to flatten, though not at Z zero. Strange.
        (vlax-put obj 'Coordinates (ZZeroCoord (vlax-get obj 'Coordinates)))
        ;; All the Z values should be the same at this point.
        (setq z (caddr (vlax-get obj 'Coordinates)))
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
        (setq renameflag T)
      )
      (T
        (setq z (caddr (vlax-get obj 'Coordinates)))
        (CheckRename z 0)
        (if (not (zerop z))
          (vlax-invoke obj 'Move (list 0.0 0.0 z) '(0.0 0.0 0.0))
        )
      )
    )
  )
  (defun FlatTable (obj / ename elst nrml ip dir)
    (setq ename (vlax-vla-object->ename obj)
			elst (entget ename)
			nrml (cdr (assoc 210 elst))
			;The original ip for case where the normal is modified.
			ip (vlax-get obj 'InsertionPoint)
			dir (vlax-get obj 'Direction)
    )
    (if
      (not
        (or
          ;; removed fuzz 6/6/2007
          (equal nrml '(0.0 0.0 1.0))
          (equal nrml '(0.0 0.0 -1.0))
        )
      )
      (progn
        (entmod (subst (cons 210 '(0.0 0.0 1.0)) (assoc 210 elst) elst))
        (setq renameflag T)
      )
    )
    (CheckRename ip (ZZeroPoint ip))
    (vlax-put obj 'InsertionPoint (ZZeroPoint ip))
    (CheckRename dir (ZZeroPoint dir))
    ;; Fix case when direction Z value is like this (0.569751 0.0 0.821818).
    (vlax-put obj 'Direction (ZZeroPoint dir))
    (vlax-invoke obj 'RecomputeTableBlock acTrue)
  )
  (defun FlatRegion (obj / lst)
    (cond
      ((TestZNormal obj))
      (T
				(if
					(not (vl-catch-all-error-p
								 (setq lst (vl-catch-all-apply 'vlax-invoke (list obj 'Explode)))))
          (progn
            (vla-delete obj)
            (setq renameflag T)
            (ProcessList lst)
          )
          ;Else send object to WMFOutIn if 2005 or later.
          (if (>= version 16.2)
            (WMFOutIn obj)
            ;; Else count as not flattened.
            (progn
              (setq cnt (1+ cnt))
              (if (not (vl-position "AcDbRegion" notflatlst))
                (setq notflatlst (cons "AcDbRegion" notflatlst))
              )
            )
          )
        )
      )
    )
  )
  (defun ProcessList (lst / objname)
    (if (or inoutlst WMFflag)
      (princ
        (strcat
					"\r正在压缩复杂对象, 请勿取消... "
          (setq *sbar (Spinbar *sbar))
					"\t"
				)
      )
      (princ
        (strcat
					"\r正在压缩选择集... "
          (setq *sbar (Spinbar *sbar))
					"\t"
				)
      )
    )
    (foreach x lst
      (if (not (vlax-erased-p x))
        (progn
          (setq objname (vlax-get x 'ObjectName))
          (cond
            ((eq "AcDbLine" objname)
              (if (< (vlax-get x 'Length) 1e-6)
                (vla-delete x)
                (FlatLine x)
              )
            )
            ((eq "AcDbCircle" objname)
              (if (< (vlax-get x 'Radius) 1e-6)
                (vla-delete x)
                (FlatCircle x)
              )
            )
            ((eq "AcDbArc" objname)
              (if
                (or
                  (< (vlax-get x 'TotalAngle) 1e-6)
                  (< (vlax-get x 'Radius) 1e-6)
                )
                (vla-delete x)
                (FlatArc x)
              )
            )
            ((eq "AcDbEllipse" objname)
              (if
                (and
                  (< (vlax-get x 'MajorRadius) 1e-6)
                  (< (vlax-get x 'MinorRadius) 1e-6)
                )
                (vla-delete x)
                (FlatEllipse x)
              )
            )
            ((or
							 (eq "AcDbPolyline" objname)
							 (eq "AcDb2dPolyline" objname)
						 )
              (if (< (vlax-curve-getDistAtParam x (vlax-curve-getEndParam x)) 1e-6)
                (vla-delete x)
                (FlatPLine x)
              )
            )
            ((eq "AcDbSpline" objname)
              (if (< (vlax-curve-getEndParam x) 1e-6)
                (vla-delete x)
                (FlatSpline x)
              )
            )
            ;; Added 9/11/2007.
            ((eq "AcDbMLeader" objname)
              (FlatMLeader x)
            )
            ;; Added 8/29/2007.
            ((eq "AcDbLeader" objname)
              (FlatLeader x)
            )
            ((or
							 (eq "AcDb3dPolyline" objname)
							 (eq "AcDbFace" objname)
							 (eq "AcDbPolygonMesh" objname)
						 )
              (FlatCoordinates x)
            )
            ((eq "AcDbPolyFaceMesh" objname)
              (FlatPolyFaceMesh x)
            )
            ((eq "AcDbPoint" objname)
              (FlatPointObj x)
            )
            ((and
							 (eq "AcDbBlockReference" objname)
							 (vlax-property-available-p x 'Path)
						 )
              (FlatXref x)
            )
            ((eq "AcDbBlockReference" objname)
              (vlax-for y (vla-item blocks (vlax-get x 'Name))
                (setq templst (cons y templst))
              )
              (if
                (and
                  (vl-every '(lambda (z)
															 (eq "AcDbBlockReference" (vlax-get z 'ObjectName)))
										templst)
                  (ModBlockScale x)
                )
                (progn
                  (setq lay (vlax-get x 'Layer))
                  (setq templst (vlax-invoke x 'Explode))
                  (foreach i templst
                    (if (eq "0" (vlax-get i 'Layer))
                      (vlax-put i 'Layer lay)
                    )
                    (if (zerop (vlax-get x 'Color))
                      (vlax-put x 'Color 256)
                    )
                  )
                  (ProcessList templst)
                  (vla-delete x)
                )
                (ExpBlockMethod x)
              ) ;if
              (setq templst nil lay nil)
            )
            ((or
               (eq "AcDbText" objname)
               (eq "AcDbAttribute" objname)
               (eq "AcDbAttributeDefinition" objname)
						 )
              (FlatText x)
            )
            ((eq "AcDbMText" objname)
              (FlatMText x)
            )
            ((eq "AcDbTable" objname)
              (FlatTable x)
            )
            ((eq "AcDbHatch" objname)
              (FlatHatch x)
            )
            ((wcmatch objname "*Dimension*")
              (FlatDimension x)
            )
            ((eq "AcDbRegion" objname)
              (FlatRegion x)
            )
            ;; Keep in mind the 2008 "flatshot" command
            ;; for flattening 3D solids.
            ((or
               (eq "AcDb3dSolid" objname)
               (eq "AcDbSurface" objname)
               ;; Added 2/4/2011.
               (eq "AcDbPlaneSurface" objname)
						 )
              ;; Added 2/6/2011. Use WMF out in with 2005 or later.
              ;; Otherwise use the CommandExplode function.
              (if (>= version 16.1)
                (WMFOutIn x)
                (CommandExplode x)
              )
            )
            ;; Added 2/7/2011.
            ;; Body objects cannot be exploded.
            ((eq "AcDbBody" objname)
              (if (>= version 16.1)
                (WMFOutIn x)
                (progn
                  (setq cnt (1+ cnt))
                  (if (not (vl-position "AcDbBody" notflatlst))
                    (setq notflatlst (cons "AcDbBody" notflatlst))
                  )
                )
              )
            )
            ((eq "AcDbShape" objname)
              (if (>= version 16.1)
                (WMFOutIn x)
                (CommandExplode x)
              )
            )
            ;; Added 9/1/2007.
            ((eq "AcDbSolid" objname)
              (FlatSolid x)
            )
            ;; Added 9/1/2007.
            ((eq "AcDbTrace" objname)
              (FlatTrace x)
            )
            ((or
               (eq "AcDbRay" objname)
               (eq "AcDbXline" objname)
						 )
              (FlatRayOrXline x)
            )
            ;; Cannot be exploded.
            ((eq "AcDbMInsertBlock" objname)
              (FlatMInsert x)
            )
            ((eq "AcDbMline" objname)
              (FlatMline x)
            )
            ((or
               (eq "AcDbWipeout" objname)
               (eq "AcDbRasterImage" objname)
						 )
              (FlatWipeoutOrRaster x)
            )
            ((eq "AcDbFcf" objname)
              (FlatTolerance x)
            )
            ;; Removed support for AEC objects 8/25/2007.
            ((wcmatch (strcase objname) "AEC*")
              (setq cnt (1+ cnt))
              (if (not (vl-position "DbAecObject" notflatlst))
                (setq notflatlst (cons "DbAecObject" notflatlst))
              )
            )
            ;; Added 9/9/2007. Explodes to a spline.
            ((eq "AcDbHelix" objname)
              (CommandExplode x)
            )
            ;; Added proxy option 8/25/2007.
            ((eq "AcDbZombieEntity" objname)
              (CommandExplode x)
            )
            ;; Revised 2/4/2011. Both object types deleted.
            ((or
               (eq "AcDbLight" objname)
               (eq "AcDbCamera" objname)
               (eq "AcDbSun" objname)
             )
              (vla-delete x)
            )
            ;; Ignore viewports.
            ((eq "AcDbViewport" objname))
            ;; Any object not included above.
            (T
              (setq cnt (1+ cnt))
              (if (not (vl-position objname notflatlst))
                (setq notflatlst (cons objname notflatlst))
              )
            )
          ) ;cond
        ) ;progn
      ) ;if not erased
    ) ;foreach
  ) ;end ProcessList
	;;开始正式运行
	(setvar "CMDECHO" 0)
	;(setvar "nomutt" 1)
  (setq views (vla-get-Views *doc*))
  (if (setq i (ValidItem views "SFview"))
    (vl-catch-all-apply 'vla-delete (list i))
  )
  (vla-StartUndoMark *doc*)
  ;;切换至模型空间
  (if (= 0 (vlax-get *doc* 'ActiveSpace))
    (vlax-put *doc* 'ActiveSpace 1)
  )
	;;确认视口位置
	(setq
		pt (av:getscr4pt)
		pt11 (vlax-3d-point (car pt))
		pt22 (vlax-3d-point (caddr pt))
	)
	(setq istop (equal (getvar "VIEWDIR") (list 0.0 0.0 1.0)))
	(cond
		((setq ssall (ssget "X")))
		(t
			(princ "图中无图")
			(exit)
		)
	)
	(princ (strcat "\n请选择压缩对象<回车" (if istop "进入观察模式" "全选") ">"))
	(setq n 0)
	(while (progn
					 (or ss (setq ss (ssget (list (cons 410 (getvar "ctab"))))))
					 (null ss)
				 )
		(cond
			((and (= n 0) istop)
				(command-s "view" "front")
				;(vla-zoomwindow *acad* pt11 pt22)
				(command-s "3dorbit")
				(setq n (1+ n))
				(princ "\n当前状态直接回车，则全选图纸中的全部对象")
			)
			(t (setq ss ssall))
		)
	)
  (if ss
    (progn
      (if (>= version 16)
        (setq hpa (getvar "hpassoc"))
      )
      (if (>= version 16.2)
        (progn
          (setq slu (getvar "showlayerusage"))
          (setvar "showlayerusage" 0)
        )
      )
      (setq
				blocks (vla-get-Blocks *doc*)
				layouts (vla-get-Layouts *doc*)
				sscol (vla-get-SelectionSets *doc*)
				mspace (vla-get-ModelSpace *doc*)
				mspacecnt (vlax-get mspace 'Count)
				elevms (vlax-get *doc* 'ElevationModelSpace)
				elevps (vlax-get *doc* 'ElevationPaperSpace)
				version (atof (getvar "AcadVer"))
				locklst (UnlockLayers *doc*)
				expm (getvar "explmode")
				ucsfol (getvar "ucsfollow")
				pksty (getvar "pickstyle")
				cnt 0
				expblkcnt 0
      )
      (vlax-put *doc* 'ElevationModelSpace 0.0)
      (vlax-put *doc* 'ElevationPaperSpace 0.0)
      (setvar "ucsfollow" 0)
      (setvar "explmode" 1)
      (setvar "pickstyle" 0)
      (if (= 0 (getvar "worlducs"))
        (setq UCSflag T)
      )
      (command-s "._ucs" "_world")
      (setq lst (SSVLAList ss))
      (setq testlst lst)
      (while testlst
        (princ (strcat
								 "\r正在分析选择集... "
								 (setq *sbar (Spinbar *sbar))
							 )
				)
        (setq
					obj (car testlst)
					objname (vlax-get obj 'ObjectName)
        )
        (if
          (or
            (eq "AcDb3dSolid" objname)
            (eq "AcDbBody" objname)
            (eq "AcDbSurface" objname)
            (eq "AcDbPlaneSurface" objname)
          )
          (setq WMFflag T)
        )
        (if
          (and
            (eq "AcDbBlockReference" objname)
            (not (vlax-property-available-p obj 'Path))
          )
          (progn
            (setq name (vlax-get obj 'Name))
            (if (not (vl-position name blknamelst))
              (setq blknamelst (cons name blknamelst))
            )
            (foreach i (GetNestedNames blocks name)
              (if (not (vl-position i blknamelst))
                (setq blknamelst (cons i blknamelst))
              )
            )
          )
        )
        (if (eq "AcDbZombieEntity" objname)
          (setq proxylst (cons obj proxylst))
        )
        (setq testlst (cdr testlst))
      )
      (foreach x blknamelst
        (if (CheckBlock x "AcDbZombieEntity")
          (DeleteBlockProxies x)
        )
        (if (not WMFflag)
          (if
            (or
              (CheckBlock x "AcDb3dSolid")
              (CheckBlock x "AcDbBody")
              (CheckBlock x "AcDbSurface")
              (CheckBlock x "AcDbPlaneSurface")
            )
            (setq WMFflag T)
          )
        )
      )
      (if WMFflag
        (progn
          (command-s "-view" "s" "SFview")
          (if (>= version 17)
            (progn
              (command-s "-view" "E" "V" "SFview" "二维线框" "B" "SFview" "N" "" "")
              (command-s "-view" "R" "SFview")
            )
          )
        )
      )
      (cond
        ((not WMFflag))
        ((and
					 WMFflag
           (not (>= version 16.1))
         )
					(princ "\n  Flattening 3D solids using WMF out/in requires 2005 or later. ")
        )
        ((and
           WMFflag
           (>= version 16.1)
         )
					(princ "\n  Flattening 3D solids using WMF out/in. ")
        )
      )
      (or *expans* (setq *expans* "No"))
      (or *overkillans* (setq *overkillans* "No"))
      (or *proxyans* (setq *proxyans* "No"))
      (setq optans T)
      (cond
        ((and (< version 16.2) (not acet-ss-remove-dups))
          (while optans
            (if presufstr
              (princ (strcat "\n当前选项: Rename=" renameans "> " presufstr))
              (princ (strcat "\n当前选项: Rename=未指定"))
            )
            (princ (strcat "  Proxies=" *proxyans*))
            (initget "Rename Proxies")
            (setq optans
              (getkword "\nSuperFlatten options [Rename blocks/Proxies] < >: ")
            )
            (cond
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
              ((eq optans "Proxies")
                (initget "Yes No")
                (setq *proxyans*
                  (getkword "\nFlatten proxy objects? [Yes/No] <N>: ")
                )
                (if (not *proxyans*) (setq *proxyans* "No"))
              )
            )
          )
        )
        ((and (< version 16.2) acet-ss-remove-dups)
          (while optans
            (if presufstr
              (princ (strcat "\n当前选项: Rename=" renameans "> " presufstr))
              (princ (strcat "\n当前选项: Rename=未指定"))
            )
            (princ (strcat "  Overkill=" *overkillans*
										 "  Proxies=" *proxyans*
                   )
            )
            (if (not proxyreport)
              (progn
                (if proxylst
                  (princ (strcat "\n  " (itoa (length proxylst)) " proxies selected. "))
                  ;(princ "\n  No proxies selected. ")
                )
                (setq proxyreport T)
              )
            )
            (initget "Rename Overkill Proxies")
            (setq optans
              (getkword
                "\nSuperFlatten options [Rename blocks/Overkill/Proxies] < >: ")
            )
            (cond
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
              ((eq optans "Overkill")
                (initget "Yes No")
                (setq *overkillans*
                  (getkword "\nRun Overkill after flattening? [Yes/No] <N>: ")
                )
                (if (not *overkillans*) (setq *overkillans* "No"))
              )
              ((eq optans "Proxies")
                (initget "Yes No")
                (setq *proxyans*
                  (getkword "\nFlatten proxy objects? [Yes/No] <N>: ")
                )
                (if (not *proxyans*) (setq *proxyans* "No"))
              )
            )
          )
        )
        ((and (>= version 16.2) (not acet-ss-remove-dups))
          (while optans
            (if presufstr
              (princ (strcat "\n当前选项: Rename=" renameans "> " presufstr))
              (princ (strcat "\n当前选项: Rename=未指定"))
            )
            (princ (strcat "  Explodable=" *expans*
										 "  Proxies=" *proxyans*
                   )
            )
            (if (not proxyreport)
              (progn
                (if proxylst
                  (princ (strcat "\n  " (itoa (length proxylst)) " proxies selected. "))
                  ;(princ "\n  No proxies selected. ")
                )
                (setq proxyreport T)
              )
            )
            (initget "Rename Explodable Proxies")
            (setq optans
              (getkword
                "\nSuperFlatten options [Rename blocks/Explodable blocks/Proxies] < >: ")
            )
            (cond
              ((eq optans "Rename")
                (initget "Prefix Suffix")
                (setq renameans
                  (getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
                )
                (if (not renameans) (setq renameans "Suffix"))
                (cond
                  ((eq renameans "Prefix")
                    (setq presufstr (PrefixSuffix "prefix"))
                  )
                  ((eq renameans "Suffix")
                    (setq presufstr (PrefixSuffix "suffix"))
                  )
                )
              )
              ((eq optans "Explodable")
                (initget "Yes No")
                (setq *expans*
                  (getkword
                    "\nTemporarily set all blocks explodable? [Yes/No] <N>: ")
                )
                (if (not *expans*) (setq *expans* "No"))
              )
              ((eq optans "Proxies")
                (initget "Yes No")
                (setq *proxyans*
                  (getkword "\nFlatten proxy objects? [Yes/No] <N>: ")
                )
                (if (not *proxyans*) (setq *proxyans* "No"))
              )
            )
          )
        )
        ((and (>= version 16.2) acet-ss-remove-dups)
          (while optans
            (if presufstr
              (princ (strcat "\n当前选项: Rename=" renameans "> " presufstr))
              (princ (strcat "\n当前选项: Rename=未指定"))
            )
            (princ (strcat "  Explodable=" *expans*
										 "  Overkill=" *overkillans*
										 "  Proxies=" *proxyans*
                   )
            )
            (if (not proxyreport)
              (progn
                (if proxylst
                  (princ (strcat "\n  " (itoa (length proxylst)) " proxies selected. "))
                  ;(princ "\n  No proxies selected. ")
                )
                (setq proxyreport T)
              )
            )
            (initget "Rename Explodable Overkill Proxies")
            (setq optans (getkword "\n选项[Rename blocks/Explodable blocks/Overkill/Proxies]<默认回车>: "))
						(cond
							((eq optans "Rename")
								(initget "Prefix Suffix")
								(setq renameans
									(getkword "\nBlock name options: [Prefix/Suffix] <S>: ")
								)
								(if (not renameans) (setq renameans "Suffix"))
								(cond
									((eq renameans "Prefix")
										(setq presufstr (PrefixSuffix "prefix"))
									)
									((eq renameans "Suffix")
										(setq presufstr (PrefixSuffix "suffix"))
									)
								)
							)
							((eq optans "Explodable")
								(initget "Yes No")
								(setq *expans*
									(getkword
										"\nTemporarily set all blocks explodable? [Yes/No] <N>: ")
								)
								(if (not *expans*) (setq *expans* "No"))
							)
							((eq optans "Overkill")
								(initget "Yes No")
								(setq *overkillans*
									(getkword "\nRun Overkill after flattening? [Yes/No] <N>: ")
								)
								(if (not *overkillans*) (setq *overkillans* "No"))
							)
							((eq optans "Proxies")
								(initget "Yes No")
								(setq *proxyans*
									(getkword "\nFlatten proxy objects? [Yes/No] <N>: ")
								)
								(if (not *proxyans*) (setq *proxyans* "No"))
							)
						)
					)
				)
			)
			(if (eq "Yes" *expans*)
				(vlax-for x blocks
					(if
						(and
							(vlax-property-available-p x 'Explodable)
							(eq acFalse (vlax-get x 'Explodable))
						)
						(progn
							(setq expblklst (cons x expblklst))
							(vlax-put x 'Explodable acTrue)
						)
					)
				)
			)
			;; Remove proxy objects from the selection list if proxyans is No.
			(if (eq "No" *proxyans*)
				(foreach x proxylst
					(setq lst (vl-remove x lst))
				)
			)
			(ProcessList lst)
			(foreach x blknamelst
				(if (ValidItem blocks x)
					(setq validlst (cons x validlst))
				)
			)
			(setq blknamelst (reverse validlst))
			(if blknamelst
				(progn
					(vl-catch-all-apply '(lambda () (vla-delete (vla-item layouts "SuperFlatten layout"))))
					(setq actlayout (vlax-get *doc* 'ActiveLayout)
						templayout (vlax-invoke layouts 'Add  "SuperFlatten layout")
						layoutblk (vlax-get templayout 'Block)
					)
					(vlax-put *doc* 'ActiveLayout templayout)
					(foreach x blknamelst
						(setq blkdef (vla-item blocks x)
							inoutlst nil
							renameflag nil
						)
						(setq orig (vlax-get blkdef 'Origin))
						(CheckRename orig (ZZeroPoint orig))
						(vlax-put blkdef 'Origin (ZZeroPoint (vlax-get blkdef 'Origin)))
						(vlax-for i blkdef
							(or
								(eq "AcDbViewport" (vlax-get i 'ObjectName))
								(setq inoutlst (cons i inoutlst))
							)
						)
						(if inoutlst
							(progn
								;; Copy list to the layout block.
								(setq inoutlst (vlax-invoke *doc* 'CopyObjects inoutlst layoutblk))
								;; Empty the source block, except for viewports.
								(vlax-for i blkdef
									(or
										(eq "AcDbViewport" (vlax-get i 'ObjectName))
										(vl-catch-all-apply 'vla-delete (list i))
									)
								)
								;; Flatten objects in layout.
								(ProcessList inoutlst)
								(setq inoutlst nil)
								;; List the flattened objects, filter out viewports.
								(vlax-for i layoutblk
									(or
										(eq "AcDbViewport" (vlax-get i 'ObjectName))
										(setq inoutlst (cons i inoutlst))
									)
								)
								;; Copy the flattened objects in the layout back into
								;; the block definition and delete objects in the layout.
								(if inoutlst
									(progn
										(vlax-invoke *doc* 'CopyObjects inoutlst blkdef)
										(mapcar 'vla-delete inoutlst)
									)
								)
							)
						)
						(if
							(and
								;; Cannot rename anonymous blocks.
								(not (vl-string-search "*" x))
								renameflag
								renameans
								presufstr
							)
							(cond
								((and
									 (eq renameans "Prefix")
									 (setq newname (strcat presufstr x))
								 )
									;; Added existing block name check 8/9/2007.
									(if (ValidItem blocks newname)
										(setq notrenamedlst (cons x notrenamedlst))
										(progn
											(vlax-put blkdef 'Name newname)
											(setq newnamelst (cons newname newnamelst))
										)
									)
								)
								((and
									 (eq renameans "Suffix")
									 (setq newname (strcat x presufstr))
								 )
									;; Added existing block name check 8/9/2007.
									(if (ValidItem blocks newname)
										(setq notrenamedlst (cons x notrenamedlst))
										(progn
											(vlax-put blkdef 'Name newname)
											(setq newnamelst (cons newname newnamelst))
										)
									)
								)
							)
						)
					) ;foreach
					(vlax-put *doc* 'ActiveLayout actlayout)
					;templayout is deleted in the error handler.
				) ;progn
			) ;if blknamelst
			(if blknamelst
				(vla-regen *doc* acActiveViewport)
			)
			(if
				(and
					(eq "Yes" *overkillans*)
					(setq ss
						(cadr
							(acet-ss-remove-dups
								(ssget "_x" '((410 . "Model"))) 1e-6 nil)
						)
					)
				)
				(command-s "._erase" ss "")
			)
			(if (or newnamelst notrenamedlst)
				(textscr)
			)
			(if newnamelst
				(progn
					(princ "\nThe following blocks were renamed: ")
					(foreach x newnamelst
						(print x)
					)
				)
			)
			(if notrenamedlst
				(progn
					(princ "\nThe following blocks were not renamed due to existing block name conflict: ")
					(foreach x notrenamedlst
						(print x)
					)
				)
			)
			(if (> expblkcnt 0)
				(princ (strcat "\nNumber of blocks exploded: " (itoa expblkcnt)))
			)
			(princ
				(strcat "\n模型空间对象，之前"
					(itoa mspacecnt) "，之后" (itoa (vlax-get mspace 'Count)) " \n"
				)
			)
			(if (> cnt 0)
				(progn
					(princ
						(strcat "\nNumber of objects not processed or flattened: " (itoa cnt) " \n")
					)
					(if notflatlst
						(progn
							(princ "\nObject types not flattened: ")
							(foreach x notflatlst
								(setq pos (+ 3 (vl-string-search "Db" x)))
								(princ (strcat (substr x pos) " "))
							)
							(print)
						)
					)
				)
			)
			(if proxyerror (princ "\nA problem occurred with proxies inside blocks. "))
			(if UCSflag (command-s "._ucs" "_previous"))
			(if (setq i (ValidItem views "SFview"))
				(vl-catch-all-apply 'vla-delete (list i))
			)
			(if istop (getpoint "\n回车或ESC返回平面"))
		)
		(princ "\nNothing selected. ")
	)
	(*error* nil)
)
;(c:SuperFlatten)
