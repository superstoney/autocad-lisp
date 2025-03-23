(vl-load-com)
(Defun c:ZDGJ (/ ThisDrawing ss @k1 #k1 @k2 @k3 @k4 @k5 bili) 
  (defun *error* (msg)  ;出错处理程序
    (setvar "cmdecho" 1) ;_ 恢复cmdecho系统变量
    (princ "error: ")
    (princ msg) ;_ 打印错误信息
    (princ)
  )
  (setvar "cmdecho" 0)
  (setq ThisDrawing (vla-get-ActiveDocument (vlax-get-acad-object)))
  (while (not (setq @k1 (nentselp "\n 请选择源文字:"))))
  (if (/= @k1 nil) 
    (progn 
      (setq #k1 (vlax-ename->vla-object (car @k1)))
      (setq ss (entget (car @k1)))
      (setq @k3 (cdr (assoc 0 ss)))
      (setq @k4 (cdr (assoc 8 ss)))
      (setq @k5 (cdr (assoc 62 ss)))
      (if (= @k5 NIL) (setq @k5 7))
      (setq bili (/ (getvar "HPSCALE") 100)) ;获取当前的比例
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(70 . 0) '(6 . "Continuous") (cons 2 "字段图层") ) )
      (while 
        (and 
          (entmake 
            (list '(0 . "MTEXT") 
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbMText")
                  (cons 1 "xxx")
                  (cons 7 "STANDARD")
                  (cons 8 "字段图层")
                  (cons 10 (getpoint "\n选择插入点:"))
                  (cons 40 (* 400 bili))
                  (cons 62 @k5)
            )
          )
          (setq @k2 (vlax-ename->vla-object (entlast)))
        )
        (cond 
          ((= @k3 "TCH_TEXT")
           (vla-put-textstring 
             @k2
             (strcat "%<\\AcObjProp Object(%<\\_ObjId " 
                     (itoa (vla-get-objectid #k1))
                     ">%).Text>%"
             )
           )
          )

          ((= @k3 "TCH_DRAWINGNAME")
           (vla-put-textstring 
             @k2
             (strcat "%<\\AcObjProp Object(%<\\_ObjId " 
                     (itoa (vla-get-objectid #k1))
                     ">%).NameText>%"
             )
           )
          )

          ((= @k3 "TCH_MULTILEADER")
           (vla-put-textstring 
             @k2
             (strcat "%<\\AcObjProp Object(%<\\_ObjId " 
                     (itoa (vla-get-objectid #k1))
                     ">%).UpText>%"
             )
           )
          )
          
          ((= @k3 "TCH_ELEVATION")   
           (vla-put-textstring 
             @k2
             (strcat "%<\\AcObjProp Object(%<\\_ObjId " 
                     (itoa (vla-get-objectid #k1))
                     ">%).Text>%"
             )
           )
          )

          (t
           (vla-put-textstring 
             @k2
             (strcat "%<\\AcObjProp Object(%<\\_ObjId " 
                     (itoa (vla-get-objectid #k1))
                     ">%).TextString>%"
             )
           )
          )
        )
      )
    )
  )
  (princ)
)
