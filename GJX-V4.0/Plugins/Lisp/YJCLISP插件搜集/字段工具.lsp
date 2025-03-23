(vl-load-com)
(Defun c:ZDGJ (/ ThisDrawing ss @k1 #k1 @k2 @k3 @k4 @k5 bili) 
  (defun *error* (msg)  ;���������
    (setvar "cmdecho" 1) ;_ �ָ�cmdechoϵͳ����
    (princ "error: ")
    (princ msg) ;_ ��ӡ������Ϣ
    (princ)
  )
  (setvar "cmdecho" 0)
  (setq ThisDrawing (vla-get-ActiveDocument (vlax-get-acad-object)))
  (while (not (setq @k1 (nentselp "\n ��ѡ��Դ����:"))))
  (if (/= @k1 nil) 
    (progn 
      (setq #k1 (vlax-ename->vla-object (car @k1)))
      (setq ss (entget (car @k1)))
      (setq @k3 (cdr (assoc 0 ss)))
      (setq @k4 (cdr (assoc 8 ss)))
      (setq @k5 (cdr (assoc 62 ss)))
      (if (= @k5 NIL) (setq @k5 7))
      (setq bili (/ (getvar "HPSCALE") 100)) ;��ȡ��ǰ�ı���
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(70 . 0) '(6 . "Continuous") (cons 2 "�ֶ�ͼ��") ) )
      (while 
        (and 
          (entmake 
            (list '(0 . "MTEXT") 
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbMText")
                  (cons 1 "xxx")
                  (cons 7 "STANDARD")
                  (cons 8 "�ֶ�ͼ��")
                  (cons 10 (getpoint "\nѡ������:"))
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
