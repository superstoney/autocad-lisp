(defun c:CopyLayerNameToClipboard ()
;;��ȡͼ������д�������
  (vl-load-com)
  ;; ��ʾ�û�ѡ��һ������
  (if (setq sel (ssget))
    (progn
      ;; ��ȡѡ�еĵ�һ������
      (setq ent (vlax-ename->vla-object (ssname sel 0)))
      ;; ��ȡ�����ͼ������
      (setq layerName (vla-get-Layer ent))
      ;; ʹ��Windows API������ͼ�����Ƹ��Ƶ�������
      (setq clipdata (vlax-create-object "htmlfile"))
      (vlax-invoke-method (vlax-get-property (vlax-get-property clipdata 'parentWindow) 'clipboardData) 'setData "Text" layerName)
      (vlax-release-object clipdata)
      (princ (strcat "\nͼ������ " layerName " �Ѹ��Ƶ������塣"))
    )
    (princ "\nδѡ���κζ���")
  )
  (princ)
)

(defun c:listSlectLayersSorted (/ dxf p1 ss layers uniqueLayers sortedLayers)
    ;; ������������ȡDXF����ֵ
    (defun dxf (ent i) (cdr (assoc i (entget ent))))
    
    ;; �Զ���ȥ�غ������޸�����ȱʧ��
    (defun unique (lst / out)
        (while lst
            (if (not (member (car lst) out))
                (setq out (cons (car lst) out))
            )
            (setq lst (cdr lst))
        )
        (reverse out)
    )

    ;; �������߼�
    (if (and (setq ss (ssget))
             (setq p1 (getpoint "\n��ָ�������: "))
        )
        (progn
            ;; �ռ�����ͼ������
            (setq layers 
                (mapcar '(lambda (x) (dxf x 8)) 
                    (vl-remove-if-not 
                        '(lambda (x) (= 'ENAME (type x))) 
                        (mapcar 'cadr (ssnamex ss))
                    )
                )
            )
            
            ;; ȥ�ز��������������򣨲����ִ�Сд��
            (setq uniqueLayers (unique layers))
            (setq sortedLayers 
                (vl-sort uniqueLayers 
                    '(lambda (a b) 
                        (< (strcase a) (strcase b))
                    )
                )
            )
            
            ;; �������������
            (foreach layer sortedLayers
                (entmake 
                    (list 
                        '(0 . "TEXT")
                        (cons 1 layer)       ; ��������
                        (cons 8 layer)       ; ͼ������
                        (cons 10 p1)         ; ����㣨���½ǣ�
                        (cons 11 p1)         ; �����=�����
                        (cons 40 20)         ; ���ָ߶�
                        (cons 72 0)          ; ˮƽ�����
                        (cons 73 0)          ; ��ֱ���߶���
                        (cons 50 0)          ; ��ת�Ƕ�
                    )
                )
                (setq p1 (polar p1 (* 1.5 pi) 50)) ; ����50��λ
            )
        )
    )
    (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;�г�����ͼ������
(defun C:listAllLayersSorted (/ *acad* *doc* *lays* layerNames p1)
  (setq
    *acad* (vlax-get-acad-object)
    *doc* (vla-get-ActiveDocument *acad*)
    *lays* (vla-get-Layers *doc*)
    layerNames '()  ; ��ʼ��ͼ�������б�
  )

  ;; ��ȡ����ͼ������
  (vlax-for obj *lays*
    (setq layerNames (cons (vla-get-Name obj) layerNames))
  )

  ;; �������������򣨺��Դ�Сд��
  (setq layerNames
    (vl-sort layerNames
      '(lambda (a b) (< (strcase a) (strcase b)))  ; �Զ���ȽϺ���
    )
  )

  ;; �û�������ʼ��
  (if (setq p1 (getpoint "\n��������ʼ��: "))
    (progn
      ;; ѭ�������ı�
      (foreach layer layerNames
        (entmake
          (list
            '(0 . "TEXT")
            (cons 1 layer)
            (cons 8 layer)       ; ͼ������
            (cons 10 p1)         ; ����㣨���½ǣ�
            (cons 11 p1)         ; �����=�����
            (cons 40 20)         ; ���ָ߶�
            (cons 72 0)          ; ˮƽ�����
            (cons 73 0)          ; ��ֱ���߶���
            (cons 50 0)          ; ��ת�Ƕ�
          )
        )
        ;; ������һ���ı���λ�ã�Y�����ƣ�
        (setq p1 (list (car p1) (- (cadr p1) 50) (caddr p1)))
      )
    )
  )
  (princ "\nͼ���б��Ѱ��������ɡ�")
  (princ)
)
(defun c:listSlectLayersSorted (/ dxf p1 ss layers uniqueLayers sortedLayers)
    ;; ������������ȡDXF����ֵ
    (defun dxf (ent i) (cdr (assoc i (entget ent))))
    
    ;; �Զ���ȥ�غ������޸�����ȱʧ��
    (defun unique (lst / out)
        (while lst
            (if (not (member (car lst) out))
                (setq out (cons (car lst) out))
            )
            (setq lst (cdr lst))
        )
        (reverse out)
    )

    ;; �������߼�
    (if (and (setq ss (ssget))
             (setq p1 (getpoint "\n��ָ�������: "))
        )
        (progn
            ;; �ռ�����ͼ������
            (setq layers 
                (mapcar '(lambda (x) (dxf x 8)) 
                    (vl-remove-if-not 
                        '(lambda (x) (= 'ENAME (type x))) 
                        (mapcar 'cadr (ssnamex ss))
                    )
                )
            )
            
            ;; ȥ�ز��������������򣨲����ִ�Сд��
            (setq uniqueLayers (unique layers))
            (setq sortedLayers 
                (vl-sort uniqueLayers 
                    '(lambda (a b) 
                        (< (strcase a) (strcase b))
                    )
                )
            )
            
            ;; �������������
            (foreach layer sortedLayers
                (entmake 
                    (list 
                        '(0 . "TEXT")
                        (cons 1 layer)       ; ��������
                        (cons 8 layer)       ; ͼ������
                        (cons 10 p1)         ; ����㣨���½ǣ�
                        (cons 11 p1)         ; �����=�����
                        (cons 40 20)         ; ���ָ߶�
                        (cons 72 0)          ; ˮƽ�����
                        (cons 73 0)          ; ��ֱ���߶���
                        (cons 50 0)          ; ��ת�Ƕ�
                    )
                )
                (setq p1 (polar p1 (* 1.5 pi) 50)) ; ����50��λ
            )
        )
    )
    (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;�г�ѡ��Ԫ�ص�ͼ������
(defun C:ListElementLayers (/ ss layers uniqueLayers)
  ;; ������������ȡDXF����ֵ
  (defun dxf (ent i) (cdr (assoc i (entget ent))))
  
  ;; ����������ȥ��
  (defun unique (lst / out)
    (while lst
      (if (not (member (car lst) out))
        (setq out (cons (car lst) out))
      )
      (setq lst (cdr lst))
    )
    (reverse out)
  )

  ;; ������
  (if (setq ss (ssget))  ; ��ʾ�û�ѡ��Ԫ��
    (progn
      ;; �ռ�����ѡ��Ԫ�ص�ͼ������
      (setq layers 
        (mapcar '(lambda (x) (dxf x 8)) 
          (vl-remove-if-not 
            '(lambda (x) (= 'ENAME (type x))) 
            (mapcar 'cadr (ssnamex ss))
          )
        )
      )
      
      ;; ȥ�ز���������������
      (setq uniqueLayers 
        (vl-sort (unique layers) 
          '(lambda (a b) 
            (< (strcase a) (strcase b))
          )
        )
      )
      
      ;; ��ӡͼ������
      (princ "\nѡ��Ԫ�����ڵ�ͼ�����ƣ�")
      (foreach layer uniqueLayers
        (princ (strcat "\n" layer))
      )
      (princ (strcat "\n�� " (itoa (length uniqueLayers)) " ��ͼ��"))
    )
    (princ "\nδѡ���κ�Ԫ�أ�")
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;�½�ͼ�㲢��Ϊ��ǰ
(defun c:CreateLayer (/ layerName)
  ; ��ʾ��������
  (princ "\n*** �Ѽ��� CreateLayer ���� ***")
  (princ "\n�������� CreateLayer ��ʼ����ͼ��")

  ; ��ȡ�û������ͼ������
  (setq layerName (getstring t "\n������ͼ������: "))

  ; ������ͼ�㣨��������ڣ�����Ϊ��ǰ
  (command "_.-layer" "_m" layerName "")

  ; ��ʽ���õ�ǰͼ�㣨ȷ�������ɹ���
  (setvar "CLAYER" layerName)

  ; ��ʾ�������
  (princ (strcat "\n�Ѵ������л���ͼ��: " layerName))
  (princ)
)

; ����ʱ��ʾ��ʾ��Ϣ
(princ "\n*** CreateLayer �����Ѽ��� ***")
(princ "\n�������� CreateLayer ��ʼ����ͼ��")
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;�ϲ�ͼ��
(defun c:MLYR (/ ent1 ent2 layA layB ss oldLayer acadDoc layers)
  (vl-load-com) ; ����ActiveX֧��
  (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldLayer (getvar "CLAYER"))
  
  ; ѡ��Դͼ��Ԫ��
  (while (not (setq ent1 (car (entsel "\n��ѡ��Ҫ�ϲ���Դͼ��Ԫ��: "))))
    (alert "����ѡ��һ����ЧԪ�أ�"))
  (setq layA (cdr (assoc 8 (entget ent1))))

  ; ѡ��Ŀ��ͼ��Ԫ��
  (while (not (setq ent2 (car (entsel "\n��ѡ��Ŀ��ͼ��Ԫ��: "))))
    (alert "����ѡ��һ����ЧԪ�أ�"))
  (setq layB (cdr (assoc 8 (entget ent2))))

  ; ͼ����Ч�Լ��
  (cond
    ((= layA layB) (alert "����Դͼ����Ŀ��ͼ����ͬ��"))
    ((null (tblsearch "LAYER" layB)) (alert "����Ŀ��ͼ�㲻���ڣ�"))
    (T
     (if (setq ss (ssget "X" (list (cons 8 layA))))
       (progn
         ; ��Ĭ�޸�ͼ��
         (command "_.chprop" ss "" "_layer" layB "")
         (while (> (getvar "CMDACTIVE") 0) (command "")) ; ȷ������������

         ; ���õ�ǰͼ��
         (setvar "CLAYER" layB)

         ; ʹ��ActiveXɾ��ͼ��
         (if (and (tblsearch "LAYER" layA)
                  (/= layA oldLayer)
                  (not (wcmatch (strcase layA) "0,DEFPOINTS")))
           (progn
             (if (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-item (list (vla-get-layers acadDoc) layA)))
               (princ (strcat "\n" layA " ͼ�㲻����"))
               (vla-delete (vla-item (vla-get-layers acadDoc) layA))
             )
           )
         )
         (alert (strcat "������ɣ�\n" layA " �� " layB "\n��ǰͼ�㣺" layB))
       )
       (alert "���棺Դͼ��û�пɺϲ�����")
     )
    )
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
