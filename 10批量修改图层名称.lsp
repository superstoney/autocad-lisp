(defun c:ChangeLayersFromCSV (/ csvFile file line lst oldLayer newLayer ss doc layers layerObj result)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layers (vla-get-layers doc))
  
  ;; ����ϵͳͼ���б�����ɾ��/��������
  (setq *system-layers* '("0" "DEFPOINTS")) ; ���������Ҫ���˵�ͼ��
  
  ;; ����CSV�ļ�·��
  (setq csvFile (getfiled "ѡ��CSV�ļ�" "" "csv" 16))
  (princ (strcat "\nѡ���CSV�ļ�: " csvFile))
  
  ;; ����ļ��Ƿ����
  (if (not (findfile csvFile))
    (progn (princ "\n�����ļ�δ�ҵ���") (exit))
  )
  
  ;; ��CSV�ļ�
  (setq file (open csvFile "r"))
  (princ "\n��ʼ��ȡCSV�ļ�...")
  
  ;; ��ȡ�ļ�����
  (while (setq line (read-line file))
    (setq line (vl-string-trim " \t\r\n\"" line)) ; ȥ����β�ո�/����
    (if (> (strlen line) 0)
      (progn
        (princ (strcat "\n������: " line))
        ;; �ָ�CSV�У���������ŵ��ֶΣ�
        (setq lst (csv-split line))
        (if (>= (length lst) 2)
          (progn
            ;; ��ȡ������ͼ����
            (setq oldLayer (vl-string-trim " \t\"" (nth 0 lst)))
            (setq newLayer (vl-string-trim " \t\"" (nth 1 lst)))
            (princ (strcat "\n������[" oldLayer "] �� [" newLayer "]"))
            
            ;; ����ϵͳͼ�����
            (cond
              ((member (strcase oldLayer) (mapcar 'strcase *system-layers*))
               (princ "\n���棺����ϵͳͼ�����"))
              
              ;; ԭʼͼ�����ʱ�Ĵ���
              ((tblsearch "LAYER" oldLayer)
               (cond
                 ;; �¾�ͼ������ͬ
                 ((= (strcase oldLayer) (strcase newLayer))
                  (princ "\n�������¾�ͼ������ͬ"))
                 
                 ;; Ŀ��ͼ���Ѵ��� �� �ϲ�
                 ((tblsearch "LAYER" newLayer)
                  (princ "\nĿ��ͼ����ڣ�ִ�кϲ�...")
                  ;; �ƶ�����
                  (if (setq ss (ssget "_X" (list (cons 8 oldLayer))))
                    (progn
                      (princ (strcat "\n�ƶ� " (itoa (sslength ss)) " ������"))
                      (vlax-for obj (vla-get-activeselectionset doc)
                        (vla-put-layer obj newLayer)
                      )
                      (vla-delete (vla-get-activeselectionset doc))
                    )
                    (princ "\n�޶�����Ҫ�ƶ�")
                  )
                  ;; ����ɾ����ͼ�㣨�ų�ϵͳͼ�㣩
                  (if (not (member (strcase oldLayer) (mapcar 'strcase *system-layers*)))
                    (progn
                      (setq layerObj (vl-catch-all-apply 'vla-item (list layers oldLayer)))
                      (if (and (not (vl-catch-all-error-p layerObj)) (vlax-write-enabled-p layerObj))
                        (progn
                          (setq result (vl-catch-all-apply 'vla-delete (list layerObj)))
                          (if (vl-catch-all-error-p result)
                            (princ (strcat "\nɾ��ʧ��: " (vl-catch-all-error-message result)))
                            (princ "\n��ͼ����ɾ��")
                          )
                        )
                        (princ "\nͼ�㲻��ɾ����ֻ����ϵͳ������")
                      )
                    )
                    (princ "\n����ϵͳͼ��ɾ��")
                  )
                 )
                 
                 ;; Ŀ��ͼ�㲻���� �� ������
                 (t
                  (princ "\nִ������������...")
                  (setq layerObj (vl-catch-all-apply 'vla-item (list layers oldLayer)))
                  (if (not (vl-catch-all-error-p layerObj))
                    (progn
                      (setq result (vl-catch-all-apply 'vla-put-name (list layerObj newLayer)))
                      (if (vl-catch-all-error-p result)
                        (princ (strcat "\n������ʧ��: " (vl-catch-all-error-message result)))
                        (princ "\n�������ɹ�")
                      )
                    )
                    (princ "\nԭʼͼ�����ʧ��")
                  )
                 )
               )
              )
              
              ;; ԭʼͼ�㲻����
              (t (princ "\n����ԭʼͼ�㲻����"))
            )
          )
          (princ "\n����CSV�и�ʽ��Ч")
        )
      )
      (princ "\n��������")
    )
  )
  (close file)
  (princ "\n������ɣ�")
  (princ)
)

;; ��ǿ��CSV������������������ŵ��ֶΣ�
(defun csv-split (str / pos in-quote chunk lst)
  (setq in-quote nil)
  (while (> (strlen str) 0)
    (if in-quote
      ;; �����ڴ���
      (if (setq pos (vl-string-search "\"" str))
        (progn
          (setq chunk (substr str 1 pos))
          (setq str (substr str (+ pos 2))) ; �������źͿ��ܵĶ���
          (if (= (substr str 1 1) ",")
            (setq str (substr str 2))
          )
          (setq lst (cons chunk lst))
          (setq in-quote nil)
        )
        (progn ; ����δ�պ�
          (setq lst (cons str lst))
          (setq str "")
        )
      )
      ;; �����⴦��
      (if (setq pos (vl-string-search "," str))
        (progn
          (setq chunk (substr str 1 pos))
          (setq str (substr str (+ pos 2)))
          (if (= (substr chunk 1 1) "\"")
            (progn
              (setq chunk (substr chunk 2))
              (setq in-quote t)
            )
          )
          (setq lst (cons chunk lst))
        )
        (progn ; ���һ���ֶ�
          (setq lst (cons str lst))
          (setq str "")
        )
      )
    )
  )
  (reverse lst)
)