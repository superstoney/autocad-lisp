(defun c:ChangeLayersFromCSV (/ csvFile file line lst oldLayer newLayer ss doc layers layerObj result)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layers (vla-get-layers doc))
  
  ;; 定义系统图层列表（不可删除/重命名）
  (setq *system-layers* '("0" "DEFPOINTS")) ; 添加其他需要过滤的图层
  
  ;; 设置CSV文件路径
  (setq csvFile (getfiled "选择CSV文件" "" "csv" 16))
  (princ (strcat "\n选择的CSV文件: " csvFile))
  
  ;; 检查文件是否存在
  (if (not (findfile csvFile))
    (progn (princ "\n错误：文件未找到！") (exit))
  )
  
  ;; 打开CSV文件
  (setq file (open csvFile "r"))
  (princ "\n开始读取CSV文件...")
  
  ;; 读取文件内容
  (while (setq line (read-line file))
    (setq line (vl-string-trim " \t\r\n\"" line)) ; 去除首尾空格/引号
    (if (> (strlen line) 0)
      (progn
        (princ (strcat "\n处理行: " line))
        ;; 分割CSV行（处理带引号的字段）
        (setq lst (csv-split line))
        (if (>= (length lst) 2)
          (progn
            ;; 获取清理后的图层名
            (setq oldLayer (vl-string-trim " \t\"" (nth 0 lst)))
            (setq newLayer (vl-string-trim " \t\"" (nth 1 lst)))
            (princ (strcat "\n操作：[" oldLayer "] → [" newLayer "]"))
            
            ;; 跳过系统图层操作
            (cond
              ((member (strcase oldLayer) (mapcar 'strcase *system-layers*))
               (princ "\n警告：跳过系统图层操作"))
              
              ;; 原始图层存在时的处理
              ((tblsearch "LAYER" oldLayer)
               (cond
                 ;; 新旧图层名相同
                 ((= (strcase oldLayer) (strcase newLayer))
                  (princ "\n跳过：新旧图层名相同"))
                 
                 ;; 目标图层已存在 → 合并
                 ((tblsearch "LAYER" newLayer)
                  (princ "\n目标图层存在，执行合并...")
                  ;; 移动对象
                  (if (setq ss (ssget "_X" (list (cons 8 oldLayer))))
                    (progn
                      (princ (strcat "\n移动 " (itoa (sslength ss)) " 个对象"))
                      (vlax-for obj (vla-get-activeselectionset doc)
                        (vla-put-layer obj newLayer)
                      )
                      (vla-delete (vla-get-activeselectionset doc))
                    )
                    (princ "\n无对象需要移动")
                  )
                  ;; 尝试删除旧图层（排除系统图层）
                  (if (not (member (strcase oldLayer) (mapcar 'strcase *system-layers*)))
                    (progn
                      (setq layerObj (vl-catch-all-apply 'vla-item (list layers oldLayer)))
                      (if (and (not (vl-catch-all-error-p layerObj)) (vlax-write-enabled-p layerObj))
                        (progn
                          (setq result (vl-catch-all-apply 'vla-delete (list layerObj)))
                          (if (vl-catch-all-error-p result)
                            (princ (strcat "\n删除失败: " (vl-catch-all-error-message result)))
                            (princ "\n旧图层已删除")
                          )
                        )
                        (princ "\n图层不可删除（只读或系统依赖）")
                      )
                    )
                    (princ "\n跳过系统图层删除")
                  )
                 )
                 
                 ;; 目标图层不存在 → 重命名
                 (t
                  (princ "\n执行重命名操作...")
                  (setq layerObj (vl-catch-all-apply 'vla-item (list layers oldLayer)))
                  (if (not (vl-catch-all-error-p layerObj))
                    (progn
                      (setq result (vl-catch-all-apply 'vla-put-name (list layerObj newLayer)))
                      (if (vl-catch-all-error-p result)
                        (princ (strcat "\n重命名失败: " (vl-catch-all-error-message result)))
                        (princ "\n重命名成功")
                      )
                    )
                    (princ "\n原始图层访问失败")
                  )
                 )
               )
              )
              
              ;; 原始图层不存在
              (t (princ "\n错误：原始图层不存在"))
            )
          )
          (princ "\n错误：CSV行格式无效")
        )
      )
      (princ "\n跳过空行")
    )
  )
  (close file)
  (princ "\n操作完成！")
  (princ)
)

;; 增强版CSV解析函数（处理带引号的字段）
(defun csv-split (str / pos in-quote chunk lst)
  (setq in-quote nil)
  (while (> (strlen str) 0)
    (if in-quote
      ;; 引号内处理
      (if (setq pos (vl-string-search "\"" str))
        (progn
          (setq chunk (substr str 1 pos))
          (setq str (substr str (+ pos 2))) ; 跳过引号和可能的逗号
          (if (= (substr str 1 1) ",")
            (setq str (substr str 2))
          )
          (setq lst (cons chunk lst))
          (setq in-quote nil)
        )
        (progn ; 引号未闭合
          (setq lst (cons str lst))
          (setq str "")
        )
      )
      ;; 引号外处理
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
        (progn ; 最后一个字段
          (setq lst (cons str lst))
          (setq str "")
        )
      )
    )
  )
  (reverse lst)
)