(defun c:CopyLayerNameToClipboard ()
;;获取图层名称写入剪贴板
  (vl-load-com)
  ;; 提示用户选择一个对象
  (if (setq sel (ssget))
    (progn
      ;; 获取选中的第一个对象
      (setq ent (vlax-ename->vla-object (ssname sel 0)))
      ;; 获取对象的图层名称
      (setq layerName (vla-get-Layer ent))
      ;; 使用Windows API函数将图层名称复制到剪贴板
      (setq clipdata (vlax-create-object "htmlfile"))
      (vlax-invoke-method (vlax-get-property (vlax-get-property clipdata 'parentWindow) 'clipboardData) 'setData "Text" layerName)
      (vlax-release-object clipdata)
      (princ (strcat "\n图层名称 " layerName " 已复制到剪贴板。"))
    )
    (princ "\n未选择任何对象。")
  )
  (princ)
)

(defun c:listSlectLayersSorted (/ dxf p1 ss layers uniqueLayers sortedLayers)
    ;; 辅助函数：获取DXF组码值
    (defun dxf (ent i) (cdr (assoc i (entget ent))))
    
    ;; 自定义去重函数（修复括号缺失）
    (defun unique (lst / out)
        (while lst
            (if (not (member (car lst) out))
                (setq out (cons (car lst) out))
            )
            (setq lst (cdr lst))
        )
        (reverse out)
    )

    ;; 主程序逻辑
    (if (and (setq ss (ssget))
             (setq p1 (getpoint "\n请指定插入点: "))
        )
        (progn
            ;; 收集所有图层名称
            (setq layers 
                (mapcar '(lambda (x) (dxf x 8)) 
                    (vl-remove-if-not 
                        '(lambda (x) (= 'ENAME (type x))) 
                        (mapcar 'cadr (ssnamex ss))
                    )
                )
            )
            
            ;; 去重并按名称升序排序（不区分大小写）
            (setq uniqueLayers (unique layers))
            (setq sortedLayers 
                (vl-sort uniqueLayers 
                    '(lambda (a b) 
                        (< (strcase a) (strcase b))
                    )
                )
            )
            
            ;; 生成左对齐文字
            (foreach layer sortedLayers
                (entmake 
                    (list 
                        '(0 . "TEXT")
                        (cons 1 layer)       ; 文字内容
                        (cons 8 layer)       ; 图层属性
                        (cons 10 p1)         ; 插入点（左下角）
                        (cons 11 p1)         ; 对齐点=插入点
                        (cons 40 20)         ; 文字高度
                        (cons 72 0)          ; 水平左对齐
                        (cons 73 0)          ; 垂直基线对齐
                        (cons 50 0)          ; 旋转角度
                    )
                )
                (setq p1 (polar p1 (* 1.5 pi) 50)) ; 下移50单位
            )
        )
    )
    (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;列出所有图层名称
(defun C:listAllLayersSorted (/ *acad* *doc* *lays* layerNames p1)
  (setq
    *acad* (vlax-get-acad-object)
    *doc* (vla-get-ActiveDocument *acad*)
    *lays* (vla-get-Layers *doc*)
    layerNames '()  ; 初始化图层名称列表
  )

  ;; 提取所有图层名称
  (vlax-for obj *lays*
    (setq layerNames (cons (vla-get-Name obj) layerNames))
  )

  ;; 按名称升序排序（忽略大小写）
  (setq layerNames
    (vl-sort layerNames
      '(lambda (a b) (< (strcase a) (strcase b)))  ; 自定义比较函数
    )
  )

  ;; 用户输入起始点
  (if (setq p1 (getpoint "\n请输入起始点: "))
    (progn
      ;; 循环创建文本
      (foreach layer layerNames
        (entmake
          (list
            '(0 . "TEXT")
            (cons 1 layer)
            (cons 8 layer)       ; 图层属性
            (cons 10 p1)         ; 插入点（左下角）
            (cons 11 p1)         ; 对齐点=插入点
            (cons 40 20)         ; 文字高度
            (cons 72 0)          ; 水平左对齐
            (cons 73 0)          ; 垂直基线对齐
            (cons 50 0)          ; 旋转角度
          )
        )
        ;; 更新下一个文本的位置（Y轴下移）
        (setq p1 (list (car p1) (- (cadr p1) 50) (caddr p1)))
      )
    )
  )
  (princ "\n图层列表已按升序生成。")
  (princ)
)
(defun c:listSlectLayersSorted (/ dxf p1 ss layers uniqueLayers sortedLayers)
    ;; 辅助函数：获取DXF组码值
    (defun dxf (ent i) (cdr (assoc i (entget ent))))
    
    ;; 自定义去重函数（修复括号缺失）
    (defun unique (lst / out)
        (while lst
            (if (not (member (car lst) out))
                (setq out (cons (car lst) out))
            )
            (setq lst (cdr lst))
        )
        (reverse out)
    )

    ;; 主程序逻辑
    (if (and (setq ss (ssget))
             (setq p1 (getpoint "\n请指定插入点: "))
        )
        (progn
            ;; 收集所有图层名称
            (setq layers 
                (mapcar '(lambda (x) (dxf x 8)) 
                    (vl-remove-if-not 
                        '(lambda (x) (= 'ENAME (type x))) 
                        (mapcar 'cadr (ssnamex ss))
                    )
                )
            )
            
            ;; 去重并按名称升序排序（不区分大小写）
            (setq uniqueLayers (unique layers))
            (setq sortedLayers 
                (vl-sort uniqueLayers 
                    '(lambda (a b) 
                        (< (strcase a) (strcase b))
                    )
                )
            )
            
            ;; 生成左对齐文字
            (foreach layer sortedLayers
                (entmake 
                    (list 
                        '(0 . "TEXT")
                        (cons 1 layer)       ; 文字内容
                        (cons 8 layer)       ; 图层属性
                        (cons 10 p1)         ; 插入点（左下角）
                        (cons 11 p1)         ; 对齐点=插入点
                        (cons 40 20)         ; 文字高度
                        (cons 72 0)          ; 水平左对齐
                        (cons 73 0)          ; 垂直基线对齐
                        (cons 50 0)          ; 旋转角度
                    )
                )
                (setq p1 (polar p1 (* 1.5 pi) 50)) ; 下移50单位
            )
        )
    )
    (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;列出选择元素的图层名称
(defun C:ListElementLayers (/ ss layers uniqueLayers)
  ;; 辅助函数：获取DXF组码值
  (defun dxf (ent i) (cdr (assoc i (entget ent))))
  
  ;; 辅助函数：去重
  (defun unique (lst / out)
    (while lst
      (if (not (member (car lst) out))
        (setq out (cons (car lst) out))
      )
      (setq lst (cdr lst))
    )
    (reverse out)
  )

  ;; 主程序
  (if (setq ss (ssget))  ; 提示用户选择元素
    (progn
      ;; 收集所有选中元素的图层名称
      (setq layers 
        (mapcar '(lambda (x) (dxf x 8)) 
          (vl-remove-if-not 
            '(lambda (x) (= 'ENAME (type x))) 
            (mapcar 'cadr (ssnamex ss))
          )
        )
      )
      
      ;; 去重并按名称升序排序
      (setq uniqueLayers 
        (vl-sort (unique layers) 
          '(lambda (a b) 
            (< (strcase a) (strcase b))
          )
        )
      )
      
      ;; 打印图层名称
      (princ "\n选中元素所在的图层名称：")
      (foreach layer uniqueLayers
        (princ (strcat "\n" layer))
      )
      (princ (strcat "\n共 " (itoa (length uniqueLayers)) " 个图层"))
    )
    (princ "\n未选择任何元素！")
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;新建图层并置为当前
(defun c:CreateLayer (/ layerName)
  ; 显示程序名称
  (princ "\n*** 已加载 CreateLayer 程序 ***")
  (princ "\n输入命令 CreateLayer 开始创建图层")

  ; 获取用户输入的图层名称
  (setq layerName (getstring t "\n请输入图层名称: "))

  ; 创建新图层（如果不存在）并置为当前
  (command "_.-layer" "_m" layerName "")

  ; 显式设置当前图层（确保操作成功）
  (setvar "CLAYER" layerName)

  ; 提示操作完成
  (princ (strcat "\n已创建并切换到图层: " layerName))
  (princ)
)

; 加载时显示提示信息
(princ "\n*** CreateLayer 程序已加载 ***")
(princ "\n输入命令 CreateLayer 开始创建图层")
(princ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;合并图层
(defun c:MLYR (/ ent1 ent2 layA layB ss oldLayer acadDoc layers)
  (vl-load-com) ; 加载ActiveX支持
  (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldLayer (getvar "CLAYER"))
  
  ; 选择源图层元素
  (while (not (setq ent1 (car (entsel "\n请选择要合并的源图层元素: "))))
    (alert "必须选择一个有效元素！"))
  (setq layA (cdr (assoc 8 (entget ent1))))

  ; 选择目标图层元素
  (while (not (setq ent2 (car (entsel "\n请选择目标图层元素: "))))
    (alert "必须选择一个有效元素！"))
  (setq layB (cdr (assoc 8 (entget ent2))))

  ; 图层有效性检查
  (cond
    ((= layA layB) (alert "错误：源图层与目标图层相同！"))
    ((null (tblsearch "LAYER" layB)) (alert "错误：目标图层不存在！"))
    (T
     (if (setq ss (ssget "X" (list (cons 8 layA))))
       (progn
         ; 静默修改图层
         (command "_.chprop" ss "" "_layer" layB "")
         (while (> (getvar "CMDACTIVE") 0) (command "")) ; 确保命令队列清空

         ; 设置当前图层
         (setvar "CLAYER" layB)

         ; 使用ActiveX删除图层
         (if (and (tblsearch "LAYER" layA)
                  (/= layA oldLayer)
                  (not (wcmatch (strcase layA) "0,DEFPOINTS")))
           (progn
             (if (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-item (list (vla-get-layers acadDoc) layA)))
               (princ (strcat "\n" layA " 图层不存在"))
               (vla-delete (vla-item (vla-get-layers acadDoc) layA))
             )
           )
         )
         (alert (strcat "操作完成：\n" layA " → " layB "\n当前图层：" layB))
       )
       (alert "警告：源图层没有可合并对象！")
     )
    )
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
