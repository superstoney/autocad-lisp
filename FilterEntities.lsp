(defun C:SelFilter (/ choice filterList filterFunc ss result new_ss)
  ;; 显示选择提示 <button class="citation-flag" data-index="1"><button class="citation-flag" data-index="6">
  (initget "点 多段线(闭合) 多段线(不闭合) 单行文字 多行文字 三角网曲面 体积曲面")
  (setq choice (getkword "\n请选择要筛选的元素类型 [点/多段线(闭合)/多段线(不闭合)/单行文字/多行文字/三角网曲面/体积曲面]: "))
  
  ;; 设置过滤条件与验证函数 <button class="citation-flag" data-index="2"><button class="citation-flag" data-index="7">
  (cond
    ((= choice "点") 
      (setq filterList '((0 . "POINT"))))
    
    ((= choice "多段线(闭合)") 
      (setq filterList '((0 . "LWPOLYLINE"))
            filterFunc '(lambda (ent) 
                         (= 1 (logand 1 (cdr (assoc 70 (entget ent)))))))  ;; 修正闭合判断 <button class="citation-flag" data-index="2">
    )
    
    ((= choice "多段线(不闭合)") 
      (setq filterList '((0 . "LWPOLYLINE"))
            filterFunc '(lambda (ent) 
                         (= 0 (logand 1 (cdr (assoc 70 (entget ent)))))))
    )
    
    ((= choice "单行文字") 
      (setq filterList '((0 . "TEXT"))))
    
    ((= choice "多行文字") 
      (setq filterList '((0 . "MTEXT"))))
    
    ((= choice "三角网曲面") 
      (setq filterList '((0 . "AECC_TIN_SURFACE"))))
    
    ((= choice "体积曲面") 
      (setq filterList '((0 . "AECC_VOLUME_SURFACE"))))
  )
  
  ;; 获取框选对象 <button class="citation-flag" data-index="3"><button class="citation-flag" data-index="6">
  (princ "\n框选要筛选的对象: ")
  (setq ss (ssget '((0 . "*") 
                    (-4 . "<OR") 
                    (0 . "POINT,LWPOLYLINE,TEXT,MTEXT,AECC_TIN_SURFACE,AECC_VOLUME_SURFACE") 
                    (-4 . "OR>"))))
  
  ;; 过滤与选中处理 <button class="citation-flag" data-index="4"><button class="citation-flag" data-index="6">
  (if ss
    (progn
      ;; 双重过滤机制：先类型后属性 <button class="citation-flag" data-index="7">
      (setq result 
        (vl-remove-if-not
          (lambda (ent)
            (and 
              (wcmatch (cdr (assoc 0 (entget ent))) (cdr (assoc 0 filterList)))
              (if filterFunc (eval filterFunc) T)  ;; 应用闭合状态验证 <button class="citation-flag" data-index="2">
            )
          )
          (mapcar 'cadr (ssnamex ss))
        )
      )
      
      ;; 创建并激活选择集 <button class="citation-flag" data-index="6">
      (if (> (length result) 0)
        (progn
          (setq new_ss (ssadd))
          (foreach ent result (ssadd ent new_ss))
          (sssetfirst nil new_ss)  ;; 关键函数：设置选中状态 <button class="citation-flag" data-index="6">
          (princ (strcat "\n已选中 " (itoa (length result)) " 个" choice))
        )
        (princ "\n未找到符合条件的对象")
      )
    )
    (princ "\n未选择任何对象")
  )
  (princ)
)  ;; 此处补全最后缺失的右括号 <button class="citation-flag" data-index="9">