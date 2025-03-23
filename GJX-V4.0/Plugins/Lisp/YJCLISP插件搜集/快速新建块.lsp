(defun C:KSJK (/ TKname ss pt ent entData)
  ;(setvar "cmdecho" 0)
  (setq TKname (menucmd "M=$(edtime,$(getvar,date),YYYYMODDHHMMSS)"))
  (print "----------请选择需要建块的对象----------:")
  (setq ss (ssget))
  (if (= ss nil) 
    (print "选择的对象为空，请重新选择：")
    (progn 
      ;; 获取第一个对象的基点
      (setq ent (ssname ss 0))       ; 获取选择集中第一个对象
      (setq entData (entget ent))   ; 获取该对象的属性列表
      (setq pt (cdr (assoc 10 entData))) ; 提取基点（assoc 10 通常是插入点）
      (if (not pt) (setq pt '(0 0 0)))   ; 如果没有基点，默认设为(0 0 0)
      ;; 创建块
      (command "-block" TKname "o" "c" pt ss "")
      (princ (strcat "\n已完成块的新建,块名为<" TKname ">"))
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)
