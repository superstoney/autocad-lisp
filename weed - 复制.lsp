(defun c:test ( / AREA_H AREA_WS ERRHAN NEWENTLAST OBJ OBJ_AREA 
                 OBJ_I OBJ_NAME OBJ_VLA OLDCMDECHO OLDDELOBJ OLDENTLAST PT)
  
  ;; 错误处理函数
  (defun *error* (msg)
    (if (not (member msg '("*Cancel*" "Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (if oldcmdecho (setvar "cmdecho" oldcmdecho))
    (if oldDELOBJ (setvar "DELOBJ" oldDELOBJ))
    (princ)
  )

  ;; 初始化变量
  (setq AREA_H 3.0)    ;文字高度
  (setq AREA_WS 3)     ;小数位数
  (setq ERRHAN nil)    ;错误处理列表
  
  ;; 保存系统变量
  (setq oldcmdecho (getvar "cmdecho"))
  (setq oldDELOBJ (getvar "DELOBJ"))
  
  ;; 设置系统变量
  (setvar "cmdecho" 0)
  (setvar "DELOBJ" 0)
  
  ;; 选择所有多段线（包括普通多段线和轻量多段线）
  (setq obj (ssget "X" '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if obj
    (progn
      (setq obj_i -1)
      (while (< (setq obj_i (1+ obj_i)) (sslength obj))
        (setq obj_name (ssname obj obj_i))
        (if obj_name
          (progn
            (setq obj_vla (vlax-ename->vla-object obj_name))
            (if (vlax-curve-isClosed obj_vla)
              (progn
                (setq oldentlast (entlast))
                (command "._region" obj_name "")
                (setq newentlast (entlast))
                (if (equal oldentlast newentlast)
                  (setq ERRHAN (cons (cdr (assoc 5 (entget obj_name))) ERRHAN))
                  (progn
                    (setq obj_area (vla-get-Area obj_vla))
                    (setq pt (vlax-safearray->list 
                             (vlax-variant-value 
                               (vla-get-centroid (vlax-ename->vla-object newentlast)))))
                    (command "._text" "J" "MC" pt AREA_H 0.0 
                            (rtos obj_area 2 AREA_WS))
                    (entdel newentlast)
                  )
                )
              )
            )
          )
        )
      )
      
      ;; 显示未处理的图元句柄
      (if ERRHAN
        (progn
          (princ "\n以下图元无法计算面积:\n")
          (princ ERRHAN)
        )
      )
    )
    (princ "\n未找到多段线对象！")
  )
  
  ;; 恢复系统变量
  (setvar "DELOBJ" oldDELOBJ)
  (setvar "cmdecho" oldcmdecho)
  
  (princ "\n命令完成。")
  (princ)
)