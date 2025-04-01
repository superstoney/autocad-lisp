(defun c:a2f3 ( / *error* fmt sel str pline center )
    ;; 修改字段格式以确保正确显示面积
    (setq fmt "%.2f") ;; 将显示格式改为保留2位小数

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    ;; 计算多边形面积的函数
    (defun calculate-area ( pts / i area len)
        (setq i 0
              area 0.0
              len (length pts))
        ;; 使用多边形顶点坐标计算面积
        (while (< i (1- len))
            (setq area 
                (+ area 
                    (* (caar pts) (cadar (nth (1+ i) pts)))
                    (- (* (cadar pts) (caar (nth (1+ i) pts))))
                )
            )
            (setq pts (cdr pts)
                  i (1+ i))
        )
        ;; 处理最后一个点和第一个点
        (setq area 
            (+ area 
                (* (caar pts) (cadar (last pts)))
                (- (* (cadar pts) (caar (last pts))))
            )
        )
        (abs (* 0.5 area))
    )

    ;; 获取多段线的顶点
    (defun get-vertices ( ent / entdata pts vertex bulge angle center rad)
        (setq entdata (entget ent)
              pts '())
        ;; 遍历实体数据
        (while entdata
            (if (= 10 (caar entdata))  ; 10是顶点坐标的DXF组码
                (setq pts (cons (cdar entdata) pts))
            )
            (setq entdata (cdr entdata))
        )
        (reverse pts)  ; 返回正确顺序的点列表
    )

    ;; 计算质心
    (defun calculate-centroid ( pts / sumx sumy n)
        (setq sumx 0.0
              sumy 0.0
              n (length pts))
        (foreach pt pts
            (setq sumx (+ sumx (car pt))
                  sumy (+ sumy (cadr pt)))
        )
        (list (/ sumx n) (/ sumy n) 0.0)
    )

    ;; 获取多段线数据的主函数
    (defun get-pline-data ( ent / vertices area centroid)
        (setq vertices (get-vertices ent))
        (setq area (calculate-area vertices))
        (setq centroid (calculate-centroid vertices))
        (list area centroid)
    )

    ;; 主程序逻辑
    (if (setq sel (ssget '((0 . "LWPOLYLINE"))))
        (progn
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx))))
                (if (= 1 (cdr (assoc 70 (entget ent)))) ; 检查是否闭合
                    (progn
                        (setq result (get-pline-data ent))
                        (setq area (car result)
                              center (cadr result))
                        
                        ;; 创建面积文字
                        (setq str (rtos area 2 2))
                        
                        ;; 使用命令行方式添加文字
                        (command "._text"
                                "j"  ; 指定对齐方式
                                "m"  ; 中点对齐
                                center  ; 插入点
                                2.5  ; 文字高度
                                0  ; 旋转角度
                                str)  ; 文字内容
                    )
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Support functions
(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com) (princ)