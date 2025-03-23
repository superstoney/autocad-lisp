;;;; 多段线高程赋值程序（改进版）
;;;; 作者: superstoney
;;;; 日期: 2023-03-22
;;;; 最后修改: 2023-11-15
;;;; 功能说明: 根据用户指定的参考线，为与之相交的多段线赋予高程值

(defun c:SET_ELEV (/ pt1 pt2 h b lines int_pts old_cmd old_error *error* ss_before)
  (vl-load-com)
  
  (defun *error* (msg)
    (if old_error (setq *error* old_error))
    (if old_cmd (setvar "CMDECHO" old_cmd))
    (if ss_before (command "_.UNDO" "Back"))
    (princ "\n错误: ")
    (princ msg)
    (princ)
  )
  
  (setq old_cmd (getvar "CMDECHO"))
  (setq old_error *error*)
  (setvar "CMDECHO" 0)
  
  (command "_.UNDO" "Begin")
  
  (initget 6)
  (setq h (getreal "\n请输入起始高程 <0.0>: "))
  (if (null h) (setq h 0.0))
  
  (initget 6)
  (setq b (getreal "\n请输入增量高程 <1.0>: "))
  (if (null b) (setq b 1.0))
  
  (setq pt1 (getpoint "\n请选择起点: "))
  (if (null pt1)
    (progn
      (command "_.UNDO" "End")
      (princ "\n操作已取消。")
      (exit)
    )
  )
  
  (setq pt2 (getpoint pt1 "\n请选择终点: "))
  (if (null pt2)
    (progn
      (command "_.UNDO" "End")
      (princ "\n操作已取消。")
      (exit)
    )
  )
  
  ;; 记录操作前的状态，用于撤销
  (setq ss_before T)
  
  ;; 获取所有多段线
  (setq lines (ssget "X" '((0 . "LWPOLYLINE"))))
  
  (if lines
    (progn
      (setq int_pts (get_intersections pt1 pt2 lines))
      
      (if int_pts
        (progn
          ;; 按距离排序交点及对应的实体
          (setq int_pts (vl-sort int_pts
                                '(lambda (a b)
                                   (< (distance pt1 (car a))
                                      (distance pt1 (car b))))))
          
          ;; 去除重复的实体引用
          (setq int_pts (remove_duplicate_ents int_pts))
          
          ;; 按组赋值高程
          (assign_elevations_by_pairs int_pts h b)
          ;; 修改字符串输出部分
          (princ (strcat "\n高程赋值完成，共处理 " 
                         (vl-princ-to-string (length int_pts))
                         " 个多段线。"))
          (princ "\n输入 U 可撤销操作。")
        )
        (princ "\n未找到交点，请检查参考线是否与多段线相交。")
      )
    )
    (princ "\n图中未找到多段线，请先创建多段线。")
  )
  
  ;; 结束撤销标记
  (command "_.UNDO" "End")
  
  ;; 恢复命令回显状态
  (setvar "CMDECHO" old_cmd)
  (princ)
)

(defun get_intersections (p1 p2 ss / i result ent int_pts)  ;; 添加 int_pts 到局部变量
  (setq i 0
        result '())
  (while (setq ent (ssname ss i))
    (setq int_pts (find_inters p1 p2 ent))
    (if int_pts
      (foreach int_pt int_pts
        (setq result (cons (list int_pt ent) result))
      )
    )
    (setq i (1+ i))
  )
  (if result result '())  ;; 确保返回空列表而不是 nil
)

(defun find_inters (p1 p2 ent / pts result)
  (setq pts (get_pline_points ent)
        result '())  ;; 初始化为空列表
  
  (while (> (length pts) 1)
    (setq int_pt (inters p1 p2 (car pts) (cadr pts) nil))
    (if int_pt
      (setq result (cons int_pt result))
    )
    (setq pts (cdr pts))
  )
  (if result result '())  ;; 确保返回空列表而不是 nil
)

(defun get_pline_points (ent / pts en closed last_pt)
  ;; 获取多段线的所有顶点，并处理闭合多段线
  (setq pts '())
  (setq en (entget ent))
  
  ;; 检查多段线是否闭合
  (setq closed (= (logand (cdr (assoc 70 en)) 1) 1))
  
  ;; 获取所有顶点
  (foreach item en
    (if (= (car item) 10)
      (setq pts (cons (cdr item) pts))
    )
  )
  
  ;; 反转列表以保持正确顺序
  (setq pts (reverse pts))
  
  ;; 如果是闭合多段线，添加第一个点作为最后一个点
  (if (and closed (> (length pts) 0))
    (setq pts (append pts (list (car pts))))
  )
  
  pts
)

(defun remove_duplicate_ents (int_pts / result used_ents)
  ;; 去除重复的实体引用，保持顺序
  (setq result '()
        used_ents '())
  (foreach item int_pts
    (if (not (member (cadr item) used_ents))
      (progn
        (setq result (cons item result))
        (setq used_ents (cons (cadr item) used_ents))
      )
    )
  )
  (reverse result)
)

(defun assign_elevations_by_pairs (int_pts h b / i pairs count)
  ;; 将多段线分组并赋值
  (setq pairs (group_by_two int_pts)
        i 0
        count 0)
  
  ;; 为每组多段线赋值相同的高程
  (foreach pair pairs
    (setq current_elev (+ h (* i b)))
    (foreach item pair
      (if (set_pline_elevation (cadr item) current_elev)
        (setq count (1+ count))
      )
    )
    (setq i (1+ i))
  )
  
  ;; 返回处理的多段线数量
  count
)

(defun group_by_two (lst / result pair)
  ;; 将列表每两个元素分为一组
  (setq result '())
  (while (>= (length lst) 2)
    (setq pair (list (car lst) (cadr lst))
          result (cons pair result)
          lst (cddr lst)))
  ;; 处理最后剩余的单个元素
  (if lst
    (setq result (cons (list (car lst)) result))
  )
  (reverse result)
)

(defun set_pline_elevation (ent elev / en new_en result)
  ;; 设置多段线高程，返回操作是否成功
  (setq result nil)
  (if (and ent (not (null elev)))
    (progn
      (setq en (entget ent))
      (if (assoc 38 en)
        ;; 如果已有高程，替换它
        (setq new_en (subst (cons 38 elev)
                          (assoc 38 en)
                          en))
        ;; 如果没有高程，添加它
        (setq new_en (append en (list (cons 38 elev))))
      )
      (entmod new_en)
      ;; 在 set_pline_elevation 函数中修改
      (princ (strcat "\n已设置多段线高程为: " 
                     (vl-princ-to-string (rtos elev 2 2))))
      (setq result T)
    )
  result
)

;; 添加命令别名
(defun c:SE () (c:SET_ELEV))

;; 添加帮助命令
(defun c:HELP_ELEV (/ )
  (princ "\n==== 多段线高程赋值程序使用说明 ====\n")
  (princ "\n功能: 根据用户指定的参考线，为与之相交的多段线赋予高程值")
  (princ "\n使用方法:")
  (princ "\n  1. 输入 SET_ELEV 或 SE 启动命令")
  (princ "\n  2. 输入起始高程值和增量高程值")
  (princ "\n  3. 选择参考线的起点和终点")
  (princ "\n  4. 程序会自动为与参考线相交的多段线赋予高程值")
  (princ "\n  5. 输入 U 可撤销操作")
  (princ "\n  6. 输入 HELP_ELEV 显示此帮助信息")
  (princ "\n===============================")
  (princ)
)

;; 自动加载设置
(if (not (vl-bb-ref '*elevation_assign_loaded*))
  (progn
    (vl-bb-set '*elevation_assign_loaded* T)
    (princ "\n多段线高程赋值程序已加载")
    (princ "\n输入 SET_ELEV 或 SE 运行命令")
    (princ "\n输入 HELP_ELEV 获取帮助信息")
    (princ)
  )
)

(princ)