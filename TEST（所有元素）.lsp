;;;; 多段线高程赋值程序
;;;; 作者: superstoney
;;;; 日期: 2025-03-22

(defun c:SET_ELEV (/ pt1 pt2 h b lines int_pts)
  (vl-load-com)
  (setq pt1 (getpoint "\n请选择起点: "))     
  (setq pt2 (getpoint pt1 "\n请选择终点: ")) 
  (setq h (getreal "\n请输入起始高程: "))    
  (setq b (getreal "\n请输入增量高程: "))    
  
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
          (princ "\n高程赋值完成。")
        )
        (princ "\n未找到交点。")
      )
    )
    (princ "\n图中未找到多段线。")
  )
  (princ)
)

(defun get_intersections (p1 p2 ss / i result ent)
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
  result
)

(defun find_inters (p1 p2 ent / pts result)
  ;; 获取多段线的所有顶点
  (setq pts (get_pline_points ent))
  (setq result '())
  
  ;; 检查每个线段与参考线的交点
  (while (> (length pts) 1)
    (setq int_pt (inters p1 p2 (car pts) (cadr pts) nil))
    (if int_pt
      (setq result (cons int_pt result))
    )
    (setq pts (cdr pts))
  )
  result
)

(defun get_pline_points (ent / pts en)
  ;; 获取多段线的所有顶点
  (setq pts '())
  (setq en (entget ent))
  (foreach item en
    (if (= (car item) 10)
      (setq pts (cons (cdr item) pts))
    )
  )
  (reverse pts)
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

(defun assign_elevations_by_pairs (int_pts h b / i pairs)
  ;; 将多段线分组并赋值
  (setq pairs (group_by_two int_pts)
        i 0)
  
  ;; 为每组多段线赋值相同的高程
  (foreach pair pairs
    (setq current_elev (+ h (* i b)))
    (foreach item pair
      (set_pline_elevation (cadr item) current_elev)
    )
    (setq i (1+ i))
  )
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

(defun set_pline_elevation (ent elev / en new_en)
  ;; 设置多段线高程
  (setq en (entget ent))
  (setq new_en (subst (cons 38 elev)
                      (assoc 38 en)
                      en))
  (entmod new_en)
  (princ (strcat "\n已设置多段线高程为: " (rtos elev 2 2)))
)

;; 自动加载设置
(if (not *elevation_assign_loaded*)
  (progn
    (setq *elevation_assign_loaded* T)
    (prompt "\n多段线高程赋值程序已加载。")
    (prompt "\n输入 SET_ELEV 运行命令。")
  )
)

(princ)