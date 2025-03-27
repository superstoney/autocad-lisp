;;;; 多段线高程赋值程序
;;;; 作者: superstoney
;;;; 日期: 2025-03-26 11:24:31
;;;; 更新: 修复选择框错误

(defun c:SET_ELEV (/ pt1 pt2 h b lines int_pts)
  (vl-load-com)
  (setq pt1 (getpoint "\n请选择起点: "))     
  (setq pt2 (getpoint pt1 "\n请选择终点: ")) 
  (setq h (getreal "\n请输入起始高程: "))    
  (setq b (getreal "\n请输入增量高程: "))    
  
  ;; 获取可能相交的多段线
  (setq lines (get_possible_intersecting_plines pt1 pt2))
  
  (if lines
    (progn
      (setq int_pts (get_intersections pt1 pt2 lines))
      
      (if int_pts
        (progn
          ;; 按距离排序交点
          (setq int_pts (vl-sort int_pts
                                '(lambda (a b)
                                   (< (distance pt1 (car a))
                                      (distance pt1 (car b))))))
          
          ;; 去除重复实体
          (setq int_pts (remove_duplicate_ents int_pts))
          
          ;; 赋值高程
          (assign_elevations_by_pairs int_pts h b)
          (princ (strcat "\n共处理 " (rtos (length int_pts) 2 0) " 个交点。"))
          (princ "\n高程赋值完成。")
        )
        (princ "\n未找到交点。")
      )
    )
    (princ "\n在指定范围内未找到可见的多段线。")
  )
  (princ)
)

(defun get_possible_intersecting_plines (pt1 pt2 / ss extend-dist min-x max-x min-y max-y)
  (princ "\n开始查找可能相交的多段线...")
  
  ;; 计算扩展距离（线段长度的10%）
  (setq extend-dist (* (distance pt1 pt2) 0.1))
  
  ;; 计算选择框的范围
  (setq min-x (- (min (car pt1) (car pt2)) extend-dist)
        max-x (+ (max (car pt1) (car pt2)) extend-dist)
        min-y (- (min (cadr pt1) (cadr pt2)) extend-dist)
        max-y (+ (max (cadr pt1) (cadr pt2)) extend-dist))
  
  ;; 使用矩形选择框选择多段线
  (setq ss (ssget "W" 
                  (list min-x min-y)
                  (list max-x max-y)
                  '((0 . "LWPOLYLINE"))))
  
  ;; 过滤获取可见的多段线
  (if ss
    (progn
      (princ (strcat "\n选择框内找到多段线总数: " (rtos (sslength ss) 2 0)))
      (filter_visible_plines ss)
    )
    (progn
      (princ "\n选择框内未找到多段线。")
      nil
    )
  )
)

(defun filter_visible_plines (ss / ent lst i)
  (setq lst '()
        i 0)
  (while (setq ent (ssname ss i))
    (if (layer-on-and-thawed-p (cdr (assoc 8 (entget ent))))
      (setq lst (cons ent lst))
    )
    (setq i (1+ i))
  )
  
  (if lst
    (progn
      (princ (strcat "\n可见多段线数量: " (rtos (length lst) 2 0)))
      (ssadd-list lst)
    )
    nil
  )
)

(defun layer-on-and-thawed-p (layer-name / doc lay)
  (if (and layer-name
           (setq doc (vla-get-activedocument (vlax-get-acad-object)))
           (setq lay (vla-item (vla-get-layers doc) layer-name)))
    (and
      (= (vla-get-freeze lay) :vlax-false)    ;; 检查是否未冻结
      (= (vla-get-layeron lay) :vlax-true)    ;; 检查是否开启
    )
    nil
  )
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
  (setq pts (get_pline_points ent))
  (setq result '())
  
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
  (setq pairs (group_by_two int_pts)
        i 0)
  
  (foreach pair pairs
    (setq current_elev (+ h (* i b)))
    (foreach item pair
      (set_pline_elevation (cadr item) current_elev)
    )
    (setq i (1+ i))
  )
)

(defun group_by_two (lst / result pair)
  (setq result '())
  (while (>= (length lst) 2)
    (setq pair (list (car lst) (cadr lst))
          result (cons pair result)
          lst (cddr lst)))
  (if lst
    (setq result (cons (list (car lst)) result))
  )
  (reverse result)
)

(defun set_pline_elevation (ent elev / en new_en)
  (setq en (entget ent))
  (setq new_en (subst (cons 38 elev)
                      (assoc 38 en)
                      en))
  (entmod new_en)
  (princ (strcat "\n已设置多段线高程为: " (rtos elev 2 2)))
)

(defun ssadd-list (lst / ss)
  (setq ss (ssadd))
  (foreach ent lst
    (ssadd ent ss)
  )
  ss
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