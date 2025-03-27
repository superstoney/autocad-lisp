;;;; ����߸̸߳�ֵ����
;;;; ����: superstoney
;;;; ����: 2025-03-22

(defun c:SET_ELEV (/ pt1 pt2 h b lines int_pts)
  (vl-load-com)
  (setq pt1 (getpoint "\n��ѡ�����: "))     
  (setq pt2 (getpoint pt1 "\n��ѡ���յ�: ")) 
  (setq h (getreal "\n��������ʼ�߳�: "))    
  (setq b (getreal "\n�����������߳�: "))    
  
  (setq lines (ssget "X" '((0 . "LWPOLYLINE"))))
  
  (if lines
    (progn
      (setq int_pts (get_intersections pt1 pt2 lines))
      
      (if int_pts
        (progn
          ;; ���������򽻵㼰��Ӧ��ʵ��
          (setq int_pts (vl-sort int_pts
                                '(lambda (a b)
                                   (< (distance pt1 (car a))
                                      (distance pt1 (car b))))))
          
          ;; ȥ���ظ���ʵ������
          (setq int_pts (remove_duplicate_ents int_pts))
          
          ;; ���鸳ֵ�߳�
          (assign_elevations_by_pairs int_pts h b)
          (princ "\n�̸߳�ֵ��ɡ�")
        )
        (princ "\nδ�ҵ����㡣")
      )
    )
    (princ "\nͼ��δ�ҵ�����ߡ�")
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
  ;; ��ȡ����ߵ����ж���
  (setq pts (get_pline_points ent))
  (setq result '())
  
  ;; ���ÿ���߶���ο��ߵĽ���
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
  ;; ��ȡ����ߵ����ж���
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
  ;; ȥ���ظ���ʵ�����ã�����˳��
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
  ;; ������߷��鲢��ֵ
  (setq pairs (group_by_two int_pts)
        i 0)
  
  ;; Ϊÿ�����߸�ֵ��ͬ�ĸ߳�
  (foreach pair pairs
    (setq current_elev (+ h (* i b)))
    (foreach item pair
      (set_pline_elevation (cadr item) current_elev)
    )
    (setq i (1+ i))
  )
)

(defun group_by_two (lst / result pair)
  ;; ���б�ÿ����Ԫ�ط�Ϊһ��
  (setq result '())
  (while (>= (length lst) 2)
    (setq pair (list (car lst) (cadr lst))
          result (cons pair result)
          lst (cddr lst)))
  ;; �������ʣ��ĵ���Ԫ��
  (if lst
    (setq result (cons (list (car lst)) result))
  )
  (reverse result)
)

(defun set_pline_elevation (ent elev / en new_en)
  ;; ���ö���߸߳�
  (setq en (entget ent))
  (setq new_en (subst (cons 38 elev)
                      (assoc 38 en)
                      en))
  (entmod new_en)
  (princ (strcat "\n�����ö���߸߳�Ϊ: " (rtos elev 2 2)))
)

;; �Զ���������
(if (not *elevation_assign_loaded*)
  (progn
    (setq *elevation_assign_loaded* T)
    (prompt "\n����߸̸߳�ֵ�����Ѽ��ء�")
    (prompt "\n���� SET_ELEV �������")
  )
)

(princ)