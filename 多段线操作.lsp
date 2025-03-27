;;;; ����߸̸߳�ֵ����
;;;; ����: superstoney
;;;; ����: 2025-03-26 11:24:31
;;;; ����: �޸�ѡ������

(defun c:SET_ELEV (/ pt1 pt2 h b lines int_pts)
  (vl-load-com)
    (setq h (getreal "\n��������ʼ�߳�: "))    
  (setq b (getreal "\n�����������߳�: ")) 
  (setq pt1 (getpoint "\n��ѡ�����: "))     
  (setq pt2 (getpoint pt1 "\n��ѡ���յ�: ")) 
   
  
  ;; ��ȡ�����ཻ�Ķ����
  (setq lines (get_possible_intersecting_plines pt1 pt2))
  
  (if lines
    (progn
      (setq int_pts (get_intersections pt1 pt2 lines))
      
      (if int_pts
        (progn
          ;; ���������򽻵�
          (setq int_pts (vl-sort int_pts
                                '(lambda (a b)
                                   (< (distance pt1 (car a))
                                      (distance pt1 (car b))))))
          
          ;; ȥ���ظ�ʵ��
          (setq int_pts (remove_duplicate_ents int_pts))
          
          ;; ��ֵ�߳�
          (assign_elevations_by_pairs int_pts h b)
          (princ (strcat "\n������ " (rtos (length int_pts) 2 0) " �����㡣"))
          (princ "\n�̸߳�ֵ��ɡ�")
        )
        (princ "\nδ�ҵ����㡣")
      )
    )
    (princ "\n��ָ����Χ��δ�ҵ��ɼ��Ķ���ߡ�")
  )
  (princ)
)

(defun get_possible_intersecting_plines (pt1 pt2 / ss extend-dist min-x max-x min-y max-y)
  (princ "\n��ʼ���ҿ����ཻ�Ķ����...")
  
  ;; ������չ���루�߶γ��ȵ�10%��
  (setq extend-dist (* (distance pt1 pt2) 0.1))
  
  ;; ����ѡ���ķ�Χ
  (setq min-x (- (min (car pt1) (car pt2)) extend-dist)
        max-x (+ (max (car pt1) (car pt2)) extend-dist)
        min-y (- (min (cadr pt1) (cadr pt2)) extend-dist)
        max-y (+ (max (cadr pt1) (cadr pt2)) extend-dist))
  
  ;; ʹ�þ���ѡ���ѡ������
  (setq ss (ssget "W" 
                  (list min-x min-y)
                  (list max-x max-y)
                  '((0 . "LWPOLYLINE"))))
  
  ;; ���˻�ȡ�ɼ��Ķ����
  (if ss
    (progn
      (princ (strcat "\nѡ������ҵ����������: " (rtos (sslength ss) 2 0)))
      (filter_visible_plines ss)
    )
    (progn
      (princ "\nѡ�����δ�ҵ�����ߡ�")
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
      (princ (strcat "\n�ɼ����������: " (rtos (length lst) 2 0)))
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
      (= (vla-get-freeze lay) :vlax-false)    ;; ����Ƿ�δ����
      (= (vla-get-layeron lay) :vlax-true)    ;; ����Ƿ���
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
  (princ (strcat "\n�����ö���߸߳�Ϊ: " (rtos elev 2 2)))
)

(defun ssadd-list (lst / ss)
  (setq ss (ssadd))
  (foreach ent lst
    (ssadd ent ss)
  )
  ss
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(vl-load-com)

(defun c:OffsetPolylineElevation ( / ent a b h c dp obj closestParam pt tangent normal vec dot sign dist-list all-offsets)
;; �Ľ���Ϊ b, h, c ����Ĭ��ֵ
  (initget 6) ; ���������
  (setq b (getdist (strcat "\n����ˮƽ����b <" (rtos 25) ">: ")))
  (if (null b) (setq b 25.0)) ; Ĭ��ֵΪ 25

  (initget 6) ; ���������
  (setq h (getdist (strcat "\n������ֱ�߶�h <" (rtos 10) ">: ")))
  (if (null h) (setq h 10.0)) ; Ĭ��ֵΪ 10

  (initget 6) ; ���������
  (setq c (getdist (strcat "\n����������c <" (rtos 2) ">: ")))
  (if (null c) (setq c 2.0)) ; Ĭ��ֵΪ 2
  ;; ����1��������ǿѡ��ɿ���
  (while (null ent)
    (setq ent (car (entsel "\nѡ������: ")))
    (if (null ent)
      (princ "\nδѡ����Ч����ߣ�������ѡ��")
    )
  )

  (setq entdata (entget ent))
  (setq a (cdr (assoc 38 entdata)))
  (if (null a) (setq a 0.0))
  
  
  
  ;; ����2��������ȷ�����ж�
  (setq dp (getpoint "\nѡ��ƫ�Ʒ����: "))
  (setq obj (vlax-ename->vla-object ent))
  
  ;; ��ȡ��������
  (setq closestParam (vlax-curve-getParamAtPoint obj (vlax-curve-getClosestPointTo obj dp)))
  (setq pt (vlax-curve-getPointAtParam obj closestParam))
  
  ;; ����ʵ�����߷��򣨿����߶�����
  (setq tangent (vlax-curve-getFirstDeriv obj closestParam))
  (setq normal (list (cadr tangent) (- (car tangent)) 0.0))
  
  ;; ��ά��������
  (setq vec (mapcar '- dp pt))
  (setq vec (list (car vec) (cadr vec) 0.0)) ; ͶӰ��XYƽ��
  
  ;; ��ǿ�����ж�
  (setq dot (+ (* (car normal) (car vec)) (* (cadr normal) (cadr vec))))
  (setq sign (if (> dot 0) 1.0 -1.0))
  
  ;; ���ɶ�̬ƫ�Ʋ���
  (setq dist-list
    (mapcar '(lambda (x) (* sign x))
      (list
        b 
        (+ b c) 
        (+ (* 2 b) c) 
        (+ (* 2 b) (* 2 c)) 
        (+ (* 3 b) (* 2 c)) 
        (+ (* 3 b) (* 3 c)) 
        (+ (* 4 b) (* 3 c)) 
        (+ (* 4 b) (* 4 c)) 
        (+ (* 5 b) (* 4 c)) 
        (+ (* 5 b) (* 5 c))
      )
    )
  )

  (defun create-offset (obj dist elevation / result)
    (setq result (vl-catch-all-apply 'vla-offset (list obj dist)))
    (if (vl-catch-all-error-p result)
      (progn
        (princ (strcat "\n[����] ƫ��ʧ�ܣ����룺" (rtos dist)))
        nil
      )
      (progn
        (mapcar 
          '(lambda (x) 
            (vla-put-Elevation x elevation)
            (vla-update x)
            (vlax-vla-object->ename x)
           )
          (vlax-safearray->list (vlax-variant-value result))
        )
      )
    )
  )

  ;; ��������ƫ��
  (setq all-offsets 
    (apply 'append
      (mapcar
        '(lambda (d e) (create-offset obj d e))
        dist-list
        (list 
          (- a h) (- a h) 
          (- a h h) (- a h h)
          (- a h h h) (- a h h h)
          (- a h h h h) (- a h h h h)
          (- a h h h h h) (- a h h h h h)
        )
      )
    )
  )

  ;; �������
  (setq ss (ssadd))
  (ssadd ent ss)
  (foreach x (vl-remove nil all-offsets) (ssadd x ss))
  
  (cond
    ((> (sslength ss) 1)
      (sssetfirst nil ss)
      (princ (strcat "\n�ɹ����� " (itoa (- (sslength ss) 1)) " ��ƫ����"))
    )
    ((= (sslength ss) 1) (princ "\nδ������Чƫ����"))
  )
  (princ)
)

