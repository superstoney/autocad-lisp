;;;=========================================================================
;;; Command: tmj2
;;; Function: Adjust polygon by single edge according to area
;;; Author: Atsai 2015.08.16
;;; Modified by: superstoney 2025-03-30 00:42:38
;;;=========================================================================

(defun c:tmj3 (/ obj1 en el-tmp el p1 p2 p3 p4 ang1 ang2)
  (vl-load-com)
  (SetIErr)
  (setvar "cmdecho" 0)
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  
  ;; 先提示输入目标面积
  (setq area-n nil)
  (while (null area-n)
    (setq area-n
           (getreal "\nEnter target area: ")
    )
    (if (<= area-n 0)
      (progn
        (princ "\nArea must be positive!")
        (setq area-n nil)
      )
    )
  )
  
  ;; 然后选择多边形
  (setq obj1 nil)
  (while (null obj1)
    (setq temp-sel (entsel "\nSelect polygon:"))
    (if temp-sel
      (setq obj1 (vlax-ename->vla-object (car temp-sel)))
      (princ "\nPlease select a valid polygon.")
    )
  )
  
  ;; 计算当前面积
  (setq ptlst nil)
  (setq ptlst (vxs obj1))
  (setq area-o1 nil)
  (setq area-o1 (getplarea ptlst))
  (if (< area-o1 0)
    (tmj2_VxRevPline obj1)
  )
  (setq ptlst (vxs obj1))
  (setq area-o1 (abs area-o1))
  
  ;; 显示当前面积和目标面积
  (princ (strcat "\nCurrent area: " (rtos area-o1 2 4)))
  (princ (strcat "\nTarget area: " (rtos area-n 2 4)))
  
  ;; 最后选择要调整的边
  (setq en nil)
  (while (null en)
    (setq en (entsel "\nSelect edge to adjust:"))
    (if (null en)
      (princ "\nPlease select a valid edge.")
    )
  )
  
  (setq endata (entget (car en)))
  (setq obj2 (vlax-ename->vla-object (car en)))
  
  ;; 获取端点
  (setq endpoints (HH:PickSegEndPt2 obj2 (cadr en)))
  (if (or (null endpoints) (< (length endpoints) 4))
    (progn
      (princ "\nFailed to get edge points.")
      (exit)
    )
  )
  
  (setq p1 (nth 0 endpoints)
        p2 (nth 1 endpoints)
        p3 (nth 2 endpoints)
        p4 (nth 3 endpoints))

  (setq ptlst-n nil)
  (setq ptlst-n (vl-remove p2 ptlst))
  (setq ptlst-n (vl-remove p3 ptlst-n))

  (setq area-o2 (abs (getplarea ptlst-n)))
  (setq area-m (- area-n area-o1))

  ;; 初始化变量
  (setq m1 0.0 m2 0.0 B1 0.0 ang1 0.0 ang2 0.0)
  (setq B1 (distance p2 p3))
  (setq ang1 (- (angle p2 p3) (angle p2 p1)))
  (setq ang2 (- (angle p3 p4) (angle p3 p2)))

  ;; 角度计算
  (cond
    ((< ang1 (* 0.5 pi))
     (setq ang1 (- (* 0.5 pi) ang1))
     (setq m1 (/ (sin ang1) (cos ang1)))
    )
    ((and (> ang1 (* 0.5 pi)) (< ang1 pi))
     (setq ang1 (- ang1 (* 0.5 pi)))
     (setq m1 (* -1 (/ (sin ang1) (cos ang1))))
    )
    ((and (> ang1 pi) (< ang1 (* 1.5 pi)))
     (setq ang1 (- (* 1.5 pi) ang1))
     (setq m1 (/ (sin ang1) (cos ang1)))
    )
    ((and (> ang1 (* 1.5 pi)) (< ang1 (* 2 pi)))
     (setq ang1 (- ang1 (* 1.5 pi)))
     (setq m1 (* -1 (/ (sin ang1) (cos ang1))))
    )
  )

  (cond
    ((< ang2 (* 0.5 pi))
     (setq ang2 (- (* 0.5 pi) ang2))
     (setq m2 (/ (sin ang2) (cos ang2)))
    )
    ((and (> ang2 (* 0.5 pi)) (< ang2 pi))
     (setq ang2 (- ang2 (* 0.5 pi)))
     (setq m2 (* -1 (/ (sin ang2) (cos ang2))))
    )
    ((and (> ang2 pi) (< ang2 (* 1.5 pi)))
     (setq ang2 (- (* 1.5 pi) ang2))
     (setq m2 (/ (sin ang2) (cos ang2)))
    )
    ((and (> ang2 (* 1.5 pi)) (< ang2 (* 2 pi)))
     (setq ang2 (- ang2 (* 1.5 pi)))
     (setq m2 (* -1 (/ (sin ang2) (cos ang2))))
    )
  )

  (if (< area-m 0)
    (setq m1 m1)
    (setq m1 (* -1 m1))
  )

  (if (< area-m 0)
    (setq m2 m2)
    (setq m2 (* -1 m2))
  )

  ;; 定义精确调整函数
  (defun adjust-area-precise (h-val / curr-area)
    (if (< area-m 0)
      (progn
        (setq sp-el (polar p2 (+ (angle p2 p3) (* 0.5 pi)) h-val))
        (setq ep-el (polar p3 (+ (angle p2 p3) (* 0.5 pi)) h-val))
      )
      (progn
        (setq sp-el (polar p2 (- (angle p2 p3) (* 0.5 pi)) h-val))
        (setq ep-el (polar p3 (- (angle p2 p3) (* 0.5 pi)) h-val))
      )
    )
    
    (setq p2-new (inters p1 p2 sp-el ep-el nil))
    (setq p3-new (inters p3 p4 sp-el ep-el nil))
    
    (if (and p2-new p3-new)
      (progn
        (setq endata-tmp (entget (car en)))
        (setq endata-tmp (subst (cons 10 (list (car p2-new) (cadr p2-new)))
                               (cons 10 (list (car p2) (cadr p2)))
                               endata-tmp))
        (setq endata-tmp (subst (cons 10 (list (car p3-new) (cadr p3-new)))
                               (cons 10 (list (car p3) (cadr p3)))
                               endata-tmp))
        (entmod endata-tmp)
        (command "area" "o" en)
        (getvar "area")
      )
      nil
    )
  )

  ;; 计算初始高度
  (if (not (equal (+ m1 m2) 0.0 1e-6))
    (progn
      (setq a (+ m1 m2))
      (setq b (* 2 B1))
      (setq c (* -2 (abs area-m)))

      (if (> (- (* b b) (* 4 a c)) 0)
        (setq h+ (/ (+ (* -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
        (progn
          (alert "\nPolygon cannot be adjusted to new area!")
          (exit)
        )
      )
    )
    (setq h+ (/ (abs area-m) B1))
  )

  ;; 改进的二分法求解
  (defun binary-search-precise (target h-init / h1 h2 h-mid area-curr iter best-h best-area)
    (setq h1 (* h-init 0.9)
          h2 (* h-init 1.1)
          iter 0
          best-h h-init
          best-area nil)
    
    (while (and (< iter 200) (or (null best-area) (> (abs (- target best-area)) 1e-8)))
      (setq h-mid (+ h1 (* 0.5 (- h2 h1))))
      (setq area-curr (adjust-area-precise h-mid))
      
      (if (and area-curr (or (null best-area) (< (abs (- target area-curr)) (abs (- target best-area)))))
        (setq best-h h-mid
              best-area area-curr)
      )
      
      (if (and area-curr (> area-curr target))
        (setq h2 h-mid)
        (setq h1 h-mid)
      )
      
      (setq iter (1+ iter))
    )
    (list best-h best-area)
  )

  ;; 执行精确调整
  (setq result (binary-search-precise area-n h+))
  (if result
    (progn
      (setq final-h (car result))
      (setq final-area (cadr result))
      (adjust-area-precise final-h)
      
      (if (< (abs (- final-area area-n)) 1e-7)
        (princ (strcat "\nOK!! Area adjusted to: " (rtos final-area 2 8)))
        (princ (strcat "\nWarning: Area adjusted to: " (rtos final-area 2 8)
                      "\nTarget was: " (rtos area-n 2 8)
                      "\nDifference: " (rtos (abs (- final-area area-n)) 2 8)))
      )
    )
    (alert "\nFailed to adjust polygon!")
  )

  (princ)
  (setvar "osmode" os)
  (ReErr)
)

;; 获取多段线顶点列表
(defun vxs (e / i v lst)
  (setq i -1)
  (while (setq v (vlax-curve-getpointatparam e (setq i (1+ i))))
    (setq lst (cons v lst))
  )
  (reverse lst)
)

;; 计算多边形面积
(defun getplarea (l / a b)
  (if (and l (> (length l) 2))
    (* 0.5
       (apply '+
              (mapcar '(lambda (a b)
                         (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                       )
                      l
                      (append (cdr l) (list (car l)))
              )
       )
    )
    0.0
  )
)

;; 获取线段端点
(defun HH:PickSegEndPt2 (obj p / pp n ll)
  (if (and obj p)
    (progn
      (setq pp (vlax-curve-getclosestpointto obj (trans p 1 0)))
      (if pp
        (progn
          (setq n (fix (vlax-curve-getparamatpoint obj pp)))
          (setq ll (length (vxs obj)))
          (if (and (>= ll 4) (numberp n))
            (list
              (if (= n 0)
                (vlax-curve-getPointAtParam obj (- ll 2))
                (vlax-curve-getPointAtParam obj (- n 1))
              )
              (vlax-curve-getPointAtParam obj n)
              (vlax-curve-getPointAtParam obj (+ n 1))
              (if (> (+ n 2) (- ll 1))
                (vlax-curve-getPointAtParam obj 1)
                (vlax-curve-getPointAtParam obj (+ n 2))
              )
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;; 反转多段线方向
(defun tmj2_VxRevPline (Obj / BlgLst ObjName PntLst SegCnt TmpLst Ubound)
  (if Obj
    (progn
      (setq ObjName (vlax-get Obj 'ObjectName)
            TmpLst  (vlax-get Obj 'Coordinates)
      )
      (if (and ObjName TmpLst)
        (progn
          (if (eq ObjName "AcDbPolyline")
            (repeat (/ (length TmpLst) 2)
              (setq PntLst (cons (list (car TmpLst) (cadr TmpLst)) PntLst)
                    TmpLst (cddr TmpLst)
              )
            )
            (repeat (/ (length TmpLst) 3)
              (setq PntLst (cons (list (car TmpLst) (cadr TmpLst) (caddr TmpLst))
                                PntLst
                         )
                    TmpLst (cdddr TmpLst)
              )
            )
          )
          (vlax-put Obj 'Coordinates (apply 'append PntLst))
          (if (not (eq ObjName "AcDb3dPolyline"))
            (progn
              (setq Ubound (1- (length PntLst))
                    BlgLst (list (* (vla-GetBulge Obj Ubound) -1))
                    SegCnt 0
              )
              (repeat Ubound
                (setq BlgLst (cons (* (vla-GetBulge Obj SegCnt) -1) BlgLst)
                      SegCnt (1+ SegCnt)
                )
              )
              (setq SegCnt 0)
              (foreach memb BlgLst
                (vla-SetBulge Obj SegCnt memb)
                (setq SegCnt (1+ SegCnt))
              )
            )
          )
          (vla-Update Obj)
          Obj
        )
        nil
      )
    )
    nil
  )
)

;; 错误处理设置
(defun SetIErr ()
  (setq varlst '("ATTDIA" "CMDECHO" "ORTHOMODE" "MIRRTEXT" "CECOLOR" "celtype" "osmode" "dimzin" "ATTREQ")
        var_new '(0 0 0 0 "BYLAYER" "BYLAYER" 767 8 1)
  )
  (mapcar 'setvar varlst var_new)
  (defun *error* (inf)
    (setq inf (strcase inf t))
    (cond
      ((wcmatch inf "*break,*cancel*,*exit*,*cancel*")
       (princ "\nFunction cancelled\n")
       (mapcar 'setvar varlst var_old)
      )
      (t
       (princ (strcat "\n" inf))
       (mapcar 'setvar varlst var_old)
      )
    )
    (vla-EndUndoMark
      (vla-get-ActiveDocument (vlax-get-acad-object))
    )
    (princ)
  )

  (vla-StartUndoMark
    (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (setq varlst '("ATTDIA" "CMDECHO" "ORTHOMODE" "MIRRTEXT" "CECOLOR" "celtype" "osmode" "dimzin" "ATTREQ")
        var_new '(0 0 0 0 "BYLAYER" "BYLAYER" 767 8 1)
        var_old (mapcar 'getvar varlst)
  )
  (mapcar 'setvar varlst var_new)
)

;; 错误处理清理
(defun ReErr ()
  (mapcar 'setvar varlst var_old)
  (vla-EndUndoMark
    (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (princ)
)