;;曲线线所点击子段的两端点列表----(一级)---
;;(sl:pick2pt (car (setq e (entsel))) (cadr e))
(defun sl:pick2pt (nam p / pp n ll lis tp obj)
  (setq tp (dxf1 nam 0) obj (en2obj nam) ll (length (get-pl-pt nam)))
  (if (or
        (and (= "LWPOLYLINE" tp) (= (dxf1 nam 90) 2))
        (and (= "POLYLINE" tp) (= ll 2))
        (= "LINE" tp)
      )
    (setq lis (list (vlax-curve-getstartpoint obj) (vlax-curve-getendpoint obj)))
    (progn
      (setq pp (vlax-curve-getclosestpointto obj (trans p 1 0))
        n (fix (vlax-curve-getparamatpoint obj pp))
      )
      (setq lis
        (list
          (vlax-curve-getPointAtParam obj n)
          (if (> (1+ n) (1- ll))
            (vlax-curve-getPointAtParam obj 0)
            (vlax-curve-getPointAtParam obj (1+ n))
          )
        )
      )
    )
  )
  lis
)

;;多边型边-->动态调整面积并显示
;; by 尘缘一生 qq 15290049
;;支持 LWPOLYLINE,POLYLINE
(defun c:kk (/ loop oldos p0 e nam nam1 h obj ang ps pe str n1 n3 k p p1 p2 p3 p4 p1-1 p3-1 plis pts ent lw ly cl lt)
  (setq loop t oldos (getvar "osmode"))
  (setvar "osmode" 514)
  (setq p0 (getpoint "pick point:"))
  (setq e (ssget "C" (polar p0 pi4 0.001) (polar p0 5pi4 0.001) '((0 . "LWPOLYLINE,POLYLINE"))))
  (setq nam (ssname e 0))
  (if (= (dxf1 nam 0) "POLYLINE")
    (progn
      (setq ent (entget nam) lw (linwind nam) ly (dxf1 ent 8) cl (ss-getcolor ent) lt (ss-linetype ent) pts (get-pl-pt nam))
      (entdel nam)
      (if (= (dxf1 ent 70) 9) ;闭合
        (slch:lwpolyline pts t lw ly cl 1.0) ;闭合
        (slch:lwpolyline pts nil lw ly cl 1.0) ;不闭合
      )
      (setq nam (entlast))
      (vla-put-linetype (en2obj nam) lt)
    )
  )
  (if nam
    (progn
      (setq
        obj (en2obj nam)
        pts (sl:pick2pt nam p0)
        p1 (car pts)
        p3 (cadr pts)
        ang (angle p1 p3)
        str (rtos (vlax-curve-getarea obj) 2 3)
        ps (vlax-curve-getstartpoint nam)
        pe (vlax-curve-getendpoint nam)
      )
      (entmake (list '(0 . "CIRCLE") (cons 10 p1) (cons 40 0.01)))
      (setq nam1 (entlast))
      (setq pts (sl-Curveinters nam nam1 0))
      (entdel nam1)
      (repeat (setq k (length pts))
        (setq p0 (nth (setq k (1- k)) pts))
        (if (> (- (+ (distance p0 p1) (distance p0 p3)) (distance p1 p3)) 0)
          (setq p2 p0)
        )
      )
      (if (null p2)
        (if (> (- (+ (distance ps p1) (distance ps p3)) (distance p1 p3)) 0)
          (setq p2 ps)
          (setq p2 pe)
        )
      )
      (entmake (list '(0 . "CIRCLE") (cons 10 p3) (cons 40 0.01)))
      (setq nam1 (entlast))
      (setq pts (sl-Curveinters nam nam1 0))
      (entdel nam1)
      (repeat (setq k (length pts))
        (setq p0 (nth (setq k (1- k)) pts))
        (if (> (- (+ (distance p0 p1) (distance p0 p3)) (distance p1 p3)) 0)
          (setq p4 p0)
        )
      )
      (if (null p4)
        (if (> (- (+ (distance ps p1) (distance ps p3)) (distance p1 p3)) 0)
          (setq p4 ps)
          (setq p4 pe)
        )
      )
      (if nam$ (entdel nam$))
      (slmkwz str (e-mid nam) nil nil nil "PUB_TEXT" nil nil "m")
      (setq nam$ (entlast))
      (setq ent (entget nam$))
      (setq pts (vlax-variant-value (vla-get-coordinates obj)))
      (setq n1 (fix (vlax-curve-getparamatpoint obj (vlax-curve-getclosestpointto obj (trans p1 1 0)))))
      (setq n3 (fix (vlax-curve-getparamatpoint obj (vlax-curve-getclosestpointto obj (trans p3 1 0)))))
      (while loop
        (setq p (grread t) k (car p) p (cadr p))
        (if (= k 3) (setq loop nil))
        (if (= (setq p1-1 (inters p1 p2 p (polar p ang 2.0) nil)) nil) ;bug出在这里
          (setq p1-1 p1)
        )
        (if (= (setq p3-1 (inters p3 p4 p (polar p ang 2.0) nil)) nil) ;bug出在这里
          (setq p3-1 p3)
        )
        (setq d1 (distance p1-1 p1) d2 (distance p3-1 p3))
        (if (> d1 0)
          (progn
            (vlax-safearray-put-element pts (* n1 2) (car p1-1))
            (vlax-safearray-put-element pts (1+ (* n1 2)) (cadr p1-1))
          )
        )
        (if (> d2 0)
          (progn
            (vlax-safearray-put-element pts (* n3 2) (car p3-1))
            (vlax-safearray-put-element pts (1+ (* n3 2)) (cadr p3-1))
          )
        )
        (if (or (> d1 0) (> d2 0))
          (progn
            (vla-put-coordinates obj pts)
            (setq
              plis (e-box4 nam t)
              p0 (sl:mid (car plis) (caddr plis))
              h (* 0.2 (distance (car plis) (cadddr plis)))
            )
            (entmod (emod (emod (emod (emod ent 1 (rtos (vlax-curve-getarea obj) 2 3)) 40 h) 10 p0) 11 p0))
          )
        )
      )
    )
  )
  (setvar "osmode" oldos)
)


;;多边型顶点-->动态调整面积并显示
;; by 尘缘一生 qq 15290049
;;支持 LWPOLYLINE,POLYLINE
(defun c:kk1 (/ loop p0 e nam h obj n str k p plis pts ent lw ly cl lt)
  (setq loop t oldos (getvar "osmode"))
  (setvar "osmode" 37)
  (setq p0 (getpoint "pick point:"))
  (setq e (ssget "C" (polar p0 pi4 0.001) (polar p0 5pi4 0.001) '((0 . "LWPOLYLINE,POLYLINE"))))
  (setq nam (ssname e 0))
  (if (= (dxf1 nam 0) "POLYLINE")
    (progn
      (setq ent (entget nam) lw (linwind nam) ly (dxf1 ent 8) cl (ss-getcolor ent) lt (ss-linetype ent) pts (get-pl-pt nam))
      (entdel nam)
      (if (= (dxf1 ent 70) 9) ;闭合
        (slch:lwpolyline pts t lw ly cl 1.0) ;闭合
        (slch:lwpolyline pts nil lw ly cl 1.0) ;不闭合
      )
      (setq nam (entlast))
      (vla-put-linetype (en2obj nam) lt)
    )
  )
  (if nam
    (progn
      (setq
        obj (en2obj nam)  
        n (fix (vlax-curve-getparamatpoint obj (vlax-curve-getclosestpointto obj (trans p0 1 0))))
        str (rtos (vlax-curve-getarea obj) 2 3)
      )
      (if nam$ (entdel nam$))
      (slmkwz str (e-mid nam) nil nil nil "PUB_TEXT" nil nil "m")
      (setq nam$ (entlast))
      (setq ent (entget nam$))
      (setq pts (vlax-variant-value (vla-get-coordinates obj)))
      (while loop
        (setq p (grread t))
        (setq k (car p) p (cadr p))
        (if (= k 3) (setq loop nil))
        (setq
          plis (e-box4 nam t)
          p0 (sl:mid (car plis) (caddr plis))
          h (* 0.2 (distance (car plis) (cadddr plis)))
        )
        (vlax-safearray-put-element pts (* n 2) (car p))
        (vlax-safearray-put-element pts (1+ (* n 2)) (cadr p))
        (vla-put-coordinates obj pts)
        (entmod (emod (emod (emod (emod ent 1 (rtos (vlax-curve-getarea obj) 2 3)) 40 h) 10 p0) 11 p0))
      )
    )
  )
  (setvar "osmode" oldos)
)