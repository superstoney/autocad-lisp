;;; ================================
;;;    《剪切成虚线》v3.1(支持框选)
;;; 功能：将直线、圆、圆弧剪切成虚线
;;; 使用：选择到目标左键确认右键删除
;;;       a,s键调整虚线线型比例
;;;       未选择到目标时右键退出程序
;;;  by:langjs            2020.8.28
;;; ================================
(defun c:jq (/ #erryx001 $orr a b bh code e1 e2 elst1 elst2 elst3 elst4 en en1 ent f gr i j jiao1 jiao2 len len_lst loop lst lstlst
               lstlst1 mypt name name2 name3 name4 name5 nearpt nenalst nilpd obj obj1 obj2 p0 pd pdlst pend pls pn psta pt pt2 ptl
               ptlst pts r r1 r2 snap ss ss1 ss2 vc vh vs x xuname zw
            )
  (defun hh:twoentsinters (e1 e2 / obj1 obj2 ptl pts) ; 两对象交点列表
    (setq obj1 (vlax-ename->vla-object e1)
          obj2 (vlax-ename->vla-object e2)
          pts (vlax-invoke obj1 'intersectwith obj2 0)
    )
    (while pts
      (setq ptl (cons (list (car pts) (cadr pts)) ptl)
            pts (cdddr pts)
      )
    )
    ptl
  )
  (defun pypx (pt lst name / i mypt obj x) ; 返回点在对象上相邻点
    (setq obj (vlax-ename->vla-object name))
    (if (= (cdr (assoc 0 (entget name))) "CIRCLE")
      (progn
        (if (or
              (<= (vlax-curve-getdistatpoint obj pt) (vlax-curve-getdistatpoint obj (car lst)))
              (>= (vlax-curve-getdistatpoint obj pt) (vlax-curve-getdistatpoint obj (last lst)))
            )
          (progn
            (setq mypt (list (last lst) (car lst)))
          )
          (progn
            (setq i 0)
            (foreach x lst
              (if (>= (vlax-curve-getdistatpoint obj pt) (vlax-curve-getdistatpoint obj x))
                (setq i (1+ i))
              )
            )
            (if (nth i lst)
              (setq mypt (list (nth (1- i) lst) (nth i lst)))
              (setq mypt (list (nth (- i 2) lst) (nth (1- i) lst)))
            )
          )
        )
      )
      (progn
        (setq i 0)
        (foreach x lst
          (if (>= (vlax-curve-getdistatpoint obj pt) (vlax-curve-getdistatpoint obj x))
            (setq i (1+ i))
          )
        )
        (if (nth i lst)
          (if (/= i 0)
            (setq mypt (list (nth (1- i) lst) (nth i lst)))
            (setq mypt (list (car lst) (cadr lst)))
          )
          (setq mypt (list (nth (- i 2) lst) (nth (1- i) lst)))
        )
      )
    )
    mypt
  )
  (defun #erryx001 (s)
    (if (= pd "Y")
      (progn
        (foreach x nenalst
          (entdel x)
        )
        (entdel (last pdlst))
        (setq nenalst nil
              ptlst nil
              pdlst nil
              pd "N"
        )
      )
    )
    (setvar "osmode" snap)               ; 恢复捕捉
    (command ".UNDO" "E")
    (setq *error* $orr)
  )
  (defun sub (i x ent)                       ; 更新列表
    (subst
      (cons i x)
      (assoc i ent)
      ent
    )
  )
  (defun assname (name i)               ; 取得列表
    (setq ent (entget name))
    (cdr (assoc i ent))
  )
  (defun huatu (pt pd /)
    (if (setq nearpt (osnap pt "_NEA"))
      (if (and
            (setq ss (ssget "C" nearpt nearpt '((0 . "LINE,CIRCLE,ARC,LWPOLYLINE"))))
            (/= pd "Y")
          )
        (progn
          (princ (strcat "\n[左键]确认,[右键]删除,[A,S键]虚线比例<" (rtos calebak 2 2) ">:"))
          (setq name (ssname ss 0)
                obj (vlax-ename->vla-object name)
                ent (entget name)
          )
          (if (not (member name pdlst))
            (progn
              (setq pdlst (cons name pdlst)
                    mypt '()
                    i 0
              )
              (cond
                ((= (assname name 0) "LINE")
                  (setq psta (assname name 10)
                        pend (assname name 11)
                        mypt (list psta pend)
                  )
                )
                ((= (assname name 0) "CIRCLE")
                  (setq p0 (assname name 10)
                        r (assname name 40)
                        psta nil
                        pend nil
                  )
                  (repeat 360
                    (setq mypt (cons (polar p0 (* i (/ pi 180)) r) mypt)
                          i (1+ i)
                    )
                  )
                )
                ((= (assname name 0) "ARC")
                  (setq p0 (assname name 10)
                        r (assname name 40)
                        r1 (assname name 50)
                        r2 (assname name 51)
                        psta (polar p0 r1 r)
                        pend (polar p0 r2 r)
                  )
                  (if (< r2 r1)
                    (setq r2 (+ r2 pi pi))
                  )
                  (repeat 180
                    (setq mypt (cons (polar p0 (+ r1 (* i (/ (- r2 r1) 180))) r) mypt)
                          i (1+ i)
                    )
                  )
                )
                ((= (assname name 0) "LWPOLYLINE")
                  (if (= (assname name 70) 1)
                    (setq bh "Y")
                    (setq bh nil)
                  )
                  (setq mypt (mapcar
                               'cdr
                               (vl-remove-if-not '(lambda (x)
                                                    (= (car x) 10)
                                                  ) ent
                               )
                             )
                        psta (car mypt)
                  )
                  (if (= bh "Y")
                    (setq pend (polar (last mypt) (angle (last mypt) psta) (- (distance (last mypt) psta) 0.01))
                          mypt (reverse (cons pend (reverse mypt)))
                    )
                  )
                  (setq pend (last mypt))
                )
              )
              (setq vc (trans (getvar "viewctr") 1 2) ; 计算当前窗口坐标
                    vh (getvar "viewsize")
                    vs (mapcar
                         '/
                         (list (* (apply
                                    '/
                                    (getvar "screensize")
                                  ) vh
                               ) vh
                         )
                         '(2 2)
                       )
              )
              (setq zw (mapcar
                         '(lambda (f)
                            (trans (mapcar
                                     f
                                     vc
                                     vs
                                   ) 2 1
                            )
                          )
                         '(- +)
                       )
              )
              (command "zoom" "A")
              (if (setq ss (ssget "f" mypt '((0 . "LINE,CIRCLE,ARC,ELLIPSE,LWPOLYLINE,SPLINE"))))
                (progn
                  (if (= (assname name 0) "CIRCLE")
                    (if (not (setq ss (ssdel name ss)))
                      (setq ss (ssadd))
                    )
                  )
                  (if (>= (sslength ss) 1)
                    (progn
                      (repeat (setq i (sslength ss))
                        (setq name2 (ssname ss (setq i (1- i)))
                              ptlst (append
                                      (hh:twoentsinters name name2)
                                      ptlst
                                    )
                        )
                      )
                      (setq ptlst (append
                                    (if psta
                                      (list psta)
                                    )
                                    ptlst
                                    (if pend
                                      (list pend)
                                    )
                                  )
                            len_lst '()
                      )
                      (foreach x ptlst
                        (setq len (vlax-curve-getdistatpoint obj x)
                              len_lst (cons (list len x) len_lst)
                        )
                      )
                      (setq len_lst (vl-sort len_lst '(lambda (a b)
                                                        (< (car a) (car b))
                                                      )
                                    )
                      )
                      (setq ptlst (mapcar
                                    'cadr
                                    len_lst
                                  )
                      )
                      (setq pls (pypx nearpt ptlst name))
                      (cond
                        ((= (assname name 0) "LINE")
                          (if (not (equal psta (car pls) 0.0001))
                            (progn
                              (setq en (cdr ent)
                                    en (sub 10 psta en)
                              )
                              (entmake (sub 11 (car pls) en))
                              (setq nenalst (cons (entlast) nenalst))
                            )
                          )
                          (setq en (cdr ent)
                                en (sub 10 (car pls) en)
                                en (sub 11 (cadr pls) en)
                                en1 (reverse (cons (cons 48 calebak) (reverse en)))
                          )
                          (entmake (sub 8 "4虚线层" en1))
                          (setq xuname (entlast)
                                nenalst (cons xuname nenalst)
                                pdlst (cons xuname pdlst)
                                pd "Y"
                          )
                          (if (not (equal pend (cadr pls) 0.0001))
                            (progn
                              (setq en (cdr ent)
                                    en (sub 10 (cadr pls) en)
                              )
                              (entmake (sub 11 pend en))
                              (setq nenalst (cons (entlast) nenalst))
                            )
                          )
                          (entdel name)
                        )
                        ((= (assname name 0) "CIRCLE")
                          (setq en (cdr ent)
                                p0 (cdr (assoc 10 ent))
                                en (sub 0 "ARC" en)
                                en (append
                                     en
                                     (list (cons 50 (angle p0 (cadr pls))))
                                     (list (cons 51 (angle p0 (car pls))))
                                   )
                          )
                          (entmake en)
                          (setq nenalst (cons (entlast) nenalst)
                                en (sub 8 "4虚线层" en)
                                en (sub 50 (angle p0 (car pls)) en)
                                en (sub 51 (angle p0 (cadr pls)) en)
                                en1 (reverse (cons (cons 48 calebak) (reverse en)))
                          )
                          (entmake en1)
                          (setq xuname (entlast)
                                nenalst (cons xuname nenalst)
                                pdlst (cons xuname pdlst)
                                pd "Y"
                          )
                          (entdel name)
                        )
                        ((= (assname name 0) "ARC")
                          (setq en (cdr ent)
                                p0 (cdr (assoc 10 ent))
                          )
                          (if (not (equal psta (car pls) 0.0001))
                            (progn
                              (setq en (sub 50 (angle p0 psta) en)
                                    en (sub 51 (angle p0 (car pls)) en)
                              )
                              (entmake en)
                              (setq nenalst (cons (entlast) nenalst))
                            )
                          )
                          (if (not (equal pend (cadr pls) 0.0001))
                            (progn
                              (setq en (sub 50 (angle p0 (cadr pls)) en)
                                    en (sub 51 (angle p0 pend) en)
                              )
                              (entmake en)
                              (setq nenalst (cons (entlast) nenalst))
                            )
                          )
                          (setq en (sub 8 "4虚线层" en)
                                en (sub 50 (angle p0 (car pls)) en)
                                en (sub 51 (angle p0 (cadr pls)) en)
                                en1 (reverse (cons (cons 48 calebak) (reverse en)))
                          )
                          (entmake en1)
                          (setq xuname (entlast)
                                nenalst (cons xuname nenalst)
                                pdlst (cons xuname pdlst)
                                pd "Y"
                          )
                          (entdel name)
                        )
                        ((= (assname name 0) "LWPOLYLINE")
                          (setq len_lst '()
                                en (cdr ent)
                                en (sub 70 0 en)
                                pn (list 10 (car psta) (cadr psta))
                                elst1 (reverse (cdr (member pn (reverse en))))
                          )
                          (if (= bh "Y")
                            (setq jiao1 (cadr ptlst)
                                  jiao2 (cadr (reverse ptlst))
                            )
                          )
                          (if (not (equal psta (car pls) 0.0001))
                            (setq mypt (cons (car pls) mypt))
                            (if (= bh "Y")
                              (setq mypt (cons jiao2 mypt))
                            )
                          )
                          (if (not (equal pend (cadr pls) 0.0001))
                            (setq mypt (cons (cadr pls) mypt))
                            (if (= bh "Y")
                              (setq mypt (cons jiao1 mypt))
                            )
                          )
                          (foreach x mypt
                            (setq len (vlax-curve-getdistatpoint obj x)
                                  len_lst (cons (list len x) len_lst)
                            )
                          )
                          (setq len_lst (vl-sort len_lst '(lambda (a b)
                                                            (< (car a) (car b))
                                                          )
                                        )
                                mypt (mapcar
                                       'cadr
                                       len_lst
                                     )
                          )
                          (if (= bh "Y")
                            (progn
                              (if (equal pend (cadr pls) 0.0001)
                                (progn
                                  (setq elst2 (reverse (member jiao1 (reverse mypt))))
                                  (setq elst3 (reverse (cdr (reverse (member jiao2 mypt)))))
                                  (setq elst2 (append
                                                elst3
                                                elst2
                                              )
                                  )
                                  (setq elst2 (mapcar
                                                '(lambda (pt)
                                                   (cons 10 pt)
                                                 )
                                                elst2
                                              )
                                  )
                                  (setq elst1 (sub 90 (length elst2) elst1))
                                  (setq elst2 (append
                                                elst1
                                                elst2
                                              )
                                  )
                                  (setq elst2 (sub 8 "4虚线层" elst2))
                                  (setq elst2 (reverse (cons (cons 48 calebak) (reverse elst2))))
                                  (entmake elst2)
                                  (setq xuname (entlast)
                                        nenalst (cons (entlast) nenalst)
                                        pdlst (cons (entlast) pdlst)
                                        pd "Y"
                                  )
                                  (setq elst2 (member jiao1 mypt))
                                  (setq elst2 (reverse (member jiao2 (reverse elst2))))
                                  (setq elst1 (sub 90 (length elst2) elst1))
                                  (setq elst2 (mapcar
                                                '(lambda (pt)
                                                   (cons 10 pt)
                                                 )
                                                elst2
                                              )
                                  )
                                  (setq elst2 (append
                                                elst1
                                                elst2
                                              )
                                  )
                                  (entmake elst2)
                                  (setq nenalst (cons (entlast) nenalst))
                                )
                                (if (equal psta (car pls) 0.0001)
                                  (progn
                                    (setq elst2 (mapcar
                                                  '(lambda (pt)
                                                     (cons 10 pt)
                                                   )
                                                  pls
                                                )
                                    )
                                    (setq elst1 (sub 90 (length elst2) elst1))
                                    (setq elst2 (append
                                                  elst1
                                                  elst2
                                                )
                                    )
                                    (setq elst2 (sub 8 "4虚线层" elst2))
                                    (setq elst2 (reverse (cons (cons 48 calebak) (reverse elst2))))
                                    (entmake elst2)
                                    (setq xuname (entlast)
                                          nenalst (cons (entlast) nenalst)
                                          pdlst (cons (entlast) pdlst)
                                          pd "Y"
                                    )
                                    (setq elst2 (member (cadr pls) ptlst))
                                    (setq elst1 (sub 90 (length elst2) elst1))
                                    (setq elst2 (mapcar
                                                  '(lambda (pt)
                                                     (cons 10 pt)
                                                   )
                                                  elst2
                                                )
                                    )
                                    (setq elst2 (append
                                                  elst1
                                                  elst2
                                                )
                                    )
                                    (entmake elst2)
                                    (setq nenalst (cons (entlast) nenalst))
                                  )
                                  (progn
                                    (setq elst2 (member (car pls) mypt))
                                    (setq elst2 (reverse (member (cadr pls) (reverse elst2))))
                                    (setq elst2 (mapcar
                                                  '(lambda (pt)
                                                     (cons 10 pt)
                                                   )
                                                  elst2
                                                )
                                    )
                                    (setq elst1 (sub 90 (length elst2) elst1))
                                    (setq elst2 (append
                                                  elst1
                                                  elst2
                                                )
                                    )
                                    (setq elst2 (sub 8 "4虚线层" elst2))
                                    (setq elst2 (reverse (cons (cons 48 calebak) (reverse elst2))))
                                    (entmake elst2)
                                    (setq xuname (entlast)
                                          nenalst (cons (entlast) nenalst)
                                          pdlst (cons (entlast) pdlst)
                                          pd "Y"
                                    )
                                    (setq elst2 (reverse (cdr (reverse (member (cadr pls) mypt)))))
                                    (setq elst3 (reverse (member (car pls) (reverse mypt))))
                                    (setq elst2 (append
                                                  elst2
                                                  elst3
                                                )
                                    )
                                    (setq elst2 (mapcar
                                                  '(lambda (pt)
                                                     (cons 10 pt)
                                                   )
                                                  elst2
                                                )
                                    )
                                    (setq elst1 (sub 90 (length elst2) elst1))
                                    (setq elst2 (append
                                                  elst1
                                                  elst2
                                                )
                                    )
                                    (entmake elst2)
                                    (setq nenalst (cons (entlast) nenalst))
                                  )
                                )
                              )
                            )
                            (progn
                              (if (not (equal psta (car pls) 0.001))
                                (progn
                       
                                  (setq elst2 (reverse (member (car pls) (reverse mypt)))
                                        elst2 (mapcar
                                                '(lambda (pt)
                                                   (cons 10 pt)
                                                 )
                                                elst2
                                              )
                                        elst1 (sub 90 (length elst2) elst1)
                                        elst2 (append
                                                elst1
                                                elst2
                                              )
                                  )
                                  (entmake elst2)
                                  (setq nenalst (cons (entlast) nenalst))
                                )
                              )
                              (setq elst3 (member (car pls) mypt)
                                    elst3 (reverse (member (cadr pls) (reverse elst3)))
                                    elst3 (mapcar
                                            '(lambda (pt)
                                               (cons 10 pt)
                                             )
                                            elst3
                                          )
                                    elst1 (sub 90 (length elst3) elst1)
                                    elst3 (append
                                            elst1
                                            elst3
                                          )
                                    elst3 (sub 8 "4虚线层" elst3)
                                    en1 (reverse (cons (cons 48 calebak) (reverse elst3)))
                              )
                              (entmake en1)
                              (setq xuname (entlast)
                                    nenalst (cons (entlast) nenalst)
                                    pdlst (cons (entlast) pdlst)
                                    pd "Y"
                              )
                              (if (not (equal pend (cadr pls) 0.0001))
                                (progn
                                  (setq elst4 (member (cadr pls) mypt)
                                        elst4 (mapcar
                                                '(lambda (pt)
                                                   (cons 10 pt)
                                                 )
                                                elst4
                                              )
                                        elst1 (sub 90 (length elst4) elst1)
                                        elst4 (append
                                                elst1
                                                elst4
                                              )
                                  )
                                  (entmake elst4)
                                  (setq nenalst (cons (entlast) nenalst))
                                )
                              )
                            )
                          )
                          (entdel name)
                        )
                      )
                    )
                    (progn
                      (setq en (cdr ent)
                            en (sub 8 "4虚线层" en)
                            en1 (reverse (cons (cons 48 calebak) (reverse en)))
                      )
                      (entmake en1)
                      (setq xuname (entlast)
                            nenalst (cons xuname nenalst)
                            pdlst (cons xuname pdlst)
                            pd "Y"
                      )
                      (entdel name)
                    )
                  )
                )
                (progn
                  (princ "\n非有效选择，请先缩小窗口")
                  (setq nenalst nil
                        ptlst nil
                        pdlst nil
                        pd "N"
                  )
                )
              )
              (command "zoom" "W" (car zw) (cadr zw))
            )
          )
        )
        (if (setq ss (ssget "C" nearpt nearpt '((0 . "LINE,CIRCLE,ARC,LWPOLYLINE"))))
          (progn
            (setq name3 (ssname ss 0))
            (if (and
                  (= pd "Y")
                  (not (member name3 pdlst))
                )
              (progn
                (foreach x nenalst
                  (entdel x)
                )
                (entdel (last pdlst))
                (setq nenalst nil
                      ptlst nil
                      pdlst nil
                      pd "N"
                )
              )
            )
          )
        )
      )
      (progn
        (if (= pd "Y")
          (progn
            (foreach x nenalst
              (entdel x)
            )
            (entdel (last pdlst))
            (setq nenalst nil
                  ptlst nil
                  pdlst nil
                  pd "N"
            )
            (princ "\n请指定对象,[右键]退出:")
          )
        )
      )
    )
    pd
  )
  (setq $orr *error*)
  (setq *error* #erryx001)
  (setvar "cmdecho" 0)                       ; 关闭命令响应
  (command ".UNDO" "BE")
  (setq snap (getvar "osmode"))               ; 关闭捕捉
  (setvar "osmode" 0)
  (vl-load-com)
  (if (null (tblsearch "ltype" "DASHED"))
    (command "-linetype" "L" "DASHED" "" "")
  )
  (if (= (tblsearch "layer" "4虚线层") nil)
    (command "layer" "new" "4虚线层" "c" 6 "4虚线层" "lt" "DASHED" "4虚线层" "")
  )
  (setq loop t
        pdlst '()
        ptlst '()
        pd nil
  )
  (if (null calebak)
    (setq calebak 1.0)
  )
  (princ "\n请指定对象,[右键]退出:")
  (while loop
    (setq gr (grread t 15 2)
          code (car gr)
          pt (cadr gr)
          bh nil
    )
    (cond
      ((= code 2)                       ; 键盘
        (setq i (cond
                  ((< calebak 0.1)
                    0.01
                  )
                  ((< calebak 1)
                    0.1
                  )
                  ((< calebak 10)
                    1.0
                  )
                  ((< calebak 100)
                    10.0
                  )
                  ((< calebak 1000)
                    100.0
                  )
                  (t
                    0
                  )
                )
        )
        (setq j (cond
                  ((<= calebak 0.02)
                    0
                  )
                  ((<= calebak 0.2)
                    0.01
                  )
                  ((<= calebak 1)
                    0.10
                  )
                  ((<= calebak 10)
                    1.0
                  )
                  ((<= calebak 100)
                    10.0
                  )
                  ((<= calebak 1000)
                    100.0
                  )
                  (t
                    0
                  )
                )
        )
        (cond
          ((member (vl-list->string (cdr gr)) '("A" "a"))
            (setq calebak (- calebak j))
            (if xuname
              (entmod (sub 48 calebak (entget xuname)))
            )
          )
          ((member (vl-list->string (cdr gr)) '("S" "s"))
            (setq calebak (+ calebak i))
            (if xuname
              (entmod (sub 48 calebak (entget xuname)))
            )
          )
        )
        (redraw)
        (princ (strcat "\n[左键]确认,[右键]删除,[A,S键]虚线比例<" (rtos calebak 2 2) ">:"))
      )
      ((= code 3)                       ; 鼠标左击
        (if (= pd "Y")
          (setq nenalst nil
                ptlst nil
                pdlst nilpd
                pd "N"
          )
          (progn
            (if (setq pt2 (getcorner pt "\n窗交对象,指定对角点:"))
              (progn
                (if (setq ss1 (ssget "C" pt pt2))
                  (progn
                    (setq lstlst1 '())
                    (setq lstlst (list pt (list (car pt) (cadr pt2)) pt2 (list (car pt2) (cadr pt)) pt))
                    (entmake (append
                               (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 90 (length lstlst)))
                               (mapcar
                                 '(lambda (pt)
                                    (cons 10 pt)
                                  )
                                 lstlst
                               )
                             )
                    )
                    (setq name4 (entlast))
                    (repeat (setq i (sslength ss1))
                      (setq name5 (ssname ss1 (setq i (1- i))))
                      (setq lstlst1 (append
                                      lstlst1
                                      (hh:twoentsinters name4 name5)
                                    )
                      )
                    )
                    (entdel name4)
                    (foreach i lstlst1
                      (huatu i pd)
                    )
                    (setq nenalst '()
                          pdlst '()
                    )
                    (if (setq ss2 (ssget "W" pt pt2))
                      (repeat (setq i (sslength ss2))
                        (setq name4 (entget (ssname ss2 (setq i (1- i)))))
                        (setq name4 (sub 8 "4虚线层" name4))
                        (entmod (reverse (cons (cons 48 calebak) (reverse name4))))
                      )
                    )
                    (princ "\n请指定对象,[右键]退出:")
                  )
                )
              )
            )
          )
        )
      )
      ((or
         (= code 11)
         (= code 25)
       )                               ; 鼠标右击
        (if (= pd "Y")
          (progn
            (princ "\n请指定对象,[右键]退出:")
            (entdel (car pdlst))
            (setq nenalst nil
                  ptlst nil
                  pdlst nil
                  pd "N"
            )
          )
          (progn
            (setq loop nil)
          )
        )
      )
      ((= code 5)                       ; 鼠标移动
        (setq pd (huatu pt pd))
      )
    )
  )
  (setvar "osmode" snap)               ; 恢复捕捉
  (setq *error* $orr)
  (command ".UNDO" "E")
  (princ)
)
(princ)