


;;在位编辑块
(defun c:bjkuai () 
  (command "-REFEDIT" pause "o"  "A"  "N" "")
  (princ)
)
;;保存修改块
(defun c:bckuai () 
  (command "_refclose" "S" "")
  (princ)
)

;;放弃修改块
(defun c:fqkuai () 
  (command "_refclose" "D" "")
  (princ)
)



;;命令改造--快速批量复制到指定位置...........................................................................................................................................................................
(defun c:zz (/ en0 en1 en2 pt1 pt2 ss n ent-x) 
  (setvar "cmdecho" 0)
  (print "选择复制的对象:") 
    (setq en0 (ssget))
    (while (setq en1 (car (entsel "\n选择起始基点:")))
      (redraw en1 3)
      (setq pt1 (cdr (assoc 10 (entget en1))))
      (print "选择终点基点:")
      (setq ss (ssget))
      (setq n 0) ;开始重复执行
      (repeat (sslength ss) 
        (setq ent-x (ssname ss n))
        (setq pt2 (cdr (assoc 10 (entget ent-x))))
        (command "Copy" en0 "" pt1 pt2 "")
        (setq n (1+ n))
      )
      (redraw en1 4)
    )
    (setvar "cmdecho" 1)
    (princ)
  )












