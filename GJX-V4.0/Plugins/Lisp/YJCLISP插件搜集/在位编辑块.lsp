


;;��λ�༭��
(defun c:bjkuai () 
  (command "-REFEDIT" pause "o"  "A"  "N" "")
  (princ)
)
;;�����޸Ŀ�
(defun c:bckuai () 
  (command "_refclose" "S" "")
  (princ)
)

;;�����޸Ŀ�
(defun c:fqkuai () 
  (command "_refclose" "D" "")
  (princ)
)



;;�������--�����������Ƶ�ָ��λ��...........................................................................................................................................................................
(defun c:zz (/ en0 en1 en2 pt1 pt2 ss n ent-x) 
  (setvar "cmdecho" 0)
  (print "ѡ���ƵĶ���:") 
    (setq en0 (ssget))
    (while (setq en1 (car (entsel "\nѡ����ʼ����:")))
      (redraw en1 3)
      (setq pt1 (cdr (assoc 10 (entget en1))))
      (print "ѡ���յ����:")
      (setq ss (ssget))
      (setq n 0) ;��ʼ�ظ�ִ��
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












