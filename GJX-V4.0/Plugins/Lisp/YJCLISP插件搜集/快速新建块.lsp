(defun C:KSJK (/ TKname ss pt ent entData)
  ;(setvar "cmdecho" 0)
  (setq TKname (menucmd "M=$(edtime,$(getvar,date),YYYYMODDHHMMSS)"))
  (print "----------��ѡ����Ҫ����Ķ���----------:")
  (setq ss (ssget))
  (if (= ss nil) 
    (print "ѡ��Ķ���Ϊ�գ�������ѡ��")
    (progn 
      ;; ��ȡ��һ������Ļ���
      (setq ent (ssname ss 0))       ; ��ȡѡ���е�һ������
      (setq entData (entget ent))   ; ��ȡ�ö���������б�
      (setq pt (cdr (assoc 10 entData))) ; ��ȡ���㣨assoc 10 ͨ���ǲ���㣩
      (if (not pt) (setq pt '(0 0 0)))   ; ���û�л��㣬Ĭ����Ϊ(0 0 0)
      ;; ������
      (command "-block" TKname "o" "c" pt ss "")
      (princ (strcat "\n����ɿ���½�,����Ϊ<" TKname ">"))
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)
