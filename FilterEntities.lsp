(defun C:SelFilter (/ choice filterList filterFunc ss result new_ss)
  ;; ��ʾѡ����ʾ <button class="citation-flag" data-index="1"><button class="citation-flag" data-index="6">
  (initget "�� �����(�պ�) �����(���պ�) �������� �������� ���������� �������")
  (setq choice (getkword "\n��ѡ��Ҫɸѡ��Ԫ������ [��/�����(�պ�)/�����(���պ�)/��������/��������/����������/�������]: "))
  
  ;; ���ù�����������֤���� <button class="citation-flag" data-index="2"><button class="citation-flag" data-index="7">
  (cond
    ((= choice "��") 
      (setq filterList '((0 . "POINT"))))
    
    ((= choice "�����(�պ�)") 
      (setq filterList '((0 . "LWPOLYLINE"))
            filterFunc '(lambda (ent) 
                         (= 1 (logand 1 (cdr (assoc 70 (entget ent)))))))  ;; �����պ��ж� <button class="citation-flag" data-index="2">
    )
    
    ((= choice "�����(���պ�)") 
      (setq filterList '((0 . "LWPOLYLINE"))
            filterFunc '(lambda (ent) 
                         (= 0 (logand 1 (cdr (assoc 70 (entget ent)))))))
    )
    
    ((= choice "��������") 
      (setq filterList '((0 . "TEXT"))))
    
    ((= choice "��������") 
      (setq filterList '((0 . "MTEXT"))))
    
    ((= choice "����������") 
      (setq filterList '((0 . "AECC_TIN_SURFACE"))))
    
    ((= choice "�������") 
      (setq filterList '((0 . "AECC_VOLUME_SURFACE"))))
  )
  
  ;; ��ȡ��ѡ���� <button class="citation-flag" data-index="3"><button class="citation-flag" data-index="6">
  (princ "\n��ѡҪɸѡ�Ķ���: ")
  (setq ss (ssget '((0 . "*") 
                    (-4 . "<OR") 
                    (0 . "POINT,LWPOLYLINE,TEXT,MTEXT,AECC_TIN_SURFACE,AECC_VOLUME_SURFACE") 
                    (-4 . "OR>"))))
  
  ;; ������ѡ�д��� <button class="citation-flag" data-index="4"><button class="citation-flag" data-index="6">
  (if ss
    (progn
      ;; ˫�ع��˻��ƣ������ͺ����� <button class="citation-flag" data-index="7">
      (setq result 
        (vl-remove-if-not
          (lambda (ent)
            (and 
              (wcmatch (cdr (assoc 0 (entget ent))) (cdr (assoc 0 filterList)))
              (if filterFunc (eval filterFunc) T)  ;; Ӧ�ñպ�״̬��֤ <button class="citation-flag" data-index="2">
            )
          )
          (mapcar 'cadr (ssnamex ss))
        )
      )
      
      ;; ����������ѡ�� <button class="citation-flag" data-index="6">
      (if (> (length result) 0)
        (progn
          (setq new_ss (ssadd))
          (foreach ent result (ssadd ent new_ss))
          (sssetfirst nil new_ss)  ;; �ؼ�����������ѡ��״̬ <button class="citation-flag" data-index="6">
          (princ (strcat "\n��ѡ�� " (itoa (length result)) " ��" choice))
        )
        (princ "\nδ�ҵ����������Ķ���")
      )
    )
    (princ "\nδѡ���κζ���")
  )
  (princ)
)  ;; �˴���ȫ���ȱʧ�������� <button class="citation-flag" data-index="9">