(defun c:CreateLayer (/ layerName)
  ; ��ʾ��������
  (princ "\n*** �Ѽ��� CreateLayer ���� ***")
  (princ "\n�������� CreateLayer ��ʼ����ͼ��")

  ; ��ȡ�û������ͼ������
  (setq layerName (getstring t "\n������ͼ������: "))

  ; ������ͼ�㣨��������ڣ�����Ϊ��ǰ
  (command "_.-layer" "_m" layerName "")

  ; ��ʽ���õ�ǰͼ�㣨ȷ�������ɹ���
  (setvar "CLAYER" layerName)

  ; ��ʾ�������
  (princ (strcat "\n�Ѵ������л���ͼ��: " layerName))
  (princ)
)

; ����ʱ��ʾ��ʾ��Ϣ
(princ "\n*** CreateLayer �����Ѽ��� ***")
(princ "\n�������� CreateLayer ��ʼ����ͼ��")
(princ)