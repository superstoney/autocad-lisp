(defun c:CopyLayerNameToClipboard ()
  (vl-load-com)
  ;; ��ʾ�û�ѡ��һ������
  (if (setq sel (ssget))
    (progn
      ;; ��ȡѡ�еĵ�һ������
      (setq ent (vlax-ename->vla-object (ssname sel 0)))
      ;; ��ȡ�����ͼ������
      (setq layerName (vla-get-Layer ent))
      ;; ʹ��Windows API������ͼ�����Ƹ��Ƶ�������
      (setq clipdata (vlax-create-object "htmlfile"))
      (vlax-invoke-method (vlax-get-property (vlax-get-property clipdata 'parentWindow) 'clipboardData) 'setData "Text" layerName)
      (vlax-release-object clipdata)
      (princ (strcat "\nͼ������ " layerName " �Ѹ��Ƶ������塣"))
    )
    (princ "\nδѡ���κζ���")
  )
  (princ)
)