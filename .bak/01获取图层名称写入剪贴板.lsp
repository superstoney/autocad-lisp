(defun c:CopyLayerNameToClipboard ()
  (vl-load-com)
  ;; 提示用户选择一个对象
  (if (setq sel (ssget))
    (progn
      ;; 获取选中的第一个对象
      (setq ent (vlax-ename->vla-object (ssname sel 0)))
      ;; 获取对象的图层名称
      (setq layerName (vla-get-Layer ent))
      ;; 使用Windows API函数将图层名称复制到剪贴板
      (setq clipdata (vlax-create-object "htmlfile"))
      (vlax-invoke-method (vlax-get-property (vlax-get-property clipdata 'parentWindow) 'clipboardData) 'setData "Text" layerName)
      (vlax-release-object clipdata)
      (princ (strcat "\n图层名称 " layerName " 已复制到剪贴板。"))
    )
    (princ "\n未选择任何对象。")
  )
  (princ)
)