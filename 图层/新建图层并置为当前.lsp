(defun c:CreateLayer (/ layerName)
  ; 显示程序名称
  (princ "\n*** 已加载 CreateLayer 程序 ***")
  (princ "\n输入命令 CreateLayer 开始创建图层")

  ; 获取用户输入的图层名称
  (setq layerName (getstring t "\n请输入图层名称: "))

  ; 创建新图层（如果不存在）并置为当前
  (command "_.-layer" "_m" layerName "")

  ; 显式设置当前图层（确保操作成功）
  (setvar "CLAYER" layerName)

  ; 提示操作完成
  (princ (strcat "\n已创建并切换到图层: " layerName))
  (princ)
)

; 加载时显示提示信息
(princ "\n*** CreateLayer 程序已加载 ***")
(princ "\n输入命令 CreateLayer 开始创建图层")
(princ)