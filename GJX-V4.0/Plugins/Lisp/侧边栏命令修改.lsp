; 命令修改
(defun c:XX ()
  (command "hhyjc" )
  (princ)
)
; 加载路径
(defun LoadFileDirYJC ()
(setq LoadFileDir "D:\OneDrive - tju.edu.cn\ProgramData\autocad\04 LISP程序\GJX-V4.0\Plugins")
(princ)
)
(LoadFileDirYJC)
