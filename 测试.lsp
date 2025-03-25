;;;; AutoCAD 面积计算与复制程序
;;;; 作者: superstoney
;;;; 日期: 2024-01-09
;;;; 功能说明: 计算选定对象的面积并自动复制到剪贴板
;;;; 支持对象: 圆形、椭圆、直线、多段线、样条曲线、圆弧、填充图案

;; 将文本复制到系统剪贴板的函数
(defun SET-CLIP-STRING (STR / HTML RESULT)
    ;; 检查输入是否为字符串，并执行剪贴板操作
    (and (= (type STR) 'STR)
         (vl-catch-all-apply
           '(lambda ()
              ;; 创建 HTML 文件对象用于访问剪贴板
              (setq HTML (vlax-create-object "htmlfile"))
              ;; 设置剪贴板内容
              (setq RESULT (vlax-invoke
                            (vlax-get (vlax-get HTML 'PARENTWINDOW)
                                    'CLIPBOARDDATA)
                            'SETDATA
                            "Text"
                            STR))
              ;; 释放 HTML 对象
              (vlax-release-object HTML)
              T))
    )
)

;; 主命令函数：计算面积并复制到剪贴板
(defun C:CopyAreaToClipboard (/ CURVE TAREA SS N SUMAREA old_error *error*)
  ;; 定义错误处理函数
  (defun *error* (msg)
    (if old_error (setq *error* old_error))
    (if (= 'vla-object (type CURVE))
      (vlax-release-object CURVE)
    )
    (princ (strcat "\n错误: " msg))
    (princ)
  )
  (setq old_error *error*)
  
  (if (not (vl-load-com))
    (progn
      (princ "\n无法加载 COM 接口")
      (exit)
    )
  )
  (princ "\n请选择要计算面积的对象...")
  
  (setq SUMAREA 0)
  (if (setq SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC,HATCH"))))
    (progn
      (setq N 0)
      (repeat (sslength SS)
        (if (setq CURVE (vl-catch-all-apply 'vlax-ename->vla-object (list (ssname SS N))))
          (if (vl-catch-all-error-p CURVE)
            (princ (strcat "\n跳过无效对象: " (vl-princ-to-string N)))
            (progn
              (setq TAREA (vla-get-area CURVE))
              (setq SUMAREA (+ SUMAREA TAREA))
            )
          )
        )
        (setq N (1+ N))
      )
      
      (if (> SUMAREA 0)
        (progn
          (SET-CLIP-STRING (rtos SUMAREA 2 3))
          (setq c SUMAREA)
          (princ (strcat "\n总面积: " (rtos SUMAREA 2 3)))
          (princ "\n面积已复制到剪贴板")
        )
        (princ "\n计算面积为0或无效")
      )
    )
    (princ "\n未选择任何对象，操作已取消")
  )
  (princ)
)

;; 添加命令别名 CA 用于快速调用
(defun C:CA () (C:CopyAreaToClipboard))

;; 提示信息
(princ "\n面积计算程序已加载")
(princ "\n输入 CopyAreaToClipboard 或 CA 运行命令")
(princ)