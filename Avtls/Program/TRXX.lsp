;超级修剪
(defun c:trr (/ *error* dd os p0 p1 ss)
	(defun *ERROR* (msg)
		(setvar "osmode" os)
		(vla-EndUndoMark *doc*)
		(setvar "CMDECHO" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(vla-StartUndoMark *doc*)
	(setq os (getvar "osmode"));记录两个原始系统变量
	(setvar "osmode" 0);关闭捕捉
	(princ "\n点击开始或结束:")
	(while (setq p0 (getpoint));开始点
		(setq dd (* 0.01 (getvar "viewsize")));设置随意线长度
		(while (= 5 (car (setq p1 (grread t 4 0))));捕捉鼠标移动
			(setq p1 (cadr p1));鼠标位置
			(if (>= (distance p0 p1) dd);移动超过长度才继续
				(progn
					(grdraw p0 p1 1);屏幕显示矢量线
					(command "trim" "" "f" p0 p1 "" "");修剪
					(if (setq ss (ssget "f" (list p0 p1)));获取先交又没有修剪的
						(command "erase" ss "");删除
					)
					(setq p0 p1);设置p为后一点
				)
			)
		)
		(redraw)
	)
	(*ERROR* msg)
)
(princ)