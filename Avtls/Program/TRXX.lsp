;�����޼�
(defun c:trr (/ *error* dd os p0 p1 ss)
	(defun *ERROR* (msg)
		(setvar "osmode" os)
		(vla-EndUndoMark *doc*)
		(setvar "CMDECHO" 1)
		(princ)
	)
	(setvar "cmdecho" 0)
	(vla-StartUndoMark *doc*)
	(setq os (getvar "osmode"));��¼����ԭʼϵͳ����
	(setvar "osmode" 0);�رղ�׽
	(princ "\n�����ʼ�����:")
	(while (setq p0 (getpoint));��ʼ��
		(setq dd (* 0.01 (getvar "viewsize")));���������߳���
		(while (= 5 (car (setq p1 (grread t 4 0))));��׽����ƶ�
			(setq p1 (cadr p1));���λ��
			(if (>= (distance p0 p1) dd);�ƶ��������Ȳż���
				(progn
					(grdraw p0 p1 1);��Ļ��ʾʸ����
					(command "trim" "" "f" p0 p1 "" "");�޼�
					(if (setq ss (ssget "f" (list p0 p1)));��ȡ�Ƚ���û���޼���
						(command "erase" ss "");ɾ��
					)
					(setq p0 p1);����pΪ��һ��
				)
			)
		)
		(redraw)
	)
	(*ERROR* msg)
)
(princ)