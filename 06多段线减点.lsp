;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defun c:ddxjd( / XTBL_MingLingTiShi   ang1_input   ang2_input  e  pts  n  have_arc  first_lst  )
	;������
	(setq XTBL_MingLingTiShi (getvar "cmdecho"))	;��ȡ��ǰ����ͨ������ʾ״̬
	(if (/= dis nil)
		(progn 
			(setq dis (getdist (strcat "\n��ָ����С���<�ϴ��� " (rtos dis 2 3) " ��Ĭ����nil����Ҫֱ�ӻس�>��")))
			(while (not dis)
				(setq dis (getdist (strcat "\n��ָ����С���<�ϴ��� nil ����Ҫֱ�ӻس�>��")))
			)
		)
		
		(progn 
			(setq dis (getdist "\n��ָ����С��ࣺ"))
			(while (not dis)
				(setq dis (getdist "\n��ָ����С��ࣺ"))
			)
		)
	)
	
	(if (or (= myang1 nil) (= myang1 ""))
		(progn
			(setq ang1_input (getstring (strcat "\n����������һ�ߵĽǶ��������<��λ �� ��Ĭ��Ϊ 0.1 >��" )))
			(if (/= ang1_input "")
				(setq myang1 ang1_input)
				(setq myang1 "0.1")
			)			
		); ;��ang1��ֵΪnil���ʱ��ִ�д�progn
		
		(progn
			(setq ang1_input (getstring (strcat "\n����������һ�ߵĽǶ��������<��λ �� ��Ĭ�� " myang1 " >��")))
			(if (/= ang1_input "")
				(setq myang1 ang1_input)
			)
		); ��ang1��ֵΪ�Ȳ���nilҲ���ǿ�ʱ��ִ�д�progn
	);end if
	
	(if (or (= myang2 nil) (= myang2 ""))
		(progn
			(setq ang2_input (getstring (strcat "\n�����������߶μнǴ��ڶ��ٶ�ʱ������<ȡֵ��Χ�� 0~90�� ��Ĭ��Ϊ 15 >��" )))
			(if (/= ang2_input "")
				(setq myang2 ang2_input)
				(setq myang2 "15")
			)			
		); ;��ang2��ֵΪnil���ʱ��ִ�д�progn
		
		(progn
			(setq ang2_input (getstring (strcat "\n�����������߶μнǴ��ڶ��ٶ�ʱ������<ȡֵ��Χ�� 0~90�� ��Ĭ�� " myang2 " >��")))
			(if (/= ang2_input "")
				(setq myang2 ang2_input)
			)
		); ��ang2��ֵΪ�Ȳ���nilҲ���ǿ�ʱ��ִ�д�progn
	);end if	
	
	(setq e (car (entsel "\nѡ�����ߣ�")))
	(while (/= (cdr (assoc 0 (entget e))) "LWPOLYLINE")
		(setq e (car (entsel "\nѡ�����ߣ�")))
	);end while,	
	
	(setq pts (ff:polyline_vertices->lst e))	;��ȡ����߶��������б�,д����pts������ӱ���2��Ԫ�أ����α�ʾ����߶������ꡢԲ��Բ������
	(setq n (length pts))	;��ȡ���pts���ӱ����
	(setq have_arc nil)	;����have_arc���������ڱ�ǵ��pts���Ƿ���Բ������
	(repeat n
		(setq first_lst (car pts))
		(setq pts (cdr pts))
		(if (> (length first_lst) 1) 
			(setq have_arc T) ;�������ӱ�����Բ������ʱ��������have_arc��ֵ��ΪT
		);end if,
	)
	
	(command "_.undo" "_be")
	(if (= have_arc nil)
		(progn
			(ff:ddxJD1 e  dis myang1  myang2)
		);������Բ���Ķ����
		
		(progn
			(ff:ddxJD2 e  dis myang1  myang2)
		);������Բ���Ķ����
	)
	(command "_.undo" "_e")
	
	(setvar "cmdecho" XTBL_MingLingTiShi) 	;�ָ���ͨ������ʾԭ������ʾ״̬
	(princ)
	
)



(defun ff:ddxJD1 ( e  dis  ang1  ang2 / x  pts  mylst)
	;������,����߼��㡣������߰���Բ��ʱ��Բ��������Ϊ�߶Σ��߶ε������յ�ֱ���Բ���������˵�
	
	(setq pts (mapcar 'cdr (vl-remove-if  '(lambda (x) (/= (car x) 10))(entget e))))	;��ȡ����߶��������б�
	(setq mylst (ff:delt_pt  pts  dis  ang1  ang2))
	(ff:makeplc mylst);����м�㣬�������С��dis�ĵ�������
	
	(command "_matchprop" e (entlast) "")	;���ɵ�������ԭ������ƥ��
	(command "erase" e "")	;ɾ��ԭʼ�����
	(princ)
)


;==============================�Ӻ���==============================
;���߶νڵ��ϡ
(defun ff:delt_pt (pts  dis  ang1  ang2 /  pts1)
	
	;�����Ӻ�����������������������n��lst����ff:delt_sub_lst���������õ�ʱ��n�ĸ�ֵ����1
	;�������ܣ�ȥ��lst�ĵڶ���Ԫ�غ󷵻ر�
	(defun ff:delt_sub_lst (n lst)
		(if (null lst) 
			nil
			
			;���lst��Ϊnil��ִ�������if
			(if (zerop n) 
				(cdr lst)		;��n=0ʱ����lstȥ����1��Ԫ�غ󷵻ر�
				(cons (car lst) (ff:delt_sub_lst (- n 1) (cdr lst)))	;��n��0ʱ��ȥ��lst�ĵ�2��Ԫ�غ󷵻ر�
			)
		)
	)
	
	
	;�����Ӻ��������ܣ��ж������Ƿ��ߣ�����T����nil
	;(defun ff:3pt_in_line (p1 p2 p3 ang1 / v1 v2 mylst)
	;   (setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
	;   (if (equal (car v1) 0.0 1e-4)
	;		(equal (car v1) (car v2) 1e-1)	;��x1-x2=0ʱ���ж�(x1-x2)��(x2-x3)�Ƿ���ȡ����(x1-x2)=(x2-x3)=0��֤��p1p2p3���㹲��
	;		(equal (/ (cadr v1) (car v1))  (/ (cadr v2) (car v2))  (/ (sin (angtof ang1)) (cos (angtof ang1))));��x1-x2��0ʱ���ж�ֱ��p1p2��ֱ��p2p3��б���Ƿ����,�˴�equal���ݲ�ȡֵΪtanA�����б����ȣ�֤��p1p2p3���㹲��
	;   )
	;)	
	
	
	(defun ff:3pt_in_line (p1 p2 p3 ang1 / boln  v1 v2 )
		(setq boln nil)
		(setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
		
		(if (and 
					(equal (car v1) 0 1e-4)
					(equal (car v2) 0 1e-4)
				)
			(setq boln T)	;��x1-x2=0,��x2-x3=0ʱ��֤��p1p2p3���㹲�ߣ��Ҹ���ƽ����y��(б�������)
		)
		
		(if (and 
					(/= (car v1) 0)
					(/= (car v2) 0)
					(equal   (/ (cadr v1) (car v1))    (/ (cadr v2) (car v2))    (/ (sin (angtof ang1)) (cos (angtof ang1))))
				)
			(setq boln T)	;��x1-x2��0,��x2-x3��0����ֱ��p1p2��ֱ��p2p3��б�����ݲΧ�����ʱ��֤��p1p2p3���㹲�ߡ��˴�equal���ݲ�ȡֵΪtanA
		)
		boln
	)
	
	
	(setq pts1 nil)
	
	(while (> (length pts) 2)
		
		(if (ff:3pt_in_line (car pts) (cadr pts) (caddr pts) ang1)
			
			(setq pts (ff:delt_sub_lst 1 pts))		;�����㹲��ʱ��ֱ��ִ��ff:delt_sub_lst�Ӻ�����ȥ��pts�ĵڶ���Ԫ�غ󷵻ر�
			
			
			(progn 
				(if (< (distance (car pts) (cadr pts)) dis)
					(if (< (ff:calculate_line_intersect_angle (car pts) (cadr pts) (cadr pts) (caddr pts)) (atof  ang2))
						(setq pts (ff:delt_sub_lst 1 pts))	;�����߶�p1p2��p2p3�ļнǣ�����߶μн�С�ڸ���ang2��ֱ��ִ��ff:delt_sub_lst�Ӻ�����ȥ��pts�ĵڶ���Ԫ�غ󷵻ر�
						(setq 
							pts1 (cons (car pts) pts1)	;���߶μнǴ��ڵ��ڸ���ang2����ô�Ȱѱ�pts�ĵ�1��Ԫ�طŽ���pts1��
							pts (cdr pts)	;���߶μнǴ��ڵ��ڸ���ang2���ٰѱ�ptsȥ����1��Ԫ�غ󷵻�pts
						)						
					);���p1p2�ľ���С�ڸ����ľ���dis��ִ�д�if
					
					(setq 
						pts1 (cons (car pts) pts1)	;���lst�ĵ�2��Ԫ�ز���ɾ������ô�Ȱѱ�pts�ĵ�1��Ԫ�طŽ���pts1��
						pts (cdr pts)	;���lst�ĵ�2��Ԫ�ز���ɾ�����ٰѱ�ptsȥ����1��Ԫ�غ󷵻�pts
					);���p1p2�ľ�����ڵ��ڸ����ľ���dis��ִ�д�setq			
				)
			);�����㲻����ʱ����ôִ�д�progn
			
		);����pts��Ԫ�ظ�������2ʱ����whileѭ��
		
		(setq mylst (cons (cadr pts) (cons (car pts) pts1)))	;����ptsʣ���2��Ԫ����pts1�ϲ�������mylst���У���ʱ���˳����ԭ˳���෴
		(reverse mylst)	;���ñ�ĵ�˳����ԭ˳��һ��
	)
)


;==============================�Ӻ���==============================
(defun ff:makeplc(pts)
	(entmakex(append(mapcar 'cons (list 0 100 100 90 38 43)
										(list "LWPOLYLINE" "AcDbEntity" "AcDbPolyline" (length pts)  0 0.0))
						 (mapcar '(lambda (p)(cons 10 p)) pts)))
	(princ)
)



(defun ff:ddxJD2 ( e  dis  ang1  ang2 / x  pts  mylst)
	;������,����߼��㣬����߰���Բ����ʱ��Ҳ����
	
	(setq pts (ff:polyline_vertices->lst e))	;��ȡ����߶��������б�,д����pts������ӱ���2��Ԫ�أ����α�ʾ����߶������ꡢԲ��Բ������
	;���ؽ��ʾ����(((549.574 730.432)) ((560.882 725.683) (563.78 732.584)) ((567.226 725.94)) ((578.562 731.82)) ((588.348 741.904) (593.719 736.692)) ((593.152 744.155)) ((606.16 745.143)))
	(setq pts (ff:pl_lst_add_arc_midpt e pts))	;�������Բ���е����������pts�У�����ӱ���3��Ԫ�أ����α�ʾ����߶������ꡢԲ���е����ꡢԲ��Բ������
	;���ؽ��ʾ����(((549.574 730.432)) ((560.882 725.683) (564.083 725.106) (563.78 732.584)) ((567.226 725.94)) ((578.562 731.82)) ((588.348 741.904) (590.544 743.469) (593.719 736.692)) ((593.152 744.155)) ((606.16 745.143)))
	
	(setq mylst (ff:delete_pt  pts  dis  ang1  ang2)) 
	(ff:draw_LWPOLYLINE mylst) ;n1������n2��˵����Բ������������Ӻ������ɶ����
	
	(command "_matchprop" e (entlast) "")	;���ɵ�������ԭ������ƥ��
	(command "erase" e "")	;ɾ��ԭʼ�����
	(princ)
)


;==============================�Ӻ���==============================
;���߶νڵ��ϡ
(defun ff:delete_pt (pts  dis  ang1 ang2 /  pts1)
	
	;�����Ӻ�����������������������n��lst����ff:delete_ptlst_element���������õ�ʱ��n�ĸ�ֵ����1
	;�������ܣ�
	;n=0ʱȥ��lst�ĵ�1��Ԫ�غ󷵻ر�
	;n=1ʱȥ��lst�ĵ�2��Ԫ�غ󷵻ر�
	(defun ff:delete_ptlst_element (n  lst)
		(if (null lst) 
			nil
			
			;���lst��Ϊnil��ִ�������if
			(if (zerop n) 
				(cdr lst)		;��n=0ʱ����lstȥ����1��Ԫ�غ󷵻ر�
				(cons (car lst) (ff:delete_ptlst_element (- n 1) (cdr lst)))	;��n��0ʱ��ȥ��lst�ĵ�2��Ԫ�غ󷵻ر�
			)
		)
	)
	
	
	;�����Ӻ��������ܣ��ж������Ƿ��ߣ�����T����nil
	;(defun ff:check_3pt_in_line (p1 p2 p3 ang1 / v1 v2 mylst)
	;   (setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
	;   (if (equal (car v1) 0.0 1e-4)
	;		(equal (car v1) (car v2) 1e-1)	;��x1-x2=0ʱ���ж�(x1-x2)��(x2-x3)�Ƿ���ȡ����(x1-x2)=(x2-x3)=0��֤��p1p2p3���㹲��
	;		(equal (/ (cadr v1) (car v1))  (/ (cadr v2) (car v2))  (/ (sin (angtof ang1)) (cos (angtof ang1))));��x1-x2��0ʱ���ж�ֱ��p1p2��ֱ��p2p3��б���Ƿ����,�˴�equal���ݲ�ȡֵΪtanA��A=2�㣬���б����ȣ�֤��p1p2p3���㹲��
	;   )
	;)	
	
	;�����Ӻ��������ܣ��ж������Ƿ��ߣ�����T����nil
	(defun ff:check_3pt_in_line (p1 p2 p3 ang1 / boln  v1 v2 )
		(setq boln nil)
		(setq v1 (mapcar '- p1 p2)   v2 (mapcar '- p2 p3))
		
		(if (and 
					(equal (car v1) 0 1e-4)
					(equal (car v2) 0 1e-4)
				)
			(setq boln T)	;��x1-x2=0,��x2-x3=0ʱ��֤��p1p2p3���㹲�ߣ��Ҹ���ƽ����y��(б�������)
		)
		
		(if (and 
					(/= (car v1) 0)
					(/= (car v2) 0)
					(equal   (/ (cadr v1) (car v1))    (/ (cadr v2) (car v2))    (/ (sin (angtof ang1)) (cos (angtof ang1))))
				)
			(setq boln T)	;��x1-x2��0,��x2-x3��0����ֱ��p1p2��ֱ��p2p3��б�����ݲΧ�����ʱ��֤��p1p2p3���㹲�ߡ��˴�equal���ݲ�ȡֵΪtanA
		)
		boln
	)	
	
	
	
	(setq pts1 nil)
	
	(while (> (length pts) 2)
		
		;���жϵ�p1��p2�ǲ���Բ�����
		(if (or (= (length (car pts)) 3) (= (length (cadr pts)) 3))
			
			(progn 
				(setq pts1 (cons (car pts) pts1))	;�Ȱѱ�pts�ĵ�1��Ԫ�طŽ���pts1��
				(setq pts (cdr pts))											;�ٰѱ�ptsȥ����1��Ԫ�غ󷵻�pts				
			);�����pts�ĵ�1���ӱ���2���ӱ���Բ�����ʱ��ִ�д�progn
			
			(progn 
				
				(if (ff:check_3pt_in_line (car (car pts)) (car (cadr pts)) (car (caddr pts)) ang1)	;ȡ���pts��ǰ3��Ԫ�أ��ж��Ƿ�����һ��
					
					(setq pts (ff:delete_ptlst_element 1 pts))		;�����㹲��ʱ��ֱ��ִ��ff:delete_ptlst_element�Ӻ�����ȥ��pts�ĵڶ���Ԫ�غ󷵻ر�
					
					(progn 
						(if (< (distance (car (car pts)) (car (cadr pts))) dis)
							(if (< (ff:calculate_line_intersect_angle (car (car pts)) (car (cadr pts)) (car (cadr pts)) (car (caddr pts))) (atof  ang2))
								(setq pts (ff:delete_ptlst_element 1 pts))	;�����߶�p1p2��p2p3�ļнǣ�����߶μн�С�ڸ���ang2��ֱ��ִ��ff:delete_ptlst_element�Ӻ�����ȥ��pts�ĵڶ���Ԫ�غ󷵻ر�
								(setq 
									pts1 (cons (car pts) pts1)	;���߶μнǴ��ڵ��ڸ���ang2����ô�Ȱѱ�pts�ĵ�1��Ԫ�طŽ���pts1��
									pts (cdr pts)	;���߶μнǴ��ڵ��ڸ���ang2���ٰѱ�ptsȥ����1��Ԫ�غ󷵻�pts
								)						
							);���p1p2�ľ���С�ڸ����ľ���dis��ִ�д�if
							
							(setq 
								pts1 (cons (car pts) pts1)	;���lst�ĵ�2��Ԫ�ز���ɾ������ô�Ȱѱ�pts�ĵ�1��Ԫ�طŽ���pts1��
								pts (cdr pts)	;���lst�ĵ�2��Ԫ�ز���ɾ�����ٰѱ�ptsȥ����1��Ԫ�غ󷵻�pts
							);���p1p2�ľ�����ڵ��ڸ����ľ���dis��ִ�д�setq			
						)
					);�����㲻����ʱ����ôִ�д�progn
					
				)				
				
			);�����pts�ĵ�1���ӱ�͵�2���ӱ�����Բ�����ʱ��ִ�д�progn
			
		);end if,
		
	);end while,�����ptsֻ��2��Ԫ��ʱ�˳�ѭ��
	
	(setq mylst (cons (cadr pts) (cons (car pts) pts1)))	;����ptsʣ���2��Ԫ����pts1�ϲ�������mylst���У���ʱ���˳����ԭ˳���෴
	(reverse mylst)	;���ñ�ĵ�˳����ԭ˳��һ��
)


;==============================�Ӻ���==============================
(defun ff:draw_LWPOLYLINE ( pts / v1 n i k  p1  p2  p3  sunit)
	;�������ܣ����ݵ��pts�����ƶ����
	;ptsʾ����(((549.574 730.432)) ((560.882 725.683) (564.083 725.106) (563.78 732.584)) ((567.226 725.94)) ((578.562 731.82)) ((588.348 741.904) (590.544 743.469) (593.719 736.692)) ((593.152 744.155)) ((606.16 745.143)))
	
	(setq v1 (getvar "osmode"))
	
	(setq n (length pts))	;������pts��Ԫ�ظ���
	(setq i 0)
	(setq sunit (ssadd))
	(repeat (1- n)
		
		(setq k (length (car pts)))
		
		(if (= k 3)
			(progn 
				(setq p1 (car (car pts)))
				(setq p2 (cadr (car pts)))
				(setq p3 (car (cadr pts)))
				;��command�������Բ��	
				(setvar "osmode" 0)
				(command "arc" p1 p2 p3)	;�������������Բ��
				(setvar "osmode" v1)
				;��entmake��������Բ��
				;(setq arc_lst (ff:calculate_arc_property p1 p2 p3))
				;(setq 
				;	po (car arc_lst)
				;	R (cadr arc_lst)
				;	ang1 (caddr arc_lst)
				;	ang2 (caddr (cdr arc_lst))
				;)
				;(entmake (list '(0 . "ARC") (cons 10 po) (cons 40 R) (cons 50 ang1) (cons 51 ang2)))
				(ssadd (entlast) sunit)	;����һ�����Ƶ�Բ������ѡ��sunit��
				(setq pts (cdr pts))
			);k=3ʱִ�д�progn
			
			(progn 
				(setq p1 (car (car pts)))
				(setq p2 (car (cadr pts)))
				;��command�������ֱ��
				;(setvar "osmode" 0)
				;(command "line" p1 p2 "")
				;(setvar "osmode" v1)
				;��entmake��������ֱ��
				(entmake (list '(0 . "LINE") (cons 10 p1) (cons 11 p2)))
				(ssadd (entlast) sunit)	;����һ�����Ƶ�ֱ�߼���ѡ��sunit��
				(setq pts (cdr pts))
			);k��3ʱ��֤��k=1,ִ�д�progn
		)		
		
		(setq i (1+ i))
	);end repeat
	
	(if sunit 
		(command "pedit" "M" sunit "" "Y" "J" 0 "")	;��ѡ��sunit������ж���ϲ�Ϊ�����
	)
	
	(princ)
)


(defun ff:polyline_vertices->lst (m_plent   /  m_pttab   m_pt1     m_pt2
							m_tmp     m_ptlist m_xc   m_radius  m_pt3
							m_ptcenter
						)
	;���ض���ߵĶ������������������Ԫ�������������ߵĶ���������ͬ
	;ÿ���ӱ�����1�������2���������
	;�ӱ��������1���㣬��õ���Ƕ���ߵĶ�������
	;�ӱ��������2���㣬��ʾ�õ���Բ������ʼ�˵㣬�ӱ���ĵ�1�����Ƕ���ߵĶ������꣬��2������Բ����Բ������
	;���صĶ����ʾ����(((1019.5 2257.22)) ((1013.69 2238.17) (1051.95 2226.5)) ((1019.44 2203.19)) ((1027.84 2191.48)) ((1068.14 2184.99)) ((1078.78 2175.75) (1062.38 2156.88)) ((1085.88 2148.35)) ((1078.7 2128.57)))
	;!!!!!!!!��Ҫע����ǣ����������Բ��ʱ�������ֻ���ҵ�Բ�������˵㼰Բ�����꣬��3�����껹�޷�ȷ��һ��Բ����Բ��������˳ʱ�룬Ҳ��������ʱ��
	
	(setq m_pttab (entget m_plent))
	
	(while (setq m_pt1 (assoc '10 m_pttab))
		(setq m_tmp (assoc '42 m_pttab))
		
		(if (/= 0.0 (cdr m_tmp))
			(if (setq m_pt2 (assoc '10 (cdr (member m_pt1 m_pttab)))) ;��һ��
				(progn
					(setq m_xc (distance (cdr m_pt1) (cdr m_pt2))) ;�ҳ�
					
					(setq	m_radius 
						(abs (/ (* m_xc (1+ (* (cdr m_tmp) (cdr m_tmp))))
									 (* 4 (cdr m_tmp))
								 )
						)
					)    ;�뾶R
					
					(setq m_pt3 
						(polar (cdr m_pt1)
							(angle (cdr m_pt1) (cdr m_pt2))
							(/ m_xc 2.0)
						)
					)
					
					(if (> 0.0 (cdr m_tmp))
						(setq m_ptcenter
							(polar m_pt3
								(- (angle (cdr m_pt1) (cdr m_pt2))
									(angtof "90")
								)
								
								(- m_radius
									(/ (* (abs (cdr m_tmp)) m_xc) 2.0)
								)
							)
						)
						
						(setq m_ptcenter
							(polar m_pt3
								(- (angle (cdr m_pt1) (cdr m_pt2))
									(angtof "270")
								)
								
								(- m_radius
									(/ (* (abs (cdr m_tmp)) m_xc) 2.0)
								)
							)
						)
					);end if
					
					(setq m_ptlist 
						(append m_ptlist
							(list (list (cdr m_pt1) m_ptcenter))
						)
					)
				);end progn
				(setq m_ptlist (append m_ptlist (list (list (cdr m_pt1)))))
			);end if
			
			(setq m_ptlist (append m_ptlist (list (list (cdr m_pt1)))))
		);end if
		
		(setq m_pttab (cdr (member m_tmp m_pttab)))
	);end while
	
	m_ptlist
)


(defun ff:pl_lst_add_arc_midpt ( ent lst / obj  n  i  sublst1  k  sublst2  p1  p2  lth1  lth2   lth_mid   pt_mid   sublst1_new)
	;�������ܣ����ն����ͼԪ���Ͷ�����������ʾ�����£�
	;(((434.099 602.085)) ((526.095 753.303) (601.891 707.192)) ((576.707 792.263)) ((735.127 839.162)) ((816.704 941.702) (747.275 996.937)) ((832.725 1020.8)) ((802.416 1129.32)))
	;������������Բ������Բ�����е�����д������
	;!!!!!!!!!!ע�⣺�����������ff:polyline_vertices->lstʹ�ã��ȵ���ff:polyline_vertices->lst�����õ���ʼ���Ȼ���ٵ��ñ���������Բ���е�����
	
	;(setq ent (car (entsel)))	;����������
	;(setq lst (ff:polyline_vertices->lst ent))	;����������
	(setq obj (vlax-ename->vla-object ent))	;�������entתΪVLA����
	
	(setq n (length lst))
	(setq i 0)
	(repeat n
		(setq sublst1 (nth i lst))	;��ȡ��i������ĵ������sublst1��
		(setq k (length sublst1))		;��ѯsublst1���ӱ���������k=2��˵����ǰ������Բ������㣬���ʾ��((526.095 753.303) (601.891 707.192))���ӱ����α�ʾ����߶�������(Ҳ��Բ���������)��Բ��Բ������
		(if (= k 2)
			(progn
				(setq sublst2 (nth (+ 1 i) lst))
				(setq 
					p1 (car sublst1)	;��ȡ�������Բ��������ꡣע�⣺����㲢��һ���ǰ�˳ʱ��ȷ����Բ�������
					p2 (car sublst2)	;��ȡ�������Բ���յ����ꡣע�⣺���յ㲢��һ���ǰ�˳ʱ��ȷ����Բ�����յ�
					lth1 (vlax-curve-getDistAtPoint obj p1)	;��������������p1�ĳ���
					lth2 (vlax-curve-getDistAtPoint obj p2)	;��������������p2�ĳ���
					lth_mid (/ (+ lth1 lth2) 2)	;�������������Բ���е�ĳ���
					pt_mid (vlax-curve-getPointAtDist obj lth_mid)	;����Բ���е����꣬�õ�������ά�㣬ʾ����(547.774 777.495 0.0)
					pt_mid (list (car pt_mid) (cadr pt_mid))	;ȥ��Z���꣬�õ�XY������ʾ����(547.774 777.495)
				)
				
				(setq sublst1_new (list (car sublst1) pt_mid (cadr sublst1)))	;�����µĵ��sublst1_new��ʾ����((526.095 753.303) (547.774 777.495) (601.891 707.192))���ӱ����α�ʾ����߶�������(Ҳ��Բ���������)��Բ���е����ꡢԲ��Բ������
				
				(setq lst (ff:subst_n i sublst1_new lst))

			)
		)
		
		(setq i (1+ i))
	)
	lst
)


(defun ff:calculate_line_intersect_angle(p1 p2 p3 p4 / k1 k2 tan_ang   ang)
	;�������ܣ�����2ֱ�ߵļн�
	;ֱ��l1��������p1,p2��ֱ��l2��������p3,p4
	(setq k1 (/ (sin (angle p1 p2)) (cos (angle p1 p2))))
	(setq k2 (/ (sin (angle p3 p4)) (cos (angle p3 p4))))
	
	(setq tan_ang (abs (/ (- k1 k2) (+ 1 (* k1 k2)))))
	;(setq ang (angtos (atan tan_ang) 0 8))			;����нǣ���תΪ�ַ���
	(setq ang (/ (* 180 (atan tan_ang)) PI))		;����нǣ����Ϊʵ������λ�Ƕ�
)


(defun ff:subst_n (n a lst)
	;�������ܣ���ָ�����ӱ��滻lst�е�n��Ԫ�أ�n��0��ʼ����
	;�﷨��(ff:subst_n   ˳���n  �ӱ�a  ԭ��lst)
  (cond
    ((numberp n)
			(if (zerop n)
				(append (list a) (cdr lst))
				(cons (car lst) (ff:subst_n (1- n) a (cdr lst)))
			)
    )
    ((listp n)
			(cond
				((equal (length n) 1)
					(if (zerop (car n))
						(append (list a) (cdr lst))
						(cons (car lst) (ff:subst_n (1- (car n)) a (cdr lst)))
					)
				)
				((> (length n) 1)
					(if (zerop (car n))
						(cons (ff:subst_n (cdr n) a (car lst)) (cdr lst))
						(cons (car lst)
							(ff:subst_n (append (list (1- (car n))) (cdr n)) a (cdr lst))
						)
					)
				)
			)
    )
  )
)



