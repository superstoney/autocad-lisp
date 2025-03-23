;(defun c:ld() (load "weed"))
;(defun c:ed() (command "q" "d:\\acad\\support\\weed.lsp"))

;*----- Error Routine

(defun w-error (s) (redraw) (grtext)
  (princ "\nWeed Error: ") (princ s)
  (exit)
)

;*----- Exit Routine

(defun exit()
  (if (boundp 'f) (setq f (close f)))
  (setvar "cmdecho"  cmdecho)
  (setvar "blipmode" blipmode)
  (setq *error* olderr)
  (princ)
)

;*----- Extract a field from a list

(defun fld (num lst) (cdr (assoc num lst)))

;*----- Plot a temporary X

(defun blip (blpoint / s x1 y1 x2 y2 p1 p2 p3 p4)

   (setq s  (/ (getvar "viewsize") 100)      ; 1/100 of viewsize
         x1 (+ (car blpoint) s)
         y1 (- (cadr blpoint) s)
         x2 (- (car blpoint) s)
         y2 (+ (cadr blpoint) s)
         p1 (list x1 y1) p2 (list x2 y2)
         p3 (list x2 y1) p4 (list x1 y2))
   (grdraw p1 p2 -1) (grdraw p3 p4 -1)
)

;*----- Convert a line to a polyline entity

(defun line2pline(ent / dat etype epnt ss1 ss2)
  (if ent (progn
    (setq dat   (entget ent)
          etype (fld 0 dat))
    (if (= etype "LINE") (progn
      (princ "\nConverting LINE to PLINE")
      (setq epnt (fld 10 dat)
            ss1 (ssadd ent)
            ss2 (ssget "C" (getvar "EXTMIN") (getvar "EXTMAX")))
      (ssdel ent ss2)
      (command "pedit" ss1 "y" "j" ss2 "" "x")
      (ssname (ssget epnt) 0) ; return the new entity name
    )
    ;else return nil
      (progn (princ "\nNot a Line")
        nil)
    )
  );else
  (progn
    (princ "\nNothing selected")
    nil)
  )
)

;*-----  Get a polyline or line entity

(defun fetch(/ pl etyp flgs ans)
  (setq etyp nil)
  (while (not (or (= etyp "LINE") (= etyp "POLYLINE"))) (progn
    (setq ename nil)
    (setq e (car (entsel "\nSelect a PolyLine or Line: ")))
    (if e (progn
      (setq pl	   (entget e)
	    etyp   (fld 0 pl)
	    ename  e)
      (if (or (= etyp "LINE") (= etyp "POLYLINE")) (progn
	(princ (strcat "\n" etyp " selected"))
	(if (= etyp "LINE")
	  (setq e      (line2pline e)
		ename  e
		pl     (entget e))
	))
      ;else
	(progn	(princ "\nThat's not a LINE or POLYLINE, it's a ") (princ etyp))
      ); end if
    ); end progn
    ; else
      (princ "\nNothing Selected")
    ); end if
  )); end while
  (setq flgs   (fld 70 pl))
  (setq closed (=(boole 1 flgs 1) 1))
  (if closed (princ "\nClosed Polyline"))
  (cond
    ((=(boole 1 flgs 2) 2) (progn
      (setq ptyp "F")
      (princ "\nFit curve verticies have been added")))
    ((=(boole 1 flgs 4) 4) (progn
      (setq ptyp "S")
      (princ "\nSpline curve verticies have been added...")))
    (t	(setq ptyp "N"))      ;Normal polyline
  )
  (if(/= ptyp "N") (progn
    (initget "Y N")
    (if(= (getkword "\nDecurve polyline during weeding[y/N]:") "Y")
      (setq ptyp "N"))
  ))
)

;*----- Check vertex type

(defun vt_ok ()
  (if (= etype "VERTEX")
    (cond
      ((= ptyp "F") (or(=(boole 1 flags 1) 1) (= flags 0)))
      ((= ptyp "S") (>(boole 1 flags 9) 0))
      (t	    (=(boole 1 flags 25) 0)) ;"N" normal, 1 8 16 off
    )
  ;else
    t
  )
)

;*----- extract the list containing vertex coordinates

(defun get_vertex(/ vert etype sub_ent flags)
  (setq vert nil
        etype nil)
  (while (and e (null vert) (/= etype "SEQEND")) (progn
    (setq v     (entnext e)
          e     v
          etype nil)
    (if e (progn
      (setq sub_ent   (entget v)
	    flags     (fld 70 sub_ent)
            etype     (fld 0  sub_ent))
      ;(princ "flags =")(princ flags)
      (if (vt_ok)
	(if (= etype "VERTEX")
	  (setq vert_cnt (1+ vert_cnt)
		vert	 (fld 10 sub_ent))
	; else return
	  nil
	)
      )
    ))
  ))
)

;*----- Add a vertex to the temporary file for the new pline

(defun add_vert(vt)
  (if (null f) (setq f (open "weedtmp.$$$" "w")))
  (prin1 vt f)
  (princ "\n" f)
)

;*----- Read a vertex from the temporary file for the new pline

(defun read_vert(/ pt)
   (setq pt (read-line f))
   (if pt (read pt) nil)
)

;*----- Read new polyline from the tempory file

(defun retrieve()
    (setq f (open "weedtmp.$$$" "r"))
    (command ".PLINE")
    (setq v (read_vert))
    (while v (progn
      (command v)
      (setq v (read_vert))
    ))
    (command "")
   ;(command "del" "weedtmp.$$$")
)

;*----- Check the internal angle and leg lengths then add or delete

(defun check_it(/ ang dist1 dist2 dist offset off)
  (setq ang12  (abs(angle v1 v2))
	ang13  (abs(angle v1 v3))
	ang    (abs(- ang12 ang13))
	dist1  (distance v1 v2)
	dist2  (distance v2 v3)
	dist   (max dist1 dist2)       ; largest distance
	off    (* dist1 (sin ang))
	offset (+ p_off off)
	p_off  offset
  )
  (if
    (and
      (< offset max_offset)		;offset distance criteria
      (< dist	min_dist)		;minimum leg length criteria
    )
    ;then skip middle vertex
    (progn (blip v2)			;mark the deleted vertex
       (setq v2 	v3
	     v3 	(get_vertex)
	     skip_cnt	(1+ skip_cnt))
      (princ "\nSkipping vertex # ") (princ (- vert_cnt 2))
;     (princ (strcat ", max_offset " (rtos max_offset 2 2) "
;			min_dist " (rtos min_dist 2 2)))
;     (princ (strcat ", offset " (rtos offset 2 2) " dist " (rtos dist 2 2)))
    )
  ;else add first vertex to list
    (progn
      (add_vert v2)
      (setq v1 v2
            v2 v3
	    v3 (get_vertex)
	    p_off  0)
    ); end progn
  ); end if
)

;*----- The main routine...

(defun C:WEED( / v1 v2 v3 ename v skip_cnt vert_cnt cmdecho blipmode f
		 olderr max_offset min_dist closed spline fit e_del
		 p_off vstart ptyp)

  (setq cmdecho  (getvar "cmdecho")
        blipmode (getvar "blipmode")
	olderr	 *error*
	*error*  w-error
;  *error* nil
	skip_cnt 0
	p_off	 0
	f	 nil
	vert_cnt 0
  )
  (setvar "cmdecho" 0)
  (setvar "blipmode" 0)
  (initget (+ 1 2 4))
  (setq max_offset (getdist "\nEnter offset distance: "))
  (initget (+ 1 2 4))
  (setq min_dist (getdist "\nEnter leg length: "))
  (initget "Y N")
  (setq e_del (getkword "\nDelete original Polyline [Y/n]: "))
  (if (null e_del) (setq e_del "Y"))
  (fetch)
  (princ "\nChecking polyline verticies...")
  (setq v1 (get_vertex)
	vstart v1
	v2 (get_vertex)
        v3 (get_vertex))
  (add_vert v1)
  (while v3 (check_it))
  (if (< (distance v1 v2) min_dist)
    (progn (setq skip_cnt (1+ skip_cnt))
	   (princ "\nSkipping vertex # ") (princ vert_cnt))
  ;else
    (add_vert v1)
  ); end if
  (add_vert v2)
  (if closed (add_vert vstart))
  ; Delete old line and draw new Pline
  (if (> skip_cnt 0) (progn
    (close f)
    (if (= e_del "Y") (entdel ename))
    (retrieve)
    (princ (strcat "\n" (itoa skip_cnt) " verticies removed "
		   "out of " (itoa vert_cnt) " tested ("
		   (rtos(/ (* 100.0 skip_cnt) vert_cnt) 2 2)
		   ") percent"))
  )
  ;else
    (princ "\nNothing to change!")
  )
  (exit)
)