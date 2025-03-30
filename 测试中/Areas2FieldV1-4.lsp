(defun c:a2f2 ( / *error* fmt sel str pline center )
    ;; 修改字段格式以确保正确显示面积
    (setq fmt "%.2f") ;; 将显示格式改为保留2位小数

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    ;; Function to calculate the center of a closed polyline
    (defun get-centroid (pline / area pts cnt sumx sumy xc yc)
        (setq area (vla-get-area pline))
        ;; Get the coordinates of the polyline
        (setq pts (vlax-get pline 'Coordinates))
        
        ;; Calculate centroid using coordinate averaging
        (setq cnt 0
              sumx 0.0
              sumy 0.0)
        (while (< cnt (length pts))
            (setq sumx (+ sumx (nth cnt pts))
                  sumy (+ sumy (nth (1+ cnt) pts))
                  cnt (+ cnt 2))
        )
        (setq xc (/ sumx (/ (length pts) 2))
              yc (/ sumy (/ (length pts) 2)))
        
        ;; Return the centroid point
        (vlax-3d-point xc yc 0.0)
    )

    (if (setq sel (ssget '((0 . "LWPOLYLINE") (70 . 1))))
        (progn
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (setq pline (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
                (setq center (get-centroid pline))
                ;; 修改字段表达式以确保正确显示面积
                (setq str
                    (strcat
                        "%<\\AcObjProp Object(%<\\_ObjId "
                        (LM:ObjectID pline)
                        ">%).Area \\f \"" fmt "\">%"
                    )
                )
                (vla-addmtext
                    (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    center
                    0.0
                    str
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Rest of the support functions remain the same
(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
)

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com) (princ)