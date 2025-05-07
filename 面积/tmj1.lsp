;;;=========================================================================
;;; Polygon Area Adjustment Program (TMJ1)
;;;=========================================================================
;;; Original Author: Atsai
;;; Current Version: 1.0.3
;;; Last Modified by: superstoney
;;; Last Modified Date: 2025-04-03 23:30:50 UTC
;;;
;;; DESCRIPTION:
;;; This program allows users to adjust the area of a closed polygon by moving
;;; a single vertex while maintaining the positions of other vertices.
;;;
;;; FEATURES:
;;; - Adjusts polygon area by moving a single vertex
;;; - Maintains other vertices' positions
;;; - Real-time area calculation
;;; - Automatic error handling
;;; - System variable protection
;;;
;;; REQUIREMENTS:
;;; - AutoCAD 2013 or later
;;; - Basic understanding of polygon manipulation in AutoCAD
;;;
;;; USAGE INSTRUCTIONS:
;;; 1. Load the program:
;;;    - Type "APPLOAD" in AutoCAD command line
;;;    - Select and load this LISP file
;;;
;;; 2. Run the command:
;;;    - Type "TMJ1" in the command line
;;;
;;; 3. Follow the prompts:
;;;    a. Select the target polygon
;;;    b. Enter the desired new area (or press Enter to keep current value)
;;;    c. Select the vertex you want to move
;;;
;;; 4. Results:
;;;    - The program will adjust the selected vertex to achieve the target area
;;;    - A success message will appear if the operation is completed
;;;    - If the target area cannot be achieved, an error message will appear
;;;
;;; NOTES:
;;; - The polygon must be a closed polyline
;;; - The program preserves the original position of all vertices except the selected one
;;; - The adjustment maintains the angle between adjacent segments at the moved vertex
;;;
;;; TROUBLESHOOTING:
;;; - If the command fails, ensure that:
;;;   1. The selected object is a closed polygon
;;;   2. The target area is achievable by moving a single vertex
;;;   3. The selected vertex is valid

(setq *TMJ1-VERSION* "1.0.3")

(defun c:tmj1 (/ obj1 en el-tmp el ps pe)
  (vl-load-com)
  (setierr)
  (setvar "cmdecho" 0)
  
  ;; Program information output
  (prompt "\n====================================================================")
  (prompt (strcat "\nCommand: TMJ1  Version: " *TMJ1-VERSION*))
  (prompt "\nFunction: Adjust polygon area by single vertex")
  (prompt "\nOriginal Author: Atsai")
  (prompt "\nModified by: superstoney")
  (prompt "\n====================================================================")
  
  ;; Save and set system variables
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  
  ;; Select polygon
  (setq obj1 nil
        en nil
        obj1 (vlax-ename->vla-object (car (entsel "\nSelect polygon: "))))
  
  ;; Get point list and original area
  (setq ptlst nil
        ptlst (vxs obj1)
        area-o1 nil
        area-o1 (abs (getplarea ptlst)))
  
  ;; Input new area
  (setq area-n nil
        area-n (getreal (strcat "\nEnter new polygon area <" 
                               (rtos area-o1 2 3) 
                               ">: ")))
  
  ;; Select vertex
  (setq en (entsel "\nSelect a vertex of the polygon: "))
  (setq endata nil
        endata (entget (car en))
        obj2 nil
        obj2 (vlax-ename->vla-object (car en)))
  
  ;; Get related points
  (setq p nil
        p (HH:PickClosePt obj2 (cadr en))
        ps (car (HH:PickSegEndPt obj2 (cadr en)))
        pe (cadr (HH:PickSegEndPt obj2 (cadr en))))
  
  ;; Calculate new area
  (setq ptlst-n nil
        ptlst-n (vl-remove p ptlst)
        area-o2 nil
        area-o2 (abs (getplarea ptlst-n))
        area-m nil
        area-m (- area-o2 area-n))
  
  ;; Get parameter point
  (setq index (fix (vlax-curve-getparamatpoint obj2 (vlax-curve-getclosestpointto obj2 p))))
  
  ;; Determine start point
  (if (= index 0)
    (setq sp (vlax-curve-getPointAtParam obj2 (- (length ptlst) 2)))
    (setq sp (vlax-curve-getPointAtParam obj2 (- index 1))))
  
  ;; Determine end point
  (if (> (+ index 1) (- (length ptlst) 1))
    (setq ep (vlax-curve-getPointAtParam obj2 1))
    (setq ep (vlax-curve-getPointAtParam obj2 (+ index 1))))
  
  ;; Calculate offset distance
  (setq B nil
        h nil
        B (distance sp ep)
        h (abs (/ (* 2 area-m) B)))
  
  ;; Create temporary line
  (setq el-tmp nil
        el nil)
  (command "line" sp ep "")
  (setq el-tmp (entlast))
  
  ;; Offset line
  (command "offset" h el-tmp p "")
  (setq el (entlast))
  
  ;; Calculate intersection point
  (setq sp-el (cdr (assoc 10 (entget el)))
        ep-el (cdr (assoc 11 (entget el)))
        p-tmp (inters ps pe sp-el ep-el nil))
  
  ;; Delete temporary lines
  (command "erase" el-tmp el "")
  
  ;; Update entity data
  (setq endata (subst (cons 10 (list (car p-tmp) (cadr p-tmp)))
                      (cons 10 (list (car p) (cadr p)))
                      endata))
  (entmod endata)
  
  ;; Verify area
  (command "area" "o" en)
  (setq area-o (getvar "area"))
  (if (equal area-o area-n 1e-6)
    (prompt "\nOperation completed successfully!")
    (alert "\nWarning: Adjusted area does not match target area. Please try again."))
  
  ;; Restore system variables
  (setvar "osmode" os)
  (reerr)
  (princ)
)

;; Get polygon vertex list
(defun vxs (e / i v lst)
  (setq i -1)
  (while (setq v (vlax-curve-getpointatparam e (setq i (1+ i))))
    (setq lst (cons v lst)))
  (reverse lst)
)

;; Calculate polygon area (counterclockwise positive, clockwise negative)
(defun getplarea (l / a b)
  (* 0.5
     (apply '+
            (mapcar '(lambda (a b)
                      (- (* (car a) (cadr b)) 
                         (* (car b) (cadr a))))
                    l
                    (append (cdr l) (list (car l))))))
)

;; Get closest vertex
(defun HH:PickClosePt (obj p / N P1 P2 PP)
  (setq pp (vlax-curve-getclosestpointto obj (trans p 1 0))
        n  (fix (vlax-curve-getparamatpoint obj pp)))
  (setq ll (length (vxs obj)))
  (setq p1 (vlax-curve-getPointAtParam obj n))
  (if (> (+ n 1) (- ll 1))
    (setq p2 (vlax-curve-getPointAtParam obj 1))
    (setq p2 (vlax-curve-getPointAtParam obj (1+ n))))
  (if (< (distance pp p1) (distance pp p2))
    p1
    p2)
)

;; Get segment endpoints
(defun HH:PickSegEndPt (obj p / pp n)
  (setq pp (vlax-curve-getclosestpointto obj (trans p 1 0))
        n  (fix (vlax-curve-getparamatpoint obj pp)))
  (setq ll (length (vxs obj)))
  (list
    (vlax-curve-getPointAtParam obj n)
    (if (> (+ n 1) (- ll 1))
      (vlax-curve-getPointAtParam obj 1)
      (vlax-curve-getPointAtParam obj (1+ n))))
)

;; Error handling initialization
(defun setierr ()
  (setq varlst '("ATTDIA" "CMDECHO" "ORTHOMODE" "MIRRTEXT" 
                 "CECOLOR" "celtype" "osmode" "dimzin" "ATTREQ")
        var_new '(0 0 0 0 "BYLAYER" "BYLAYER" 767 8 1))
  
  (mapcar 'setvar varlst var_new)
  
  ;; Define error handler
  (defun *error* (inf)
    (setq inf (strcase inf t))
    (cond
      ((wcmatch inf "*break,*cancel*,*exit*,*quit*")
       (prompt "\nFunction cancelled\n")
       (mapcar 'setvar varlst var_old))
      (t
       (prompt (strcat "\n" inf))
       (mapcar 'setvar varlst var_old)))
    (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
    (princ))
  
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq varlst '("ATTDIA" "CMDECHO" "ORTHOMODE" "MIRRTEXT" 
                 "CECOLOR" "celtype" "osmode" "dimzin" "ATTREQ")
        var_new '(0 0 0 0 "BYLAYER" "BYLAYER" 767 8 1)
        var_old (mapcar 'getvar varlst))
  (mapcar 'setvar varlst var_new)
)

;; Error handling recovery
(defun reerr ()
  (mapcar 'setvar varlst var_old)
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)

;; Loading prompt
(prompt (strcat "\nPolygon Area Adjustment Program v" *TMJ1-VERSION* " loaded successfully."))
(prompt "\nType 'TMJ1' to execute the command.")
(princ)