(vl-load-com)

(defun SET-CLIP-STRING-ALT (str / sh cmd)
    ;; Create the command string with the actual value
    (setq cmd (strcat "cmd /c echo " str "| clip"))
    (setq sh (vlax-create-object "WScript.Shell"))
    (vl-catch-all-apply
        '(lambda ()
            (vlax-invoke sh "Run" cmd 0)
         )
    )
    (vlax-release-object sh)
    str
)

(defun C:CopyAreaToClipboard (/ CURVE TAREA SS N SUMAREA str) 
    (vl-load-com) 
    (setq SUMAREA 0.0) 
    
    (princ "\nSelect objects: ")
    (setq SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC,HATCH")))) 
    
    (if SS 
        (progn
            (setq N 0) 
            (repeat (sslength SS) 
                (setq CURVE (vlax-ename->vla-object (ssname SS N))) 
                (if CURVE
                    (progn
                        (setq TAREA (vla-get-area CURVE))
                        (setq SUMAREA (+ SUMAREA TAREA))
                    )
                )
                (setq N (1+ N))
            ) 
            
            ; Convert to string with 2 decimal places
            (setq str (rtos SUMAREA 2 2))
            
            ; Copy to clipboard
            (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'SET-CLIP-STRING-ALT (list str))))
                (progn
                    (setq c SUMAREA)
                    (princ (strcat "\nArea copied to clipboard: " str))
                )
                (princ "\nFailed to copy to clipboard!")
            )
        )
        (princ "\nNo objects selected!")
    )
    (princ)
)

(defun *error* (msg)
    (if (= msg "Function cancelled")
        (princ "\nCommand cancelled.")
        (princ (strcat "\nError: " msg))
    )
    (princ)
)