(vl-load-com)

;; Function to set string to clipboard using Windows Shell
(defun SET-CLIP-STRING-ALT (str / sh cmd)
    ;; Create command string with the actual value
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

;; Main function to calculate and copy length
(defun C:CopyLengthToClipboard (/ CURVE TLEN SS N SUMLEN str) 
    (vl-load-com) 
    (setq SUMLEN 0.0) 
    
    (princ "\nSelect objects: ")
    (setq SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC")))) 
    
    (if SS 
        (progn
            (setq N 0) 
            (repeat (sslength SS) 
                (setq CURVE (vlax-ename->vla-object (ssname SS N))) 
                (if CURVE
                    (progn
                        (setq TLEN (vlax-curve-getdistatparam CURVE (vlax-curve-getendparam CURVE)))
                        (setq SUMLEN (+ SUMLEN TLEN))
                    )
                )
                (setq N (1+ N))
            ) 
            
            ; Convert to string with 3 decimal places (keeping original precision)
            (setq str (rtos SUMLEN 2 3))
            
            ; Copy to clipboard
            (if (not (vl-catch-all-error-p 
                     (vl-catch-all-apply 'SET-CLIP-STRING-ALT (list str))))
                (progn
                    (setq c SUMLEN)
                    (princ (strcat "\nLength copied to clipboard: " str))
                )
                (princ "\nFailed to copy to clipboard!")
            )
        )
        (princ "\nNo objects selected!")
    )
    (princ)
)

;; Error handler
(defun *error* (msg)
    (if (= msg "Function cancelled")
        (princ "\nCommand cancelled.")
        (princ (strcat "\nError: " msg))
    )
    (princ)
)