(defun SET-CLIP-STRING (STR / HTML RESULT)

    (and (= (type STR) 'STR)

         (setq HTML (vlax-create-object "htmlfile"))

         (setq RESULT (vlax-invoke

                          (vlax-get (vlax-get HTML 'PARENTWINDOW)

                                    'CLIPBOARDDATA

                          )

                          'SETDATA

                          "Text"

                          STR

                      )

         )

         (vlax-release-object HTML)

    )

)

(defun C:CopyAreaToClipboard (/ CURVE TAREA SS N SUMAREA) 

  (vl-load-com) 

  (setq SUMAREA 0) 

  (setq SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC,HATCH")))) 

  (setq N 0) 

  (repeat (sslength SS) 

    (setq CURVE (vlax-ename->vla-object (ssname SS N))) 

    (setq TAREA (vla-get-area CURVE)) 

    (setq SUMAREA (+ SUMAREA TAREA)) 

    (setq N (1+ N))

  ) 

  (SET-CLIP-STRING (rtos SUMAREA 2 3))
  (setq c SUMAREA)
)

