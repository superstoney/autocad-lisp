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

(defun C:CopyLengthToClipboard (/ CURVE TLEN SS N SUMLEN) 

  (vl-load-com) 

  (setq SUMLEN 0) 

  (setq SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC")))) 

  (setq N 0) 

  (repeat (sslength SS) 

    (setq CURVE (vlax-ename->vla-object (ssname SS N))) 

    (setq TLEN (vlax-curve-getdistatparam CURVE (vlax-curve-getendparam CURVE))) 

    (setq SUMLEN (+ SUMLEN TLEN)) 

    (setq N (1+ N))

  ) 

  (SET-CLIP-STRING (rtos SUMLEN 2 3))
  (setq c sumlen)
)