
;-------------------------------------------------------------;

(defun c:Eddd ( / *error* _StartUndo _EndUndo acdoc e )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )
  
  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (while (setq e (car (nentsel "\n把对象从块中删除: ")))
    (_StartUndo acdoc) (LM:RemovefromBlock acdoc e) (_EndUndo acdoc)
  )
  (princ)
)

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;