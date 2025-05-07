;;; Get Civil 3D Surface Max and Min Elevation
;;; Author: superstoney
;;; Date: 2025-04-22
;;; Version: 1.1

(defun c:GetSurfElevation (/ surf surf_obj surf_name max_elev min_elev)
  ;; Initialize .NET environment
  (vl-load-com)
  
  ;; Error handler
  (defun *error* (msg)
    (if msg
      (princ (strcat "\nError: " msg))
      (princ "\nError: Program terminated")
    )
    (princ "\nCommand: GETSURFELEV")
    (princ "\nDate/Time: 2025-04-22 08:38:51 UTC")
    (princ "\nUser: superstoney")
    (princ)
    (setq *error* nil)
    (princ)
  )
  
  ;; Check if Civil 3D is loaded
  (if (null (vl-bb-ref "AeccRxAppName"))
    (progn
      (princ "\nError: Civil 3D is not loaded. Please ensure you are running Civil 3D, not plain AutoCAD.")
      (exit)
    )
  )
  
  ;; Check if there's an active document
  (if (null (vla-get-activedocument (vlax-get-acad-object)))
    (progn
      (princ "\nError: No drawing file is open")
      (exit)
    )
  )
  
  ;; Prompt user to select a surface
  (princ "\nPlease select a Civil 3D surface: ")
  (setq surf (entsel))
  
  ;; Exit if no object is selected
  (if (null surf)
    (progn
      (princ "\nNo surface selected, exiting program")
      (exit)
    )
  )
  
  ;; Get the entity name from selection
  (setq surf (car surf))
  
  ;; Try to convert to VLA object with error checking
  (if (null (setq surf_obj (vla-ename->vla-object surf)))
    (progn
      (princ "\nError: Unable to access selected object")
      (exit)
    )
  )
  
  ;; Get object type with error checking
  (setq obj_type (vlax-get-property surf_obj 'ObjectName))
  (princ (strcat "\nSelected object type: " obj_type))
  
  ;; Verify if selected object is a Civil 3D surface
  (if (/= obj_type "AeccDbTinSurface")
    (progn
      (princ "\nError: Selected object is not a Civil 3D surface")
      (princ "\nValid surface types: AeccDbTinSurface")
      (exit)
    )
  )
  
  (princ "\nProcessing surface...")
  
  ;; Get surface properties with error checking
  (setq surf_name 
    (vl-catch-all-apply 'vlax-get-property (list surf_obj 'Name))
  )
  
  (if (vl-catch-all-error-p surf_name)
    (setq surf_name "Unknown")
  )
  
  ;; Get elevation data with error checking
  (setq max_elev 
    (vl-catch-all-apply 'vlax-get-property 
      (list surf_obj 'MaximumElevation)
    )
  )
  
  (setq min_elev 
    (vl-catch-all-apply 'vlax-get-property 
      (list surf_obj 'MinimumElevation)
    )
  )
  
  ;; Check if elevation data was retrieved successfully
  (if (or (vl-catch-all-error-p max_elev)
          (vl-catch-all-error-p min_elev))
    (progn
      (princ "\nError: Unable to get surface elevation data")
      (exit)
    )
  )
  
  ;; Output results
  (princ "\n----------------------------------------")
  (princ (strcat "\nSurface Name: " surf_name))
  (princ (strcat "\nMaximum Elevation: " (rtos max_elev 2 3)))
  (princ (strcat "\nMinimum Elevation: " (rtos min_elev 2 3)))
  (princ "\n----------------------------------------")
  (princ)
)

;; Set command alias
(vl-cmdf "COMMAND" "GETSURFELEV" "GetSurfElevation")

;; Provide command load confirmation
(princ "\nCommand loaded - Type GETSURFELEV to run the program")
(princ)