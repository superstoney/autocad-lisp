;;-------------------------------------------------------------------------------
;; Description: Selects all objects on the same layer(s) as the user-selected objects.
;;              After running the command, the user is prompted to select objects.
;;              Once selection is complete, all objects residing on the same layers
;;              as the initially selected objects will be selected.
;;
;; Creation Date: April 16, 2025
;; Creator:       Gemini (Google AI)
;;-------------------------------------------------------------------------------
(defun c:SELBYLAYER ()
  (vl-load-com) ; Load Visual LISP extensions

  (princ "\nSelect one or more objects: ")
  (setq initial_sset (ssget)) ; 1. Prompt user to select elements and create initial selection set

  (if initial_sset
    (progn
      (setq final_sset (ssadd)) ; 2. Create an empty selection set to accumulate results

      ; Add the initially selected objects to the final selection set
      (setq n (sslength initial_sset))
      (repeat n
        (ssadd (ssname initial_sset (setq n (1- n))) final_sset)
      )

      (setq processed_layers '()) ; Keep track of processed layers to avoid duplicates
      (setq num_initial (sslength initial_sset))

      ; 3. Iterate through the initially selected elements
      (repeat num_initial
        (setq ent (ssname initial_sset (setq num_initial (1- num_initial))))
        (setq ent_data (entget ent))
        (setq layer (cdr (assoc 8 ent_data))) ; Get the layer of the current element

        ; Check if this layer has already been processed
        (if (not (member layer processed_layers))
          (progn
            (setq processed_layers (cons layer processed_layers)) ; Mark this layer as processed

            ; 4. Select all elements on the current layer
            (setq layer_filter (list (cons 8 layer)))
            (setq all_on_layer (ssget "X" layer_filter))

            ; Add all elements from this layer to the final selection set
            (if all_on_layer
              (progn
                (setq num_on_layer (sslength all_on_layer))
                (repeat num_on_layer
                  (ssadd (ssname all_on_layer (setq num_on_layer (1- num_on_layer))) final_sset)
                )
              )
            )
          )
        )
      )

      ; Set the final selection set as the current selection
      (if (sslength final_sset)
        (sssetfirst nil final_sset)
        (princ "\nNo additional objects found on the same layers.")
      )
    )
    (princ "\nNo objects were selected.")
  )
  (princ) ; Silent exit
)
;; Define an alias for the command
(defun c:AC ()
  (c:SELBYLAYER)
)