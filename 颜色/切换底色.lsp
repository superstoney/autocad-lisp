(defun c:DCP( / aci col dclid dclname dclstr dd display filen i tempname vvla)
(defun gps->aci2ole (aci / col)
  (setq col (vla-get-truecolor (vla-get-ActiveLayer(vla-get-ActiveDocument(vlax-get-acad-object)))))
  (vla-put-ColorIndex col aci)
  (vlax-variant-value (vlax-make-variant (+ (* (vla-get-blue  col) 65536)
					    (* (vla-get-green col) 256)
					       (vla-get-red   col)
					  ) vlax-vblong
		      )
  )
)
(defun actdefaultbutton()
  (setq varBlack (vlax-Make-Variant 0 		vlax-vbLong)) 
  (setq varWhite (vlax-Make-Variant 16777215 	vlax-vbLong))
  (vla-put-graphicswinmodelbackgrndcolor DISPLAY varBlack)
  (vla-Put-ModelCrosshairColor DISPLAY varWhite)
  (vla-put-graphicswinlayoutbackgrndcolor DISPLAY varWhite)
  (vla-Put-LayoutCrosshairColor DISPLAY varBlack)
  (vla-put-TextWinBackgrndColor DISPLAY varWhite)
  (vla-put-TextWinTextColor DISPLAY varBlack) 
)
(defun actradiobutton($value)
  (setq #changebackradio# $value)
  (cond 
     ((= $value "a")(setvar "TILEMODE" 1)(setq vvla 'vla-put-graphicswinmodelbackgrndcolor) )
     ((= $value "b")(setvar "TILEMODE" 1)(setq vvla 'vla-Put-ModelCrosshairColor) )
     ((= $value "c")(setvar "TILEMODE" 0)(setq vvla 'vla-put-graphicswinlayoutbackgrndcolor) )
     ((= $value "d")(setvar "TILEMODE" 0)(setq vvla 'vla-Put-LayoutCrosshairColor) )
     ((= $value "e")(setvar "TILEMODE" 1)(setq vvla 'vla-put-TextWinBackgrndColor))
     ((= $value "f")(setvar "TILEMODE" 1)(setq vvla 'vla-put-TextWinTextColor))
  )
)
(setq DISPLAY (vla-get-display(vla-get-preferences(vlax-get-acad-object))))
(setq dclstr 
"mimg:image_button {aspect_ratio = 0.9 ;fixed_height = true ;
    fixed_width = true ; width = 2.8 ;horizontal_margin = none ;
    vertical_margin = none ;
}
mmimg:image {
    aspect_ratio = 0.9 ;    color = -15 ;
    fixed_height = true ;    fixed_width = true ;
    width = 2.8 ;    horizontal_margin = none ;
    vertical_margin = none ;
}
rbt:radio_button{horizontal_margin=none;vertical_margin=none;fixed_width = true ; width = 36;}\n
sss:dialog {label = \"颜色选项修改 V1.0\" ;
    :text {
        label = \"AutoCAD 颜色索引 (ACI):\" ;
    }
    :column {
        :column {
"
)
(setq i 18)
(repeat 5
    (setq j i)	
    (setq dclstr (strcat dclstr ":row{\n"))
    (repeat 24      
      (setq dclstr (strcat dclstr ":mimg{color=" (itoa j) ";key=\""(itoa j)"\";}\n" ))      
      (setq j (+ 10 j))
    )
    (setq i (- i 2))
    (setq dclstr (strcat dclstr  "}\n"))   
)
(setq dclstr (strcat dclstr  "}}:spacer { } :spacer {} :column {\n"))
(setq i 11)
(repeat 5
    (setq j i)	
    (setq dclstr (strcat dclstr ":row{\n"))
    (repeat 24      
      (setq dclstr (strcat dclstr ":mimg{color=" (itoa j) ";key=\""(itoa j)"\";}\n" ))      
      (setq j (+ 10 j))
    )
    (setq i (+ i 2))
    (setq dclstr (strcat dclstr  "}\n"))   
)
(setq dclstr (strcat dclstr  "}:spacer { } :row { :column {\n:row {\n"))       
 (setq j 1)   
 (repeat 9      
      (setq dclstr (strcat dclstr ":mimg{color=" (itoa j) ";width = 3;key=\""(itoa j)"\";}\n" ))      
      (setq j (1+ j))
 )    
(setq dclstr (strcat dclstr  "}:row {\n"))   
(setq j 250)   
(repeat 6      
    (setq dclstr (strcat dclstr ":mimg{color=" (itoa j) ";width = 3;key=\""(itoa j)"\";}\n" ))    
    (setq j (1+ j))
) 
(setq dclstr (strcat dclstr " :mmimg {width = 3;}\n" ))
(setq dclstr (strcat dclstr ":mimg{color=7;width = 3;key=\"7a\";}\n" ))
(setq dclstr (strcat dclstr ":mimg{color=0;width = 3;key=\"0\";}\n" ))
              
 (setq dclstr (strcat dclstr                
    "}:edit_box {key=\"box\";edit_width = 15 ;fixed_width = true ;label = \"颜色\" ;}} \n"    
     ":boxed_radio_column{key=\"radio\";\n"
           " :rbt {key=\"a\";  label = \"模型窗口背景颜色\" ;}\n"
           " :rbt {key=\"b\";label = \"模型十字光标颜色\" ;  }\n"                    
           " :rbt {key=\"e\";label = \"命令行背景颜色\" ;  }\n"
           " :rbt {key=\"f\";label = \"命令行文字颜色\" ;}\n"
           " :rbt {key=\"c\";label = \"布局窗口背景颜色\" ;    }\n"
           " :rbt {key=\"d\";label = \"布局十字光标颜色\" ;   }   \n"
           " }} "
           ":column {
    : row {
        fixed_width = true;
        alignment = centered;
        :button {key=\"default\";label=\"全部默认\";}
        :spacer {width = 2; }
        :button {label=\"    取消    \";is_cancel=true;}
    }
}}"
     ))

;;;;;;;;
(setq dclname 
(cond  
((setq tempname (vl-filename-mktemp "gps-dcl-tmp.dcl") filen (open tempname "w"))  
(princ dclstr filen)
 
(close filen)
tempname
)))
(setq dclid (load_dialog dclname))
(if (not (new_dialog "sss" dclid)) (progn (alert "dcl对话框加载失败.")(exit)))
(setq i 0)          
(repeat 256
 (action_tile (itoa i)  "(set_tile \"box\" $key)((eval vvla) DISPLAY (gps->aci2ole (atoi $key)))")
 (setq i (1+ i))
)
(if (null #changebackradio#)(setq #changebackradio# "a"))
(actradiobutton #changebackradio#)
(set_tile #changebackradio# "1")

(action_tile "7a"  "(set_tile \"box\" \"7\")((eval vvla) DISPLAY (gps->aci2ole 7))")
(action_tile "radio" "(actradiobutton $value)")
(action_tile "default" "(actdefaultbutton)")
(action_tile "cancel" "(done_dialog 0)")

(action_tile "help" "(alert \"模型,布局及命令行背景光标颜色修改!\n xshrimp 写于 2010.10\")")
(setq dd (start_dialog))
(unload_dialog dclid)
(vl-file-delete dclname) 
)