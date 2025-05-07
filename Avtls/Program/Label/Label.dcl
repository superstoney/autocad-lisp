/*★★★★★ListDCL @ fsxm.mjtd.com★★★★★*/

label:dialog {
    label = "【A维制造】特殊标注与箭头" ;
    :row {
        :boxed_column {
            label = "特殊标注" ;
            :row {
                :button {
                    key = "111" ;
                    label = "递增编号(&D)" ;
                }
                :button {
                    key = "112" ;
                    label = "自动计数(&Z)" ;
                }
            }
            :row {
                :button {
                    key = "121" ;
                    label = "常规坐标(&C)" ;
                }
                :button {
                    key = "122" ;
                    label = "原点坐标(&Y)" ;
                }
            }
            :row {
                :button {
                    key = "131" ;
                    label = "动态标高(&B)" ;
                }
                :button {
                    is_cancel = true ;
                    label = "           " ;
                }
            }
        }
        :boxed_column {
            label = "箭头" ;
            :button {
                key = "211" ;
                label = "&1.实体箭头" ;
            }
            :button {
                key = "221" ;
                label = "&2.空心箭头" ;
            }
            :button {
                key = "231" ;
                label = "&3.其它箭头" ;
            }
        }
    }
}
