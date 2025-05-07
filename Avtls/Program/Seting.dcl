/*★★★★★ListDCL @ fsxm.mjtd.com★★★★★*/

seting:dialog {
    initial_focus = "cancel" ;
    children_alignment = right ;
    key = "title" ;
    label = "AVTLS工具箱参数设置" ;
    :boxed_column {
        label = "1.功能键设置" ;
        :row {
            :column {
                :toggle {
                    key = "F1" ;
                    label = "F1：软双屏窗口切换" ;
                }
                :toggle {
                    is_enabled = false ;
                    key = "F2" ;
                    label = "F2：文本窗口开关" ;
                }
                :toggle {
                    key = "F3" ;
                    label = "F3：窗口缩小至0.4倍" ;
                }
                :toggle {
                    key = "F4" ;
                    label = "F4：窗口缩放至全显" ;
                }
                :toggle {
                    key = "F7" ;
                    label = "F7：特性浮窗开关" ;
                }
            }
            :column {
                :toggle {
                    key = "F8" ;
                    label = "F8：当前文件目录" ;
                }
                :toggle {
                    key = "F9" ;
                    label = "F9：工程备忘录" ;
                }
                :toggle {
                    key = "F10" ;
                    label = "F10：界面循环切换" ;
                }
                :toggle {
                    key = "F11" ;
                    label = "F11：全屏开关" ;
                }
                :toggle {
                    key = "F12" ;
                    label = "F12：文件廋身保存" ;
                }
            }
        }
        :row {
            :button {
                key = "hotkeys" ;
                label = "单键快捷" ;
            }
            :button {
                key = "allss" ;
                label = "全选" ;
            }
            :button {
                key = "setfkey" ;
                label = "刷新菜单" ;
            }
        }
    }
    :boxed_column {
        label = "2.工具箱设置" ;
        :row {
            :column {
                :column {
                    :toggle {
                        key = "right" ;
                        label = "鼠标右键单击确认" ;
                    }
                    :toggle {
                        key = "dyn" ;
                        label = "DYN光标指针与标注输入" ;
                    }
                    :toggle {
                        key = "autoosmode" ;
                        label = "始终启用如下↓捕捉" ;
                    }
                }
                :row {
                    :column {
                        :edit_box {
                            key = "osmode" ;
                            label = "对像捕捉参数" ;
                        }
                        :edit_box {
                            key = "cursorsize" ;
                            label = "十字光标尺寸" ;
                        }
                    }
                }
            }
            :column {
                :toggle {
                    key = "mypgp" ;
                    label = "开图运行自定义快捷键" ;
                }
                :toggle {
                    key = "2click" ;
                    label = "启用本面板双击打开" ;
                }
                :toggle {
                    key = "ltls" ;
                    label = "开图启用屏幕菜单" ;
                }
                :toggle {
                    key = "autototalreader" ;
                    label = "自动测量长度与面积" ;
                }
                :toggle {
                    key = "theui1" ;
                    label = "F10循环包含精简模式" ;
                }
                :toggle {
                    key = "len" ;
                    label = "常用状态栏(重启有效)" ;
                }
            }
        }
        :row {
            :button {
                is_cancel = true ;
                key = "cancel" ;
                label = "关闭窗口" ;
            }
            :button {
                key = "just" ;
                label = "推荐" ;
            }
            :button {
                key = "accept" ;
                label = "更新设置" ;
            }
        }
    }
    :boxed_row {
        label = "3.自动加载文件夹(vlx,fas,lsp,arx,dll)" ;
        :edit_box {
            key = "path" ;
            width = 33 ;
        }
        :button {
            key = "rev" ;
            label = "修改" ;
        }
    }
    :boxed_column {
        label = "4.其它设置" ;
        :row {
            :button {
                key = "reavtlsenv" ;
                label = "环境初始化" ;
            }
            :button {
                key = "wechat" ;
                label = "交流沟通" ;
            }
            :button {
                key = "1colour" ;
                label = "单色环境" ;
            }
        }
    }
}
