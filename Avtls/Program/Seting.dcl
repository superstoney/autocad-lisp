/*������ListDCL @ fsxm.mjtd.com������*/

seting:dialog {
    initial_focus = "cancel" ;
    children_alignment = right ;
    key = "title" ;
    label = "AVTLS�������������" ;
    :boxed_column {
        label = "1.���ܼ�����" ;
        :row {
            :column {
                :toggle {
                    key = "F1" ;
                    label = "F1����˫�������л�" ;
                }
                :toggle {
                    is_enabled = false ;
                    key = "F2" ;
                    label = "F2���ı����ڿ���" ;
                }
                :toggle {
                    key = "F3" ;
                    label = "F3��������С��0.4��" ;
                }
                :toggle {
                    key = "F4" ;
                    label = "F4������������ȫ��" ;
                }
                :toggle {
                    key = "F7" ;
                    label = "F7�����Ը�������" ;
                }
            }
            :column {
                :toggle {
                    key = "F8" ;
                    label = "F8����ǰ�ļ�Ŀ¼" ;
                }
                :toggle {
                    key = "F9" ;
                    label = "F9�����̱���¼" ;
                }
                :toggle {
                    key = "F10" ;
                    label = "F10������ѭ���л�" ;
                }
                :toggle {
                    key = "F11" ;
                    label = "F11��ȫ������" ;
                }
                :toggle {
                    key = "F12" ;
                    label = "F12���ļ��C����" ;
                }
            }
        }
        :row {
            :button {
                key = "hotkeys" ;
                label = "�������" ;
            }
            :button {
                key = "allss" ;
                label = "ȫѡ" ;
            }
            :button {
                key = "setfkey" ;
                label = "ˢ�²˵�" ;
            }
        }
    }
    :boxed_column {
        label = "2.����������" ;
        :row {
            :column {
                :column {
                    :toggle {
                        key = "right" ;
                        label = "����Ҽ�����ȷ��" ;
                    }
                    :toggle {
                        key = "dyn" ;
                        label = "DYN���ָ�����ע����" ;
                    }
                    :toggle {
                        key = "autoosmode" ;
                        label = "ʼ���������¡���׽" ;
                    }
                }
                :row {
                    :column {
                        :edit_box {
                            key = "osmode" ;
                            label = "����׽����" ;
                        }
                        :edit_box {
                            key = "cursorsize" ;
                            label = "ʮ�ֹ��ߴ�" ;
                        }
                    }
                }
            }
            :column {
                :toggle {
                    key = "mypgp" ;
                    label = "��ͼ�����Զ����ݼ�" ;
                }
                :toggle {
                    key = "2click" ;
                    label = "���ñ����˫����" ;
                }
                :toggle {
                    key = "ltls" ;
                    label = "��ͼ������Ļ�˵�" ;
                }
                :toggle {
                    key = "autototalreader" ;
                    label = "�Զ��������������" ;
                }
                :toggle {
                    key = "theui1" ;
                    label = "F10ѭ����������ģʽ" ;
                }
                :toggle {
                    key = "len" ;
                    label = "����״̬��(������Ч)" ;
                }
            }
        }
        :row {
            :button {
                is_cancel = true ;
                key = "cancel" ;
                label = "�رմ���" ;
            }
            :button {
                key = "just" ;
                label = "�Ƽ�" ;
            }
            :button {
                key = "accept" ;
                label = "��������" ;
            }
        }
    }
    :boxed_row {
        label = "3.�Զ������ļ���(vlx,fas,lsp,arx,dll)" ;
        :edit_box {
            key = "path" ;
            width = 33 ;
        }
        :button {
            key = "rev" ;
            label = "�޸�" ;
        }
    }
    :boxed_column {
        label = "4.��������" ;
        :row {
            :button {
                key = "reavtlsenv" ;
                label = "������ʼ��" ;
            }
            :button {
                key = "wechat" ;
                label = "������ͨ" ;
            }
            :button {
                key = "1colour" ;
                label = "��ɫ����" ;
            }
        }
    }
}
