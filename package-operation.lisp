;; 软件包操作：
;; * 构建。执行 pkg_setup，src_unpack，src_prepare，src_configure，src_compile，src_test，src_install 函数
;;   格式为：(:build 要构建的<软件包记录>)
;; * 合并。执行 pkg_preinst 函数，合并软件包文件，执行 pkg_postinst 函数
;;   格式为：(:merge 要合并的<软件包记录> 被替换的<软件包记录>列表)
;; * 配置软件包。执行 pkg_config 函数
;;   格式为：(:config 待配置的<软件包记录>)
;; * 卸载。执行 pkg_prerm 函数，删除软件包文件，执行 pkg_postrm 函数
;;   格式为：(:uninstall 要卸载的<软件包记录>)

(defmacro mk-build-op (pkgrec)
  `(list :build ,pkgrec))
(defmacro mk-merge-op (pkgrec old-pkgrec-list)
  `(list :merge ,pkgrec ,old-pkgrec-list))
(defmacro mk-config-op (pkgrec)
  `(list :config ,pkgrec))
(defmacro mk-uninstall-op (pkgrec)
  `(list :uninstall ,pkgrec))
