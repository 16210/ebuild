;; 检查软件包是否可卸载
;   如果状态不是已安装则不可卸载，
;   如果在“不可卸载”的分组中则不可卸载，
;   如果构建反向匹配中有状态为“待编译”的软件包
;    且卸载会导致该软件包构建依赖不满足则不可卸载，
;   如果安装反向匹配中有状态为“已编译”的软件包
;    且卸载会导致该软件包安装依赖不满足则不可卸载，
;   不符合上述条件的可以卸载
;; 参数：
;;	pkg-record	<软件包记录>
;;	pkgrlst		<软件包记录> 表
;; 返回值：
;;	t 或 nil
(defun uninstallable-p (pkg-record pkgrlst)
  (if (not (eql (pkgrec-status pkg-record) :installed))
    (return-from uninstallable-p nil))
  (if (intersection (pkgrec-grplst pkg-record)
		    *bkxz-group-list*
		    :test #'string=)
    (return-from uninstallable-p nil))
  (dolist (xkpkg (free-rev-match pkgrlst pkg-record 0 t))
    (if (eql (pkgrec-status xkpkg) :not-built)
      (return-from uninstallable-p nil)))
  (dolist (xkpkg (free-rev-match pkgrlst pkg-record 1 t) t)
    (if (eql (pkgrec-status xkpkg) :built)
      (return-from uninstallable-p nil))))

;; 软件包卸载操作构造
;; 参数：
;;	pkg-record		要卸载的 <软件包记录>
;;	destination		主调函数需要满足的 <软件包依赖说明符*> 列表
;;	pkgrlst			用于推演的 <软件包记录> 表
;;	loop-check-list		强运行反向匹配循环检查列表
;;				每一项是一个软件包记录，表示强运行反向匹配路径上的一个节点
;;	forcep			是否卸载强运行反向依赖
;; 返回值：
;;	* 成功：
;;	  1. 操作列表
;;	  2. 推演得到的 <软件包记录> 表
;;	  3. 回滚函数
;;	* 失败：
;;	  1. <错误描述节点>
;;	  2. 已回滚的 <软件包记录> 表
;;	  3. :error
;;	成功还是失败根据第 3 个返回值区分
(defun uninstall-op-cons (pkg-record destination pkgrlst loop-check-list &optional forcep)
  ;; 检查强运行反向匹配是否循环
  (when (member pkg-record loop-check-list)
    (return-from uninstall-op-cons (values nil pkgrlst #'identity)))
  ;; 检查软件包是否可卸载
  (unless (uninstallable-p pkg-record pkgrlst)
    (return-from uninstall-op-cons (values (mk-errnode pkg-record :not-uninstallable nil)
					   pkgrlst
					   :error)))
  ;; 检查是否和目的相悖
  (dolist (depspec* destination)
    (when (and (not (depspec-blockp depspec*))
	       (eql (depend*-match depspec*
				   (pkgrec-category pkg-record)
				   (pkgrec-pkgname pkg-record)
				   (pkgrec-version pkg-record)
				   (slotpair-regular (pkgrec-slotpair pkg-record))
				   (slotpair-sub (pkgrec-slotpair pkg-record))
				   (pkgrec-uselst pkg-record)
				   (pkgrec-iuse_effective pkg-record))
		    t))
      (unless (and *uninstall-ignore-weak-conflict*
		   (find-if #'(lambda (pkgrec)
				(and (not (eql pkgrec pkg-record))
				     (not (member pkgrec loop-check-list))
				     (eql (depend*-match depspec*
							 (pkgrec-category pkgrec)
							 (pkgrec-pkgname pkgrec)
							 (pkgrec-version pkgrec)
							 (slotpair-regular (pkgrec-slotpair pkgrec))
							 (slotpair-sub (pkgrec-slotpair pkgrec))
							 (pkgrec-uselst pkgrec)
							 (pkgrec-iuse_effective pkgrec))
					  t)))
			    pkgrlst))
	(return-from uninstall-op-cons (values (mk-errnode pkg-record :destination-conflict nil)
					       pkgrlst
					       :error)))))
  ;; 解除强运行依赖的反向匹配关系
  (let (rollback-func new-op-list (xuankong-rev-rdep (free-rev-match pkgrlst pkg-record 2 nil)))
    (let ((x pkg-record))
      (setf rollback-func (lambda (rlst) (establish-rev-match rlst x 2 nil) rlst)))
    ;; 卸载已合并或已安装的悬空软件包
    (when xuankong-rev-rdep
      (let ((new-loop-check-list (append loop-check-list (list pkg-record))))
	(dolist (rec xuankong-rev-rdep)
	  (when (and (not (eql (pkgrec-status rec) :not-built))
		     (not (eql (pkgrec-status rec) :built)))
	    (unless forcep
	      (return-from uninstall-op-cons (values (mk-errnode pkg-record :keep-rev-rdep nil)
						     (funcall rollback-func pkgrlst)
						     :error)))
	    (multiple-value-bind (oplst rlst func) (uninstall-op-cons rec destination pkgrlst new-loop-check-list t)
	      (if (eql func :error)
		(return-from uninstall-op-cons (values (mk-errnode pkg-record
								   :recursive-failed
								   (list (mk-errnode rec
										     :uninstall-failed
										     oplst)))
						       (funcall rollback-func rlst)
						       :error))
		(progn
		  (setf pkgrlst rlst
			new-op-list (nconc new-op-list oplst))
		  (let ((f rollback-func))
		    (setf rollback-func (lambda (rl) (funcall f (funcall func rl))))))))))))
    ;; 解除构建，安装，弱运行依赖的反向匹配关系
    (dolist (idx '(0 1 3))
      (free-rev-match pkgrlst pkg-record idx nil)
      (let ((func rollback-func) (x0 pkg-record) (x1 idx))
	(setf rollback-func (lambda (rlst) (establish-rev-match rlst x0 x1 nil) (funcall func rlst)))))
    ;; 添加卸载操作，删除软件包记录
    (setf new-op-list (nconc new-op-list (list (mk-uninstall-op pkg-record))))
    (setf pkgrlst (delete-pkg-record pkgrlst pkg-record))
    (let ((func rollback-func) (x pkg-record))
      (setf rollback-func (lambda (rlst) (funcall func (cons x rlst)))))
    ;; 解除四种依赖的匹配关系
    (dotimes (idx 4)
      (free-match pkgrlst pkg-record idx)
      (let ((func rollback-func) (x0 pkg-record) (x1 idx))
	(setf rollback-func (lambda (rlst) (establish-match rlst x0 x1) (funcall func rlst)))))
    (values new-op-list pkgrlst rollback-func)))
