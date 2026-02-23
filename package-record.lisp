(defmacro mk-slotpair (regular-slot sub-slot)
  `(cons ,regular-slot ,sub-slot))
(defmacro slotpair-regular (slotpair)
  `(car ,slotpair))
(defmacro slotpair-sub (slotpair)
  `(cdr ,slotpair))

(defmacro mk-depnode* (stype content &optional (match nil))
  `(cons (cons ,stype ,content) ,match))
(defmacro depnode*-type (n)
  `(caar ,n))
(defmacro depnode*-content (n)
  `(cdar ,n))
(defmacro depnode*-match (n)
  `(cdr ,n))

(defmacro mk-revmatch (category pkgname version pathlist)
  `(list ,category ,pkgname ,version ,pathlist))
(defmacro revmatch-category (r)
  `(car ,r))
(defmacro revmatch-pkgname (r)
  `(cadr ,r))
(defmacro revmatch-version (r)
  `(caddr ,r))
(defmacro revmatch-pathlist (r)
  `(cadddr ,r))

(defmacro pkgrec-category (pkgrec)
  `(car ,pkgrec))
(defmacro pkgrec-pkgname (pkgrec)
  `(cadr ,pkgrec))
(defmacro pkgrec-version (pkgrec)
  `(caddr ,pkgrec))
(defmacro pkgrec-locaux (pkgrec)
  `(cadddr ,pkgrec))
(defmacro pkgrec-grplst (pkgrec)
  `(nth 4 ,pkgrec))
(defmacro pkgrec-uselst (pkgrec)
  `(nth 5 ,pkgrec))
(defmacro pkgrec-iuse_effective (pkgrec)
  `(nth 6 ,pkgrec))
(defmacro pkgrec-slotpair (pkgrec)
  `(nth 7 ,pkgrec))
(defmacro pkgrec-depnode*-bdep (pkgrec)
  `(nth 8 ,pkgrec))
(defmacro pkgrec-depnode*-idep (pkgrec)
  `(nth 9 ,pkgrec))
(defmacro pkgrec-depnode*-rdep (pkgrec)
  `(nth 10 ,pkgrec))
(defmacro pkgrec-depnode*-pdep (pkgrec)
  `(nth 11 ,pkgrec))
(defmacro pkgrec-depnode* (pkgrec class-idx)
  `(nth (+ 8 ,class-idx) ,pkgrec))
(defmacro pkgrec-reverse-bdep (pkgrec)
  `(nth 12 ,pkgrec))
(defmacro pkgrec-reverse-idep (pkgrec)
  `(nth 13 ,pkgrec))
(defmacro pkgrec-reverse-rdep (pkgrec)
  `(nth 14 ,pkgrec))
(defmacro pkgrec-reverse-pdep (pkgrec)
  `(nth 15 ,pkgrec))
(defmacro pkgrec-reverse (pkgrec class-idx)
  `(nth (+ 12 ,class-idx) ,pkgrec))
(defmacro pkgrec-status (pkgrec)
  `(nth 16 ,pkgrec))
(defmacro mk-pkgrec (category pkgname version locaux grplst uselst iuse-effective slotpair &optional bdep idep rdep pdep rev-bdep rev-idep rev-rdep rev-pdep (status :not-built))
  `(list ,category ,pkgname ,version ,locaux ,grplst ,uselst ,iuse-effective ,slotpair ,bdep ,idep ,rdep ,pdep ,rev-bdep ,rev-idep ,rev-rdep ,rev-pdep ,status))

(defmacro atom-satisfip (depnode*)
  (let ((g (gensym)))
    `(let ((,g ,depnode*))
       (xor (depspec-blockp
	      (depnode*-content ,g))
	    (depnode*-match ,g)))))
(defmacro group-satisfip (depnode*)
  `(depnode*-match ,depnode*))
;; [谓词] <依赖说明符节点*> 已满足
;; 参数：
;;	depnode*	<依赖说明符节点*>
;; 返回值：
;;	t 或 nil
(declaim (inline satisfip))
(defun satisfip (depnode*)
  (if (eql (depnode*-type depnode*) t)
    (atom-satisfip depnode*)
    (group-satisfip depnode*)))

;; 添加匹配版本
;   检查一个软件包记录能否和 <依赖说明符节点*> 树中的原子说明符
;   忽略阻塞符地匹配，如果匹配则更新路径节点上的 <匹配情况>
;; 参数：
;;	dsnode*		<依赖说明符节点*>
;;	pkgrec		一个状态为“已安装”的 <软件包记录>
;;	update?		是否更新。
;;			nil 表示仅模拟添加，
;;			正常得到返回值而不实际更新各个 <匹配情况>
;; 返回值：
;;	1. <广义路径列表>：
;;	   如果 dsnode* 是原子说明符且匹配，等于 (nil)；
;;	   如果 dsnode* 是非原子说明符且有原子说明符匹配，
;;	   <广义路径列表> 是一个 <路径列表>；
;;	   如果不匹配则等于 nil
;;	2. <满足变更>：
;;	   如果 dsnode* 的 <匹配情况> 由不满足变为满足，等于 :+；
;;	   如果 dsnode* 的 <匹配情况> 由满足变为不满足，等于 :-；
;;	   如果 dsnode* 的 <匹配情况> 满足与否不变则等于 nil
(defun add-match-version (dsnode* pkgrec &optional (update? t))
  (let ((dntype (depnode*-type dsnode*)) satchg)
    (if (eql dntype t)
      (when (eql (depend*-match (depnode*-content dsnode*)
				(pkgrec-category pkgrec)
				(pkgrec-pkgname pkgrec)
				(pkgrec-version pkgrec)
				(slotpair-regular (pkgrec-slotpair pkgrec))
				(slotpair-sub (pkgrec-slotpair pkgrec))
				(pkgrec-uselst pkgrec)
				(pkgrec-iuse_effective pkgrec)
				t)
		 t)
	(unless (depnode*-match dsnode*)
	  (if (depspec-blockp (depnode*-content dsnode*))
	    (setf satchg :-)
	    (setf satchg :+)))
	(when update?
	  (setf (depnode*-match dsnode*)
		(nconc (depnode*-match dsnode*)
		       (list (pkgrec-version pkgrec)))))
	(values (list nil) satchg))
      (let (path-list
	    (oldmatch (depnode*-match dsnode*))
	    (newmatch (eql dntype :all)))
	(dotimes (idx (length (depnode*-content dsnode*)))
	  (let ((sub-dsnode* (nth idx (depnode*-content dsnode*))))
	    (multiple-value-bind (p s) (add-match-version sub-dsnode*
							  pkgrec
							  update?)
	      (setf path-list (nconc path-list (mapcar #'(lambda (path)
							   (cons idx path))
						       p)))
	      (let ((subsat (satisfip sub-dsnode*)))
		(if (and (null update?) s)
		  (setf subsat (eql s :+)))
		(if (eql dntype :all)
		  (unless subsat
		    (setf newmatch nil))
		  (when subsat
		    (setf newmatch t)))))))
	(when update?
	  (setf (depnode*-match dsnode*) newmatch))
	(unless (eql oldmatch newmatch)
	  (if newmatch
	    (setf satchg :+)
	    (setf satchg :-)))
	(values path-list satchg)))))

;; 移除匹配版本
;   从原子依赖说明符节点的 <匹配情况> 中删除一个 <软件包版本>，
;   同时更新路径节点上的 <匹配情况>
;; 参数：
;;	depspec-node*	<依赖说明符节点*> 树的根节点
;;	path-list	<路径列表>
;;	version		<软件包版本>
;;	update?		是否更新。nil 表示仅模拟删除，正常得到返回值而不实际更新数据结构
;;	new-pkgrec	升/降级后的 <软件包记录>。不是升/降级则填 nil 或不填
;; 返回值：
;;	1. <满足变更>：
;;	   如果 depspec-node* 的 <匹配情况> 由不满足变为满足，等于 :+；
;;	   如果 depspec-node* 的 <匹配情况> 由满足变为不满足，等于 :-；
;;	   如果 depspec-node* 的 <匹配情况> 满足与否不变则等于 nil
;;	2. 如果 <满足变更> 等于 :- 且 new-pkgrec 不是 nil，则还会返回第二个值：
;;	   <重编可修复性>：升/降级后 depspec-node* 的依赖问题能否自动修复或通过重编(重选插槽)来修复
(defun del-match-version (depspec-node* path-list version &optional (update? t) (new-pkgrec nil))
  (let ((dntype (depnode*-type depspec-node*)))
    (if (eql dntype t)
      (let ((matchlen (length (depnode*-match depspec-node*))))
	(if update?
	  (setf (depnode*-match depspec-node*)
		(delete version (depnode*-match depspec-node*) :test #'equal)))
	(if (= matchlen 1)
	  (if (depspec-blockp (depnode*-content depspec-node*))
	    :+
	    (if new-pkgrec
	      (let ((newmatch (depend*-match (depnode*-content depspec-node*)
					     (pkgrec-category new-pkgrec)
					     (pkgrec-pkgname new-pkgrec)
					     (pkgrec-version new-pkgrec)
					     (slotpair-regular (pkgrec-slotpair new-pkgrec))
					     (slotpair-sub (pkgrec-slotpair new-pkgrec))
					     (pkgrec-uselst new-pkgrec)
					     (pkgrec-iuse_effective new-pkgrec))))
		(values :- (or (eql newmatch t) (eql newmatch :rebd))))
	      :-))))
      (let* ((oldmatch (depnode*-match depspec-node*))
	     (newmatch (eql dntype :all))
	     (rebuildp newmatch))
	(dotimes (i (length (depnode*-content depspec-node*)))
	  (let ((sub-dsnode (nth i (depnode*-content depspec-node*)))
		(sub-path-list (mapcan #'(lambda (path)
					   (if (= (car path) i)
					     (list (cdr path))))
				       path-list))
		subsat sub-satchg sub-rebuildp)
	    (if sub-path-list
	      (multiple-value-bind (s r) (del-match-version sub-dsnode sub-path-list version update? new-pkgrec)
		(setf sub-satchg s
		      sub-rebuildp r
		      subsat (satisfip sub-dsnode))
		(if (and (not update?) s)
		  (setf subsat (eql s :+))))
	      (setf subsat (satisfip sub-dsnode)))
	    (if subsat
	      (if (eql dntype :any)
		(setf newmatch t))
	      (progn
		(if (eql dntype :all)
		  (setf newmatch nil))
		(if sub-satchg
		  (if sub-rebuildp
		    (if (eql dntype :any)
		      (setf rebuildp t))
		    (if (eql dntype :all)
		      (setf rebuildp nil))))))))
	(unless (eql newmatch oldmatch)
	  (if update?
	    (setf (depnode*-match depspec-node*) newmatch))
	  (if newmatch
	    :+
	    (if new-pkgrec
	      (values :- rebuildp)
	      :-)))))))
