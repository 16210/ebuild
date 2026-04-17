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

;; 构造 <依赖说明符节点*>
;; 参数：
;;	depvar			ebuild 依赖变量的值
;;	use-list		启用的应用标志列表
;;	iuse-effective		应用标志列表形式的 IUSE_EFFECTIVE
;;	=->nil?			是否将插槽依赖中的“=”处理成 nil
;; 返回值：
;;	成功返回：
;;	一个类型为全选组的 <依赖说明符节点*> 或 nil，
;;	树形结构中所有原子节点的 <匹配情况> 设为 nil，
;;	非原子节点的 <匹配情况> 在此基础上填写
;;	失败返回：
;;	* depvar 不合法返回 :error
;;	* 如果在应用标志条件组中出现了
;;	  IUSE_EFFECTIVE 中不存在的应用标志则返回 :not-effective
;;	* 遇到 单选组 或 至多单选组 则返回 :=1 或 :<=1
(defun cons-depspec-node* (depvar use-list iuse-effective =->nil?)
  (labels (;; <说明符> 转 <依赖说明符节点*>
	   ;; 参数：
	   ;;		spec	一个 <说明符>
	   ;;		ag	是否位于任选组中
	   ;; 返回值：
	   ;;		转换成功返回一个 <依赖说明符节点*> 或 nil
	   ;;		遇到错误则从 cons-depspec-node* 函数
	   ;;		返回错误对应的返回值
	   (parse-dsnode* (spec ag)
	     (let* ((stype (spec-type spec))
		    (stext (spec-text spec))
		    (depspec-node* (mk-depnode* stype nil)))
	       (when (or (eql stype :=1) (eql stype :<=1))
		 (return-from cons-depspec-node* stype))
	       (when (spectype-usecond-p stype)
		 (unless (find (usecond-flag stype)
			       iuse-effective :test #'string=)
		   (return-from cons-depspec-node* :not-effective))
		 (when (xor (usecond-enable? stype)
			    (find (usecond-flag stype)
				  use-list :test #'string=))
		   (return-from parse-dsnode* nil))
		 (setf stype :all
		       (depnode*-type depspec-node*) :all))
	       (if (eql stype t)
		 (unless (setf (depnode*-content depspec-node*)
			       (parse-depspec* stext use-list iuse-effective
					       (or ag =->nil?)))
		   (return-from cons-depspec-node* :error))
		 (let ((spec-list (split-spec stext #'identity nil)))
		   (when (eql spec-list :error)
		     (return-from cons-depspec-node* :error))
		   (setf (depnode*-match depspec-node*) (eql stype :all))
		   (dolist (sp spec-list)
		     (let ((dsnode* (parse-dsnode* sp (eql stype :any))))
		       (when dsnode*
			 (setf (depnode*-content depspec-node*)
			       (nconc (depnode*-content depspec-node*) (list dsnode*)))
			 (if (eql stype :all)
			   (unless (satisfip dsnode*)
			     (setf (depnode*-match depspec-node*) nil))
			   (when (satisfip dsnode*)
			     (setf (depnode*-match depspec-node*) t))))))))
	       (when (depnode*-content depspec-node*)
		 depspec-node*))))
    (when (some (lambda (c) (char/= c #\Space)) depvar)
      (parse-dsnode* (make-spec :all depvar) nil))))

;; 新增软件包记录
;   向给定的 <软件包记录> 表中添加一个状态为“待编译”的 <软件包记录>，
;   四种依赖的 <依赖说明符节点*> 根据依赖变量构造，
;    原子节点的 <匹配情况> 设为 nil，非原子节点的 <匹配情况> 在此基础上填写，
;    对于构建依赖、安装依赖和弱运行依赖，在构造节点时将插槽依赖中的“=”处理成 nil
;   四种依赖的 <反向匹配> 列表设为 nil
;; 参数：
;;	pkgrlst			<软件包记录> 表
;;	category		类别名称
;;	pkgname			非限定的软件包名称
;;	version			<软件包版本>
;;	form			<形式>
;;	vstr			<版本字符串>
;;	group-list		<所在分组列表>
;;	use-list		启用的应用标志列表
;;	iuse-effective		应用标志列表形式的 IUSE_EFFECTIVE
;;	regular-slot		<主插槽>
;;	sub-slot		<子插槽>
;;	bash-declare-list	<declare> 列表
;; 返回值：
;;	成功返回 2 个值：
;;	1. 添加的 <软件包记录>
;;	2. 完成新增操作后的 <软件包记录> 表
;;	失败返回 1 个值：
;;	* 构建，安装，强运行，弱运行 <依赖说明符节点*> 构造失败分别返回
;;	  :cons-bdepnode-failed
;;	  :cons-idepnode-failed
;;	  :cons-rdepnode-failed
;;	  :cons-pdepnode-failed
(defun new-pkg-record (pkgrlst category pkgname version form vstr group-list use-list iuse-effective regular-slot sub-slot bash-declare-list)
  (let ((pkg-record (mk-pkgrec category pkgname version
			       (mk-locaux form vstr)
			       group-list
			       use-list iuse-effective
			       (mk-slotpair regular-slot sub-slot))))
    (dolist (idx-depname-err '((0 "BDEPEND" :cons-bdepnode-failed)
			       (1 "IDEPEND" :cons-idepnode-failed)
			       (2 "RDEPEND" :cons-rdepnode-failed)
			       (3 "PDEPEND" :cons-pdepnode-failed)))
      (let ((depvar (declare-value-scalar (find (cadr idx-depname-err) bash-declare-list
						:key #'(lambda (d) (declare-name d)) :test #'string=))))
	(if depvar
	  (let ((dsnode* (cons-depspec-node* depvar use-list iuse-effective (/= (car idx-depname-err) 2))))
	    (if (and (symbolp dsnode*) dsnode*)
	      (return-from new-pkg-record (caddr idx-depname-err))
	      (setf (pkgrec-depnode* pkg-record (car idx-depname-err)) dsnode*))))))
    (values pkg-record (cons pkg-record pkgrlst))))

;; 建立匹配关系
;   根据软件包记录表填写给定软件包记录的
;   一个 <依赖说明符节点*> 树中各个节点的 <匹配情况>，
;   同时更新对应软件包记录的反向匹配
;; 参数：
;;	pkgrlst		<软件包记录> 表
;;	pkg-record	<软件包记录>
;;	class-idx	依赖种类索引(取值为0/1/2/3，分别表示构建/安装/强运行/弱运行依赖)
;; 返回值：
;;	[未定义] 目前的实现为 nil
(defun establish-match (pkgrlst pkg-record class-idx)
  (dolist (pkgrec-iter (mapcan #'(lambda (rec)
				   (if (eql (pkgrec-status rec) :installed)
				     (list rec)))
			       pkgrlst))
    (let ((path-list (add-match-version (pkgrec-depnode* pkg-record class-idx) pkgrec-iter t)))
      (when path-list
	(setf (pkgrec-reverse pkgrec-iter class-idx)
	      (nconc (pkgrec-reverse pkgrec-iter class-idx)
		     (list (mk-revmatch (pkgrec-category pkg-record)
					(pkgrec-pkgname pkg-record)
					(pkgrec-version pkg-record)
					path-list))))))))

;; 计算依赖的目标列表
;; 参数：
;;	depspec-node*	<依赖说明符节点*> 或 nil
;; 返回值：
;;	depspec-node* 的目标列表或 nil
(defun dep-target-list (depspec-node*)
  (when depspec-node*
    (if (eql (depnode*-type depspec-node*) t)
      (list (list depspec-node*))
      (let (target-list (fn (if (eql (depnode*-type depspec-node*) :all)
			      #'append-product
			      #'nconc)))
	(dolist (sub (depnode*-content depspec-node*))
	  (let ((sub-target-list (dep-target-list sub)))
	    (if (null target-list)
	      (setf target-list sub-target-list)
	      (setf target-list (funcall fn target-list sub-target-list)))))
	target-list))))

;; 建立反向匹配关系
;   遍历软件包记录表，填写给定软件包记录的一个 <反向匹配> 列表
;   同时更新对应软件包记录 <依赖说明符节点*> 树中的 <匹配情况>
;; 参数：
;;	pkgrlst		<软件包记录> 表
;;	pkg-record	<软件包记录>
;;	class-idx	依赖种类索引(取值为0/1/2/3，分别表示构建/安装/强运行/弱运行依赖)
;;	check-only?	仅检查。t 表示仅仅得到返回值而不实际更新数据结构
;; 返回值：
;;	遍历到的 <软件包记录> 当中，
;;	依赖说明符根节点更新前满足但更新后不满足的 <软件包记录> 组成的列表
(defun establish-rev-match (pkgrlst pkg-record class-idx &optional check-only?)
  (let (xklist)
    (dolist (pkgrec-iter pkgrlst xklist)
      (when (pkgrec-depnode* pkgrec-iter class-idx)
	(multiple-value-bind (path-list satchg) (add-match-version
						  (pkgrec-depnode* pkgrec-iter class-idx)
						  pkg-record
						  (not check-only?))
	  (when path-list
	    (when (eql satchg :-)
	      (setf xklist (nconc xklist (list pkgrec-iter))))
	    (unless check-only?
	      (setf (pkgrec-reverse pkg-record class-idx)
		    (nconc (pkgrec-reverse pkg-record class-idx)
			   (list (mk-revmatch (pkgrec-category pkgrec-iter)
					      (pkgrec-pkgname pkgrec-iter)
					      (pkgrec-version pkgrec-iter)
					      path-list)))))))))))

;; 解除匹配关系
;   遍历软件包记录的一个 <依赖说明符节点*> 树，
;   从原子节点 <匹配情况> 中删除
;   在软件包记录表中能找到对应软件包记录的 <软件包版本>
;   并更新路径上节点的 <匹配情况>，
;   同时将被删除的 <软件包版本> 对应的 <反向匹配> 删除
;; 参数：
;;	pkgrlst		<软件包记录> 表
;;	pkg-record	<软件包记录>
;;	class-idx	依赖种类索引(取值为0/1/2/3，分别表示构建/安装/强运行/弱运行依赖)
;; 返回值：
;;	[未定义] 目前的实现为 nil
(defun free-match (pkgrlst pkg-record class-idx)
  (let ((category (pkgrec-category pkg-record))
	(pkgname (pkgrec-pkgname pkg-record))
	(version (pkgrec-version pkg-record))
	(depspec-root-node (pkgrec-depnode* pkg-record class-idx)))
    (labels (;; 遍历 <依赖说明符节点*> 树
	     (bl-dsnode-tree (dsnode)
			     (if (eql (depnode*-type dsnode) t)
			       (let ((depspec (depnode*-content dsnode)))
				 (dolist (mver (copy-list (depnode*-match dsnode)))
				   (let ((beiyilai-rec (find-if #'(lambda (rec)
								    (and (string= (qualified-pkgname (pkgrec-category rec)
												     (pkgrec-pkgname rec))
										  (depspec-qpkgname depspec))
									 (equal (pkgrec-version rec) mver)))
								pkgrlst)))
				     (if beiyilai-rec
				       (let* ((revmatch-list (pkgrec-reverse beiyilai-rec class-idx))
					      (revmatch (find-if #'(lambda (rev)
								     (and (string= (revmatch-category rev) category)
									  (string= (revmatch-pkgname rev) pkgname)
									  (equal (revmatch-version rev) version)))
								 revmatch-list)))
					 (del-match-version depspec-root-node (revmatch-pathlist revmatch) mver t)
					 (setf (pkgrec-reverse beiyilai-rec class-idx) (delete revmatch revmatch-list)))))))
			       (dolist (n (depnode*-content dsnode))
				 (bl-dsnode-tree n)))))
      (bl-dsnode-tree depspec-root-node))))

;; 解除反向匹配关系
;   遍历给定软件包记录的一个 <反向匹配> 列表，从中删除
;   在软件包记录表中能找到对应软件包记录的 <反向匹配>，
;   同时根据 <路径列表> 从对应软件包记录的 <依赖说明符节点*> 树中
;   删除给定软件包记录的 <软件包版本> 并更新路径上节点的 <匹配情况>
;; 参数：
;;	pkgrlst		<软件包记录> 表
;;	pkg-record	<软件包记录>
;;	class-idx	依赖种类索引(取值为0/1/2/3，分别表示构建/安装/强运行/弱运行依赖)
;;	check-only?	仅检查。t 表示仅仅得到返回值而不实际更新数据结构
;;	new-pkgrec	升/降级后的 <软件包记录>。不是升/降级则填 nil 或不填
;; 返回值：
;;	“悬空软件包记录列表”，每个元素是因此次更新导致依赖说明符根节点不满足的 <软件包记录>
;;	如果 new-pkgrec 不是 nil，则还会返回第二个值：
;;	“重编可修复性列表”，每项的取值为 t 或 nil，
;;	表示“悬空软件包记录列表”中索引和自己相同的 <软件包记录> 能否通过重编(重选插槽)来修复依赖问题
(defun free-rev-match (pkgrlst pkg-record class-idx &optional check-only? (new-pkgrec nil))
  (let (xuankong-list rebuildp-list
	(version (pkgrec-version pkg-record)))
    (dolist (revmatch (copy-list (pkgrec-reverse pkg-record class-idx)))
      (let ((depend-pkg (find-if #'(lambda (rec)
				     (and (string= (pkgrec-category rec)
						   (revmatch-category revmatch))
					  (string= (pkgrec-pkgname rec)
						   (revmatch-pkgname revmatch))
					  (equal (pkgrec-version rec)
						 (revmatch-version revmatch))))
				 pkgrlst)))
	(if depend-pkg
	  (multiple-value-bind (s r) (del-match-version (pkgrec-depnode* depend-pkg class-idx)
							(revmatch-pathlist revmatch)
							version
							(not check-only?)
							new-pkgrec)
	    (if (eql s :-)
	      (setf xuankong-list (nconc xuankong-list (list depend-pkg))
		    rebuildp-list (nconc rebuildp-list (list r))))
	    (if (not check-only?)
	      (setf (pkgrec-reverse pkg-record class-idx)
		    (delete revmatch (pkgrec-reverse pkg-record class-idx))))))))
    (if new-pkgrec
      (values xuankong-list rebuildp-list)
      xuankong-list)))

;; 删除软件包记录
;   从给定的 <软件包记录> 表中删除一个 <软件包记录>
;; 参数：
;;	pkgrlst		<软件包记录> 表
;;	pkg-record	<软件包记录>
;; 返回值：
;;	执行删除后的 <软件包记录> 表
(declaim (inline delete-pkg-record))
(defun delete-pkg-record (pkgrlst pkg-record)
  (delete pkg-record pkgrlst))
