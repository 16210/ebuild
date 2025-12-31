;; 计算 IUSE_EFFECTIVE
;; 参数：
;;	iuse		IUSE 值
;;	make.defaults	系统轮廓的 <构建配置>
;; 返回值：
;;	一个应用标志列表
(defun calc-iuse-effective (iuse make.defaults)
  (let (iuse-effective)
    ;; IUSE
    (dolist (f (delete "" (string-split iuse #\Space) :test #'string=))
      (when (char= (char f 0) #\+)
	(setf f (subseq f 1)))
      (setf iuse-effective (nconc iuse-effective (list f))))
    ;; IUSE_IMPLICIT
    (let ((iuse-implicit (assoc "IUSE_IMPLICIT" make.defaults :test #'string=)))
      (dolist (f (cdr iuse-implicit))
	(setf iuse-effective (nconc iuse-effective (list f)))))
    ;; 折叠应用标志
    (let ((use-expand-implicit (assoc "USE_EXPAND_IMPLICIT" make.defaults :test #'string=))
	  (use-expand (assoc "USE_EXPAND" make.defaults :test #'string=))
	  (use-expand-unprefixed (assoc "USE_EXPAND_UNPREFIXED" make.defaults :test #'string=)))
      ;; 带前缀的折叠应用标志
      (dolist (v (intersection (cdr use-expand) (cdr use-expand-implicit) :test #'string=))
	(let ((use-expand-values (assoc (concatenate 'string "USE_EXPAND_VALUES_" v) make.defaults :test #'string=))
	      (lower_v (string-downcase v)))
	  (dolist (x (cdr use-expand-values))
	    (setf iuse-effective (nconc iuse-effective (list (concatenate 'string lower_v "_" x)))))))
      ;; 不带前缀的折叠应用标志
      (dolist (v (intersection (cdr use-expand-unprefixed) (cdr use-expand-implicit) :test #'string=))
	(let ((use-expand-values (assoc (concatenate 'string "USE_EXPAND_VALUES_" v) make.defaults :test #'string=)))
	  (when use-expand-values
	    (setf iuse-effective (nconc iuse-effective (copy-list (cdr use-expand-values))))))))
    iuse-effective))


(defmacro make-spec (stype stext)
  `(cons ,stype ,stext))
(defmacro spec-type (spec)
  `(car ,spec))
(defmacro spec-text (spec)
  `(cdr ,spec))

(defmacro mk-usecond (enable? flag)
  `(cons ,enable? ,flag))
(defmacro usecond-enable? (usecond)
  `(car ,usecond))
(defmacro usecond-flag (usecond)
  `(cdr ,usecond))
;; [谓词] <说明符类型> 是不是 <应用标志条件组>
(defmacro spectype-usecond-p (stype)
  `(consp ,stype))


;; 拆分说明符
;; 参数：
;;	spec-string	说明符字符串
;;	atom-spec-p	原子说明符判别函数
;;	src-uri-p	拆分的是 SRC_URI
;; 返回值：
;;	一个 <说明符> 列表或 :error
(defun split-spec (spec-string atom-spec-p src-uri-p)
  (labels (;; 创建非原子说明符
	   ;; 参数：
	   ;;		group-head	说明符左圆括号之前的部分
	   ;;		content		说明符圆括号中的部分
	   ;; 创建成功返回一个 <说明符>，
	   ;; 说明符不合法则从 split-spec 函数返回 :error
	   (cons-group-spec (group-head content)
			    (when (every #'(lambda (c)		; 此处判断的逻辑应该是
					     (char= c #\Space))	; 长度为0或所有字符都是空格
					 content)		; 但长度为0时every返回t，所以省略长度判断
			      (return-from split-spec :error))
			    (cond
			      ((string= group-head "")
			       (make-spec :all content))
			      ((string= group-head "||")
			       (make-spec :any content))
			      ((string= group-head "^^")
			       (make-spec :=1 content))
			      ((string= group-head "??")
			       (make-spec :<=1 content))
			      (t		; 应用标志条件组
				(let ((enable? (char/= (char group-head 0) #\!))
				      flag (len (length group-head)))
				  (when (char/= (char group-head (1- len)) #\?)
				    (return-from split-spec :error))
				  (setf flag (subseq group-head (if enable? 0 1) (1- len)))
				  (unless (use-flag-name-p flag t)
				    (return-from split-spec :error))
				  (make-spec (mk-usecond enable? flag) content)))))
	   ;; 给定一个说明符字符串和一个 #\) 的索引，
	   ;; 向左查找相匹配的 #\( 并将其索引返回，
	   ;; 找不到则从 split-spec 函数返回 :error
	   (prev-open-parenthesis (spec-str idx)
				  (do ((k 1))
				    ((minusp (decf idx))
				     (return-from split-spec :error))
				    (if (char= (char spec-str idx) #\))
				      (incf k)
				      (when (and (char= (char spec-str idx) #\()
						 (zerop (decf k)))
					(return-from prev-open-parenthesis idx)))))
	   ;; 说明符字符串转 <说明符> 列表
	   ;; 转换成功返回一个 <说明符> 列表，
	   ;; 如果有某个组说明符不合法
	   ;; 则从 split-spec 函数返回 :error
	   (parse-spectext (spec-str)
			   (let ((ep (position #\Space spec-str :from-end t :test #'char/=))
				 sp tail-spec)
			     (when ep
			       (if (char= (char spec-str ep) #\))	; 检查是组说明符还是原子说明符
				 (let ((kp (prev-open-parenthesis spec-str ep)))
				   (setf sp (position #\Space spec-str :from-end t :test #'char= :end kp)
					 sp (if sp (1+ sp) 0)
					 tail-spec (cons-group-spec (subseq spec-str sp kp) (subseq spec-str (1+ kp) ep))))
				 (setf sp (position #\Space spec-str :from-end t :test #'char= :end ep)
				       sp (if sp (1+ sp) 0)
				       tail-spec (make-spec t (subseq spec-str sp (1+ ep)))))
			       (nconc (parse-spectext (subseq spec-str 0 sp)) (list tail-spec))))))

    (let ((spec-list (parse-spectext spec-string)))
      ;; 处理 "->"
      (when src-uri-p
	(do ((prev nil iter)
	     (iter spec-list (cdr iter))
	     (next (cdr spec-list) (cdr next)))
	  ((null iter))
	  (when (and (eql (spec-type (car iter)) t)
		     (string= (spec-text (car iter)) "->"))
	    (if (and (eql (spec-type (car prev)) t)
		     (eql (spec-type (car next)) t))
	      (setf (spec-text (car prev)) (concatenate 'string
							(spec-text (car prev))
							" -> "
							(spec-text (car next)))
		    (cdr prev) (cdr next)
		    iter prev
		    next (cdr next))
	      (return-from split-spec :error)))))
      ;; 检查原子说明符是否合法
      (dolist (spec spec-list spec-list)
	(when (and (eql (spec-type spec) t)
		   (not (funcall atom-spec-p (spec-text spec))))
	  (return-from split-spec :error))))))

(defmacro make-spec* (stype scontent)
  `(cons ,stype ,scontent))
(defmacro spec*-content (spec*)
  `(cdr ,spec*))

;; 组说明符匹配判别
;; 参数：
;;	gtype		<说明符类型>，必须是 :all，:any，:=1 或 :<=1
;;	match?		组说明符应当匹配(t)还是不匹配(nil)
;;	prev-len	匹配情况已确定的成员数量
;;	prev-match	已匹配的成员数量
;;	rest-len	匹配情况未确定的成员数量
;;			rest-len 和 prev-len 不能同时传 0
;; 返回值：
;;	组 <说明符*> 是否有可能满足匹配要求
(defun match-discriminant (gtype match? prev-len prev-match rest-len)
  (case gtype
    (:all
      (if match?
	(= prev-match prev-len)
	(or (< prev-match prev-len)
	    (plusp rest-len))))
    (:any
      (if match?
	(or (plusp prev-match)
	    (plusp rest-len))
	(zerop prev-match)))
    (:=1
      (if match?
	(or (= prev-match 1)
	    (and (zerop prev-match)
		 (plusp rest-len)))
	(or (>= prev-match 2)
	    (zerop prev-match)
	    (plusp rest-len))))
    (:<=1
      (if match?
	(<= prev-match 1)
	(>= (+ prev-match rest-len) 2)))))

;; 计算组说明符的应用标志约束
;; 参数：
;;	use+		必须启用的应用标志列表
;;	use-		必须禁用的应用标志列表
;;	gtype		<说明符类型>，必须是 :all，:any，:=1 或 :<=1
;;	prev-len	匹配情况已确定的成员数量
;;	prev-match	已匹配的成员数量
;;	rest-list	匹配情况未确定的成员 <说明符*> 列表
;;	match?		组说明符应当匹配(t)还是不匹配(nil)
;; 返回值：
;;	* 成功：
;;	  1. rest-list 不全忽略情况下必须启用的应用标志列表组成的列表
;;	  2. rest-list 不全忽略情况下必须禁用的应用标志列表组成的列表
;;	  二者对应顺序保持一致
;;	  3. rest-list 为空或全忽略情况下必须启用的应用标志列表组成的列表
;;	  4. rest-list 为空或全忽略情况下必须禁用的应用标志列表组成的列表
;;	  二者对应顺序保持一致
;;	* 应用标志不匹配：
;;	  1. :not-matched
;;	  2. 不匹配的标志名称
;;	* 不满足匹配要求：
;;	  1. :not-matched
;;	  [2]. nil
(defun use-state-constraints (use+ use- gtype prev-len prev-match rest-list match?)
  (let ((rest-len (length rest-list))
	next-spec* stype scontent
	enum-collapse enum-use+ enum-use-
	next-ig-constraints+ next-ig-constraints-
	next-nig-constraints+ next-nig-constraints- next-match
	group-ig-ulist+ group-ig-ulist-
	group-nig-ulist+ group-nig-ulist- mismatched-flag)
    (if (zerop rest-len)
      (if (or (zerop prev-len)
	      (match-discriminant gtype match? prev-len prev-match 0))
	(return-from use-state-constraints (values nil nil (list use+) (list use-)))
	(return-from use-state-constraints :not-matched)))
    (setf next-spec* (car rest-list)
	  stype (spec-type next-spec*)
	  scontent (spec*-content next-spec*))

    (if (spectype-usecond-p stype)
      (let ((enable? (usecond-enable? stype))
	    (cond-flag (usecond-flag stype)))
	(if enable?
	  (if (member cond-flag use+ :test #'string=)
	    (setf enum-collapse (list (make-spec* :all scontent))
		  enum-use+ (list use+)
		  enum-use- (list use-))
	    (if (member cond-flag use- :test #'string=)
	      (setf enum-collapse (list :ignored)
		    enum-use+ (list use+)
		    enum-use- (list use-))
	      (setf enum-collapse (list (make-spec* :all scontent) :ignored)
		    enum-use+ (list (cons cond-flag use+) use+)
		    enum-use- (list use- (cons cond-flag use-)))))
	  (if (member cond-flag use+ :test #'string=)
	    (setf enum-collapse (list :ignored)
		  enum-use+ (list use+)
		  enum-use- (list use-))
	    (if (member cond-flag use- :test #'string=)
	      (setf enum-collapse (list (make-spec* :all scontent))
		    enum-use+ (list use+)
		    enum-use- (list use-))
	      (setf enum-collapse (list (make-spec* :all scontent) :ignored)
		    enum-use+ (list use+ (cons cond-flag use+))
		    enum-use- (list (cons cond-flag use-) use-))))))
      (setf enum-collapse (list next-spec*)
	    enum-use+ (list use+)
	    enum-use- (list use-)))

    (dotimes (i (length enum-collapse))
      (setf use+ (nth i enum-use+)
	    use- (nth i enum-use-))
      (if (eql (nth i enum-collapse) :ignored)
	(setf next-ig-constraints+ (nconc next-ig-constraints+ (list (list use+)))
	      next-ig-constraints- (nconc next-ig-constraints- (list (list use-))))
	(do ((m nil t)
	     (nm 0 (1+ nm))
	     (sub-stype (spec-type (nth i enum-collapse)))
	     (sub-scontent (spec*-content (nth i enum-collapse)))
	     (subgroup-ig-not-recorded? t))
	  ((= nm 2))
	  (if (eql sub-stype t)
	    ;; 原子说明符
	    (if (match-discriminant gtype match? (1+ prev-len) (+ prev-match nm) (1- rest-len))
	      (let* ((enable? (char/= (char sub-scontent 0) #\!))
		     (flag (if enable? sub-scontent (subseq sub-scontent 1))))
		(if (eql enable? m)
		  (if (member flag use+ :test #'string=)
		    (setf next-nig-constraints+ (nconc next-nig-constraints+ (list (list use+)))
			  next-nig-constraints- (nconc next-nig-constraints- (list (list use-)))
			  next-match (nconc next-match (list nm)))
		    (if (member flag use- :test #'string=)
		      (setf mismatched-flag flag)
		      (setf next-nig-constraints+ (nconc next-nig-constraints+ (list (list (cons flag use+))))
			    next-nig-constraints- (nconc next-nig-constraints- (list (list use-)))
			    next-match (nconc next-match (list nm)))))
		  (if (member flag use- :test #'string=)
		    (setf next-nig-constraints+ (nconc next-nig-constraints+ (list (list use+)))
			  next-nig-constraints- (nconc next-nig-constraints- (list (list use-)))
			  next-match (nconc next-match (list nm)))
		    (if (member flag use+ :test #'string=)
		      (setf mismatched-flag flag)
		      (setf next-nig-constraints+ (nconc next-nig-constraints+ (list (list use+)))
			    next-nig-constraints- (nconc next-nig-constraints- (list (list (cons flag use-))))
			    next-match (nconc next-match (list nm))))))))
	    ;; 组说明符
	    (multiple-value-bind (nig-ulist+ nig-ulist- ig-ulist+ ig-ulist-) (use-state-constraints use+ use- sub-stype 0 0 sub-scontent m)
	      (unless (eql nig-ulist+ :not-matched)
		(if (and nig-ulist+
			 (match-discriminant gtype match? (1+ prev-len) (+ prev-match nm) (1- rest-len)))
		  (setf next-nig-constraints+ (nconc next-nig-constraints+ (list nig-ulist+))
			next-nig-constraints- (nconc next-nig-constraints- (list nig-ulist-))
			next-match (nconc next-match (list nm))))
		(if (and ig-ulist+
			 subgroup-ig-not-recorded?)
		  (setf next-ig-constraints+ (nconc next-ig-constraints+ (list ig-ulist+))
			next-ig-constraints- (nconc next-ig-constraints- (list ig-ulist-))
			subgroup-ig-not-recorded? nil)))
	      (if (and (eql nig-ulist+ :not-matched)
		       nig-ulist-)
		(setf mismatched-flag nig-ulist-)))))))

    (dotimes (i (length next-match))
      (let ((step-match (nth i next-match))
	    (step-nig-ulist+ (nth i next-nig-constraints+))
	    (step-nig-ulist- (nth i next-nig-constraints-)))
	(dotimes (j (length step-nig-ulist+))
	  (multiple-value-bind (nig-ulist+ nig-ulist- ig-ulist+ ig-ulist-) (use-state-constraints (nth j step-nig-ulist+)
												  (nth j step-nig-ulist-)
												  gtype
												  (1+ prev-len)
												  (+ prev-match step-match)
												  (cdr rest-list)
												  match?)
	    (if (eql nig-ulist+ :not-matched)
	      (if nig-ulist-
		(setf mismatched-flag nig-ulist-))
	      (setf group-nig-ulist+ (nconc group-nig-ulist+ nig-ulist+ ig-ulist+)
		    group-nig-ulist- (nconc group-nig-ulist- nig-ulist- ig-ulist-)))))))

    (dotimes (i (length next-ig-constraints+))
      (let ((step-ig-ulist+ (nth i next-ig-constraints+))
	    (step-ig-ulist- (nth i next-ig-constraints-)))
	(dotimes (j (length step-ig-ulist+))
	  (multiple-value-bind (nig-ulist+ nig-ulist- ig-ulist+ ig-ulist-) (use-state-constraints (nth j step-ig-ulist+)
												  (nth j step-ig-ulist-)
												  gtype
												  prev-len
												  prev-match
												  (cdr rest-list)
												  match?)
	    (if (eql nig-ulist+ :not-matched)
	      (if nig-ulist-
		(setf mismatched-flag nig-ulist-))
	      (setf group-ig-ulist+ (nconc group-ig-ulist+ ig-ulist+)
		    group-ig-ulist- (nconc group-ig-ulist- ig-ulist-)
		    group-nig-ulist+ (nconc group-nig-ulist+ nig-ulist+)
		    group-nig-ulist- (nconc group-nig-ulist- nig-ulist-)))))))

    (if (or group-nig-ulist+ group-ig-ulist+)
      (values group-nig-ulist+ group-nig-ulist-
	      group-ig-ulist+ group-ig-ulist-)
      (values :not-matched mismatched-flag))))
