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

(let* ((qiongjv-cache-list (list (list (list nil) (list t))))
       (qiongjv-cache-length 1)
       (qiongjv-cache-tailnode qiongjv-cache-list))
  ;; 计算组说明符所有可能的掩码
  ;; 参数：
  ;;	len	组说明符的成员数量
  ;;	gtype	组说明符类型，只能是 :all，:any，:=1 或 :<=1
  ;;	match?	组说明符必须匹配(t)还是必须不匹配(nil)
  ;; 返回值：
  ;;	一张 <掩码> 列表
  ;;	<掩码> 是一张长度为 len，元素是 t 或 nil 的列表，
  ;;	每个元素表示同索引的组成员是否应该匹配。
  (defun mask-list (len gtype &optional (match? t))
    (labels (;; 穷举 2^len 个 <掩码>
	     (qiongjv ()
		      (do ()
			((>= qiongjv-cache-length len))
			(setf (cdr qiongjv-cache-tailnode)
			      (cons (nconc (mapcan #'(lambda (m) (list (cons nil m)))
						   (car qiongjv-cache-tailnode))
					   (mapcan #'(lambda (m) (list (cons t m)))
						   (car qiongjv-cache-tailnode)))
				    nil)
			      qiongjv-cache-tailnode
			      (cdr qiongjv-cache-tailnode))
			(incf qiongjv-cache-length))
		      (if (= qiongjv-cache-length len)
			(copy-list (car qiongjv-cache-tailnode))
			(copy-list (nth (1- len) qiongjv-cache-list))))
	     ;; 计算 match? 等于 t 时单选组或至多单选组的 <掩码> 列表
	     (oneof-group-mask-list ()
				    (let (mlist)
				      (dotimes (i len)
					(let (mask)
					  (dotimes (j len)
					    (setf mask (cons (= j i) mask)))
					  (setf mlist (cons mask mlist))))
				      (when (eql gtype :<=1)
					(setf mlist (cons (make-list len) mlist)))
				      mlist)))
      (case gtype
	(:all
	  (if match?
	    (list (make-list len :initial-element t))
	    (cdr (nreverse (qiongjv)))))
	(:any
	  (if match?
	    (cdr (qiongjv))
	    (list (make-list len))))
	(t
	  (if match?
	    (oneof-group-mask-list)
	    (let ((mlist (qiongjv)))
	      (do ((prev-node mlist)
		   (i 1 (1+ i))
		   (power 1))
		((null (cdr prev-node)))
		(if (= i power)
		  (setf (cdr prev-node) (cddr prev-node)
			power (* power 2))
		  (setf prev-node (cdr prev-node))))
	      (if (eql gtype :=1)
		mlist
		(cdr mlist)))))))))
