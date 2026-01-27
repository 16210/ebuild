(defmacro depspec-blockp (spec)
  `(car ,spec))
(defmacro depspec-operator (spec)
  `(cadr ,spec))
(defmacro depspec-qpkgname (spec)
  `(caddr ,spec))
(defmacro depspec-version* (spec)
  `(cadddr ,spec))
(defmacro depspec-slotdep (spec)
  `(nth 4 ,spec))
(defmacro depspec-usedep (spec)
  `(nth 5 ,spec))

(defun slot-name-p (name)
  (let ((len (length name)))
    (and (plusp len)
	 ;; 检查字符构成
	 (do* ((i 0 (1+ i))
	       (c (char name i) (char name i))
	       (test1 (char-in-range c #\A #\Z)
		      (char-in-range c #\A #\Z))
	       (test2 (char-in-range c #\a #\z)
		      (char-in-range c #\a #\z))
	       (test3 (char-in-range c #\0 #\9)
		      (char-in-range c #\0 #\9))
	       (test4 (find c "+_.-") (find c "+_.-"))
	       (test5 (char-in-range c #\U+4E00 #\U+9FA5)
		      (char-in-range c #\U+4E00 #\U+9FA5))
	       (test (or test1 test2 test3 test4 test5)
		     (or test1 test2 test3 test4 test5)))
	   ((or (= (1+ i) len) (not test)) test))
	 ;; 检查开头
	 (not (find (char name 0) "-.+")))))

(defun use-flag-name-p (name &optional prefixedp)
  (let ((len (length name)))
    (and (plusp len)
	 ;; 检查字符构成
	 (do* ((i 0 (1+ i))
	       (c (char name i) (char name i))
	       (test1 (char-in-range c #\A #\Z)
		      (char-in-range c #\A #\Z))
	       (test2 (char-in-range c #\a #\z)
		      (char-in-range c #\a #\z))
	       (test3 (char-in-range c #\0 #\9)
		      (char-in-range c #\0 #\9))
	       (test4 (find c "+-") (find c "+-"))
	       (test5 (char-in-range c #\U+4E00 #\U+9FA5)
		      (char-in-range c #\U+4E00 #\U+9FA5))
	       (test6 (and prefixedp (char= c #\_))
		      (and prefixedp (char= c #\_)))
	       (test (or test1 test2 test3 test4 test5 test6)
		     (or test1 test2 test3 test4 test5 test6)))
	   ((or (= (1+ i) len) (not test)) test))
	 ;; 检查开头和结尾
	 (not (find (char name 0) "-+_"))
	 (char/= (char name (- len 1)) #\_))))

;; 解析软件包依赖说明符
;; 参数：
;;	dep	软件包依赖说明符字符串
;; 返回值：
;;	解析成功返回一个 <软件包依赖说明符>，
;;	dep 不合法返回 nil，
;;	不检查应用标志依赖的合法性
(defun parse-depspec (dep)
  (setf dep (string-split dep #\[))
  (let ((spec (list nil nil nil nil (cons nil nil) nil)))
    (when (> (length dep) 2)
      (return-from parse-depspec))
    ;; 应用标志依赖
    (when (= (length dep) 2)
      (let* ((udep (cadr dep)) (len (length udep)))
	(when (or (< len 2) (char/= (char udep (1- len)) #\]))
	  (return-from parse-depspec))
	(setf (depspec-usedep spec)
	      (string-split (subseq udep 0 (1- len)) #\,))))
    ;; 插槽依赖
    (setf dep (string-split (car dep) #\:))
    (when (> (length dep) 2)
      (return-from parse-depspec))
    (when (= (length dep) 2)
      (let* ((sdep (cadr dep)) (len (length sdep)))
	(if (string= sdep "=")
	  (setf (depspec-slotdep spec) (cons "=" "="))
	  (if (slot-name-p sdep)
	    (setf (depspec-slotdep spec) (cons sdep nil))
	    (if (and (> len 1)
		     (char= (char sdep (1- len)) #\=)
		     (slot-name-p (subseq sdep 0 (1- len))))
	      (setf (depspec-slotdep spec)
		    (cons (subseq sdep 0 (1- len)) "="))
	      (let ((p (position #\/ sdep)))
		(if (and p (slot-name-p (subseq sdep 0 p))
			 (slot-name-p (subseq sdep (1+ p))))
		  (setf (depspec-slotdep spec)
			(cons (subseq sdep 0 p) (subseq sdep (1+ p))))
		  (return-from parse-depspec))))))))
    ;; 连字符加版本
    (when (or (string= (car dep) "")
	      (char= (char (car dep) (1- (length (car dep)))) #\-))
      (return-from parse-depspec))
    (let* ((p1 (position #\- (car dep) :from-end t))
	   (p2 (position #\- (car dep) :from-end t :end p1))
	   (c (if p1 (char (car dep) (1+ p1)))))
      (when p1
	(if (char-in-range c #\0 #\9)
	  (when (setf (depspec-version* spec)
		      (parse-version* (subseq (car dep) (1+ p1))))
	    (setf dep (subseq (car dep) 0 p1)))
	  (when (and (char= c #\r) p2
		     (setf (depspec-version* spec)
			   (parse-version* (subseq (car dep) (1+ p2)))))
	    (setf dep (subseq (car dep) 0 p2)))))
      (unless (depspec-version* spec)
	(setf dep (car dep))))
    ;; 阻塞符
    (when (and (plusp (length dep)) (char= (char dep 0) #\!))
      (setf (depspec-blockp spec) t dep (subseq dep 1)))
    ;; 操作符
    (when (depspec-version* spec)
      (when (< (length dep) 2)
	(return-from parse-depspec))
      (if (char= (char dep 0) #\<)
	(if (char= (char dep 1) #\=)
	  (setf (depspec-operator spec) '<= dep (subseq dep 2))
	  (setf (depspec-operator spec) '< dep (subseq dep 1)))
	(if (char= (char dep 0) #\=)
	  (setf (depspec-operator spec) '= dep (subseq dep 1))
	  (if (char= (char dep 0) #\~)
	    (setf (depspec-operator spec) '~ dep (subseq dep 1))
	    (if (char= (char dep 0) #\>)
	      (if (char= (char dep 1) #\=)
		(setf (depspec-operator spec) '>= dep (subseq dep 2))
		(setf (depspec-operator spec) '> dep (subseq dep 1)))
	      (return-from parse-depspec)))))
      (when (and (plusp (cdr (depspec-version* spec)))
		 (not (eql (depspec-operator spec) '=)))
	(return-from parse-depspec)))
    ;; 限定的软件包名称
    (unless (qualified-package-name-p (setf (depspec-qpkgname spec) dep))
      (setf spec nil))
    spec))

(defmacro depspec*-usedep* (spec*)
  `(nth 5 ,spec*))
(defmacro mk-depspec* (blockp operator qpkgname version* slotdep usedep*)
  `(list ,blockp ,operator ,qpkgname ,version* ,slotdep ,usedep*))
(defmacro mk-usedep* (reqlist blklist)
  `(cons ,reqlist ,blklist))
(defmacro usedep*-reqlist (ud*)
  `(car ,ud*))
(defmacro usedep*-blklist (ud*)
  `(cdr ,ud*))
(defmacro mk-usedepelt* (flag defval)
  `(cons ,flag ,defval))
(defmacro usedepelt*-flag (udelt*)
  `(car ,udelt*))
(defmacro usedepelt*-defval (udelt*)
  `(cdr ,udelt*))
(defmacro mk-slotdep (regular sub)
  `(cons ,regular ,sub))
(defmacro slotdep-regular (sd)
  `(car ,sd))
(defmacro slotdep-sub (sd)
  `(cdr ,sd))

;; 插槽依赖匹配
;; 参数：
;;	slotdep		<插槽依赖>
;;	regular-slot	待匹配的主插槽
;;	sub-slot	待匹配的子插槽
;; 返回值：
;;	t		匹配
;;	:rebd		需要重新构建(插槽变更)
;;	:slot		插槽不匹配
(defun slotdep-match (slotdep regular-slot sub-slot)
  (dolist (iter (list (cons (slotdep-regular slotdep) regular-slot)
		      (cons (slotdep-sub slotdep) sub-slot))
		t)
    (let ((d (car iter)) (s (cdr iter)))
      (if d
	(let ((l (length d)))
	  (if (char= (char d (1- l)) #\=)
	    (if (and (> l 1)
		     (string/= s (subseq d 0 (1- l))))
	      (return-from slotdep-match :rebd))
	    (if (string/= s d)
	      (return-from slotdep-match :slot))))))))

(defmacro qpkgname-category (qpkgname)
  `(car (string-split ,qpkgname #\/)))
(defmacro qpkgname-pkgname (qpkgname)
  `(cadr (string-split ,qpkgname #\/)))
(defmacro qualified-pkgname (category name)
  `(concatenate 'string ,category "/" ,name))

;; 软件包依赖说明符匹配
;; 参数：
;;	depspec*	<软件包依赖说明符*>
;;	category	待匹配软件包的类别
;;	pkgname		待匹配软件包的非限定软件包名称
;;	version		待匹配软件包的 <软件包版本>
;;	regular-slot	待匹配软件包的主插槽，
;;			:ignore 表示匹配时忽略插槽依赖
;;	sub-slot	待匹配软件包的子插槽，
;;			当 regular-slot 传入 :ignore 时忽略
;;	use-list	待匹配软件包的启用应用标志列表，
;;			当 iuse-effective 传入 :ignore 时忽略
;;	iuse-effective	一个应用标志列表，
;;			表示待匹配软件包的 IUSE_EFFECTIVE，
;;			:ignore 表示匹配时忽略应用标志依赖
;;	ignore-blockp	忽略阻塞符
;; 返回值：
;;	t		匹配
;;	:qname		限定的软件包名称不匹配
;;	:block		被阻塞
;;	:ver<		版本低于要求
;;	:ver>		版本高于要求
;;	:rebd		需要重新构建(插槽变更)
;;	:slot		插槽不匹配
;;	:use+		必须启用的应用标志未启用
;;			这种情况下会返回第二个值给出是哪个标志未启用
;;	:use-		必须禁用的应用标志未禁用
;;			这种情况下会返回第二个值给出是哪个标志未禁用
;;	:not-effective	应用标志依赖不在 IUSE_EFFECTIVE 中
;;			这种情况下会返回第二个值给出是哪个应用标志
(defun depend*-match (depspec* category pkgname version regular-slot sub-slot use-list iuse-effective &optional (ignore-blockp nil))
  (labels ((depend*-match-ignore-block ()
				       ;; 匹配限定的软件包名称
				       (unless (string= (qualified-pkgname category pkgname)
							(depspec-qpkgname depspec*))
					 (return-from depend*-match-ignore-block :qname))
				       ;; 匹配版本和操作符
				       (let ((op (depspec-operator depspec*))
					     (ver* (depspec-version* depspec*)))
					 (if op
					   (let ((vercmp (version-compare version
									  (version*-version ver*)
									  (eql op '~)
									  (version*-cmpcount ver*))))
					     (if (< vercmp 0)
					       (if (and (not (eql op '<))
							(not (eql op '<=)))
						 (return-from depend*-match-ignore-block :ver<))
					       (if (> vercmp 0)
						 (if (and (not (eql op '>))
							  (not (eql op '>=)))
						   (return-from depend*-match-ignore-block :ver>))
						 (if (eql op '<)
						   (return-from depend*-match-ignore-block :ver>)
						   (if (eql op '>)
						     (return-from depend*-match-ignore-block :ver<))))))))
				       ;; 匹配插槽依赖
				       (unless (eql regular-slot :ignore)
					 (let ((slotmatch (slotdep-match (depspec-slotdep depspec*)
									 regular-slot
									 sub-slot)))
					   (if (not (eql slotmatch t))
					     (return-from depend*-match-ignore-block slotmatch))))
				       ;; 匹配应用标志依赖
				       (unless (eql iuse-effective :ignore)
					 (dolist (ud+ (usedep*-reqlist (depspec*-usedep* depspec*)))
					   (let ((f (usedepelt*-flag ud+)) (d (usedepelt*-defval ud+)))
					     (if (member f iuse-effective :test #'string=)
					       (unless (member f use-list :test #'string=)
						 (return-from depend*-match-ignore-block (values :use+ f)))
					       (if d
						 (if (eql d :-)
						   (return-from depend*-match-ignore-block (values :use+ f)))
						 (return-from depend*-match (values :not-effective f))))))
					 (dolist (ud- (usedep*-blklist (depspec*-usedep* depspec*)))
					   (let ((f (usedepelt*-flag ud-)) (d (usedepelt*-defval ud-)))
					     (if (member f iuse-effective :test #'string=)
					       (if (member f use-list :test #'string=)
						 (return-from depend*-match-ignore-block (values :use- f)))
					       (if d
						 (if (eql d :+)
						   (return-from depend*-match-ignore-block (values :use- f)))
						 (return-from depend*-match (values :not-effective f)))))))
				       t))
    (multiple-value-bind (a b) (depend*-match-ignore-block)
      (if (or ignore-blockp
	      (not (depspec-blockp depspec*)))
	(if b
	  (values a b)
	  a)
	(if (eql a t)
	  :block
	  t)))))
