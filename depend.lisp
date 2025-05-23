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
