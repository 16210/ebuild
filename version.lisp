(defmacro version-epoch (ver)
  `(car ,ver))
(defmacro version-number (ver)
  `(cadr ,ver))
(defmacro version-letter (ver)
  `(caddr ,ver))
(defmacro version-suffix (ver)
  `(cadddr ,ver))
(defmacro version-revision (ver)
  `(nth 4 ,ver))

;; 版本解析
;; 参数：
;;	ver	版本字符串
;; 返回值：
;;	解析成功返回一个 <软件包版本>
;;	ver 不合法返回 nil
(defun parse-version (ver)
  (let ((comp (string-split ver #\_))
	(pkg-version (list 0 nil nil nil 0)))
    (labels ((parse-unsigned-int (s)
				 (if (and (plusp (length s))
					  (every #'(lambda (c)
						     (char-in-range c #\0 #\9))
						 s))
				   (parse-integer s)
				   (return-from parse-version)))
	     ;; 解析数字部分和字母部分
	     (parse-number-letter (s)
				  (let ((len (length s)))
				    (when (zerop len)
				      (return-from parse-version))
				    (when (char-in-range (char s (1- len)) #\a #\z)
				      (setf (version-letter pkg-version) (char s (1- len))
					    s (subseq s 0 (1- len))))
				    (dolist (i (string-split s #\.))
				      (setf (version-number pkg-version)
					    (nconc (version-number pkg-version)
						   (list (parse-unsigned-int i)))))))
	     ;; 解析去掉下划线的后缀部分
	     (parse-suffix (s)
			   (dolist (i '("alpha" "beta" "pre" "rc" "p"))
			     (when (and (>= (length s) (length i))
					(string= (subseq s 0 (length i)) i))
			       (let ((suffix (cons i 0)))
				 (when (> (length s) (length i))
				   (setf (cdr suffix)
					 (parse-unsigned-int (subseq s (length i)))))
				 (setf (version-suffix pkg-version) suffix)
				 (return-from parse-suffix))))
			   (return-from parse-version))
	     ;; 解析去掉连字符的修订部分
	     (parse-revision (s)
			     (unless (and (> (length s) 1) (char= (char s 0) #\r))
			       (return-from parse-version))
			     (setf (version-revision pkg-version)
				   (parse-unsigned-int (subseq s 1)))))
      (case (length comp)
	(1
	 (setf comp (string-split (car comp) #\-))
	 (parse-number-letter (car comp))
	 (when (cdr comp)
	   (parse-revision (cadr comp))
	   (when (cddr comp)
	     (setf pkg-version nil))))
	(2
	 (let ((p (position #\- (cadr comp) :test #'char=)))
	   (when p
	     (parse-revision (subseq (cadr comp) (1+ p)))
	     (setf (cadr comp) (subseq (cadr comp) 0 p))))
	 (when (zerop (length (cadr comp)))
	   (return-from parse-version))
	 (if (find (char (cadr comp) 0) #(#\a #\b #\p #\r) :test #'char=)
	   (progn
	     (parse-number-letter (car comp))
	     (parse-suffix (cadr comp)))
	   (progn
	     (setf (version-epoch pkg-version) (parse-unsigned-int (car comp)))
	     (parse-number-letter (cadr comp)))))
	(3
	 (let ((p (position #\- (caddr comp) :test #'char=)))
	   (when p
	     (parse-revision (subseq (caddr comp) (1+ p)))
	     (setf (caddr comp) (subseq (caddr comp) 0 p))))
	 (setf (version-epoch pkg-version) (parse-unsigned-int (car comp)))
	 (parse-number-letter (cadr comp))
	 (parse-suffix (caddr comp)))
	(t
	  (setf pkg-version nil)))
      pkg-version)))
