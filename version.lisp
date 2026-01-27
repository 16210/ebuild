(defmacro version-epoch (ver)
  `(car ,ver))
(defmacro version-numberlist (ver)
  `(cadr ,ver))
(defmacro version-letter (ver)
  `(caddr ,ver))
(defmacro version-suffix (ver)
  `(cadddr ,ver))
(defmacro version-revnumber (ver)
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
				      (setf (version-numberlist pkg-version)
					    (nconc (version-numberlist pkg-version)
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
			     (setf (version-revnumber pkg-version)
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

;; 版本解析*
;; 参数：
;;	ver	版本字符串
;; 返回值：
;;	解析成功返回一个 <软件包版本*>
;;	ver 不合法返回 nil
(defun parse-version* (ver)
  (let ((len (length ver)) version star)
    (and (plusp len)
	 (if (char= (char ver (1- len)) #\*)
	   (setf star t len (1- len) ver (subseq ver 0 len))
	   t)
	 (setf version (parse-version ver))
	 (if star
	   (if (find #\- ver :test #'char=)
	     (cons version (+ 5 (length (version-numberlist version))))
	     (if (char-in-range (char (car (last (string-split ver #\_))) 0)
				#\a #\z)
	       (if (char-in-range (char ver (1- len)) #\0 #\9)
		 (cons version (+ 4 (length (version-numberlist version))))
		 (cons version (+ 3 (length (version-numberlist version)))))
	       (if (char-in-range (char ver (1- len)) #\a #\z)
		 (cons version (+ 2 (length (version-numberlist version))))
		 (cons version (+ 1 (length (version-numberlist version)))))))
	   (cons version 0)))))

;; 版本比较
;; 参数：
;;	a			<软件包版本>
;;	b			<软件包版本>
;;	ignore-revision-p	是否忽略修订部分
;;	cnt			<比较数量>
;; 返回值：
;;	一个整数，大于 0 代表 a > b，
;;	小于 0 代表 a < b，等于 0 代表 a = b。
(defun version-compare (a b &optional ignore-revision-p (cnt 0))
  ;; 比较纪元部分
  (when (/= (version-epoch a) (version-epoch b))
    (return-from version-compare (- (version-epoch a)
				    (version-epoch b))))
  (decf cnt)
  ;; 比较数字部分
  (do ((na (version-numberlist a) (cdr na))
       (nb (version-numberlist b) (cdr nb)))
    ((or (null na) (null nb)))
    (when (or (/= (car na) (car nb)) (zerop (decf cnt)))
      (return-from version-compare (- (car na) (car nb)))))
  (when (/= (length (version-numberlist a)) (length (version-numberlist b)))
    (return-from version-compare (- (length (version-numberlist a))
				    (length (version-numberlist b)))))
  ;; 比较字母部分
  (let ((la (if (version-letter a)
	      (char-code (version-letter a))
	      (1+ (char-code #\z))))
	(lb (if (version-letter b)
	      (char-code (version-letter b))
	      (1+ (char-code #\z)))))
    (when (or (/= la lb) (zerop (decf cnt)))
      (return-from version-compare (- la lb))))
  ;; 比较后缀部分
  (let* ((order #("alpha" "beta" "pre" "rc" "p"))
	 (suffix-a (version-suffix a))
	 (suffix-b (version-suffix b))
	 (sa (if suffix-a
	       (cons (position (car suffix-a) order :test #'string=)
		     (cdr suffix-a))
	       (cons (length order) 0)))
	 (sb (if suffix-b
	       (cons (position (car suffix-b) order :test #'string=)
		     (cdr suffix-b))
	       (cons (length order) 0))))
    (when (or (/= (car sa) (car sb)) (zerop (decf cnt)))
      (return-from version-compare (- (car sa) (car sb))))
    (when (or (/= (cdr sa) (cdr sb)) (zerop (decf cnt)))
      (return-from version-compare (- (cdr sa) (cdr sb)))))
  ;; 比较修订部分
  (if ignore-revision-p
    0
    (- (version-revnumber a) (version-revnumber b))))

(defmacro mk-version* (ver cmpcount)
  `(cons ,ver ,cmpcount))
(defmacro version*-version (ver*)
  `(car ,ver*))
(defmacro version*-cmpcount (ver*)
  `(cdr ,ver*))
