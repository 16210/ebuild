(defun category-name-p (name)
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

(defun unqualified-package-name-p (name)
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
	       (test4 (find c "+_-") (find c "+_-"))
	       (test5 (char-in-range c #\U+4E00 #\U+9FA5)
		      (char-in-range c #\U+4E00 #\U+9FA5))
	       (test (or test1 test2 test3 test4 test5)
		     (or test1 test2 test3 test4 test5)))
	   ((or (= (1+ i) len) (not test)) test))
	 ;; 检查开头
	 (not (find (char name 0) "-+"))
	 ;; 检查结尾
	 (not (char= (char name (1- len)) #\-))
	 (let* ((p1 (position #\- name :from-end t))
		(p2 (position #\- name :from-end t :end p1))
		(c (if p1 (char name (1+ p1)))))
	   (or (null p1)
	       (if (char-in-range c #\0 #\9)
		 (not (parse-version (subseq name (1+ p1))))
		 (if (char= c #\r)
		   (or (null p2)
		       (not (parse-version (subseq name (1+ p2)))))
		   t)))))))

(defun qualified-package-name-p (name)
  (let ((p (position #\/ name)))
    (and p (category-name-p (subseq name 0 p))
	 (unqualified-package-name-p (subseq name (1+ p))))))

(defun group-name-p (name)
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
	       (test4 (find c "_-") (find c "_-"))
	       (test5 (char-in-range c #\U+4E00 #\U+9FA5)
		      (char-in-range c #\U+4E00 #\U+9FA5))
	       (test (or test1 test2 test3 test4 test5)
		     (or test1 test2 test3 test4 test5)))
	   ((or (= (1+ i) len) (not test)) test))
	 ;; 检查开头和结尾
	 (not (char= (char name 0) #\-))
	 (not (char= (char name (1- len)) #\-)))))

;; 读取系统轮廓的软件包分组文件
;; 参数：
;;	path	系统轮廓的路径
;;	plist	父系统轮廓的 <软件包分组>
;;		组成的列表，顺序同继承文件
;; 返回值：
;;	执行成功返回一个 <软件包分组>，
;;	识别出语法错误返回 :error
(defun read-group (path plist)
  (let (group)
    ;; 合并父系统轮廓的 <软件包分组>
    (dolist (p plist)
      (dolist (pg p)
	(let ((g (assoc (car pg) group :test #'string=)))
	  (if g
	    (setf (cdr (last g)) (remove-if #'(lambda (cy)
						(member cy (cdr g) :test #'string=))
					    (cdr pg)))
	    (setf group (nconc group (list (copy-list pg))))))))
    ;; 读取文件解析分组定义
    (let ((liu (open (path-join path "软件包分组")
		     :direction :input :if-does-not-exist nil))
	  (status :empty) n m)
      (when liu
	(do ((c (read-char liu nil :eof) (read-char liu nil :eof)))
	  ((eql c :eof) (close liu))
	  (case status
	    (:empty
	      (if (char= c #\#)
		(read-line liu nil :eof)
		(unless (find c #(#\Space #\Tab #\Newline) :test #'char=)
		  (setf n (string c) status :name))))
	    (:name
	      (if (find c #(#\Space #\Tab) :test #'char=)
		(if (group-name-p n)
		  (setf status :splitter)
		  (progn			; 分组名称不合法
		    (close liu)
		    (return-from read-group :error)))
		(if (char= c #\Newline)
		  (progn
		    (close liu)
		    (return-from read-group :error))
		  (setf n (concatenate 'string n (string c))))))
	    (:splitter
	      (unless (find c #(#\Space #\Tab) :test #'char=)
		(if (char= c #\Newline)
		  (progn
		    (close liu)
		    (return-from read-group :error))
		  (setf m (string c) status :member))))
	    (:member
	      (if (find c #(#\Space #\Tab #\Newline) :test #'char=)
		;; 分组引用展开以及同名分组合并
		(if (string= m "-*")
		  (setf group (delete-if #'(lambda (g)
					     (string= (car g) n))
					 group)
			status :empty)
		  (let ((g (assoc n group :test #'string=)))
		    (labels ((pkgname-list (m)
					   (if (char= (char m 0) #\@)
					     (if (group-name-p (subseq m 1))
					       (cdr (assoc (subseq m 1) group :test #'string=))
					       (progn			; 引用的分组名称不合法
						 (close liu)
						 (return-from read-group :error)))
					     (if (qualified-package-name-p m)
					       (list m)
					       (progn			; 限定的软件包名称不合法
						 (close liu)
						 (return-from read-group :error))))))
		      (if (char= (char m 0) #\-)
			(when g
			  (setf (cdr g) (nset-difference (cdr g)
							 (pkgname-list (subseq m 1))
							 :test #'string=)))
			(progn
			  (unless g
			    (setf g (cons n nil) group (nconc group (list g))))
			  (setf (cdr (last g)) (remove-if #'(lambda (pn)
							      (find pn (cdr g) :test #'string=))
							  (pkgname-list m))))))
		    (setf status :empty)))
		(setf m (concatenate 'string m (string c)))))))
	(unless (eql status :empty)			; 分组定义不完整
	  (return-from read-group :error)))
      (delete-if #'(lambda (g)
		     (null (cdr g)))
		 group))))
