(defun repository-name-p (name)
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
	 ;; 检查是不是软件包名称
	 (unqualified-package-name-p name))))

(defun keyword-name-p (name)
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
	 ;; 检查开头
	 (char/= (char name 0) #\-))))

(defconstant *keyword-all "noarch")	; 特殊平台名称，表示“面向所有平台”

;; 解析 ebuild 文件名
;; 参数：
;;	filename	文件名
;;	arch.list	平台列表
;; 返回值：
;;	* (<形式> . <版本字符串>)
;;	* 软件包名称
;;	若解析失败则只返回一个 nil
(defun parse-ebuild-filename (filename arch.list)
  (let ((p (position #\. filename :from-end t))
	(form :ebuild))
    ;; 检查扩展名
    (if (and p (string= (subseq filename (1+ p)) "ebuild"))
      (setf filename (subseq filename 0 p))
      (return-from parse-ebuild-filename nil))
    ;; 解析形式
    (when (setf p (position #\. filename :from-end t))
      (setf form (subseq filename (1+ p)))
      (if (or (string= form "src")
	      (find form arch.list :test #'string=)
	      (string= form *keyword-all))
	(progn
	  (when (string= form "src")
	    (setf form :src))
	  (setf filename (subseq filename 0 p)))
	(setf form :ebuild)))
    ;; 解析名称与版本
    (let* ((p1 (position #\- filename :from-end t))
	   (p2 (position #\- filename :from-end t :end p1))
	   name ver)
      (unless p1
	(return-from parse-ebuild-filename nil))
      (if (char= (char filename (1+ p1)) #\r)
	(if p2
	  (setf name (subseq filename 0 p2)
		ver (subseq filename (1+ p2)))
	  (return-from parse-ebuild-filename nil))
	(setf name (subseq filename 0 p1)
	      ver (subseq filename (1+ p1))))
      (if (and (unqualified-package-name-p name)
	       (parse-version ver))
	(values (cons form ver) name)
	nil))))

;; 读取仓库中的类别
;; 参数：
;;	path		仓库路径
;;	arch.list	仓库的平台列表
;; 返回值：
;;	一个 <类别> 列表或 :error
(defun read-category (path arch.list)
  (unless (uiop:file-exists-p (path-join path "类别"))
    (return-from read-category :error))
  (let (clist)
    (with-open-file (liu (path-join path "类别") :direction :input)
      (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	((eql line :eof) clist)
	(let ((cline (string-trim #(#\Space #\Tab) (car (string-split line #\#)))))
	  (when (string/= cline "")
	    (unless (category-name-p cline)	; 非法类别名称
	      (return-from read-category :error))
	    (when (uiop:directory-exists-p (path-join path cline))
	      (let ((category (cons cline nil)))
		(dolist (pkg-path (uiop:subdirectories (path-join path cline)))
		  (setf pkg-path (namestring pkg-path))
		  (let* ((pkg-name (car (last (string-split (string-right-trim "/" pkg-path) #\/))))
			 (pkg-dir (cons pkg-name nil)))
		    (when (unqualified-package-name-p pkg-name)
		      (dolist (f (uiop:directory-files pkg-path))
			(multiple-value-bind (form-ver name) (parse-ebuild-filename
							       (car (last (string-split (namestring f) #\/)))
							       arch.list)
			  (when (equal name pkg-name)
			    (setf (cdr pkg-dir) (nconc (cdr pkg-dir) (list form-ver))))))
		      (when (cdr pkg-dir)
			(setf (cdr category) (nconc (cdr category) (list pkg-dir)))))))
		(when (cdr category)
		  (setf clist (nconc clist (list category))))))))))))
