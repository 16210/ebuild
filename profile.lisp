(declaim (inline char-in-range))
(defun char-in-range (ch a b)
  (and (char>= ch a)
       (char<= ch b)))

(defconstant *profile-conflict-name
	     #("平台列表" "软件包屏蔽" "摘要" "第三方镜像"
	       "全局应用标志" "局部应用标志" "折叠应用标志"
	       "继承" "eapi" "构建配置" "软件包分组"
	       "全局启用" "全局屏蔽" "稳定版全局启用" "稳定版全局屏蔽"
	       "局部启用" "局部屏蔽" "稳定版局部启用" "稳定版局部屏蔽"))

(defconstant *supported-eapi
	     #("1"))

(defun profile-name-p (name)
  (let ((len (length name)))
    (and (plusp len)
	 ; 检查字符构成
	 (do* ((i 0 (1+ i))
	       (c (char name i) (char name i))
	       (test1 (char-in-range c #\A #\Z)
		      (char-in-range c #\A #\Z))
	       (test2 (char-in-range c #\a #\z)
		      (char-in-range c #\a #\z))
	       (test3 (char-in-range c #\0 #\9)
		      (char-in-range c #\0 #\9))
	       (test4 (find c "_.-") (find c "_.-"))
	       (test5 (char-in-range c #\U+4E00 #\U+9FA5)
		      (char-in-range c #\U+4E00 #\U+9FA5))
	       (test (or test1 test2 test3 test4 test5)
		     (or test1 test2 test3 test4 test5)))
	   ((or (= (1+ i) len) (not test)) test))
	 ; 检查开头和结尾
	 (not (find (char name 0) "_.-"))
	 (not (find (char name (- len 1)) "_.-"))
	 ; 检查文件名冲突
	 (not (find name *profile-conflict-name :test #'string=)))))

;; 读取仓库的系统轮廓目录结构
;; 参数：
;;	repo-path	仓库路径
;; 返回值：
;;	一个列表，表中每个元素是
;;	未遍历继承树的 <系统轮廓>
(defun read-profile-layout (repo-path)
  (labels ((init-profile-list (path)
			      (let* ((sub-paths (mapcar #'namestring
							(uiop:subdirectories path)))
				     (dirs (mapcan #'(lambda (p)
						       (last (string-split (string-right-trim "/" p) #\/)))
						   sub-paths)))
				(do ((i 0 (1+ i)))
				  ((= i (length dirs)))
				  (if (not (profile-name-p (nth i dirs)))
				    (progn
				      (setf sub-paths (delete (nth i sub-paths) sub-paths)
					    dirs      (delete (nth i dirs) dirs))
				      (decf i))))
				(nconc (mapcar #'(lambda (dir)
						   (list dir nil (list nil nil nil
								       nil nil nil nil
								       nil nil nil nil)
							 nil nil nil nil))
					       dirs)
				       (mapcan #'(lambda (p dir)
						   (setf p (init-profile-list p))
						   (dolist (i p p)
						     (setf (car i) (concatenate 'string dir "/" (car i)))))
					       sub-paths dirs)))))
    (let* ((profile-path (concatenate 'string (path-join repo-path "系统轮廓") "/"))
	   (pf-path-len (length profile-path))
	   (profile-list (init-profile-list profile-path)))
      (dolist (pf profile-list profile-list)
	(let ((parent (open (concatenate 'string profile-path (car pf) "/继承")
			    :direction :input :if-does-not-exist nil)))
	  (if parent
	    (do ((line (read-line parent nil :eof) (read-line parent nil :eof)))
	      ((eql line :eof) (close parent))
	      (setf (cadddr pf) (nconc (cadddr pf) (list (subseq (path-join profile-path
									    (car pf)
									    line)
								 pf-path-len)))))))))))

;; 检查系统轮廓的继承树
;; 参数：
;;	profile-list	一个 <系统轮廓> 列表
;;	path		仓库路径
;;	repo-eapi	仓库遵循的 eapi
;; 返回值：
;;	* profile-list 中继承树包含循环的 <相对路径> 列表
;;	* profile-list 中 <eapi> 为 :unsupported 的 <相对路径> 列表
;; 副作用：
;;	赋值 profile-list 中每个 <系统轮廓> 的
;;	<eapi>、<已遍历?>、<继承树中包含循环?>、<继承深度>
(defun check-parent-tree (profile-list path repo-eapi)
  (setf path (path-join path "系统轮廓"))
  (let (llist elist)
    (labels ((check (bl-list pf)
		    (if (not (nth 4 pf))
		      (if (find (car pf) bl-list :test #'string=)
			(setf (nth 5 pf) t (nth 6 pf) nil
			      (nth 4 pf) t)
			(progn
			  (push (car pf) bl-list)
			  (setf (cadr pf) t (nth 6 pf) -1)
			  (dolist (p (cadddr pf))
			    (multiple-value-bind (lp ep dep)
			      (check bl-list (find p profile-list :key #'car :test #'string=))
			      (if lp
				(setf (nth 5 pf) t (cadr pf) nil (nth 6 pf) nil)
				(progn
				  (if (and (eql (cadr pf) t) (not ep))
				    (setf (cadr pf) :unsupported))
				  (if (nth 6 pf)
				    (setf (nth 6 pf) (max (nth 6 pf) dep)))))))
			  (pop bl-list)
			  (setf (nth 4 pf) t)
			  (if (nth 5 pf)
			    (progn
			      (push (car pf) llist)
			      t)
			    (progn
			      (if (eql (cadr pf) t)
				(let ((eapi-file (open (concatenate 'string path "/" (car pf) "/eapi")
						       :direction :input :if-does-not-exist nil)))
				  (if eapi-file
				    (let ((eapi (read-line eapi-file)))
				      (if (find eapi *supported-eapi :test #'string=)
					(setf (cadr pf) eapi)
					(setf (cadr pf) :unsupported))
				      (close eapi-file))
				    (setf (cadr pf) repo-eapi))))
			      (if (eql (cadr pf) :unsupported)
				(push (car pf) elist))
			      (values nil (stringp (cadr pf)) (incf (nth 6 pf)))))))
		      (values (nth 5 pf) (stringp (cadr pf)) (nth 6 pf)))))
      (dolist (pf profile-list)
	(check nil pf))
      (values llist elist))))

(defmacro profile-make.defaults (pf)
  `(nth 0 (caddr ,pf)))
(defmacro profile-group (pf)
  `(nth 1 (caddr ,pf)))
(defmacro profile-package.mask (pf)
  `(nth 2 (caddr ,pf)))
(defmacro profile-use.force (pf)
  `(nth 3 (caddr ,pf)))
(defmacro profile-use.mask (pf)
  `(nth 4 (caddr ,pf)))
(defmacro profile-use.stable.force (pf)
  `(nth 5 (caddr ,pf)))
(defmacro profile-use.stable.mask (pf)
  `(nth 6 (caddr ,pf)))
(defmacro profile-package.use.force (pf)
  `(nth 7 (caddr ,pf)))
(defmacro profile-package.use.mask (pf)
  `(nth 8 (caddr ,pf)))
(defmacro profile-package.use.stable.force (pf)
  `(nth 9 (caddr ,pf)))
(defmacro profile-package.use.stable.mask (pf)
  `(nth 10 (caddr ,pf)))

(defmacro mk-mask (depspec maskp)
  `(cons ,depspec ,maskp))
(defmacro mask-depspec (m)
  `(car ,m))
(defmacro mask-maskp (m)
  `(cdr ,m))
