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

(defmacro repo-name (repo)
  `(car ,repo))
(defmacro repo-eapi (repo)
  `(cadr ,repo))
(defmacro repo-categories (repo)
  `(caddr ,repo))
(defmacro repo-arch.list (repo)
  `(car (cadddr ,repo)))
(defmacro repo-package.mask (repo)
  `(cadr (cadddr ,repo)))
(defmacro repo-profiles.desc (repo)
  `(caddr (cadddr ,repo)))
(defmacro repo-thirdpartymirrors (repo)
  `(cadddr (cadddr ,repo)))
(defmacro repo-profiles (repo)
  `(nth 4 (cadddr ,repo)))
(defmacro repo-active-profile (repo)
  `(nth 5 (cadddr ,repo)))
(defmacro repo-eclass (repo)
  `(nth 4 ,repo))
(defmacro repo-enable (repo)
  `(nth 5 ,repo))
(defmacro repo-rootpath (repo)
  `(nth 6 ,repo))

;; 读取仓库目录
;; 参数：
;;	path	仓库目录
;; 返回值：
;;	一个 <仓库> 或错误信息
(defun read-repo (path)
  (let ((repo (list nil nil nil (list nil nil nil nil nil nil) nil nil path)))
    ;; 读取仓库名称
    (unless (uiop:file-exists-p (path-join path "仓库名称"))
      (return-from read-repo "“仓库名称”文件不存在"))
    (with-open-file (liu (path-join path "仓库名称") :direction :input)
      (let ((rname (read-line liu nil :eof)))
	(when (or (eql rname :eof) (not (repository-name-p rname)))
	  (return-from read-repo "读取仓库名称出错"))
	(setf (repo-name repo) rname)))
    ;; 读取 eapi
    (unless (uiop:file-exists-p (path-join path "eapi"))
      (return-from read-repo "“eapi”文件不存在"))
    (with-open-file (liu (path-join path "eapi") :direction :input)
      (let ((eapi (read-line liu nil :eof)))
	(when (eql eapi :eof)
	  (return-from read-repo "读取 eapi 出错"))
	(unless (find eapi *supported-eapi :test #'string=)
	  (return-from read-repo "仓库采用了不支持的 eapi"))
	(setf (repo-eapi repo) eapi)))
    ;; 读取平台列表
    (unless (uiop:file-exists-p (path-join path "系统轮廓/平台列表"))
      (return-from read-repo "“系统轮廓/平台列表”文件不存在"))
    (with-open-file (liu (path-join path "系统轮廓/平台列表") :direction :input)
      (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	((eql line :eof))
	(let ((cline (string-trim #(#\Space #\Tab) (car (string-split line #\#)))))
	  (when (string/= cline "")
	    (if (keyword-name-p cline)
	      (setf (repo-arch.list repo) (nconc (repo-arch.list repo) (list cline)))
	      (return-from read-repo (format nil "非法平台名称：~A" cline)))))))
    ;; 读取类别
    (setf (repo-categories repo) (read-category path (repo-arch.list repo)))
    (when (eql (repo-categories repo) :error)
      (return-from read-repo "读取类别出错"))
    ;; 读取软件包屏蔽
    (setf (repo-package.mask repo) (read-package.mask (path-join path "系统轮廓")))
    (when (eql (repo-package.mask repo) :error)
      (return-from read-repo "读取“系统轮廓/软件包屏蔽”出错"))
    ;; 读取摘要
    (unless (uiop:file-exists-p (path-join path "系统轮廓/摘要"))
      (return-from read-repo "“系统轮廓/摘要”文件不存在"))
    (with-open-file (liu (path-join path "系统轮廓/摘要") :direction :input)
      (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	((eql line :eof))
	(let ((cline (delete-if #'(lambda (token) (string= token ""))
				(mapcan #'(lambda (token) (string-split token #\Tab))
					(string-split (car (string-split line #\#)) #\Space)))))
	  (when cline
	    (if (or (/= (length cline) 3)
		    (not (find (car cline) (repo-arch.list repo) :test #'string=)))
	      (return-from read-repo "读取“系统轮廓/摘要”出错")
	      (setf (repo-profiles.desc repo)
		    (nconc (repo-profiles.desc repo) (list cline))))))))
    ;; 读取第三方镜像
    (when (uiop:file-exists-p (path-join path "系统轮廓/第三方镜像"))
      (with-open-file (liu (path-join path "系统轮廓/第三方镜像") :direction :input)
	(do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	  ((eql line :eof))
	  (let ((cline (delete-if #'(lambda (token) (string= token ""))
				  (mapcan #'(lambda (token) (string-split token #\Tab))
					  (string-split (car (string-split line #\#)) #\Space)))))
	    (when cline
	      (if (or (< (length cline) 2)
		      (assoc (car cline) (repo-thirdpartymirrors repo) :test #'string=))
		(return-from read-repo "读取“系统轮廓/第三方镜像”出错")
		(setf (repo-thirdpartymirrors repo)
		      (nconc (repo-thirdpartymirrors repo) (list cline)))))))))
    ;; 读取系统轮廓
    (let ((pflist (read-profile-layout path)))
      (dolist (pf pflist)
	(dolist (p (cadddr pf))
	  (unless (assoc p pflist :test #'string=)
	    (return-from read-repo (format nil "~A 继承了仓库中不存在的系统轮廓 ~A" (car pf) p)))))
      (multiple-value-bind (llist elist) (check-parent-tree pflist path (repo-eapi repo))
	(when llist
	  (return-from read-repo (reduce #'(lambda (a b) (concatenate 'string a " " b)) llist
					 :initial-value "以下系统轮廓包含继承循环：")))
	(when elist
	  (return-from read-repo (reduce #'(lambda (a b) (concatenate 'string a " " b)) elist
					 :initial-value "以下系统轮廓或其继承树中有系统轮廓采用了不支持的 eapi："))))
      (setf pflist (sort pflist #'(lambda (a b) (< (nth 6 a) (nth 6 b)))))
      (dolist (pf pflist)
	(let ((pl-list (list nil nil nil nil nil nil nil nil nil nil nil))
	      (pf-path (path-join path "系统轮廓" (car pf))))
	  ;; 获取父系统轮廓的各项内容
	  (dolist (p (cadddr pf))
	    (setf p (assoc p pflist :test #'string=))
	    (dotimes (i 11)
	      (setf (nth i pl-list) (nconc (nth i pl-list) (list (nth i (caddr p)))))))
	  ;; 读取构建配置
	  (when (eql (setf (profile-make.defaults pf)
			   (read-make.defaults pf-path (nth 0 pl-list)))
		     :error)
	    (return-from read-repo (format nil "读取“系统轮廓/~A/构建配置”出错" (car pf))))
	  ;; 读取软件包分组
	  (when (eql (setf (profile-group pf)
			   (read-group pf-path (nth 1 pl-list)))
		     :error)
	    (return-from read-repo (format nil "读取“系统轮廓/~A/软件包分组”出错" (car pf))))
	  ;; 读取软件包屏蔽
	  (let ((mlist (copy-list (repo-package.mask repo))))
	    (dolist (m (nth 2 pl-list))
	      (setf mlist (nconc mlist (copy-list m))))
	    (let ((m (read-package.mask pf-path)))
	      (when (eql m :error)
		(return-from read-repo (format nil "读取“系统轮廓/~A/软件包屏蔽”出错" (car pf))))
	      (setf (profile-package.mask pf) (nconc mlist m))))
	  ;; 读取全局应用标志控制文件
	  (mapcan #'(lambda (i fname)
		      (when (eql (setf (nth i (caddr pf))
				       (read-use-g pf-path fname (nth i pl-list)))
				 :error)
			(return-from read-repo (format nil "读取“系统轮廓/~A/~A”出错" (car pf) fname))))
		  '(3 4 5 6) '("全局启用" "全局屏蔽" "稳定版全局启用" "稳定版全局屏蔽"))
	  ;; 读取局部应用标志控制文件
	  (mapcar #'(lambda (i fname)
		      (dolist (slist (nth i pl-list))
			(setf (nth i (caddr pf)) (nconc (nth i (caddr pf)) (copy-list slist))))
		      (let ((slist (read-use-l pf-path fname (profile-group pf))))
			(if (eql slist :error)
			  (return-from read-repo (format nil "读取“系统轮廓/~A/~A”出错" (car pf) fname))
			  (setf (nth i (caddr pf)) (nconc (nth i (caddr pf)) slist)))))
		  '(7 8 9 10) '("局部启用" "局部屏蔽" "稳定版局部启用" "稳定版局部屏蔽"))))
      (dolist (pf pflist)
	(setf (cdddr pf) nil
	      (repo-profiles repo) (nconc (repo-profiles repo) (list pf))))
      ;; 检查对外提供的系统轮廓
      (dolist (desc (repo-profiles.desc repo))
	(unless (assoc (cadr desc) (repo-profiles repo) :test #'string=)
	  (return-from read-repo (format nil "对外提供的系统轮廓 ~A 不存在" (cadr desc))))))
    ;; 读取 eclass 目录
    (when (uiop:directory-exists-p (path-join path "eclass"))
      (dolist (f (uiop:directory-files (concatenate 'string (path-join path "eclass") "/")))
	(setf f (car (last (string-split (namestring f) #\/))))
	(let ((p (position #\. f :test #'char= :from-end t)))
	  (when (and p (> p 0) (string= (subseq f p) ".eclass"))
	    (setf (repo-eclass repo) (nconc (repo-eclass repo) (list (subseq f 0 p))))))))
    ;; 读取 repo.conf
    (unless (uiop:file-exists-p (path-join path "repo.conf"))
      (return-from read-repo "“repo.conf”文件不存在"))
    (let ((slist (parse-conf-section (path-join path "repo.conf"))))
      (if (eql slist :error)
	(return-from read-repo "读取“repo.conf”出错")
	(let ((plist (assoc "属性" slist :test #'string=))
	      pvlist act ena)
	  (if plist
	    (setf pvlist (property-list (cdr plist))
		  act (assoc "profile" pvlist :test #'string=)
		  ena (assoc "enable" pvlist :test #'string=))
	    (return-from read-repo "“repo.conf”缺少[属性]段"))
	  (if act
	    (if (assoc (cdr act) (repo-profiles repo) :test #'string=)
	      (setf (repo-active-profile repo) (cdr act))
	      (return-from read-repo "profile 属性错误"))
	    (return-from read-repo "“repo.conf”缺少 profile 属性"))
	  (if ena
	    (progn
	      (setf (repo-enable repo) (parse-integer (cdr ena) :junk-allowed t))
	      (unless (and (integerp (repo-enable repo)) (>= (repo-enable repo) 0))
		(return-from read-repo "enable 属性错误")))
	    (return-from read-repo "“repo.conf”缺少 enable 属性")))))
    repo))
