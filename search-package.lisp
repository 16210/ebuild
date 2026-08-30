;; <软件包定位符>：
;; (<仓库名称> <类别名称> <非限定的软件包名称> <版本字符串> <形式>)
(defmacro mk-locator (reponame category pkgname vstr form)
  `(list ,reponame ,category ,pkgname ,vstr ,form))
(defmacro locator-reponame (locator)
  `(car ,locator))
(defmacro locator-category (locator)
  `(cadr ,locator))
(defmacro locator-pkgname (locator)
  `(caddr ,locator))
(defmacro locator-vstr (locator)
  `(cadddr ,locator))
(defmacro locator-form (locator)
  `(nth 4 ,locator))

;; <ebuild 变量缓存>：
;; (<软件包定位符> . <变量>)
;; * <变量> --- 一个 <declare> 列表
(defmacro mk-evcache (locator variables)
  `(cons ,locator ,variables))
(defmacro evcache-locator (evcache)
  `(car ,evcache))
(defmacro evcache-variables (evcache)
  `(cdr ,evcache))

;; 搜索软件包
;   找出仓库中所有非限定软件包名称或描述与目标相匹配的软件包。
;   匹配规则暂定有两种：字符串相等和字符串包含，
;   软件包名称的匹配规则根据参数选择，描述的匹配规则始终采用第二种。
;; 参数：
;;	repo			执行搜索的仓库名称列表，
;;				:selected 表示在 *selected-repo-list* 中搜索
;;	category		执行搜索的类别名称列表，nil 表示在所有类别中搜索
;;	pattern			目标字符串
;;	pkg-name-mr		软件包名称的匹配规则，取值为：
;;					:=	表示字符串相等
;;					:>=	表示字符串包含
;;	description-cache	一个 <ebuild 变量缓存> 列表，
;;				每个 <ebuild 变量缓存> 的 <变量> 中
;;				只存放 DESCRIPTION 一个 <declare>。
;;				当软件包名称不匹配时从中找出
;;				软件包的 DESCRIPTION 检查是否匹配。
;;				nil 表示忽略软件包的描述，只检查软件包名称
;; 返回值：
;;	一个 <软件包定位符> 列表
;;	或 :cache-miss，表示
;;	description-cache 非空，软件包名称不匹配且
;;	description-cache 中没有对应的 <ebuild 变量缓存>
(defun search-package (repo category pattern pkg-name-mr &optional description-cache)
  ;; 将 repo 处理成 <仓库> 列表
  (if (eql repo :selected)
    (setf repo *selected-repo-list*)
    (let ((name repo))
      (setf repo nil)
      (dolist (n name)
	(setf repo (nconc repo (list (find n *repo-list*
					   :key #'(lambda (r) (repo-name r))
					   :test #'string=)))))))
  ;; 将 pkg-name-mr 处理成匹配函数
  (if (eql pkg-name-mr :=)
    (setf pkg-name-mr #'string=)
    (setf pkg-name-mr #'kmp-search))
  ;; 遍历 repo
  (let (pkg-locator-list)
    (dolist (r repo pkg-locator-list)
      (dolist (cate (repo-categories r))
	(when (or (null category)
		  (find (category-name cate) category :test #'string=))
	  (dolist (d (category-pkgdirlist cate))
	    (if (funcall pkg-name-mr (pkgdir-name d) pattern)
	      ;; 软件包名称匹配
	      (dolist (loc (pkgdir-locauxlist d))
		(setf pkg-locator-list (nconc pkg-locator-list
					      (list (mk-locator (repo-name r)
								(category-name cate)
								(pkgdir-name d)
								(locaux-vstr loc)
								(locaux-form loc))))))
	      ;; 软件包名称不匹配
	      (if description-cache
		(dolist (loc (pkgdir-locauxlist d))
		  (let* ((pkg-locator (mk-locator (repo-name r)
						  (category-name cate)
						  (pkgdir-name d)
						  (locaux-vstr loc)
						  (locaux-form loc)))
			 (desc (declare-value-scalar (car (evcache-variables
							    (find pkg-locator description-cache
								  :key #'(lambda (e)
									   (evcache-locator e))
								  :test #'equal))))))
		    (if desc
		      (if (kmp-search desc pattern)
			(setf pkg-locator-list (nconc pkg-locator-list (list pkg-locator))))
		      (return-from search-package :cache-miss))))))))))))
