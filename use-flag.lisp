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
