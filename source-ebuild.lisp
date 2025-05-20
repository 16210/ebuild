;; 构造拉入 ebuild 前的运行时环境变量
;; 参数：
;;	cname		类别名称
;;	pname		非限定的软件包名称
;;	vstr		版本字符串
;;	filesdir	FILESDIR 的值
;;	distdir		DISTDIR 的值
;;	workdir		WORKDIR 的值
;;	build-type	BUILD_TYPE 的值
;;	make.defaults	当前使用的系统轮廓的 <构建配置>
;; 返回值：
;;	一个 <declare> 列表
;; 备注：
;;	* CHOST，CBUILD 和 CTARGET 不设置
(defun cons-init-vars (cname pname vstr filesdir distdir workdir build-type make.defaults)
  (let (rcomp ecomp (mid vstr) vlist
	(env-unset (assoc "ENV_UNSET" make.defaults :test #'string=)))
    (let ((p (position #\- mid :test #'char=)))
      (when p
	(setf rcomp (subseq mid (1+ p))
	      mid (subseq mid 0 p))))
    (let ((p (position #\_ mid :test #'char=)))
      (when (and p (char-in-range (char mid (1+ p)) #\0 #\9))
	(setf ecomp (subseq mid 0 p)
	      mid (subseq mid (1+ p)))))
    (setf vlist (list (mk-declare "-rx" "P" (concatenate 'string pname "-" mid))
		      (mk-declare "-rx" "PF" (concatenate 'string pname "-" vstr))
		      (mk-declare "-rx" "PN" pname)
		      (mk-declare "-rx" "CATEGORY" cname)
		      (mk-declare "-rx" "PV" mid)
		      (mk-declare "-rx" "PE" (if ecomp ecomp "0"))
		      (mk-declare "-rx" "PR" (if rcomp rcomp "r0"))
		      (mk-declare "-rx" "PVR" vstr)
		      (mk-declare "-rx" "FILESDIR" filesdir)
		      (mk-declare "-rx" "DISTDIR" distdir)
		      (mk-declare "-x" "WORKDIR" workdir)
		      (mk-declare "-rx" "BUILD_TYPE" build-type)))
    (if env-unset
      (setf env-unset (nconc (delete "" (string-split (cdr env-unset) #\Space) :test #'string=)
			     (list "GZIP" "BZIP" "BZIP2" "CDPATH" "GREP_OPTIONS" "GREP_COLOR" "GLOBIGNORE")))
      (setf env-unset (list "GZIP" "BZIP" "BZIP2" "CDPATH" "GREP_OPTIONS" "GREP_COLOR" "GLOBIGNORE")))
    (dolist (m make.defaults)
      (unless (find (car m) env-unset :test #'string=)
	(setf vlist (nconc vlist (list (mk-declare "-x" (car m) (cdr m)))))))
    (setf vlist (nconc vlist (list (mk-declare "-x" "PATH" *ebuild-env-path*))))
    vlist))

;; 从 ebuild 流中解析 EAPI
;; 参数：
;;	liu	ebuild 流
;; 返回值：
;;	EAPI 字符串或 nil
(defun parse-eapi-stream (liu)
  (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
    ((eql line :eof))
    (let ((cline (string-trim #(#\Space #\Tab) (car (string-split line #\#)))))
      (when (string/= cline "")
	(if (and (> (length cline) 5) (string= (subseq cline 0 5) "EAPI="))
	  (progn
	    (setf cline (subseq cline 5))
	    (if (or (char= (char cline 0) #\") (char= (char cline 0) #\'))
	      (if (and (> (length cline) 2) (char= (char cline 0) (char cline (1- (length cline)))))
		(return-from parse-eapi-stream (subseq cline 1 (1- (length cline))))
		(return-from parse-eapi-stream nil))
	      (return-from parse-eapi-stream cline)))
	  (return-from parse-eapi-stream nil))))))

;; 从 ebuild 文件中解析 EAPI
;; 参数：
;;	ebuild	带路径的 ebuild 文件名
;; 返回值：
;;	EAPI 字符串或 nil
(defun parse-eapi-file (ebuild)
  (with-open-file (liu ebuild :direction :input)
    (parse-eapi-stream liu)))
