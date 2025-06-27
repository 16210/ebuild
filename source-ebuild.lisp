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
	(default-env-unset (list "GZIP" "BZIP" "BZIP2" "CDPATH" "GREP_OPTIONS" "GREP_COLOR" "GLOBIGNORE"))
	(env-unset (assoc "ENV_UNSET" make.defaults :test #'string=)))
    (let ((p (position #\- mid :test #'char=)))
      (when p
	(setf rcomp (subseq mid (1+ p))
	      mid (subseq mid 0 p))))
    (let ((p (position #\_ mid :test #'char=)))
      (when (and p (char-in-range (char mid (1+ p)) #\0 #\9))
	(setf ecomp (subseq mid 0 p)
	      mid (subseq mid (1+ p)))))
    (setf vlist (list (mk-declare-scalar "-rx" "P" (concatenate 'string pname "-" mid))
		      (mk-declare-scalar "-rx" "PF" (concatenate 'string pname "-" vstr))
		      (mk-declare-scalar "-rx" "PN" pname)
		      (mk-declare-scalar "-rx" "CATEGORY" cname)
		      (mk-declare-scalar "-rx" "PV" mid)
		      (mk-declare-scalar "-rx" "PE" (if ecomp ecomp "0"))
		      (mk-declare-scalar "-rx" "PR" (if rcomp rcomp "r0"))
		      (mk-declare-scalar "-rx" "PVR" vstr)
		      (mk-declare-scalar "-rx" "FILESDIR" filesdir)
		      (mk-declare-scalar "-rx" "DISTDIR" distdir)
		      (mk-declare-scalar "-x" "WORKDIR" workdir)
		      (mk-declare-scalar "-rx" "BUILD_TYPE" build-type)))
    (if env-unset
      (setf env-unset (append (cdr env-unset) default-env-unset))
      (setf env-unset default-env-unset))
    (dolist (m make.defaults)
      (unless (find (car m) env-unset :test #'string=)
	(let ((val (cdr m)))
	  (when (incremental-variable-p (car m))
	    (setf val "")
	    (dolist (i (cdr m))
	      (setf val (concatenate 'string val " " i)))
	    (setf val (string-left-trim #(#\Space) val)))
	  (setf vlist (nconc vlist (list (mk-declare-scalar "-x" (car m) val)))))))
    (setf vlist (nconc vlist (list (mk-declare-scalar "-x" "PATH" *ebuild-env-path*))))
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

;; 生成 inherit 和 EXPORT_FUNCTIONS 函数，
;;	以文件 inherit.sh 的形式保存到 <构建目录>/function 目录下
;; 参数：
;;	builddir	软件包的构建目录，结尾没有 "/"
;;	repo-path	仓库顶层目录，结尾没有 "/"
;; 返回值：
;;	nil
(defun gen-inherit (builddir repo-path)
  (with-open-file (liu (concatenate 'string builddir "/function/inherit.sh") :direction :output)
    ;; 生成 EXPORT_FUNCTIONS 函数
    (format liu "EXPORT_FUNCTIONS ()~%{~%")
    (format liu "[ -z \"${ECLASS}\" ] && return~%")
    (format liu "while [ $# -gt 0 ]~%do~%")
    (format liu "echo -e \"${1} ()\\n{\" > ~A/default_phase/${1}.sh~%" builddir)
    (format liu "echo -e \"\\t${ECLASS}_${1} \\$@\" >> ~A/default_phase/${1}.sh~%" builddir)
    (format liu "echo \"}\" >> ~A/default_phase/${1}.sh~%" builddir)
    (format liu "shift~%done~%}~%")
    ;; 生成 inherit 函数
    (format liu "inherit ()~%{~%")
    (format liu "if [ -n \"${ECLASS}\" ]~%then~%")
    (format liu "touch -a ~A/eclass/ECLASS.stack~%" builddir)
    (format liu "echo -e \"${ECLASS}\\n$(cat ~A/eclass/ECLASS.stack)\" | tr --squeeze-repeats '\\n' > ~A/eclass/ECLASS.stack.new~%"
		builddir builddir)
    (format liu "mv -f ~A/eclass/ECLASS.stack.new ~A/eclass/ECLASS.stack~%" builddir builddir)
    (format liu "fi~%")
    (format liu "while [ $# -gt 0 ]~%do~%")
    (format liu "export ECLASS=${1}~%")
    (format liu "if [ -z \"${INHERITED}\" ]~%then~%")
    (format liu "export INHERITED=${1}~%else~%")
    (format liu "export INHERITED=\"${INHERITED} ${1}\"~%fi~%")
    (format liu "source ~A/eclass/${1}.eclass~%" repo-path)
    (dolist (i '("IUSE" "REQUIRED_USE" "PROPERTIES" "RESTRICT" "BDEPEND" "RDEPEND" "PDEPEND" "IDEPEND"))
      (format liu "echo ${~A} >> ~A/eclass/~A~%" i builddir i)
      (format liu "unset -v ~A~%" i))
    (format liu "unset -v ECLASS~%")
    (format liu "shift~%done~%")
    (format liu "if [ -s ~A/eclass/ECLASS.stack ]~%then~%" builddir)
    (format liu "export ECLASS=$(head -n 1 ~A/eclass/ECLASS.stack)~%" builddir)
    (format liu "sed -i '1d' ~A/eclass/ECLASS.stack~%" builddir)
    (format liu "fi~%}~%")))
