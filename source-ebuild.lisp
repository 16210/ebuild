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
    (format liu "touch -a ~A/eclass/ECLASS.stack~%" builddir)
    (format liu "if [ -n \"${ECLASS}\" ]~%then~%")
    (format liu "echo -e \"${ECLASS}\\n$(cat ~A/eclass/ECLASS.stack)\" | tr --squeeze-repeats '\\n' > ~A/eclass/ECLASS.stack.new~%"
		builddir builddir)
    (format liu "mv -f ~A/eclass/ECLASS.stack.new ~A/eclass/ECLASS.stack~%" builddir builddir)
    (format liu "mkdir ~A/eclass/$(tac ~A/eclass/ECLASS.stack | tr '\\n' '/')~%" builddir builddir)
    (format liu "fi~%")
    (format liu "while [ $# -gt 0 ]~%do~%")
    (format liu "export ECLASS=${1}~%")
    (format liu "if [ -z \"${INHERITED}\" ]~%then~%")
    (format liu "export INHERITED=${1}~%else~%")
    (format liu "export INHERITED=\"${INHERITED} ${1}\"~%fi~%")
    (format liu "source ~A/eclass/${1}.eclass~%" repo-path)
    (dolist (i '("IUSE" "REQUIRED_USE" "PROPERTIES" "RESTRICT" "BDEPEND" "RDEPEND" "PDEPEND" "IDEPEND"))
      (format liu "echo ${~A} >> ~A/eclass/$(tac ~A/eclass/ECLASS.stack | tr '\\n' '/')~A~%" i builddir builddir i)
      (format liu "unset -v ~A~%" i))
    (format liu "unset -v ECLASS~%")
    (format liu "shift~%done~%")
    (format liu "if [ -s ~A/eclass/ECLASS.stack ]~%then~%" builddir)
    (dolist (i '("IUSE" "REQUIRED_USE" "PROPERTIES" "RESTRICT" "BDEPEND" "RDEPEND" "PDEPEND" "IDEPEND"))
      (format liu "~A=$(cat ~A/eclass/$(tac ~A/eclass/ECLASS.stack | tr '\\n' '/')~A | tr --squeeze-repeats '\\n' ' ' | sed 's/ $//')~%" i builddir builddir i))
    (format liu "rm -r ~A/eclass/$(tac ~A/eclass/ECLASS.stack | tr '\\n' '/')~%" builddir builddir)
    (format liu "export ECLASS=$(head -n 1 ~A/eclass/ECLASS.stack)~%" builddir)
    (format liu "sed -i '1d' ~A/eclass/ECLASS.stack~%" builddir)
    (format liu "fi~%}~%")))

;; 拉入非二进制包的 ebuild，生成 “变量.sh” 和 “函数.sh” 两个文件
;; 参数：
;;	reponame	<仓库名称>
;;	category	<类别名称>
;;	pkgname		<非限定的软件包名称>
;;	vstr		<版本字符串>
;;	form		<形式>
;;	installp	编译完是否安装
;; 返回值：
;;	“变量.sh” 的 <declare> 列表，
;;	或 :error，表示错误
(defun source-ebuild (reponame category pkgname vstr form installp)
  (let* ((builddir *builddir-location*)
	 (repo (find reponame *repo-list* :key #'(lambda (r) (repo-name r)) :test #'string=))
	 (ebuild-path (path-join (repo-rootpath repo) category pkgname))
	 (ebuild-fname (concatenate 'string pkgname "-" vstr
					    (if (eql form :ebuild) "" ".src")
					    ".ebuild")))
    ;; 创建 <仓库名称>/<类别名称>/<非限定的软件包名称>-<版本字符串> 目录
    (dolist (i (list reponame category (concatenate 'string pkgname "-" vstr)))
      (setf builddir (concatenate 'string builddir "/" i))
      (unless (uiop:directory-exists-p builddir)
	(sb-posix:mkdir builddir *builddir-mode*)))
    ;; 根据 form 复制 ebuild 或解压缩归档
    (if (eql form :ebuild)
      (uiop:copy-file (concatenate 'string ebuild-path "/" ebuild-fname)
		      (concatenate 'string builddir "/" ebuild-fname))
      (unless (zerop (sb-ext:process-exit-code
		       (sb-ext:run-program *tar-prog* (list "-C" builddir "-xf" "-") :wait t
					   :input (concatenate 'string ebuild-path "/" ebuild-fname))))
	(return-from source-ebuild :error)))
    ;; 创建 DISTDIR，D，BUILD，ENV，eclass，default_phase，function 目录
    (dolist (i '("DISTDIR" "D" "BUILD" "ENV" "eclass" "default_phase" "function"))
      (unless (uiop:directory-exists-p (concatenate 'string builddir "/" i))
	(sb-posix:mkdir (concatenate 'string builddir "/" i) *builddir-mode*)))
    ;; 生成 inherit 和 EXPORT_FUNCTIONS 命令
    (gen-inherit builddir (repo-rootpath repo))
    ;; 构造拉入前 ebuild 运行时环境变量并保存到“变量.sh”
    (let* ((pf (assoc (repo-active-profile repo)
		      (repo-profiles repo) :test #'string=))
	   (init-vars (cons-init-vars category pkgname vstr
				      (if (eql form :ebuild)
					(concatenate 'string ebuild-path "/files")
					(concatenate 'string builddir "/FILESDIR"))
				      (concatenate 'string builddir "/DISTDIR")
				      (concatenate 'string builddir "/BUILD")
				      (if installp "source" "buildonly")
				      (profile-make.defaults pf))))
      (with-open-file (liu (concatenate 'string builddir "/ENV/变量.sh") :direction :output)
	(write-bash-declare liu init-vars)))
    ;; 复制阶段函数的默认实现
    (dolist (i '("pkg_pretend.sh" "pkg_setup.sh" "src_unpack.sh" "src_prepare.sh"
		 "src_configure.sh" "src_compile.sh" "src_test.sh" "src_install.sh"
		 "pkg_preinst.sh" "pkg_postinst.sh" "pkg_prerm.sh" "pkg_postrm.sh"
		 "pkg_config.sh" "pkg_info.sh" "pkg_nofetch.sh"))
      (uiop:copy-file (concatenate 'string *pkgmgr-rootpath* "/lib/ebuild/default_phase/" i)
		      (concatenate 'string builddir "/default_phase/" i)))

    (let* ((cmd (with-output-to-string (str)
		  ;; 构造 bash 命令
		  (format str "set -e~%")
		  (format str "shopt -s failglob~%")
		  (format str "~A/lib/ebuild/command/nop~%" *pkgmgr-rootpath*)
		  (format str "declare -p > ~A/ENV/空环境变量.sh~%" builddir)
		  (format str "source ~A/ENV/变量.sh~%" builddir)
		  (dolist (i '("nonfatal.sh" "dest-opt.sh" "text-list.sh" "ver.sh" "get_libdir.sh"))
		    (format str "source ~A/lib/ebuild/function/~A~%" *pkgmgr-rootpath* i))
		  (format str "source ~A/function/inherit.sh~%" builddir)
		  (format str "source ~A/~A-~A.ebuild~%" builddir pkgname vstr)
		  (format str "unset -f inherit EXPORT_FUNCTIONS~%")
		  (format str "declare -p > ~A/ENV/变量.sh~%" builddir)
		  (dolist (i '("pkg_pretend" "pkg_setup" "src_unpack" "src_prepare"
			       "src_configure" "src_compile" "src_test" "src_install"
			       "pkg_preinst" "pkg_postinst" "pkg_prerm" "pkg_postrm"
			       "pkg_config" "pkg_info" "pkg_nofetch"))
		    (format str "[ -z \"$(declare -f ~A)\" ] && source ~A/default_phase/~A.sh~%" i builddir i))
		  (format str "declare -f > ~A/ENV/函数.sh~%" builddir)))
	   ;; 调用 bash（env -i --chdir=<WORKDIR> *bash-prog* --norc --noprofile -c <命令>）
	   (proc (sb-ext:run-program *env-prog*
				     (list "-i" (format nil "--chdir=~A/BUILD" builddir)
					   *bash-prog* "--norc" "--noprofile" "-c" cmd)
				     :wait t))

	   empty-env-var-names ebuild-env-vars)
      (unless (zerop (sb-ext:process-exit-code proc))
	(return-from source-ebuild :error))
      ;; 读取 “空环境变量.sh”，转换成变量名称列表，并剔除 PATH
      (with-open-file (liu (concatenate 'string builddir "/ENV/空环境变量.sh") :direction :input)
	(setf empty-env-var-names (delete "PATH" (mapcar #'(lambda (d) (declare-name d))
							 (parse-bash-declare liu))
					  :test #'string=)))
      (delete-file (concatenate 'string builddir "/ENV/空环境变量.sh"))
      ;; 读取 “变量.sh”，转换成 <declare> 列表，并剔除空环境中的变量
      (with-open-file (liu (concatenate 'string builddir "/ENV/变量.sh") :direction :input)
	(setf ebuild-env-vars (delete-if #'(lambda (v)
					     (find (declare-name v) empty-env-var-names :test #'string=))
					 (parse-bash-declare liu))))
      ;; 应用隐式 RDEPEND 规则
      (unless (find "RDEPEND" ebuild-env-vars :key #'(lambda (d) (declare-name d)) :test #'string=)
	(let ((bdepend (find "BDEPEND" ebuild-env-vars :key #'(lambda (d) (declare-name d)) :test #'string=)))
	  (when bdepend
	    (setf ebuild-env-vars (nconc ebuild-env-vars
					 (list (mk-declare-scalar "--" "RDEPEND" (declare-value-scalar bdepend))))))))
      ;; 处理 eclass 累加变量
      (dolist (var '("IUSE" "REQUIRED_USE" "PROPERTIES" "RESTRICT" "BDEPEND" "RDEPEND" "PDEPEND" "IDEPEND"))
	(let ((var-declare (find var ebuild-env-vars :key #'(lambda (d) (declare-name d)) :test #'string=))
	      (var-file (concatenate 'string builddir "/eclass/" var))
	      (eclass-value ""))
	  (when (uiop:file-exists-p var-file)
	    (with-open-file (liu var-file :direction :input)
	      (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
		((eql line :eof)
		 (setf eclass-value (string-left-trim #(#\Space) eclass-value)))
		(when (string/= line "")
		  (setf eclass-value (concatenate 'string eclass-value " " line)))))
	    (when (string/= eclass-value "")
	      (if var-declare
		(set-declare-value-scalar var-declare (concatenate 'string (declare-value-scalar var-declare) " " eclass-value))
		(setf ebuild-env-vars (nconc ebuild-env-vars (list (mk-declare-scalar "--" var eclass-value)))))))))
      ;; 输出 ebuild 运行环境变量到 “变量.sh”，返回这些变量的 <declare> 列表
      (with-open-file (dst (concatenate 'string builddir "/ENV/变量.sh") :direction :output :if-exists :supersede)
	(write-bash-declare dst ebuild-env-vars))
      ebuild-env-vars)))
