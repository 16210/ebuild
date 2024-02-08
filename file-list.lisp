(defconstant S-IFMT   #8r170000)	; 文件 mode 中的类型位掩码
(defconstant S-IFSOCK #8r140000)	; 套接字
(defconstant S-IFLNK  #8r120000)	; 符号链接
(defconstant S-IFREG  #8r100000)	; 普通文件
(defconstant S-IFBLK  #8r060000)	; 块设备
(defconstant S-IFDIR  #8r040000)	; 目录
(defconstant S-IFCHR  #8r020000)	; 字符设备
(defconstant S-IFIFO  #8r010000)	; 管道

;; 递归遍历目录生成文件清单
;; 参数：
;;	path		要遍历的绝对路径。必须以 #\/ 结尾
;;	bl-sym-dir	是否遍历指向目录的符号链接
;;	absolute
;; 返回值：
;;	* 一个 <文件清单>，若 absolute 是 nil
;;	  则其中的路径是相对于 path 的路径，否则是绝对路径
;;	* 一个异常文件列表，每个元素是一个特殊类型的文件
;;	  或计算校验和失败的文件，格式同 <文件清单> 元素中的第二项
(defun file-list (path &optional bl-sym-dir absolute)
  (let ((files (uiop:directory-files path))
	(dirs (uiop:subdirectories path))
	flist elist (path-len (length path)))
    (dolist (f files)
      (let* ((name (namestring f))
	     (lstat (sb-posix:lstat f))
	     (file-type (logand (sb-posix:stat-mode lstat) S-IFMT)))
	(if (not absolute)
	  (setf name (subseq name path-len)))
	(if (= file-type S-IFLNK)
	  (setf flist (nconc flist (list (list '|l| name '-> (sb-posix:readlink f) (sb-posix:stat-mtime lstat)))))
	  (if (= file-type S-IFREG)
	    (let ((proc (sb-ext:run-program *sha256sum-prog* (list (namestring f)) :wait t :output :stream)))
	      (if (zerop (sb-ext:process-exit-code proc))
		(setf flist (nconc flist (list (list '- name (sb-posix:stat-size lstat)
						     (subseq (read-line (sb-ext:process-output proc)) 0 64)
						     (sb-posix:stat-mtime lstat)))))
		(setf elist (nconc elist (list name)))))
	    (setf elist (nconc elist (list name)))))))
    (dolist (d dirs)
      (let* ((sub-path (namestring d))
	     (name (string-right-trim "/" sub-path))
	     (lstat (sb-posix:lstat d))
	     (dir-type (logand (sb-posix:stat-mode lstat) S-IFMT)))
	(if (not absolute)
	  (setf name (subseq name path-len)))
	(if (= dir-type S-IFDIR)
	  (setf flist (nconc flist (list (list '|d| (concatenate 'string name "/")))))
	  (setf flist (nconc flist (list (list '|l| name '-> (sb-posix:readlink d) (sb-posix:stat-mtime lstat))))))
	(unless (and (= dir-type S-IFLNK) (not bl-sym-dir))
	  (multiple-value-bind (sub-flist sub-elist) (file-list sub-path bl-sym-dir absolute)
	    (if (not absolute)
	      (progn
		(dolist (sf sub-flist)
		  (setf (cadr sf) (concatenate 'string name "/" (cadr sf))))
		(setf sub-elist (mapcar #'(lambda (e)
					    (concatenate 'string name "/" e))
					sub-elist))))
	    (setf flist (nconc flist sub-flist)
		  elist (nconc elist sub-elist))))))
    (values flist elist)))
