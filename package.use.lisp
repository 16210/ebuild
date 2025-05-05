;; 读取系统轮廓的全局应用标志控制文件
;; 参数：
;;	path	系统轮廓的路径
;;	fname	文件名
;;	plist	父系统轮廓对应文件的列表
;; 返回值：
;;	执行成功返回一个应用标志列表，
;;	识别出语法错误返回 :error
(defun read-use-g (path fname plist)
  (setf path (path-join path fname))
  (let ((flag-list (mapcan #'copy-list plist)))
    (labels ((read-order ()
			 (if (uiop:file-exists-p path)
			   (list path)
			   (if (uiop:directory-exists-p path)
			     (mapcar #'(lambda (d) (path-join path d))
				     (sort (delete-if #'(lambda (p) (char= (char p 0) #\.))
						      (mapcan #'(lambda (p)
								  (last (string-split (namestring p) #\/)))
							      (uiop:directory-files (concatenate 'string path "/"))))
					   #'string<))
			     nil)))
	     (merge-flag (flag)
			 (if (char= (char flag 0) #\-)
			   (if (use-flag-name-p (setf flag (subseq flag 1)) t)
			     (setf flag-list (delete-if #'(lambda (i) (string= i flag))
							flag-list))
			     (return-from read-use-g :error))
			   (if (use-flag-name-p flag t)
			     (setf flag-list (nconc flag-list (list flag)))
			     (return-from read-use-g :error)))))
      (dolist (f (read-order) flag-list)
	(with-open-file (liu f :direction :input)
	  (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	    ((eql line :eof))
	    (let ((flag (string-trim #(#\Space #\Tab) (car (string-split line #\#)))))
	      (when (string/= flag "")
		(merge-flag flag)))))))))

;; 读取系统轮廓的局部应用标志控制文件
;; 参数：
;;	path	系统轮廓的路径
;;	fname	文件名
;;	group	系统轮廓的 <软件包分组>
;; 返回值：
;;	执行成功返回一个列表，每一项的格式为
;;	(<目标> . ((<选定?> . <应用标志>)...))，
;;	识别出语法错误返回 :error
(defun read-use-l (path fname group)
  (setf path (path-join path fname))
  (let (pkg.use-list)
    (labels ((read-order ()
			 (if (uiop:file-exists-p path)
			   (list path)
			   (if (uiop:directory-exists-p path)
			     (mapcar #'(lambda (d) (path-join path d))
				     (sort (delete-if #'(lambda (p) (char= (char p 0) #\.))
						      (mapcan #'(lambda (p)
								  (last (string-split (namestring p) #\/)))
							      (uiop:directory-files (concatenate 'string path "/"))))
					   #'string<))
			     nil)))
	     (parse-cline (cline)
			  (delete-if #'(lambda (token) (string= token ""))
				     (mapcan #'(lambda (token) (string-split token #\Tab))
					     (string-split cline #\Space))))
	     (parse-uselist (tokens)
			    (let (uselist)
			      (dolist (token tokens uselist)
				(let ((s (cons (char/= (char token 0) #\-) nil)))
				  (when (not (car s))
				    (setf token (subseq token 1)))
				  (if (use-flag-name-p (setf (cdr s) token) t)
				    (setf uselist (nconc uselist (list s)))
				    (return-from read-use-l :error))))))	; 应用标志不合法
	     (parse-target (target)
			   (if (char= (char target 0) #\@)
			     (if (group-name-p (subseq target 1))
			       (mapcar #'parse-depspec
				       (cdr (assoc (subseq target 1)
						   group :test #'string=)))
			       (return-from read-use-l :error))		; 分组名称不合法
			     (let ((spec (parse-depspec target)))
			       (when (or (null spec)
					 (depspec-blockp spec)
					 (car (depspec-slotdep spec))
					 (depspec-usedep spec))
				 (return-from read-use-l :error))
			       (list spec)))))
      (dolist (f (read-order) pkg.use-list)
	(with-open-file (liu f :direction :input)
	  (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	    ((eql line :eof))
	    (let ((tokens (parse-cline (car (string-split line #\#))))
		  uselist)
	      (when (= (length tokens) 1)
		(return-from read-use-l :error))
	      (unless (zerop (length tokens))
		(setf uselist (parse-uselist (cdr tokens)))
		(dolist (spec (parse-target (car tokens)))
		  (setf pkg.use-list (nconc pkg.use-list (list (cons spec uselist)))))))))))))
