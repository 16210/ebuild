;; 读取软件包屏蔽文件
;; 参数：
;;	path	文件所在的路径
;; 返回值：
;;	执行成功返回一个 <软件包屏蔽列表>，
;;	识别出语法错误返回 :error
(defun read-package.mask (path)
  (setf path (path-join path "软件包屏蔽"))
  (let (mask-list (status :empty) spec)
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
	     (parse-mask ()
			 (let ((s (cons nil (char/= (char spec 0) #\-))))
			   (if (cdr s)
			     (setf (car s) (parse-depspec spec))
			     (setf (car s) (parse-depspec (subseq spec 1))))
			   (if (or (null (car s))
				   (depspec-blockp (car s))
				   (car (depspec-slotdep (car s))) (cdr (depspec-slotdep (car s)))
				   (depspec-usedep (car s)))
			     (return-from read-package.mask :error)
			     s))))
      (dolist (f (read-order) mask-list)
	(with-open-file (liu f :direction :input)
	  (do ((c (read-char liu nil :eof) (read-char liu nil :eof)))
	    ((eql c :eof))
	    (case status
	      (:empty
		(if (char= c #\#)
		  (read-line liu nil :eof)
		  (unless (find c #(#\Space #\Tab #\Newline) :test #'char=)
		    (setf spec (string c) status :spec))))
	      (:spec
		(if (find c #(#\Space #\Tab #\Newline #\#) :test #'char=)
		  (let ((s (parse-mask)))
		    (setf mask-list (nconc mask-list (list s)))
		    (when (char= c #\#)
		      (read-line liu nil :eof))
		    (if (or (char= c #\Newline) (char= c #\#))
		      (setf status :empty)
		      (setf status :tail)))
		  (setf spec (concatenate 'string spec (string c)))))
	      (:tail
		(if (or (char= c #\Newline) (char= c #\#))
		  (progn
		    (when (char= c #\#)
		      (read-line liu nil :eof))
		    (setf status :empty))
		  (unless (or (char= c #\Space) (char= c #\Tab))
		    (return-from read-package.mask :error)))))))
	(if (eql status :tail)
	  (setf status :empty)
	  (when (eql status :spec)
	    (setf mask-list (nconc mask-list (list (parse-mask)))
		  status :empty)))))))
