(declaim (inline incremental-variable-p))
(defun incremental-variable-p (var-name)
  (or (find var-name #("USE" "USE_EXPAND" "USE_EXPAND_HIDDEN" "CONFIG_PROTECT" "CONFIG_PROTECT_MASK"
		       "IUSE_IMPLICIT" "USE_EXPAND_IMPLICIT" "USE_EXPAND_UNPREFIXED" "ENV_UNSET")
	    :test #'string=)
      (and (> (length var-name) 18) (string= (subseq var-name 0 18) "USE_EXPAND_VALUES_"))))

;; 读取系统轮廓的构建配置文件
;; 参数：
;;	path	系统轮廓的路径
;;	plist	父系统轮廓的 <构建配置>
;;		组成的列表，顺序同继承文件
;; 返回值：
;;	执行成功返回一个 <构建配置>，
;;	识别出语法错误返回 :error
(defun read-make.defaults (path plist)
  (let ((liu (open (path-join path "构建配置")
		   :direction :input :if-does-not-exist nil))
	make.defaults vlist)
    ;; 读取文件解析变量
    (when liu
      (let ((status :empty) vari (val ""))
	(do ((c (read-char liu nil :eof) (read-char liu nil :eof)))
	  ((eql c :eof) (close liu))
	  (case status
	    (:empty
	      (if (char= c #\#)
		(read-line liu nil :eof)
		(if (or (char-in-range c #\a #\z) (char-in-range c #\A #\Z))
		  (setf vari (string c) status :var)
		  (unless (find c #(#\Space #\Tab #\Newline))
		    (close liu)
		    (return-from read-make.defaults :error)))))
	    (:var
	      (if (or (char-in-range c #\a #\z) (char-in-range c #\A #\Z)
		      (char-in-range c #\0 #\9) (char= c #\_))
		(setf vari (concatenate 'string vari (string c)))
		(if (and (char= c #\=) (eql (read-char liu nil :eof) #\"))
		  (setf status :val)
		  (progn
		    (close liu)
		    (return-from read-make.defaults :error)))))
	    (:val
	      (if (char= c #\\)
		(unless (eql (peek-char nil liu nil :eof) #\Newline)	; #\\ 用于非续行
		  (close liu)
		  (return-from read-make.defaults :error))
		(if (char= c #\")
		  (setf make.defaults (nconc make.defaults (list (cons vari val)))
			val "" status :tail)
		  (if (char= c #\Newline)
		    (setf val (concatenate 'string val " "))
		    (setf val (concatenate 'string val (string c)))))))
	    (:tail
	      (if (or (char= c #\Newline) (char= c #\#))
		(progn
		  (when (char= c #\#)
		    (read-line liu nil :eof))
		  (setf status :empty))
		(unless (or (char= c #\Space) (char= c #\Tab))
		  (close liu)
		  (return-from read-make.defaults :error))))))
	(unless (or (eql status :empty) (eql status :tail))
	  (return-from read-make.defaults :error))))
    ;; 合并父系统轮廓的 <构建配置>
    (dolist (p plist)
      (dolist (i p)
	(let* ((vari (car i))
	       (val (cdr i))
	       (var.val (assoc vari vlist :test #'string=)))
	  (if var.val
	    (if (incremental-variable-p vari)
	      (setf (cdr (last var.val)) (remove-if #'(lambda (token)
							(find token (cdr var.val) :test #'string=))
						    val))
	      (setf (cdr var.val) val))
	    (setf vlist (nconc vlist (list (copy-list i))))))))
    (dolist (m make.defaults vlist)
      (let ((vari (car m)) (val (cdr m)))
	;; 变量引用展开
	(do ((pos1 (position #\$ val) (position #\$ val))
	     pos2 offset)
	  ((null pos1))
	  (if (= pos1 (1- (length val)))
	    (return-from read-make.defaults :error)		; #\$ 位于变量值末尾
	    (if (char= (char val (1+ pos1)) #\{)
	      (unless (setf offset 2
			    pos2 (position #\} val :start (1+ pos1)))
		(return-from read-make.defaults :error))		; 缺少 #\}
	      (setf offset 1 pos2 (position-if #'(lambda (c)
						   (or (char= c #\Space) (char= c #\Tab)))
					       val :start (1+ pos1)))))
	  (let* ((ref-var (assoc (subseq val (+ pos1 offset) pos2)
				 vlist :test #'string=))
		 (ref-val (if (incremental-variable-p (car ref-var))
			    (reduce #'(lambda (&optional a b)
					(if a (concatenate 'string a " " b) ""))
				    (cdr ref-var))
			    (if ref-var (cdr ref-var) ""))))
	    (setf val (concatenate 'string (subseq val 0 pos1) ref-val
				   (subseq val (+ (or pos2 (length val)) offset -1))))))
	;; 变量叠加或覆盖
	(let ((old-var (assoc vari vlist :test #'string=)))
	  (if (incremental-variable-p vari)
	    (let ((tokens (delete "" (string-split val #\Space) :test #'string=)))
	      (unless old-var
		(setf old-var (list vari) vlist (nconc vlist (list old-var))))
	      (dolist (token tokens)
		(if (char/= (char token 0) #\-)
		  (setf (cdr (last old-var)) (list token))
		  (if (string= token "-*")
		    (setf (cdr old-var) nil)
		    (setf (cdr old-var) (delete (subseq token 1)
						(cdr old-var) :test #'string=))))))
	    (if old-var
	      (setf (cdr old-var) val)
	      (setf vlist (nconc vlist (list (cons vari val)))))))))))
