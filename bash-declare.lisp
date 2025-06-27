;; 解析 bash 命令"declare -p"的输出
;; 参数：
;;	liu		一个输入流
;; 返回值：
;;	一个 <declare> 列表
(defun parse-bash-declare (liu)
  (do ((current (read liu nil :eof) (read liu nil :eof))
       (attr (cons 'string nil) (cons 'string nil))
       (name (cons 'string nil) (cons 'string nil))
       (value nil nil) prev var-lst)
    ((eql current :eof) var-lst)
    ; attr
    (do ((current (read-char liu) (read-char liu)))
      ((char= current #\Space))
      (push (string current) attr))
    (setf attr (apply #'concatenate (nreverse attr)))
    ; name
    (do ((current (read-char liu) (read-char liu)))
      ((or (char= current #\=) (char= current #\Newline))
       (setf prev current))
      (push (string current) name))
    (setf name (apply #'concatenate (nreverse name)))
    ; value
    (if (char= prev #\=)
      (if (char= (read-char liu) #\()
	(setf value (concatenate 'string "(" (read-line liu)))
	(do ((current (read-char liu) (read-char liu))
	     (v (cons 'string nil)))
	  ((char= current #\")
	   (setf value (concatenate 'string "\""
				    (apply #'concatenate (nreverse v))
				    "\"")))
	  (push (string current) v)
	  (if (char= current #\\)
	    (push (string (read-char liu)) v)))))
    ; var-lst
    (setf var-lst (nconc var-lst (list (list attr name value))))))

(defmacro mk-declare (attribute name value)
  `(list ,attribute ,name ,value))
(defmacro declare-attribute (d)
  `(car ,d))
(defmacro declare-name (d)
  `(cadr ,d))
(defmacro declare-value (d)
  `(caddr ,d))

(defun mk-declare-scalar (attribute name value)
  (if value
    (setf value (format nil "~S" value)))
  (mk-declare attribute name value))
(defun mk-declare-array (attribute name value)
  (mk-declare attribute name value))

(defun declare-value-scalar (d)
  (let ((v (declare-value d)))
    (if v
      (subseq v 1 (1- (length v))))))
(defun declare-value-array (d)
  (declare-value d))

(defun set-declare-value-scalar (d value)
  (if value
    (setf value (format nil "~S" value)))
  (setf (declare-value d) value))
(defun set-declare-value-array (d value)
  (setf (declare-value d) value))

;; 将 <declare> 列表输出为 bash“declare”命令
;; 参数：
;;	liu			输出流
;;	bash-declare-list	<declare> 列表
;; 返回值：
;;	nil
(defun write-bash-declare (liu bash-declare-list)
  (dolist (v bash-declare-list)
    (format liu "declare ~A ~A" (declare-attribute v) (declare-name v))
    (if (declare-value v)
      (format liu "=~A~%" (declare-value v))
      (format liu "~%"))))
