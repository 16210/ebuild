;; 解析 bash 命令"declare -p"的输出
;; 参数：
;;	liu		一个输入流
;; 返回值：
;;	一个列表，每一项是一个变量的解析结果，
;;	每个解析结果是一个列表，有以下 3 个元素：
;;	* 属性选项。
;;	* 变量名。
;;	* 变量值。nil 或一个字符串，字符串内容
;;	  和"declare -p"输出中的相同，包含双引号。
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
