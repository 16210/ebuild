(defmacro absolute-path-p (path)
  `(char= (char ,path 0) #\/))

;; 字符串拆分
;; 参数：
;;	str		要拆分的字符串
;;	fengefu		分隔符，一个字符
;; 返回值：
;;	一个拆分得到的字符串组成的列表
(defun string-split (str fengefu)
  (let ((pos (position fengefu str)))
    (if (null pos)
      (list str)
      (nconc (list (subseq str 0 pos))
	     (string-split (subseq str (1+ pos)) fengefu)))))

;; 路径拼接
;; 参数：全部是路径字符串
;; 返回值：
;;	拼接得到的路径字符串。
;;	当参数全部是相对路径时，拼接得到的是相对路径，
;;	否则是绝对路径，且最后一个绝对路径参数前边的参数忽略。
;;	返回值不以 #\/ 结尾，除非是根目录。
(defun path-join (first-path &rest rest-path)
  (let (stack absolutep (bottom 0))
    (dolist (p (cons first-path rest-path))
      (if (absolute-path-p p)
	(setf stack nil absolutep t))
      (dolist (dir (string-split p #\/))
	(if (not (or (string= dir ".")
		     (string= dir "")))
	  (if (string= dir "..")
	    (if absolutep
	      (pop stack)
	      (if (= (length stack) bottom)
		(progn (push ".." stack)
		       (incf bottom))
		(pop stack)))
	    (push dir stack)))))
    (if (null stack)
      (if absolutep
	"/"
	".")
      (let (args)
	(do ((dir (pop stack) (pop stack)))
	  ((null dir))
	  (push dir args)
	  (push "/" args))
	(if absolutep
	  (push 'string args)
	  (setf (car args) 'string))
	(apply #'concatenate args)))))
