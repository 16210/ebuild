;; 读取配置文件解析各个段
;; 参数：
;;	conf	配置文件的带路径文件名
;; 返回值：
;;	((<段名称> . (<非注释行>...))...) 或 :error
;;	忽略第一个段之前的内容
(defun parse-conf-section (conf)
  (let (slist)
    (with-open-file (liu conf :direction :input)
      (do ((line (read-line liu nil :eof) (read-line liu nil :eof)))
	((eql line :eof) slist)
	(setf line (string-trim #(#\Space #\Tab) (car (string-split line #\#))))
	(when (string/= line "")
	  (if (and (char= (char line 0) #\[)
		   (char= (char line (1- (length line))) #\]))
	    (let ((sname (string-trim "[]" line)))
	      (if (string= sname "")
		(return-from parse-conf-section :error)
		(setf slist (nconc slist (list (cons sname nil))))))
	    (when slist
	      (setf (cdar (last slist)) (nconc (cdar (last slist)) (list line))))))))))

;; 配置文件属性段内容解析
;; 参数：
;;	lines	属性段的内容行列表
;; 返回值：
;;	((<属性> . <值>)...) 或 :error
(defun property-list (lines)
  (let (plist)
    (dolist (line lines plist)
      (setf line (string-split line #\=))
      (if (/= (length line) 2)
	(return-from property-list :error)
	(setf plist (nconc plist (list (cons (car line) (cadr line)))))))))
