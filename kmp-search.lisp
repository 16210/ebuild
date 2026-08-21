;; 计算“子串最长公共前后缀长度”表
;; 参数：
;;	pattern		一个字符串
;; 返回值：
;;	一个向量
(defun kmp-next (pattern)
  (do* ((pattern-len (length pattern))
	(next (make-array pattern-len :initial-element 0))
	(idx 1)
	(max-common-len 0))
    ((>= idx pattern-len) next)
    (if (char= (char pattern idx)
	       (char pattern max-common-len))
      (setf (svref next idx) (incf max-common-len)
	    idx (1+ idx))
      (if (zerop max-common-len)
	(setf (svref next idx) 0
	      idx (1+ idx))
	(setf max-common-len (svref next (1- max-common-len)))))))

;; 字符串搜索
;; 参数：
;;	text		待搜索的字符串
;;	pattern		目标字符串，非空
;;	start		搜索起始索引
;; 返回值：
;;	找到目标字符串则返回它的首字符索引，
;;	找不到则返回 nil
(defun kmp-search (text pattern &key (start 0))
  (do ((text-len (length text))
       (pattern-len (length pattern))
       (next (kmp-next pattern))
       (i 0))
    ((or (>= start text-len)
	 (= i pattern-len))
     (when (= i pattern-len)
       (- start pattern-len)))
    (if (char= (char text start) (char pattern i))
      (setf start (1+ start) i (1+ i))
      (if (zerop i)
	(incf start)
	(setf i (svref next (1- i)))))))
