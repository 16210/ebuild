;; 获取软件包的稳定性等级
;; 参数：
;;	keywords	软件包 KEYWORDS 变量的值
;;	arch.list	仓库的 <平台列表>
;; 返回值：
;;	t，:~，:!，nil 或 :error
;;	分别代表稳定版，不稳定版，未知，在此平台上不能工作，
;;	以及 keywords 错误
(defun pkg-stability (keywords arch.list)
  (let ((stability :!) hyphen*)
    (dolist (k (delete "" (string-split keywords #\Space) :test #'string=))
      (let (form arch)
	(if (or (char= (char k 0) #\~) (char= (char k 0) #\-))
	  (setf arch (subseq k 1)
		form (if (char= (char k 0) #\~) :~ nil))
	  (setf arch k
		form t))
	(if (string= k "-*")
	  (if hyphen*
	    (return-from pkg-stability :error)	; 多个"-*"
	    (setf hyphen* t))
	  (if (not (find arch arch.list :test #'string=))
	    (return-from pkg-stability :error)	; <平台列表> 中不存在的平台
	    (when (string= arch *host-architecture*)
	      (setf stability form))))))
    (if (and (eql stability :!) hyphen*)
      nil
      stability)))
