;; 追加积
;   对于两个元素都是列表的列表 A 和 B，追加积运算的结果是一个列表，
;   列表中的元素由 (append a b) 得到，
;   其中 a 是 A 中的元素，b 是 B 中的元素。
;   如果 A 和 B 有一个为空或两个都为空，则追加积为空
;; 参数：
;;	list-a		列表 A
;;	list-b		列表 B
;; 返回值：
;;	A 和 B 的追加积
(defun append-product (list-a list-b)
  (unless (and list-a list-b)
    (return-from append-product nil))
  (let (product)
    (dolist (a list-a)
      (dolist (b list-b)
	(push (append a b) product)))
    (nreverse product)))
