;; 逻辑同或
(declaim (inline xnor))
(defun xnor (a b)
  (if a
    b
    (not b)))

;; 逻辑异或
(declaim (inline xor))
(defun xor (a b)
  (if a
    (not b)
    b))
