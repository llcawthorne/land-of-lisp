;; first, we need a way to define lazy values
(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

;; and to force those lazy values
(defun force (lazy-value)
  (funcall lazy-value))

;; now we can start a list library
(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

;; now lets use our lazy library to define the integers
(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))
;; (lazy-cdr *integers*) returns a function.  if you funcall it, you get
;; (2 . #<FUNCTION)

;; not all lazy lists are infinite, so we need lazy-nil
(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

;; now we need a way to convert a regular list to a lazy list
(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

;; (take 10 *integers*) returns '(1 2 3 4 5 6 7 8 9 10)
(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
          (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))
