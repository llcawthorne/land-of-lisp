;; the let1 macro is used by split
;; let1 allows you to do a let declaration of one variable with less
;; parentheses, like: (let1 foo (+ 2 3) (* foo foo))
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; the split macro is used by pairs
;; it splits a list into head and tail, then executes yes if the list is
;; non-empty or no if there is no list left
(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))

;; groups a list into pairs
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

;; prints a single opening or closing tag
(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

;; example use of this macro
;; (tag mytag (color 'blue height (+ 4 5)))
;; OUTPUT: <mytag color="BLUE" height="9"></mytag>
(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

;; this macro generates the main SVG header and takes body
(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
             "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ,@body))

;; We're storing colors as lists of RGB values: (255 0 0)
;; this adds to all a colors RGB values to make it brighter or darker
(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

;; svg-style sets the style of an SVG picture element
;; it sets the outline color to a darker variant of the fill color
(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

;; since we don't need to nest other tags within a circle, we can use a function
(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

;; svg also support polygons
;; the poly con takes points like ((0 . 200) (10 . 100))
;; and a color like red: (255 0 0)
(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

;; and this will generate a random walk from a value and length
;; Example: (random-walk 100 10)
;; Returns: (100 101 102 101 100 101 102 103 102 103)
(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))

;; now let's draw a slew of random walks to random_walk.svg
(with-open-file (*standard-output* "random_walk.svg"
                                   :direction :output
                                   :if-exists :supersede)
  (svg (loop repeat 10
             do (polygon (append '((0 . 200))
                                 (loop for x
                                       for y in (random-walk 100 400)
                                       collect (cons x y))
                                 '((400 . 200)))
                         (loop repeat 3
                               collect (random 256))))))
