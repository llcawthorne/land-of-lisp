;; this takes the two characters after the % in an http encoded string
;; (the two hex digits) and converts it to a char.  for invalid codes,
;; it returns #\Space by default, but that is a parameter
;; for (http-char #\3 #\F) it would return "?"
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

;; this decodes a param, for example given "foo" it returns "foo"
;; for "foo%3F" it returns "foo?", and "foo+bar" is "foo bar"
(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

;; this parses params in the form "name=bob&age=25&gender=male"
;; returns an alist: ((NAME . "bob") (AGE . "25") (GENDER . "male"))
(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s(1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

;; this parses an http request header, for example:
;; "GET /lolcats.html HTTP/1.1" would return ("lolcats.html")
;; it also parses URL params, so:
;; "GET /lolcats.html?extra-funny=yes HTTP/1.1" would return
;; ("lolcats.html" (EXTRA-FUNNY . "yes"))
(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

;; parse-url handles the first list.  get-header processes the rest of the
;; request header and returns an alist
(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

;; this will parse a request body contained in a POST request
;; it's stored like URL params, so we can re-use parse-params
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

;; Now for the heart of our web server!
(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url    (parse-url (read-line stream)))
                        (path   (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-server-close socket))))

;; We're going to define a simple request handler to provide our serve function
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry...  I don't know that page.")))
