;; Useful, generic macros that extend s7 and make writing code in it
;; more expressive.

;; List manipulation macros

(define-macro (push! list-sym x)
  `(set! ,list-sym (cons ,x ,list-sym)))

(define-macro (pop! list-sym)
  (let ((v (gensym)))
    `(if (null? ,list-sym)
         #f
         (let ((,v (car ,list-sym)))
           (set! ,list-sym (cdr ,list-sym))
           ,v))))

;; Numeric macros

(define-macro (inc! x)
  `(set! ,x (+ ,x 1)))

(define-macro (dec! x)
  `(set! ,x (- ,x 1)))

;; Looping macros

(define-macro (while-let bind . body)
  (let ((loop (gensym "loop"))
        (var (car bind))
        (exp (cadr bind)))
    `(let ,loop ((,var ,exp))
       (when ,var
         ,@body
         (,loop ,exp)))))

(define-macro (repeat n . body)
  (let ((loop  (gensym "loop"))
        (count (gensym "count")))
    `(let ,loop ((,count ,n))
        (when (> ,count 0)
          ,@body
          (,loop (- ,count 1))))))
