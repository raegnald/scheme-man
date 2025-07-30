(display "Welcome to Scheme-Man!\n\n")

(set! *load-path*
      (cons (getenv "PWD")
            *load-path*))

(load "useful-macros.scm")

;; Checking that all intrinsic variables are bound
(let ((intrinsic-variables
       '(scman:playing?
         scman:action-to-perform
         scman:action-argument
         scman:action-result)))
  (while-let (variable-name (pop! intrinsic-variables))
    (unless (defined? variable-name)
      (format *stderr* "Intrinsic variable ~a is not bound"
              variable-name)
      (exit #f))))

(define (scman:reset-action-values)
  (set! scman:action-to-perform '())
  (set! scman:action-argument '())
  (set! scman:action-result '()))

(define-macro (scman:perform-action . body)
  `(when scman:playing?
     (scman:start-action)
     (scman:reset-action-values)
     ,@body
     (scman:wait-for-action-completion)
     scman:action-result))

;; Status messages

(define (scman:status message)
  (scman:perform-action
    (set! scman:action-to-perform 'set-status)
    (set! scman:action-argument message)))

(define-macro (status . fmt)
  `(scman:status (format #f ,@fmt)))

(define (clear-status)
  (status ""))


;; Action stack and anti-actions

(define scman:action-stack '())
(define scman:reverting-action? #f)

(define (scman:push-action action)
  (unless scman:reverting-action?
    (push! scman:action-stack action)))

(define (scman:anti-action action)
  (cond
   ;; (walk)
   ((equal? action '(walk))
    '(walk 1))

   ;; (walk n)
   ((and (pair? action)
         (eq? (car action) 'walk)
         (pair? (cdr action)))
    action)

   ;; (turn 'right)
   ((equal? action '(turn right))
    '(turn left))

   ;; (turn 'left)
   ((equal? action '(turn left))
    '(turn right))

   ;; (turn 'opposite)
   ((equal? action '(turn opposite))
    '(turn opposite))

   ;; (remember-place name)
   ((and (pair? action)
         (eq? (car action) 'remember-place)
         (pair? (cdr action)))
    action)

   (else '())))

(define* (remember-place name (talk #t))
  (scman:perform-action
    (set! scman:action-to-perform 'no-action)
    (scman:push-action `(remember-place ,name)))
  (when talk
    (status "I'll remember ~a" name)))

(define (scman:reached-place-name? action place-name)
  (and (pair? action)
       (eq? (car action) 'remember-place)
       (equal? (cadr action) place-name)))

(define* (go-back-to place-name (talk #t))
  (when talk
    (status "I'll return back to ~a" place-name))

  (set! scman:reverting-action? #t)
  (turn 'opposite)

  (let revert-action ()
    (let* ((action (pop! scman:action-stack))
           (reached-end (scman:reached-place-name? action place-name))
           (anti-action (scman:anti-action action)))
      (when {action and (not reached-end)}
        (eval anti-action (interaction-environment))
        (revert-action))))

  (turn 'opposite)
  (set! scman:reverting-action? #f))

(define-macro (with-route-backwards . body)
  (let ((place-name (gensym "route")))
    `(begin
       (remember-place ,place-name :talk #f)
       ,@body
       (go-back-to ,place-name :talk #f))))

;; Actions

(define* (walk (steps 1))
  (repeat steps
    (scman:perform-action
      (scman:push-action `(walk 1))
      (set! scman:action-to-perform 'walk))))

(define-macro (walk-while cond)
  `(while ,cond (walk)))

(define (turn direction)
  (case direction
    ((right r clockwise)
     (scman:perform-action
       (scman:push-action '(turn 'right))
       (set! scman:action-to-perform 'turn-right)))
    ((left l anticlockwise)
     (scman:perform-action
       (scman:push-action '(turn 'left))
       (set! scman:action-to-perform 'turn-left)))
    ((opposite)
     (repeat 2 (turn 'right)))
    (else (error "Cannot turn in that direction!"))))

(define* (go-back (steps 1))
  (turn 'opposite)
  (walk steps)
  (turn 'opposite))

(define (reset-level)
  (scman:perform-action
    (set! scman:action-stack '())
    (set! scman:action-to-perform 'reset-level)))

(define* (see (distance 1))
  (scman:perform-action
    (set! scman:action-to-perform 'see)
    (set! scman:action-argument distance)))

(define* (see? object (distance 1))
  (let ((seen (see distance)))
    (if (null? seen)
        #f
      (or (eq? seen object)
          (see? object (1+ distance))))))

(define (can-walk?)
  (not (null? (see))))

(define (playing?) scman:playing?)
