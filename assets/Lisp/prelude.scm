(read-enable 'curly-infix)

(use-modules (ice-9 threads) (ice-9 match))

(display "Welcome to Scheme-Man!\n\n")

;; Internals

(define scman-intrinsic/action-to-perform '())
(define scman-intrinsic/action-argument '())
(define scman-intrinsic/action-result '())
(define scman-intrinsic/game-ended-p #f)

(define (scman-internal/reset-action-values)
  (set! scman-intrinsic/action-to-perform '())
  (set! scman-intrinsic/action-argument '()))

(define (scman-internal/start-action)
  (scman-intrinsic/start-action))

(define-syntax scman-internal/perform-action
  (syntax-rules ()
    [(_ body ...)
     (when (not scman-intrinsic/game-ended-p)
       (set! scman-intrinsic/action-result '())
       (scman-internal/start-action)
       (scman-internal/reset-action-values)
       (begin body ...)
       (scman-intrinsic/wait-for-action-completion)
       scman-intrinsic/action-result)]))

;; Nice little macros that aid in solving levels

(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...)
     (let loop ((count n))
       (when (> count 0)
         body ...
         (loop (1- count)))))))

(define-syntax-rule (inc! x)
  (set! x (1+ x)))

(define-syntax-rule (dec! x)
  (set! x (1- x)))

(define-syntax-rule (push! list x)
  (set! list (cons x list)))

(define-syntax-rule (pop! xs)
  (let ([head (car xs)])
    (set! xs (cdr xs))
    head))

;; Status messages

(define (scman-internal/status message)
  (scman-internal/perform-action
    (set! scman-intrinsic/action-to-perform 'set-status)
    (set! scman-intrinsic/action-argument message)))

(define-syntax-rule (status fmt ...)
  (scman-internal/status (format #f fmt ...)))

(define (clear-status)
  (status ""))

;; Action stack and anti-actions

(define scman-internal/action-stack '())
(define scman-internal/reverting-action? #f)

(define (scman-internal/push-action action)
  (unless scman-internal/reverting-action?
    (push! scman-internal/action-stack action)))

(define (scman-internal/anti-action action)
  (match action
    ['(walk) '(walk 1)]
    [`(walk ,n) `(walk ,n)]
    ['(turn 'right) '(turn 'left)]
    ['(turn 'left) '(turn 'right)]
    ['(turn 'opposite) '(turn 'opposite)]
    [`(remember-place ,name) `(remember-place ,name)]
    ['() '()]))

(define* (remember-place name #:key (talk #t))
  (scman-internal/perform-action
    (set! scman-intrinsic/action-to-perform 'no-action)
    (scman-internal/push-action `(remember-place (quote ,name))))
  (when talk
    (status "I'll remember ~a" name)))

(define (scman-internal/reached-place-name? action place-name)
  (match action
    [('remember-place ,name) (eq? name place-name)]
    [else #f]))

(define* (go-back-to place-name #:key (talk #t))
  (when talk
    (status "I'll return back to ~a" place-name))
  (set! scman-internal/reverting-action? #t)
  (turn 'opposite)
  (let revert-action ()
    (let* ([action (pop! scman-internal/action-stack)]
           [reached-end (scman-internal/reached-place-name? action place-name)]
           [anti-action (scman-internal/anti-action action)])
      (when {action and (not reached-end)}
        (eval anti-action (interaction-environment))
        (revert-action))))
  (turn 'opposite)
  (set! scman-internal/reverting-action? #f))

(define-syntax-rule (with-route-backwards body ...)
  (let ([place-name (gensym)])
    (remember-place place-name #:talk #f)
    (begin body ...)
    (go-back-to place-name #:talk #f)))

;; State of Scheme-Man that only Scheme has to know about

(define scman-internal/solution-file '())

(define (scman-internal/evaluate-solution-file)
  (when scman-internal/solution-file
    (load scman-internal/solution-file)))

;; Loading a solution to a level

(define (solution file)
  (reset-level)
  (set! scman/solution-file file)
  (scman/evaluate-solution-file))

(define (restart)
  (reset-level)
  (scman/evaluate-solution-file))

;; Actions

(define* (walk #:optional (n 1))
  (repeat n
    (scman-internal/perform-action
      (scman-internal/push-action `(walk 1))
      (set! scman-intrinsic/action-to-perform 'walk))))

(define-syntax-rule (walk-while cond)
  (while cond (walk)))

(define (turn direction)
  (case direction
    [(right r clockwise)
     (scman-internal/perform-action
       (scman-internal/push-action '(turn 'right))
       (set! scman-intrinsic/action-to-perform 'turn-right))]
    [(left l anticlockwise)
     (scman-internal/perform-action
       (scman-internal/push-action '(turn 'left))
       (set! scman-intrinsic/action-to-perform 'turn-left))]
    [(opposite)
     (repeat 2 (turn 'right))]
    (else (error "Cannot turn in that direction!"))))

(define* (go-back #:optional (n 1))
  (turn 'opposite)
  (walk n)
  (turn 'opposite))

(define (reset-level)
  (set! scman-internal/action-stack '())
  (scman-internal/perform-action
    (set! scman-intrinsic/action-to-perform 'reset-level)))

(define* (see #:optional (n 1))
  (scman-internal/perform-action
    (set! scman-intrinsic/action-to-perform 'see)
    (set! scman-intrinsic/action-argument n)))

(define* (see? object #:optional (distance 1))
  (let ([seen (see distance)])
    (if (null? seen)
        #f
      (or (eq? seen object)
          (see? object (1+ distance))))))

(define (can-walk?)
  (not (null? (see))))

(define (playing?)
  (not scman-intrinsic/game-ended-p))
