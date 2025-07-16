(use-modules (ice-9 threads))

(display "Welcome to Scheme-Man!\n\n")

;; Internals

(define scman-intrinsic/action-to-perform '())
(define scman-intrinsic/action-argument '())
(define scman-intrinsic/action-result '())
(define scman-intrinsic/game-ended-p #f)

(define (scman-internal/reset-action-values)
  (set! scman-intrinsic/action-to-perform '())
  (set! scman-intrinsic/action-argument '())
  (set! scman-intrinsic/action-result '()))

(define (scman-internal/start-action)
  (scman-intrinsic/start-action))

(define-syntax scman-internal/perform-action
  (syntax-rules ()
    [(_ body ...)
     (begin
       ;; This function call tells C++ that an action is going to be
       ;; performed.
       (scman-internal/start-action)
       (scman-internal/reset-action-values)
       (begin body ...)
       '())]))

;; State of Scheme-Man that only Scheme has to know about

(define scman/solution-file '())

(define (scman/evaluate-solution-file)
  (when scman/solution-file
    (load scman/solution-file)))

;; Nice little macros that aid in solving levels

(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...)
     ;; Unhygienic!
     (let loop ((count n))
       (when (> count 0)
         body ...
         (loop (1- count)))))))

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
      (set! scman-intrinsic/action-to-perform 'walk))))

(define (turn direction)
  (case direction
    ((right)
     (scman-internal/perform-action
       (set! scman-intrinsic/action-to-perform 'turn-right)))
    ((left)
     (scman-internal/perform-action
       (set! scman-intrinsic/action-to-perform 'turn-left)))
    ((opposite)
     (repeat 2 (turn 'right)))
    (else (error "Cannot turn in that direction!"))))

(define (status message)
  (scman-internal/perform-action
    (set! scman-intrinsic/action-to-perform 'set-status)
    (set! scman-intrinsic/action-argument message)))

(define* (go-back #:optional (n 1))
  (turn 'opposite)
  (walk n)
  (turn 'opposite))

(define (reset-level)
  (scman-internal/perform-action
    (set! scman-intrinsic/action-to-perform 'reset-level)))
