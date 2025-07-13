(use-modules (ice-9 threads))

(display "Welcome to Scheme-Man!\n\n")

;; Internals

;;; A mutex that is set in C++ that signals this Scheme code that it
;;; has access to all level-related variables. When this mutex
;;; `scman-intrinsic/level-access' is locked, an action is being
;;; performed and no others should be sent.
(define scman-intrinsic/level-access
  (make-mutex 'allow-external-unlock))

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
       ;; The mutex we now lock will be unlocked by C++ after the
       ;; action to be completed is done
       ;; (lock-mutex scman-intrinsic/level-access)
       (scman-internal/start-action)

       (scman-internal/reset-action-values)
       (begin body ...)
       '())]))


;; Nice little macros that aid in solving levels

(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...)
     ;; Unhygienic!
     (let loop ((count n))
       (when (> count 0)
         body ...
         (loop (1- count)))))))

;; Actions

(define* (walk #:optional (n 1))
  (repeat n
    (scman-internal/perform-action
      (set! scman-intrinsic/action-to-perform 'walk))))

(define (turn direction)
  (case direction
    ('right
     (scman-internal/perform-action
       (set! scman-intrinsic/action-to-perform 'turn-right)))
    ('left
     (scman-internal/perform-action
       (set! scman-intrinsic/action-to-perform 'turn-left)))
    ('opposite
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
