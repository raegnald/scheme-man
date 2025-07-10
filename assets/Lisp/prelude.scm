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

(define-syntax scman-internal/perform-action
  (syntax-rules ()
    [(_ body ...)
     (begin
       ;; The mutex we now lock will be unlocked by C++ after the
       ;; action to be completed is done
       (lock-mutex scman-intrinsic/level-access)

       (scman-internal/reset-action-values)
       (begin body ...)
       '())]))

(define (scman-internal/set-action action)
  ;; TODO: assert action in '('walk 'turn-left 'turn-right)
  (set! scman-intrinsic/action-to-perform action))


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

(define (walk)
  (scman-internal/perform-action
     (scman-internal/set-action 'walk)))

(define (turn direction)
  (case direction
    ('right
     (scman-internal/perform-action
      (scman-internal/set-action 'turn-right)))
    ('left
     (scman-internal/perform-action
      (scman-internal/set-action 'turn-left)))
    (else (error "Cannot turn in that direction!"))))
