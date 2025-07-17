(define* (see? object #:optional (distance 1))
  (let ([seen (see distance)])
    (if (null? seen)
        #f
      (or (eq? seen object)
          (see? object (1+ distance))))))

(while (playing?)
  (while (and (can-walk?) (or (see? 'coin) (see? 'star))) (walk))
  (while (not (or (see? 'coin) (see? 'star))) (turn 'right)))
