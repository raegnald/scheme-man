(define (walk-towards-coins)
  (while (and (can-walk?) (or (see? 'coin) (see? 'star))) (walk)))

(define (look-for-coins)
  (while (not (or (see? 'coin) (see? 'star))) (turn 'right)))

(while (playing?)
  (walk-towards-coins)
  (look-for-coins))
