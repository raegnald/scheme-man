
(define (take-coins n)
  (while {(can-walk?) and (see? 'coin) and {n > 0}}
    (when (eq? (see) 'coin)
      (dec! n))
    (walk))
  (when {n > 0}
    (turn 'right)
    (take-coins n))
  #t)

;; Solution to level 004
(walk-while (see? 'coin))
(turn 'right)
(take-coins 2)
(with-route-backwards
 (take-coins 22))
(take-coins 2)
(with-route-backwards
 (take-coins 20))
(turn 'left)
(walk 9)
