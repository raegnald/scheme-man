
(define (take-coins n)
  (while {(can-walk?) and (see? 'coin) and {n > 0}}
    (when (eq? (see) 'coin)
      (dec! n))
    (walk))
  (zero? n))

;; Solution to level 004
(walk-while (see? 'coin))
(turn 'right)
(take-coins 2)
(with-route-as 'bifurcation
  (simple-collect))
(go-back-to 'birfucation)
(turn 'right)
(simple-collect)
