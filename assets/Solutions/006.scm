
(define (take-coins n)
  (while {(can-walk?) and (see? 'coin) and {n > 0}}
	 (when (eq? (see) 'coin)
	   (dec! n))
	 (walk))
  (when {n > 0}
    (turn 'right)
    (take-coins n))
  #t)

(define (opposite dir)
  (match dir
    [left 'right]
    [right 'left]
    [opposite 'opposite]))

(define (take-coins-zig-zag dir n)
  (turn dir)
  (unless (can-walk?)
    (turn 'opposite)
    (set! dir (opposite dir)))
  (while {(can-walk?) and {n > 0}}
	 (when (eq? (see 1) 'coin)
	   (dec! n))
	 (walk))
  (when {n > 0}
    (take-coins-zig-zag (opposite dir) n)))

;; Solution:

(take-coins 10)
(take-coins-zig-zag 'left 3)

(turn 'left)
(walk 8)

(remember-place 'bifurcation)

(walk)
(take-coins-zig-zag 'right 3)

(go-back-to 'bifurcation)

(take-coins-zig-zag 'left 8)
(turn 'right)
(walk 3)
