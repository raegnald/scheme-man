(status "Me lo paso a la primera")

;; Completar nivel
(repeat 3
        (walk 2)
        (repeat 2
                (turn 'left)
                (walk 2))
        (turn 'right)
        (walk 2)
        (turn 'right))
(walk 2)
