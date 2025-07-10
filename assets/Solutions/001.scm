
;;;;
;; NOTE: This is a temporary fix because the player is put at the map
;; origin, not the intended origin of the level. THIS IS NOT PART OF
;; THE SOLUTION!
(walk)
(turn 'right)
(walk)
(turn 'left)
;;;;

(repeat 3
  (repeat 4 (walk))
  (turn 'right))

(repeat 2
  (repeat 2 (walk))
  (turn 'right))
