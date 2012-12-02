(ns thornydev.evolution.animal)

;; dir => direction animal is facing
;; it is an integer using the following system:
;;     |0|1|2|
;;     |7|X|3|
;;     |6|5|4|

(defrecord Animal [x y energy dir genes])
