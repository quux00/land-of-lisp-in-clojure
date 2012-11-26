(ns thornydev.orc.game-test
  (:use clojure.test
        thornydev.orc.game))

(deftest test-player-dead?
  (testing "after player init"
    (init-player)
    (is (not (player-dead?)))
    ))

;; TODO: uncomment after define (init-monsters)
;; (deftest test-get-monster-choice
;;   (testing "after monster init"
;;     (init-monsters)
;;     (is (= '??? (get-monster-choice 3)))
;;     (is (thrown? IllegalArgumentException (get-monster-choice -1)))
;;     (is (thrown? IllegalArgumentException
;;                  (get-monster-choice (inc *monster-num*))))
;;     (is (thrown? IllegalArgumentException
;;                  (get-monster-choice :put-dead-monster-here)))
;;     ))

(deftest monster-hit)

(prn (run-tests))
