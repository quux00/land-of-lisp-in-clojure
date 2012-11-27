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

(deftest test-Orc
  (init-player)
  
  (let [orc (->Orc 8 5)]
    (testing "show"
      (let [show-out (with-out-str (do-monster-show orc))]
        (is (re-find #"level 5 club" show-out))
        (is (re-find #"8 health" show-out)))
      )
    (testing "attack"
      (let [b4-player-strength (:health @player)
            attack-out
            (with-out-str
              (let [mon (monster-attack orc)]
                (is (= 8 (:health mon)))
                (is (identical? orc mon))
                ))
            [_ hit] (first
                     (re-seq #"knocks off (\d+) of your health" attack-out))]
        ;; ensure current strength decreased by amount reported
        ;; from the monster-attack method
        (is (= (Integer/valueOf hit)
               (- b4-player-strength (:health @player))))
        )
      )
    )
  )

(prn (run-tests))
