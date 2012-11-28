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

  ;; use reader-form constructor notation
  (let [orc #thornydev.orc.game.Orc{:health 8, :club-level 5}]
    (testing "show"
      (let [show-out (with-out-str (do-monster-show orc))]
        (is (re-find #"level 5 club" show-out))
        (is (re-find #"8 health" show-out)))
      )
    (testing "attack"
      (let [b4-player-strength (:health @player)
            attack-out
            (with-out-str
              ;; should get back the same orc
              ;; (attacking doesn't change the orc)
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
    (testing "hit: damage the orc"
      (let [hit-out
            (with-out-str
              (let [mon (monster-hit orc 2)]
                (is (= 6 (:health mon)))
                (is (not (identical? mon orc)))
                (is (= (:club-level mon) (:club-level orc)))
                ))
            [_ hit] (first
                     (re-seq #"off (\d+) health points" hit-out))]
        (is (= 2 (read-string hit)))
        )
      )
    (testing "hit: kill the orc"
      (let [hit-out
            (with-out-str
              (let [mon (monster-hit orc 8)]
                (is (= 0 (:health mon)))
                (is (monster-dead? mon))))]
        (is (re-seq #"(?i)you killed the orc" hit-out))
        )
      )
    ))

(deftest test-Hydra
  (let [hyd (new-Hydra)
        orig-health (:health hyd)]

    (testing "show"
      (let [show-out (with-out-str (do-monster-show hyd))
            pat (re-pattern (str "hydra with " orig-health " heads"))]
        (is (re-find pat show-out))))

    (testing "attack"
      (let [b4-plyr-strength (:health @player)
            attack-out
            (with-out-str
              (let [mon (monster-attack hyd)]
                ;; when the hydra attacks it gets one of its heads back
                ;; so you get a new hydra back from this method
                (is (not (identical? mon hyd)))
                (is (= (inc orig-health) (:health mon)))
                ))
            [_ hit] (first
                     (re-seq #"attacks you with (\d+) of its heads" attack-out))
            hit-int (Integer/valueOf hit)
            ]
        (is (= (:health @player) (- b4-plyr-strength hit-int)))
        )
      )
    (testing "hit"
      ;; LEFT OFF HERE ...
      )
    )
  )

(prn (run-tests))
