(ns thornydev.orc.game-test
  (:use clojure.test
        thornydev.orc.game
        thornydev.orc.monsters))

(deftest test-player-dead?
  (testing "after player init"
    (init-player)
    (is (not (player-dead?)))
    ))

(deftest test-get-monster-choice
  (testing "after monster init"
    (binding [*monster-num* 3]
      (let [predef-mon [(atom (->Orc 10 10))
                        (atom (->Brigand 0))
                        (atom (->SlimeMold 3 4))]]
        (binding [*monsters* predef-mon]
          (is (= (->Orc 10 10) @(get-monster-choice 1)))
          (is (= (->SlimeMold 3 4) @(get-monster-choice 3)))
          (is (thrown? IllegalArgumentException (get-monster-choice -1)))
          (is (thrown? IllegalArgumentException
                       (get-monster-choice (inc *monster-num*))))
          ;; this one is already dead, so throws exception
          (is (thrown? IllegalArgumentException
                       (get-monster-choice 2))))
        )
      )
    )
  )

(deftest test-Orc
  (init-player)

  ;; use reader-form constructor notation
  (let [orc #thornydev.orc.monsters.Orc{:health 8, :club-level 5}]
    (testing "show"
      (let [show-out (with-out-str (do-monster-show orc))]
        (is (re-find #"level 5 club" show-out))
        )
      )
    (testing "attack"
      (let [b4-player-strength (:health @player)
            attack-out
            (with-out-str
              ;; should get back the same orc
              ;; (attacking doesn't change the orc)
              (let [mon (monster-attack orc player)]
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
              (let [mon (monster-attack hyd player)]
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
      (with-out-str
        (let [mon1 (monster-hit hyd (dec orig-health))
              mon2 (monster-hit hyd orig-health)]
          (is (= 1 (:health mon1)))
          (is (not (monster-dead? mon1)))
          (is (= 0 (:health mon2)))
          (is (monster-dead? mon2))))
      )
    ))


(deftest test-monsters-dead?
  (binding [*monster-builders* [new-Orc new-Hydra
                                new-SlimeMold new-Brigand]]
    (binding [*monster-num* 4]
      (binding [*monsters* (create-random-monsters)]

        (testing "all monsters with non-zero health"
          (is (= 4 (count *monsters*)))
          (is (not (monsters-dead? *monsters*))))

        (testing "some monsters with non-zero health"
          (binding [*monsters* (assoc (create-random-monsters) 0
                                      (atom (->Hydra 0)))]
            (is (monster-dead? @(first *monsters*)))
            (is (not (monster-dead? @(second *monsters*))))
            (is (not (monsters-dead? *monsters*)))
            )
          )
        
        (testing "all monsters with non-zero health = all dead"
          (binding [*monsters* [(atom (->Hydra 0))
                                (atom (->Brigand 0))
                                (atom (->SlimeMold 0 0))
                                (atom (->Orc 0 0))]]
            (is (monster-dead? @(first *monsters*)))
            (is (monster-dead? @(second *monsters*)))
            (is (monsters-dead? *monsters*))))
        ))))

(prn (run-tests))
