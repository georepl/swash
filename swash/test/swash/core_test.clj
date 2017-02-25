(ns swash.core-test
  (:require [clojure.test :refer :all]
            [swash.core :refer :all]))



(deftest velocity-profile-test
  (testing "scaling of velocity profile"
    (let [coll (velocity-profile '([300 600 5000][340 700 5100][340 600 5300][300 520 5900][330 410 6200][360 323 6500]
                                   [400 350 7600][450 400 8000][560 590 9600][620 620 12000][600 680 13600][600 700 15000]))]
(prn "COLL:" coll)
      (is (not (empty? coll))))))
