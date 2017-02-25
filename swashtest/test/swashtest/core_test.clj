(ns swashtest.core-test
  (:require [clojure.test :refer :all]
            [swashtest.core :refer :all]))

(deftest minmax-test
  (testing "obtaining the vector of extrema from a list"
    (is (= [-1.0 1.0] (reduce minmax [0.0 0.0] '(0.9 0.2 -0.5 -0.1 0.0 1.0 -1.0))))
    ))

(comment
(deftest scale-velocities-test
  (testing "scaling the y coordinates of a [x y t] list"
    (is (empty? (scale-velocities 0.1 '([1 0.9][1 0.2][1 -0.5][1 -0.1][1 0.0][1 0.6][1 -1.0]))))
    (is (empty? (scale-velocities 10 '([1 0.9][1 0.2][1 -0.5][1 -0.1][1 0.0][1 0.6][1 -1.0]))))))
)
