(ns swash.core)

(defn sqrt[x] (Math/sqrt x))
(defn acos[x] (Math/acos x))

(defn square [x]
  (* x x))

(defn dot-product [v w]
  (reduce + (map * v w)))

(defn length [p]
  (sqrt (dot-product p p)))

(defn norm [v]
  (let [len (length v)
        x (first v)
        y (second v)]
    (if (and (zero? x)(zero? y))
      [0.0 0.0]
      [(/ x len)(/ y len)])))

;===================================

(defn minmax [[a b] x]
  [(if (< x a) x a) (if (> x b) x b)])

(defn scale [curve x-units y-units]
  ;; scaling a profile
  (let [x0 (first (first curve))
        dx (/ x-units (- (first (last curve)) x0))
        [y0 yn] (reduce minmax [0.0 0.0] (map second curve))
        dy (/ y-units (- yn y0))]
    (map (fn [[x y]][(* (- x x0) dx)(* y dy)]) curve)))


;===================================


(defn velocity [[x1 y1 t1][x2 y2 t2]]
  [t1 (/ (sqrt (+ (square (- x2 x1))(square (- y2 y1)))) (- t2 t1))])

(defn curvature [t0 [x1 y1 t1][x2 y2 t2][x3 y3 t3]]
  (let [u [(- x2 x1)(- y2 y1)]
        v [(- x3 x2)(- y3 y2)]
        len (+ (length [(- x2 x1)(- y2 y1)])(length [(- x3 x2)(- y3 y2)]))]
    (if (zero? len)
      nil
      [(- t2 t0) (/ (acos (dot-product (norm u)(norm v))) len)])))



;===================================
;; swash API

(defn velocity-profile [trace]
  ;; creating the velocity profile
  (map #(velocity %1 %2) trace (rest trace)))

(defn curvature-profile [trace]
  ;; creating the curvature profile
  (let [t0 (last (first trace))]
    (filter #(not (nil? %))
            (map #(curvature t0 %1 %2 %3) trace (rest trace) (rest (rest trace))))))




(defn define [name trace]
  )

(defn define [name & names]
  )

(defn analyze [trace]
  )
