(ns swash.core)

(defn sqrt[x] (Math/sqrt x))
;;(defn acos[x] (Math/acos x))
(defn abs[x] (Math/abs x))

(defn divide [a b]
  (if (zero? b) nil (/ a b)))

(defn square [x]
  (float (* x x)))

(defn dot-product [v w]
  (float (reduce + (map * v w))))

(defn length [p]
  (sqrt (dot-product p p)))

(defn norm [v]
  (let [len (length v)
        x (first v)
        y (second v)]
    (if (and (zero? x)(zero? y))
      [0.0 0.0]
      [(float (/ x len))(float (/ y len))])))


;===================================

(defn minmax [[a b] x]
  [(min a x) (max b x)])

(defn mindist [[[x1 y1 t1][x2 y2 t2]]]
  (>= (abs (- t2 t1)) 1))

;; remove consecutive points and transform the trace segment so the t coordinates start with 0
(defn normalize-segment [traceseg]
  (let [t0 (last (first traceseg))
        coll (map (fn [[x y t]][x y (- t t0)]) traceseg)]
    (map first (filter mindist (map list coll (rest (cycle coll)))))))


;; normalize all segments of a trace and remove segments with less than 2 points
(defn normalize [trace]
  (filter #(> (count %) 2) (map normalize-segment trace)))


;; scaling a profile (that is, velocity or curvature)
(defn scale
  ([curve x-units y-units [x0 xn] [y0 yn]]
    (let [dx (if (zero? (- xn x0)) 0 (/ x-units (- xn x0)))
          dy (if (zero? (- yn y0)) 0 (/ y-units (- yn y0)))]
      (map (fn [[x y]][(* x dx)(* y dy)]) curve)))
  ([curve x-units y-units]
    (let [x0 (first (first curve))
          xn (- (first (last curve)) x0)
          [y0 yn] (reduce minmax [0.0 0.0] (map second curve))]
      (scale curve x-units y-units [x0 xn] [y0 yn]))))


;; scaling a trace
(defn scale-trace-segment [trace x-units y-units]
  (let [[x0 xn] (reduce minmax [0.0 0.0] (map first trace))
        [y0 yn] (reduce minmax [0.0 0.0] (map second trace))]
    (map #(conj (vec %1) %2)
         (scale (map butlast trace) x-units y-units [x0 xn] [y0 yn])
         (map last trace))))


;===================================


(defn velocity [t0 [x1 y1 t1][x2 y2 t2]]
  (let [v [(- x2 x1)(- y2 y1)]]
    [(- t1 t0) (/ (length v) (- t2 t1))]))

(defn curvature [t0 [x1 y1 t1][x2 y2 t2][x3 y3 t3]]
  (let [u [(- x2 x1)(- y2 y1)]
        v [(- x3 x2)(- y3 y2)]
        len (+ (length u)(length v))]
    (if (zero? len)
      [(- t1 t0) 0]
      [(- t1 t0) (/ (Math/acos (dot-product (norm u)(norm v))) len)])))


;===================================
;; swash API


;; take a vector of velocity profiles and return a similar structure
;; with the t coordinates adjusted so the result gives a new valid profile
(defn velocity-profile [trace]
  (if (coll? (first (first trace)))
    (map (fn[c](map #(velocity (last (first (first trace))) %1 %2) c (rest c))) trace)
    (let [t0 (last (first trace))]
      (map #(velocity t0 %1 %2) trace (rest trace)))))


;; take a vector of curvature profiles and return a similar structure
;; with the t coordinates adjusted so the result gives a new valid profile
(defn curvature-profile [trace]
  (if (coll? (first (first trace)))
    (map (fn[c](map #(curvature (last (first (first trace))) %1 %2 %3) c (rest c) (rest (rest c)))) trace)
    (let [t0 (last (first trace))]
       (map #(curvature t0 %1 %2 %3) trace (rest trace) (rest (rest trace))))))


;; return the initial triplet of the (already normalized) trace segment, velocity, and curvture profiles
(defn profiles [trseg]
  [trseg
   (velocity-profile trseg)
   (curvature-profile trseg)])

;; scale the profiles of trace segments so they fit into a box of 100 x 100 units
(defn scale-profiles [trseg]
  (let [tvc (profiles trseg)
        tr (first tvc)
        vp (scale (second tvc) 100 100)
        cp (scale (last tvc) 100 100)]
    [tr vp cp]))

;; merge the profiles of two corresponding trace segments. The result is a list of three dimensional vectors.
;; The first coordinate is t, the two others are either nil or the y value of the respective profile, where
;; the vectors originating from the first trace have the form [t y nil] and from the second trace look like [t nil y].
;; Then the collection is sorted by t. Since the t coordinates of the profiles of two traces don't match.
(defn merge-profiles [pf1 pf2]
  (let [col1 (map (fn[[t y]] [t y nil]) pf1)
        col2 (map (fn[[t y]] [t nil y]) pf2)]
        (sort-by first (concat col1 col2))))

(defn difference [[t0 a0 b0][t1 a1 b1][t2 a2 b2]]
  (if (= t0 t1)
    (if (not= nil a0 b1)
      (abs (- a0 b1))
      (abs (- b0 a1)))
    (let [[a b c]  (if (not= nil a0 b1 a2)
                      [a0 b1 a2]
                      [b0 a1 b2])
          dt1 (- t1 t0)
          dt2 (- t2 t0)]
      (abs (float (+ (- a b) (* (divide dt1 dt2) (- c a))))))))

(defn prune-merged-profiles [mpl]
  (filter (comp not nil?)
          (map (fn [[t1 a1 b1][t2 a2 b2]]
                 (if (or (= a1 a2 nil)(= b1 b2 nil))
                   nil
                   [t1 a1 b1]))
               mpl (rest mpl))))

;; input list format (merged profiles list): (list [t (or nil y)(or y nil)])
;; We remove vectors from the list so vectors originating from the two original traces alternate like this:
;; (list [t0 y0 nil][t1 nil y1][t2 y2 nil][t3 nil y3] ...)
(defn compare-profiles [mpl]
  (let [coll (prune-merged-profiles mpl)
        triplist (map difference coll (rest coll) (nthrest coll 2))
        reslist (filter (comp not nil?) triplist)]
    (divide (reduce + reslist) (count reslist))))


;; do the job trace-segment-wise
(defn compare-trace-segments [trseg1 trseg2]
  (let [[tr1 vp1 cp1] (scale-profiles trseg1)
        [tr2 vp2 cp2] (scale-profiles trseg2)]
    [(compare-profiles (merge-profiles vp1 vp2))
     (compare-profiles (merge-profiles cp1 cp2))]))


;; normalize the traces, get velocity and curvature profiles, scale them partitionwise so they can be compared
;; compare two traces by combining their coordinates
(defn compare-traces [trace1 trace2]
  (let [col1 (normalize trace1)
        col2 (normalize trace2)]
    (if (or (empty? col1)(empty? col2))
      0
      (map compare-trace-segments col1 col2))))



(defn define [name trace]
  )

(defn define [name & names]
  )

(defn analyze [trace]
  )
