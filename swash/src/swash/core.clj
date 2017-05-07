(ns swash.core)

(defn sqrt[x] (Math/sqrt x))
;;(defn acos[x] (Math/acos x))
(defn abs[x] (Math/abs (float x)))

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

(defn sign [x]
  (if (zero? x)
    0.0
    (if (pos? x)
      1.0
     -1.0)))

;===================================

(declare mindist)

(defn minmax [[a b] x]
  [(min a x) (max b x)])


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
          xn (first (last curve))
          [y0 yn] (reduce minmax [0.0 0.0] (map second curve))]
      (scale curve x-units y-units [x0 xn] [y0 yn]))))


;===================================

(defn velocity [[x1 y1 t1][x2 y2 t2]]
  [t1
   (/ (Math/sqrt (+ (square (- x2 x1))(square (- y2 y1))))
      (- t2 t1))])

;; signed curvature. The sign is positive for curves bending in mathematical order
(defn curvature [[x1 y1 t1][x2 y2 t2][x3 y3 t3]]
  (let [u [(- x2 x1)(- y2 y1)]
        v [(- x3 x1)(- y3 y1)]]
    [t2 (* -1.0 (- (* (first u)(second v))(* (second u)(first v))))]))


;===================================

(defn locmin-step [y1 y2 y3]
  (if (and (< y2 y1)(< y2 y3))
    y2
    nil))

(defn locmin [coll]
  (let [p-col1 (cons (+ (first coll) 1) coll)
        p-col (reverse (cons (+ (last coll) 1) (reverse p-col1)))]
    (map locmin-step p-col (rest p-col) (rest (rest p-col)))))

(defn locmax-step [y1 y2 y3]
  (if (and (> y2 y1)(> y2 y3))
    y2
    nil))

(defn locmax [coll]
  (let [p-col1 (reverse (cons (- (last coll) 1) (reverse coll)))
        p-col (cons (- (first p-col1) 1) p-col1)]
    (map locmax-step p-col (rest p-col) (rest (rest p-col)))))

(defn mrge [coll1 coll2]
  (map #(if (not (nil? %1)) %1 (if (not (nil? %2)) %2 nil))
       coll1
       coll2))

(defn locext [coll]
  (let [p-col (map second coll)]
    (mrge (locmin p-col) (locmax p-col))))


(defn smooth [coll]
  (map (fn [[t1 y1][t2 y2]] [(* (+ t2 t1) 0.5)(* (+ y2 y1) 0.5)]) coll (rest coll)))

(defn extrema-profile [curve]
  (map #(if (nil? %2) nil (vector (first %1) %2)) curve (locext curve)))

;===================================
;; swash API


(def TMIN 3)
(def DMIN 0)

(defn- mindist [[[x1 y1 t1][x2 y2 t2]]]
  (>= (abs (- t2 t1)) TMIN))


;; transform the trace so the t coordinates start with 0,
;; no two trace elements have the same timestamp (guarantee injectivity) and
;; the trace has at least three points (minimum for curvature)
(defn cleanup [t0 coll]
  (let [col1 (map (fn [[x y t]](vector x y (- t t0))) coll)
        col2 (map first (filter mindist (map list col1 (rest col1))))]
    (if (> (count col2) 2) col2 nil)))

;; find the center point of the box around the trace
(defn box-center [trace]
  (let [[t0 tn] (reduce minmax [0.0 0.0] (map first trace))
        [y0 yn] (reduce minmax [0.0 0.0] (map second trace))]
    (vector (/ (- tn t0) 2)(/ (- yn y0) 2))))

;; take a vector of velocity profiles and return a similar structure
;; with the t coordinates adjusted so the result gives a new valid profile
(defn velocity-profile [trace]
  (map #(velocity %1 %2) trace (rest trace)))

(defn curvature-profile [trace]
  (let [p0 (cons 0 (box-center trace))
        ret (map #(curvature p0 %1 %2) trace (rest trace))
        t0 (first (first ret))]
    (map (fn[[t y]](vector (- t t0) y)) ret)))


;; merge the profiles of two corresponding trace segments. The result is a list of three dimensional vectors.
;; The first coordinate is t, the two others are either nil or the y value of the respective profile, where
;; the vectors originating from the first trace have the form [t y nil] and from the second trace look like [t nil y].
;; Then the collection is sorted by t. Since the t coordinates of the profiles of two traces don't match.
(defn merge-profiles [pf1 pf2]
  (let [col1 (map (fn[[t y]] [t y nil]) pf1)
        col2 (map (fn[[t y]] [t nil y]) pf2)]
        (sort-by first (concat col1 col2))))

;; extract the first or second profile from the merged profiles
(defn extract [f mpl]
  (let [coll (map f (partition 2 mpl))]
    (map #(filter (comp not nil?) %) coll)))


;; return the [t1 a1 b1] with its' nil value replaced by the linear interpolation of the
;; respective values of the next door neighbours
(defn interpolate-merged-profiles [[t0 a0 b0][t1 a1 b1][t2 a2 b2]]
  (let [d (/ (- t1 t0)(- t2 t0))]
    (if (nil? b1)
      [t1 a1 (float (+ b0 (* (- b2 b0) d)))]
      [t1 (float (+ a0 (* (- a2 a0) d))) b1])))


;; return the squared difference of the arguments' y-values.
;; argument types p, q: [t y1 y2]
;; return: the squared diff of y1 y2
(defn squared-difference [[t y1 y2]]
  (square (- y2 y1)))

;; prune the list so the result list looks like: '([t1 y1 nil][t2 nil y2][t3 y3 nil][t4 nil y4] ...)
(defn prune-merged-profiles [mpl]
  (filter (comp not nil?)
          (map (fn [[t1 a1 b1][t2 a2 b2]]
                 (if (or (= a1 a2 nil)(= b1 b2 nil))
                   nil
                   [t1 a1 b1]))
               mpl (rest mpl))))


;; return a cumulated similarity for the profiles given by coll
;; which is a list of [t y1 y2] where t is the horizontal time coordinate
;; and y1, y2 the corresponding y-values for the profiles to be compared
(defn evaluate-profiles [mpl]
  (/ (reduce + (map squared-difference mpl)) (count mpl)))


(defn analyze-shapes [tr-col1 tr-col2]
  (let [
        ;; clean traces from all quasi doubles (i.s. successive points with too small a t-difference)
        res1 (map (partial cleanup (last (first (first tr-col1)))) tr-col1)
        res2 (map (partial cleanup (last (first (first tr-col2)))) tr-col2)

        ;; 1. get velocity and curvature profiles
        ;; 2. filter to smoothen the profiles
        ;; 3. scale the profiles so they fit in the same box
        cp1 (scale (smooth (curvature-profile (first res1))) 1 1)
        cp2 (scale (smooth (curvature-profile (first res2))) 1 1)
        vp1 (scale (smooth (velocity-profile (first res1))) 1 1)
        vp2 (scale (smooth (velocity-profile (first res2))) 1 1)

        ;; 1. merge profiles so that there is a y-value for every t in either profile
        ;; 2. remove points in the merged profile so points from either original profile alternate
        mpl1 (prune-merged-profiles (merge-profiles vp1 vp2))
        mpl2 (prune-merged-profiles (merge-profiles cp1 cp2))
        coll1 (map interpolate-merged-profiles mpl1 (rest mpl1) (rest (rest mpl1)))
        coll2 (map interpolate-merged-profiles mpl2 (rest mpl2) (rest (rest mpl2)))]
    (vector (evaluate-profiles coll1) (evaluate-profiles coll2))))

