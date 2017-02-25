(ns swashtest.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [swash.core :as swash]))


(defn abs[x] (Math/abs x))

(defn minmax [[a b] x]
  [(if (< x a) x a) (if (> x b) x b)])

(defn scale [curve x-units y-units]
  ;; scaling a profile
  (let [dx (/ x-units (- (first (last curve))(first (first curve))))
        [y0 yn] (reduce minmax [0.0 0.0] (map second curve))
        dy (/ y-units (- yn y0))]
    (map (fn [[x y]][(* x dx)(* y dy)]) curve)))

(defn mindist [[[x1 y1 t1][x2 y2 t2]]]
  (>= (abs (- t2 t1)) 3))

(defn cleanup [coll]
  (let [t0 (last (first coll))
        col (map (fn [[x y t]][x y (- t t0)]) coll)]
    (map first (filter mindist (map list col (rest col))))))

;;===================


(defn setup-state []
  (q/frame-rate 30)
  (q/background 240)
  (q/fill 255 255 255)
  { :trace '() })

(defn update-state [state]
  state)

(defn draw-line [p q]
  (q/line (first p)(second p)(first q)(second q)))

(defn draw-trace [points]
  (doseq [[p q] (map list points (rest points))]
    (draw-line p q)))

(defn draw-in-coordinates [trace x0 y0 dx dy]
  (draw-line [x0 y0][(+ x0 dx) y0])
  (draw-line [x0 y0][x0 (- y0 dy)])
  (draw-trace (map (fn[[x y]] [(+ x0 x)(- y0 y)]) trace)))

(defn draw-state [state]
  ;; draw coordinate system for velocity profile
  (q/fill 50 50 50)
  (draw-in-coordinates (:velocity-profile state) 50 100 700 100)
  (draw-in-coordinates (:curvature-profile state) 50 200 700 100)

  (q/fill 255 255 255)
  (draw-trace (:trace state))
  state)

(defn mouse-dragged [state event]
  (let [t (System/currentTimeMillis)]
    (update state :trace (partial cons [(:x event)(:y event) t]))))

(defn mouse-released [state event]
  (let [coll (cleanup (reverse (:trace state)))]
    (if (> (count coll) 3)
      (let [vp (scale (swash/velocity-profile coll) 700 50)
            cp (scale (swash/curvature-profile coll) 700 50)]
        (assoc state :trace '() :velocity-profile vp :curvature-profile cp))
    (assoc state :trace '()))))


(q/defsketch swashtest
  :title "testing swash"
  :size [800 800]
  :setup setup-state
  :update update-state
  :draw draw-state
  :mouse-dragged mouse-dragged
  :mouse-released mouse-released
  :features [:keep-on-top]
  :middleware [m/fun-mode])


(defn -main [& args]
  )
