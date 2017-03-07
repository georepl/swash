(ns swashtest.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [swash.core :as swash]))


(defn abs[x] (Math/abs x))

(def colours [[0 0 0]       ;black
              [0 250 0]     ;green
              [250 0 0]     ;red
              [255 200 0]   ;yellow
              [0 125 125]   ;turquoise
              [125 125 0]   ;olive
              [255 127 0]   ;orange
              [0 0 250]     ;blue
              [125 125 125] ;gray
             ])

(defn colour [mode]
  (let [idx (mod mode (count colours))]
    (apply q/stroke (nth colours idx))))

(defn mindist [[[x1 y1 t1][x2 y2 t2]]]
  (>= (abs (- t2 t1)) 3))

(defn cleanup [t0 coll]
  (let [col (map (fn [[x y t]][x y (- t t0)]) coll)]
    (map first (filter mindist (map list col (rest col))))))


;;======================
;; drawing primitives

(defn draw-text [x y s]
  (q/text s x y))

(defn draw-line [p q]
  (q/line (first p)(second p)(first q)(second q)))

(defn draw-trace [points]
  (doseq [[p q] (map list points (rest points))]
    (draw-line p q)))

(defn draw-button [button]
  (draw-line [(:x1 button) (:y1 button)][(:x2 button) (:y1 button)])
  (draw-line [(:x2 button) (:y1 button)][(:x2 button) (:y2 button)])
  (draw-line [(:x2 button) (:y2 button)][(:x1 button) (:y2 button)])
  (draw-line [(:x1 button) (:y2 button)][(:x1 button) (:y1 button)])
  (q/fill 200)
  (q/rect (:x1 button)(:y1 button)(- (:x2 button)(:x1 button)) (- (:y2 button)(:y1 button)))
  (q/fill 0 0 0)
  (draw-text (+ (:x1 button) 10) (+ (:y1 button) 15) (:s button)))

(defn draw-coordinates [caption x0 y0 dx dy]
  (draw-line [x0 y0][(+ x0 dx) y0])
  (draw-line [x0 y0][x0 (- y0 dy)])
  (draw-text (- x0 40) (- y0 dy 5) caption)
  (draw-text (+ x0 dx -10) (+ y0 10) "t"))

(defn draw-in-coordinates [trace x0 y0 dx dy]
  (when (not (empty? trace))
    (if (coll? (first (first trace)))
      (map #(draw-in-coordinates % x0 y0 dx dy) trace)
      (let [tr (map (fn[[x y]] [(+ x0 x)(- y0 y)]) trace)]
        (draw-trace tr)))))

(defn scale-x-axis [dx coll]
  (if (or (empty? coll) (zero? dx))
    (empty coll)
    (let [d (- (last coll)(first coll))]
      (if (zero? d)
        (empty coll)
        (let [fac (/ dx d)]
          (map (partial * fac) coll))))))

(defn draw-vertical-bars [x-coll x1 y1 dy]
  (if (or (empty? x-coll)(neg? (first x-coll)))
    nil
    (doseq [x x-coll]
      (draw-line [(+ x1 x) y1][(+ x1 x)(- y1 dy)]))))

(defn process-segment [tracecoll]
  (let [t0 (last (first (first tracecoll)))
        c1 (map (partial cleanup t0) tracecoll)
        coll (filter #(> (count %) 2) c1)]
    (if (not (empty? coll))
      (let [vp (swash/scale (swash/velocity-profile (apply concat coll)) 700 50)
            cp (swash/scale (swash/curvature-profile (apply concat coll)) 700 50)
            x-bars (scale-x-axis 700 (map (comp last last) coll))]
        [vp cp x-bars])
      [nil nil nil])))

(defn process [state]
  (let [retcoll (process-segment (:tracecoll state))
        vp (first retcoll)
        cp (second retcoll)
        x-bars (last retcoll)]
    (if (= vp cp nil)
      (assoc state :trace '() :tracecoll nil :velocity-profile nil :curvature-profile nil :vert nil :analyzed false)
      (assoc state :trace '() :tracecoll nil :velocity-profile vp :curvature-profile cp :vert x-bars :analyzed true))))

(def bReset { :x1 320 :x2 390 :y1 770 :y2 790 :s " Reset " })
(def bOk { :x1 410 :x2 480 :y1 770 :y2 790 :s "Analyze" })

;;======================
;; Quil framework

(defn inbox [x y button]
  (and (> x (:x1 button))(< x (:x2 button))(> y (:y1 button))(< y (:y2 button))))

(defn setup []
  (q/clear)
  (q/frame-rate 30)
  (q/background 240)
  (q/fill 0 0 0)
  (draw-button bOk)
  (draw-button bReset)
  (draw-coordinates "writing speed (t)" 50 100 700 50)
  (draw-coordinates "curvature (t)" 50 200 700 50)
  (q/stroke 0 0 0)
  { :trace '() :tracecoll nil :velocity-profile [] :curvature-profile [] :colour 1 :analyzed false })

(defn setup-state []
  (setup))

(defn update-state [state]
  state)

(defn draw-state [state]
  (if (:analyzed state)
    (do
      (draw-in-coordinates (:velocity-profile state) 50 100 700 50)
      (draw-in-coordinates (:curvature-profile state) 50 200 700 50)
      (draw-vertical-bars (:vert state) 50 100 40)
      (draw-vertical-bars (:vert state) 50 200 40)
      (draw-trace (:trace state))
      (assoc state :analyzed false))
    (do
      (draw-trace (:trace state))
      state)))

(defn mouse-dragged [state event]
  (let [t (System/currentTimeMillis)]
    (update state :trace (partial cons [(:x event)(:y event) t]))))

(defn mouse-pressed[state event]
  (if (:analyzed state)
    (let [newcol (inc (:colour state))]
      (colour newcol)
      (assoc state :colour newcol :analyzed false))
    state))


(defn mouse-released [state event]
  (if (inbox (:x event)(:y event) bOk)
    (process (assoc state :tracecoll (reverse (map reverse (:tracecoll state)))))
    (if (inbox (:x event)(:y event) bReset)
      (setup)
      (assoc state :trace '() :tracecoll (cons (:trace state) (:tracecoll state))))))

(q/defsketch swashtest
  :title "testing swash"
  :size [800 800]
  :setup setup-state
  :update update-state
  :draw draw-state
  :mouse-dragged mouse-dragged
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :middleware [m/fun-mode])


(defn -main [& args]
  )
