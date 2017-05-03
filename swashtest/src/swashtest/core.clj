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


;;======================
;; drawing primitives

(defn height []
  (let [h (q/height)]
    (if (zero? h) 800 h)))

(defn width []
  (let [w (q/width)]
    (if (zero? w) 800 w)))

(defn draw-text [x y s]
  (q/text s x y))

(defn draw-line [p q]
  (q/line (first p)(second p)(first q)(second q)))

(defn draw-circle [p r]
  (q/ellipse (first p)(second p) r r))

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

(defn draw-lights [button colour]
  (if (nil? colour)
    nil
    (do
      (if (= colour :green)
        (q/fill 0 250 0)
        (q/fill 250 0 0))
      (draw-circle [(:x button) (:y button)] (:r button))
      (q/fill 0 0 0))))

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

(defn process-segments [tracecoll]
  (let [t0 (last (first (first tracecoll)))
        coll (filter (comp not empty?) (map (partial swash/cleanup t0) tracecoll))  ;; filter: there may be empty traces that lead tocrashes!
        w (q/width)]
    (if (not (empty? coll))
      (let [vpraw (swash/smooth (swash/velocity-profile (apply concat coll)))
            vp (swash/scale vpraw (- w 100) 50)
            cp (swash/scale (swash/smooth (swash/curvature-profile (apply concat coll))) (- w 100) 50)
            x-bars (scale-x-axis 700 (map (comp last last) coll))]
        [vp cp x-bars])
      [nil nil nil])))

(defn process [state]
  (if (or (empty? (:tracecoll state)) (empty? (filter (comp not empty?) (:tracecoll state))))
    state
    (let [retcoll (process-segments (:tracecoll state))
          vp (first retcoll)
          cp (second retcoll)
          x-bars (last retcoll)]
      (if (= vp cp nil)
        (assoc state :trace '() :tracecoll nil :velocity-profile nil :curvature-profile nil :vert nil :analyzed false :shapes nil)
        (assoc state
          :trace '()
          :tracecoll nil
          :velocity-profile vp
          :curvature-profile cp
          :vert x-bars
          :analyzed true
          :shapes (conj (:shapes state) (:tracecoll state)))))))


(defn analyze-shapes [state tracecoll]
  (if (empty? (:shapes state))
      state
      (let [;; compare the two last traces
            shapes (reverse (:shapes state))
            tr-col1 (first shapes)
            tr-col2 (second shapes)
;            tr-col1 (first (:shapes state))
;            tr-col2 (second (:shapes state))

            ;; clean traces from all quasi doubles (i.s. successive points with too small a t-difference)
            res1 (map (partial swash/cleanup (last (first (first tr-col1)))) tr-col1)
            res2 (map (partial swash/cleanup (last (first (first tr-col2)))) tr-col2)

            ;; 1. get velocity and curvature profiles
            ;; 2. filter to smoothen the profiles
            ;; 3. scale the profiles so they fit in the same box
            cp1 (swash/scale (swash/smooth (swash/curvature-profile (first res1))) 1 1)
            cp2 (swash/scale (swash/smooth (swash/curvature-profile (first res2))) 1 1)
            vp1 (swash/scale (swash/smooth (swash/velocity-profile (first res1))) 1 1)
            vp2 (swash/scale (swash/smooth (swash/velocity-profile (first res2))) 1 1)

            ;; 1. merge profiles so that there is a y-value for every t in either profile
            ;; 2. remove points in the merged profile so points from either original profile alternate
            mpl1 (swash/prune-merged-profiles (swash/merge-profiles vp1 vp2))
            mpl2 (swash/prune-merged-profiles (swash/merge-profiles cp1 cp2))
            coll1 (map swash/interpolate-merged-profiles mpl1 (rest mpl1) (rest (rest mpl1)))
            coll2 (map swash/interpolate-merged-profiles mpl2 (rest mpl2) (rest (rest mpl2)))]
    (if (and (< (swash/evaluate-profiles coll1) 0.08)
             (< (swash/evaluate-profiles coll2) 0.06))
      (assoc state :lights :green)
      (assoc state :lights :red)))))


(defn beautify [x]
  (if (nil? x)
    nil
    (float (/ (int (* x 100)) 100))))


(defn testo [state]
  (if (empty? (:shapes state))
      state
      (let [;; compare the two first traces
            shapes (reverse (:shapes state))
            tr-col1 (first shapes)
            tr-col2 (second shapes)

            ;; clean traces from all quasi doubles (i.s. successive points with too small t-differences)
            res1 (map (partial swash/cleanup (last (first (first tr-col1)))) tr-col1)
            res2 (map (partial swash/cleanup (last (first (first tr-col2)))) tr-col2)

            ;; 1. get velocity and curvature profiles
            ;; 2. filter to smoothen the profiles
            ;; 3. scale the profiles so they fit in the same box
            cp1 (swash/scale (swash/smooth (swash/curvature-profile (first res1))) 100 100)
            cp2 (swash/scale (swash/smooth (swash/curvature-profile (first res2))) 100 100)
            vp1 (swash/scale (swash/smooth (swash/velocity-profile (first res1))) 100 100)
            vp2 (swash/scale (swash/smooth (swash/velocity-profile (first res2))) 100 100)

            ;; extract the extremal points of the profiles
            vp-ext-1 (filter (comp not nil?) (swash/extrema-profile vp1))
            vp-ext-2 (filter (comp not nil?) (swash/extrema-profile vp2))

            ;; try to match profiles by horizontal translation (here: only velocity profiles)
            maxvalvp1 (reduce max (map second vp-ext-1))
            maxvalvp2 (reduce max (map second vp-ext-2))
            c1 (filter (comp (partial = maxvalvp1) second) vp-ext-1)
            c2 (filter (comp (partial = maxvalvp2) second) vp-ext-2)
            dt (- (first (first c2))(first (first c1)))

            ;; (1. adjust profiles horizontally)
            ;; 2. merge profiles so that there is a y-value for every t in either profile
            ;; 3. remove points in the merged profile so points from either original profile alternate
;;            mpl1 (swash/prune-merged-profiles (swash/merge-profiles (map (fn [[t y]] (vector (+ t dt) y)) vp1) vp2))
            mpl1 (swash/prune-merged-profiles (swash/merge-profiles vp1 vp2))
            mpl2 (swash/prune-merged-profiles (swash/merge-profiles cp1 cp2))
;            coll1 (map swash/interpolate-merged-profiles mpl1 (rest mpl1) (rest (rest mpl1)))
;            coll2 (map swash/interpolate-merged-profiles mpl2 (rest mpl2) (rest (rest mpl2)))
            coll1 (map (fn[[t y1 y2]] (vector t (* y1 0.01) (* y2 0.01))) (map swash/interpolate-merged-profiles mpl1 (rest mpl1) (rest (rest mpl1))))
            coll2 (map (fn[[t y1 y2]] (vector t (* y1 0.01) (* y2 0.01))) (map swash/interpolate-merged-profiles mpl2 (rest mpl2) (rest (rest mpl2))))
            vm (swash/evaluate-profiles coll1)
            cm (swash/evaluate-profiles coll2)
aa1 (prn "COLL1a: " (map first (map #(vec (map beautify (rest %))) coll1)))
aa2 (prn "COLL1b: " (map second (map #(vec (map beautify (rest %))) coll1)))
bb1 (prn "COLL2a: " (map first (map #(vec (map beautify (rest %))) coll2)))
bb2 (prn "COLL2b: " (map second (map #(vec (map beautify (rest %))) coll2)))
cc1 (prn "    " vm)
cc2 (prn "    " cm)
cc3 (prn (if (and (< vm 0.08)(< 0.06)) "        =============== TRUE ===============" "        =============== FALSE ==============="))
;;           res1 (map swash/squared-difference coll1)
;;           res2 (map swash/squared-difference coll2)
            ]
      (assoc state :tr1 (map (fn [[t y]] (vector (+ (* t 2) 200) (- 700 y))) (swash/extract first mpl1))
                   :tr2 (map (fn [[t y]] (vector (+ (* t 2) 200) (- 700 y))) (swash/extract second mpl1))
                   :tr3 (map (fn [[t y]] (vector (+ (* t 2) 500) (- 700 y))) (swash/extract first mpl2))
                   :tr4 (map (fn [[t y]] (vector (+ (* t 2) 500) (- 700 y))) (swash/extract second mpl2))))))

(defn load-trace [state]
  )

(defn save-trace [state]
  (let [s (format "%d.trc" (System/currentTimeMillis))]
(prn "S:" s " TRACE: " (:tracecoll state))
    (when (not (empty? (:tracecoll state)))
      (spit s (:tracecoll state)))
    state))


;;========================
;; GUI elements (buttons)

(defn bLoad []
  (let [h (height)
        w (/ (width) 2)]
    { :x1 (- w 260) :x2 (- w 190) :y1 (- h 30) :y2 (- h 10) :s "   Load " }))

(defn bReset []
  (let [h (height)
        w (/ (width) 2)]
    { :x1 (- w 170) :x2 (- w 100) :y1 (- h 30) :y2 (- h 10) :s "  Reset " }))

(defn bSet []
  (let [h (height)
        w (/ (width) 2)]
    { :x1 (- w 80) :x2 (- w 10) :y1 (- h 30) :y2 (- h 10) :s "    Set" }))

(defn bAnalyze []
  (let [h (height)
        w (/ (width) 2)]
    { :x1 (+ w 10) :x2 (+ w 80) :y1 (- h 30) :y2 (- h 10) :s "Analyze" }))

(defn bTest []
  (let [h (height)
        w (/ (width) 2)]
    { :x1 (+ w 100) :x2 (+ w 170) :y1 (- h 30) :y2 (- h 10) :s "   Test " }))

(defn bSave []
  (let [h (height)
        w (/ (width) 2)]
    { :x1 (+ w 190) :x2 (+ w 260) :y1 (- h 30) :y2 (- h 10) :s "   Save " }))

(defn bLights []
  (let [h (height)
        w (/ (width) 2)]
    { :x (+ w 320) :y (- h 20) :r 20 }))



;;========================
;; Quil framework

(defn inbox [x y button]
  (and (> x (:x1 button))(< x (:x2 button))(> y (:y1 button))(< y (:y2 button))))

(defn setup []
  (let [w (width)]
    (q/clear)
    (q/frame-rate 60)
    (q/background 240)
    (q/fill 0 0 0)
    (draw-button (bLoad))
    (draw-button (bReset))
    (draw-button (bSet))
    (draw-button (bAnalyze))
    (draw-button (bTest))
    (draw-button (bSave))
    (draw-coordinates "writing speed (t)" 50 100 (- w 100) 50)
    (draw-coordinates "curvature (t)" 50 200 (- w 100) 50)
    (q/stroke 0 0 0)
    { :trace '() :shapes [] :tracecoll nil :velocity-profile [] :curvature-profile [] :colour 1 :analyzed false }))

(defn setup-state []
  (setup)
  (setup))

(defn update-state [state]
  state)

(defn draw-state [state]
  (if (:analyzed state)
    (let [h (height)
          w (width)]
      (draw-in-coordinates (:velocity-profile state) (/ w 16) (/ h 8) (* w 0.7) (/ h 16))
      (draw-in-coordinates (:curvature-profile state) (/ w 16) (/ h 4) (* w 0.7) (/ h 16))
      (draw-vertical-bars (:vert state) (/ w 16) (/ h 8) (/ h 20))
      (draw-vertical-bars (:vert state) (/ w 16) (/ h 4) (/ h 20))
      (draw-trace (:trace state))
      (assoc state :analyzed false))
    (do
      (when-let [colour (:lights state)]
        (draw-lights (bLights) colour))
      (when-let [tr1 (:tr1 state)]
        (draw-trace tr1))
      (when-let [tr2 (:tr2 state)]
        (draw-trace tr2))
      (when-let [tr3 (:tr3 state)]
        (draw-trace tr3))
      (when-let [tr4 (:tr4 state)]
        (draw-trace tr4))
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
  (if (inbox (:x event)(:y event) (bLoad))
    (load-trace state)
    (if (inbox (:x event)(:y event) (bSet))
      (process (assoc state :tracecoll (reverse (map reverse (:tracecoll state)))))
      (if (inbox (:x event)(:y event) (bReset))
         (setup-state)
         (if (inbox (:x event)(:y event) (bAnalyze))
           (analyze-shapes state (reverse (map reverse (:tracecoll state))))
           (if (inbox (:x event)(:y event) (bTest))
             (testo state)
             (if (inbox (:x event)(:y event) (bSave))
               (save-trace state)
               (assoc state :trace '() :tracecoll (filter (comp not empty?) (cons (:trace state) (:tracecoll state)))))))))))

(defn -main [& args]
  (let [dx 800
        dy 800]
    (q/defsketch swashtest
      :title "testing swash"
      :features [:resizable]
      :size [dx dy]
      :setup setup-state
      :update update-state
      :draw draw-state
      :mouse-dragged mouse-dragged
      :mouse-pressed mouse-pressed
      :mouse-released mouse-released
      :middleware [m/fun-mode])))
