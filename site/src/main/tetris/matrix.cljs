(ns tetris.matrix
  (:require [tetris.state :refer [state]]
            [tetris.ws :as ws]
            ["three" :as three]
            [threeagent.alpha.core :as th]))

(defonce camera (three/OrthographicCamera. -10 10 20 -20 1 1000))

(def matrix-base-width 10)
(def matrix-base-height 20)
(def matrix-width-per-player 3)

(defn calc-width [num-players]
  (+ matrix-base-width (* matrix-width-per-player (dec num-players))))

(defn- render-matrix [m dim-x dim-y]
  [:object 
   [:plane {:scale [dim-x dim-y 1]
            :position [(/ dim-x 2.0)
                       (/ dim-y 2.0)
                       -8]
            :rotation [0 0 0]
            :material {:color 0x222255}}]
   [:plane {:scale [(inc dim-x) (inc dim-y) 1]
            :position [(/ dim-x 2.0)
                       (/ dim-y 2.0)
                       -9]
            :rotation [0 0 0]
            :material {:color 0xEEEEFF}}]
   (when-let [matrix m]
     (for [[x y c] matrix]
       [:plane {:position [(+ x 0.5)
                           (- 19.5 y)
                           0]
                :material {:color c}}]))])

(defn render []
  (let [matrix @(th/cursor state [:matrix])
        num-players @(th/cursor state [:num-players])]
    [:object
     [render-matrix matrix (calc-width num-players) 20]]))

(defn- remove-tetris [matrix matrix-width]
  (let [by-row (group-by second matrix)
        rows-to-remove (->> by-row
                            (filter (fn [[y cs]]
                                      (= (count cs) matrix-width)))
                            (map first)
                            (sort))
        new-matrix
        (reduce
         (fn [m y-coord]
           (->> m
                (remove #(= y-coord (second %)))
                (map (fn [[x y c]]
                       (if (> y-coord y)
                         [x (inc y) c]
                         [x y c])))))
         matrix
         rows-to-remove)]
    {:matrix new-matrix
     :rows-removed (count rows-to-remove)}))


(defn in-bounds? [[x y]]
  (and (>= x 0)
       (>= y 0)
       (< x (calc-width (:num-players @state)))
       (< y 20)))

(defn add-coords! [coords color]
  (let [matrix (:matrix @state)
        results (remove-tetris (concat matrix (map #(conj (vec %) color) coords))
                               (calc-width (:num-players @state)))]
    (swap! state assoc :matrix (:matrix results))
    (swap! state update :rows-cleared + (:rows-removed results))))

(defn- calc-aspect-ratio []
  (let [ctx (:context @state)
        canvas (.-domRoot ctx)
        w (.-width canvas)
        h (.-height canvas)]
    (/ h w)))
  
(defn update-camera-bounds! []
  (let [aspect-ratio (calc-aspect-ratio)
        desired-width (+ 2.5 (calc-width (:num-players @state)))
        desired-height (* desired-width aspect-ratio)
        height (max 22.5 desired-height)
        width (/ height aspect-ratio)
        half-width (/ width 2.0)
        half-height (/ height 2.0)]
    (set! (.-left camera) (- half-width))
    (set! (.-right camera) half-width)
    (set! (.-top camera) half-height)
    (set! (.-bottom camera) (- half-height))
    (.updateProjectionMatrix camera)))

(defn init! []
  (update-camera-bounds!))

(defmethod ws/handle-payload "place-tetromino" [source payload]
  (add-coords! (:tetromino payload) (:color payload))
  (ws/broadcast (ws/get-game-state)))

(defmethod ws/handle-payload "matrix-update" [source payload]
  (swap! state assoc :matrix (:matrix payload))
  (swap! state assoc :num-players (:num-players payload))
  (swap! state assoc :rows-cleared (:rows-cleared payload))
  (swap! state assoc :game-state (keyword (:game-state payload)))
  (update-camera-bounds!))

(defmethod ws/handle-payload "player-joined" [source payload]
  (when (:is-host? @state)
    (swap! state update :num-players inc)
    (ws/message-player source (ws/get-game-state))
    (update-camera-bounds!))) 
