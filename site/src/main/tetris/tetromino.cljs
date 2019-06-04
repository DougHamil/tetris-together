(ns tetris.tetromino
  (:require [tetris.state :refer [state]]
            [tetris.ws :as ws]
            [tetris.matrix :as matrix]
            [threeagent.alpha.core :as th]))

(def ^:private tetromino-coords {:I [[0 1] [1 1] [2 1] [3 1]]
                                 :O [[1 0] [2 0] [1 1] [2 1]]
                                 :J [[0 0] [0 1] [1 1] [2 1]]
                                 :L [[0 1] [1 1] [2 1] [2 0]]
                                 :S [[0 1] [1 1] [1 0] [2 0]]
                                 :Z [[0 0] [1 0] [1 1] [2 1]]
                                 :T [[0 1] [1 1] [1 0] [2 1]]})

(def ^:private tetromino-colors {:I 0x88AAFF
                                 :O 0xEE88EE
                                 :J 0x8888EE
                                 :L 0xEEEE88
                                 :S 0x88EE88
                                 :Z 0x88FFEE
                                 :T 0xAAAAFF})

(def tetromino-types (keys tetromino-colors))

(defn- vec-mat [[[a11 a12] [a21 a22]] [x y]]
  [(+ (* x a11) (* y a21))
   (+ (* x a12) (* y a22))])

(defn- rotate-coords [type coords]
  (if (= :O type)
    coords
    (let [[ox oy] [1 1]]
      (map (fn [[cx cy]]
             (let [x (- cx ox)
                   y (- cy oy)
                   [rx ry] (vec-mat [[0 1] [-1 0]] [x y])]
               [(+ ox rx) (+ oy ry)]))
           coords))))

(defn- get-local-coords [type rotation]
  (->> (iterate (partial rotate-coords type)
                (tetromino-coords type))
       (take (inc rotation))
       last))

(defn- local->world-coords [pos coords]
  (map #(map + pos %) coords))

(defn- tetromino->world-coords [{:keys [type rotation position]}]
  (->> (get-local-coords type rotation)
       (local->world-coords position)))

(defn render-piece [type]
  (let [coords (get-local-coords type 0)]
    [:object
     (for [[x y] coords]
      [:plane {:position [x (- y) 1]
               :material {:color (tetromino-colors type)}}])]))

(defn render [t]
  [:object 
   (for [[x y] (tetromino->world-coords t)]
     [:plane {:position [(+ 0.5 x)
                         (- 19.5 y)
                         -1]
              :material {:color (tetromino-colors (:type t))}}])])

(defn render-ghosts []
  [:object
   (for [[k t] @(th/cursor state [:ghost-tetrominos])]
     ^{:key k}
     [:object
      (for [[x y] t]
        [:plane {:position [(+ x 0.5)
                            (- 19.5 y)
                            -1]
                 :material {:color "gray"
                            :transparent true
                            :opacity 0.2}}])])])

(defn- coords-overlap? [a b]
  (not (empty? (clojure.set/intersection (set a) (set b)))))

(defn valid? [matrix coords]
  (let [oob-coords (filter #(not (matrix/in-bounds? %))
                           coords)]
    (and (empty? oob-coords)
         (not (coords-overlap? coords (map (partial take 2) matrix))))))

(defn- move-coords [coords delta]
  (map #(map + delta %) coords))

(defn- next-tetromino []
  {:type (rand-nth (keys tetromino-coords))
   :position [(int (/ (matrix/calc-width (:num-players @state)) 2.0))
              0]
   :rotation 0})

(defn- try-spawn-tetromino! []
  (let [t (next-tetromino)
        coords (tetromino->world-coords t)]
    (if (valid? (:matrix @state) coords)
      (swap! state assoc :active-tetromino t)
      (do
        (swap! state assoc :game-state :game-over)
        (swap! state assoc :active-tetromino nil)))))

(defn- try-move-tetromino! [delta]
  (when (= :playing (:game-state @state))
    (when-let [t (:active-tetromino @state)]
      (let [new-t (assoc t :position (map + delta (:position t)))
            new-coords (tetromino->world-coords new-t)]
        (when (valid? (:matrix @state) new-coords)
          (swap! state assoc :active-tetromino new-t)
          (when (= 1 (second delta))
            (swap! state assoc :last-tick-time (:time @state))))))))

(defn- try-rotate-tetromino! [delta]
  (when-let [t (:active-tetromino @state)]
    (let [new-t (update t :rotation + delta)
          new-coords (tetromino->world-coords new-t)]
      (when (valid? (:matrix @state) new-coords)
        (swap! state assoc :active-tetromino new-t)))))

(defn- on-keydown [evt]
  (case (.-code evt)
    "ArrowRight" (do
                   (.preventDefault evt)
                   (try-move-tetromino! [1 0]))
    "ArrowLeft" (do
                   (.preventDefault evt)
                   (try-move-tetromino! [-1 0]))
    "ArrowUp"   (do
                  (.preventDefault evt)
                  (try-rotate-tetromino! 1))
    "ArrowDown" (do
                  (.preventDefault evt)
                  (try-move-tetromino! [0 1]))
    "Space" (do
              (.preventDefault evt)
              (loop []
                (if (try-move-tetromino! [0 1])
                  (recur))))
    nil))

(defn tick! [delta-time]
  (let [time (:time @state)
        time-since-last-tick (- time (:last-tick-time @state))
        t (:active-tetromino @state)
        [px py] (:position t)]
    (when (and (>= time-since-last-tick 0.4)
               (not= :game-over (:game-state @state)))
      (let [ticked-t (assoc t :position (map + [px py] [0 1]))
            ticked-coords (tetromino->world-coords ticked-t)
            matrix (:matrix @state)]
        (if (valid? matrix ticked-coords)
          ;; Moved down
          (do
            (ws/broadcast {:action "ghost-tetromino"
                           :tetromino (tetromino->world-coords t)})
            (swap! state assoc :active-tetromino ticked-t))
          ;; Hit Ground/Tetromino
          (do
            (if (:is-host? @state)
              (do
                (matrix/add-coords! (tetromino->world-coords t) (tetromino-colors (:type t)))
                (ws/broadcast (ws/get-game-state)))
              (do
                (matrix/add-coords! (tetromino->world-coords t) (tetromino-colors (:type t)))
                (ws/message-host {:action "place-tetromino"
                                  :color (tetromino-colors (:type t))
                                  :tetromino (tetromino->world-coords t)})))
            (try-spawn-tetromino!))))
      (swap! state assoc :last-tick-time time))))

(defn init! []
  (.addEventListener js/window "keydown" on-keydown)
  (swap! state assoc :active-tetromino (next-tetromino)))

(defmethod ws/handle-payload "ghost-tetromino" [source payload]
  (swap! state update :ghost-tetrominos assoc source (:tetromino payload)))
