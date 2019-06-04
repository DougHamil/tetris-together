(ns tetris.app
  (:require [threeagent.alpha.core :as th]
            [cljs.core.async :refer [chan put! >! <!]]
            [tetris.state :refer [state]]
            [tetris.tetromino :as tetromino]
            [tetris.matrix :as matrix]
            [tetris.text :as text]
            [tetris.ui :as ui]
            [tetris.ws :as ws]
            ["three" :as three])
  (:require-macros [cljs.core.async :refer [go]]))

(defn- render-active-tetromino []
  [:object
   [tetromino/render @(th/cursor state [:active-tetromino])]])
   
(defn- score []
  [:object 
   (when-let [font @(th/cursor state [:font])]
    (let [rows-cleared @(th/cursor state [:rows-cleared])]
     (when (= :game-over @(th/cursor state [:game-state]))
       [:object
        [:plane {:scale [100 100 1]
                 :position [0 0 -5]
                 :material {:color 0x00000
                            :transparent true
                            :opacity 0.8}}]
        [:text {:font font
                :height 1
                :position [-10 0 -4]
                :size 2
                :text (str "Game Over: " rows-cleared)}]])))])

(defn- camera []
  [:instance {:object matrix/camera}])

(defn- game []
  (let [width (matrix/calc-width @(th/cursor state [:num-players]))]
    [:object {:scale [1.0 1.0 1.0]
              :position [(/ width -2.0)
                         -10
                         -6]}
     [render-active-tetromino]
     [tetromino/render-ghosts]
     [matrix/render]]))

(defn- root []
  [:object
   [:hemisphere-light {:position [0 5 -10]
                       :sky-color 0xCCDDFF
                       :intensity 0.9}]
   [:directional-light {:position [0 0 10]
                        :intensity 0.1}]
   [camera]
   [score]
   [game]])

(defn- tick! [delta-time]
  (when (= :playing (:game-state @state))
    (swap! state update :time + delta-time)
    (tetromino/tick! delta-time)))

(defn- setup-scene [ctx]
  (set! (.-camera ctx) matrix/camera)
  (swap! state assoc :camera (.-camera ctx))
  (swap! state assoc :context ctx)
  (let [renderer (.-renderer ctx)]
    (set! (.-enabled (.-shadowMap renderer)) true)
    (.setClearColor renderer 0xCCCCFF))
  (set! (.-far (.-camera ctx)) 200.0)
  (set! (.-near (.-camera ctx)) 1)
  (.updateProjectionMatrix (.-camera ctx))
  (matrix/init!))

(defn- resize-canvas []
  (let [canvas (.-domRoot (:context @state))
        renderer (.-renderer (:context @state))
        width (.-innerWidth js/window)
        height (.-innerHeight js/window)]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (.setSize renderer width height)
    (matrix/update-camera-bounds!)))

(defn init []
  (go (do
        (<! (text/init!))
        (setup-scene
         (th/render root
                    (.getElementById js/document "root")
                    {:on-before-render tick!}))))
  (ui/init!)
  (tetromino/init!)
  (ws/connect! "wss://gq5gj22v30.execute-api.us-west-2.amazonaws.com/test")
  (.addEventListener js/window "resize" resize-canvas false))

(defn ^:dev/after-load reload []
  (println "Reload")
  (setup-scene
   (th/render root
              (.getElementById js/document "root")
              {:on-before-render tick!})))
