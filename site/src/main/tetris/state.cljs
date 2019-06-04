(ns tetris.state
  (:require [threeagent.alpha.core :as th]
            [reagent.core :as r]))

(defonce state (th/atom {:game-state :lobby
                         :room-id nil
                         :is-host? false
                         :num-players 1
                         :time 0
                         :matrix []
                         :matrix-size 1
                         :active-tetromino nil}))

