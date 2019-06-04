(ns tetris.ui
  (:require [reagent.core :as r]
            [tetris.ws :as ws]
            [tetris.state :refer [state]]))

(defn lobby []
  [:div#lobby
   [:div#lobby-background]
   [:h1 "Tetris Together"]
   [:div
    [:input {:type "text"
             :placeholder "Enter game code..."
             :value @(r/cursor state [:room-id-input])
             :on-change #(swap! state assoc :room-id-input (-> % .-target .-value))}]
    [:button {:on-click #(do
                           (ws/emit-message {:action "join"
                                             :roomId (:room-id-input @state)})
                           (swap! state assoc :is-host? false)
                           (swap! state assoc :game-state :ready))}
     "Join"]]
   [:hr]
   [:div
    [:button {:on-click #(do
                           (ws/emit-message {:action "host"})
                           (swap! state assoc :is-host? true)
                           (swap! state assoc :game-state :ready))}
     "Host a Game"]]])

(defn info []
  [:div#info
     (when @(r/cursor state [:room-id])
       [:div
        [:h2 (str "Game Code: " @(r/cursor state [:room-id]))]
        [:h4 (str "Player count: " @(r/cursor state [:num-players]))]
        [:h4 (str "Score: " @(r/cursor state [:rows-cleared]))]])])

(defn await-start []
  [:div#lobby
   [:div#lobby-background]
   [:h1 "Tetris Together"]
   [:div
    [:h2 {:style {:color "white"
                  :margin "4px"}}
     (str "Game Code: " @(r/cursor state [:room-id]))]
    [:div {:style {:color "black"}}
     (str "Players: " @(r/cursor state [:num-players]))]
    (if @(r/cursor state [:is-host?])
      [:div
       [:button {:on-click #(do
                              (swap! state assoc :game-state :playing)
                              (ws/broadcast (ws/get-game-state)))}
        "Start!"]]
      [:div "Waiting for host to start"])]])

(defn root []
  (let [game-state @(r/cursor state [:game-state])]
    [:div
     [info]
     (case game-state
       :lobby [lobby]
       :ready [await-start]
       [:div])]))

(defn init! []
  (r/render [root] (.getElementById js/document "ui-root")))

(defn ^:dev/after-load reload []
  (r/render [root] (.getElementById js/document "ui-root")))

