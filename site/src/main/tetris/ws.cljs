(ns tetris.ws
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as a :refer [<! >!]]
            [haslett.client :as ws]
            [haslett.format :as fmt]
            [tetris.state :refer [state]]))

(defn get-game-state []
  {:action "matrix-update"
   :game-state (:game-state @state)
   :rows-cleared (:rows-cleared @state)
   :num-players (:num-players @state)
   :matrix (:matrix @state)})

(defn emit-message [m]
  (go (>! (:ws-sink @state) (js/JSON.stringify (clj->js m)))))

(defn broadcast [message]
  (when-let [room-id (:room-id @state)]
    (emit-message {:action "broadcast"
                   :roomId room-id
                   :payload (js/JSON.stringify (clj->js message))})))

(defn message-player [conn-id message]
  (when-let [room-id (:room-id @state)]
    (emit-message {:action "message"
                   :roomId room-id
                   :connectionId conn-id
                   :payload (js/JSON.stringify (clj->js message))})))

(defn message-host [message]
  (when-let [room-id (:room-id @state)]
    (emit-message {:action "messageHost"
                   :roomId room-id
                   :payload (js/JSON.stringify (clj->js message))})))

(defmulti handle-payload (fn [source payload] (:action payload)))


(defmethod handle-payload :default [source payload]
  (println "Unsupported action")
  (println payload))

(defn on-message [m]
  (let [message (-> m js/JSON.parse (js->clj :keywordize-keys true))]
    (when (:roomId message)
      (swap! state assoc :room-id (:roomId message))
      (broadcast {:action "player-joined"}))
    (when-let [payload (some-> (:payload message)
                               (js/JSON.parse)
                               (js->clj :keywordize-keys true))]
      (handle-payload (:source message) payload))))

(defn connect! [url]
  (println "Connecting to " url)
  (go (let [stream (<! (ws/connect url))]
        (swap! state assoc :ws-sink (:sink stream))
        (go-loop []
          (let [m (<! (:source stream))]
            (on-message m)
            (when m
              (recur)))))))

(defn do-test []
  (message-host {:test "Hi"}))

(comment
  (emit-message {:action "host"})
  (emit-message {:action "join"
                 :roomId "IWTP"})
  (broadcast "Test"))
