(ns tetris100.core
  (:require ["aws-sdk" :as AWS]
            [oops.core :refer [oget]]
            [kitchen-async.promise :as p]))

(defn- log [m]
  (.log js/console m))

(defn- rand-str [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))

(defn- get-dynamo []
  ^js (new (.-DynamoDB AWS) #js {:apiVersion "2012-08-10"}))

(defn- get-gateway-api [^js event]
  ^js (new (.-ApiGatewayManagementApi AWS) #js {:apiVersion "2018-11-29"
                                                :endpoint (str (oget event "requestContext.domainName")
                                                               "/"
                                                               (oget event "requestContext.stage"))}))

(defn- get-connection-id [^js event]
  (or (oget event "?requestContext.?connectionId")
      "unknown-connection-id"))

(defn- parse-body [^js event]
  (js/JSON.parse (.-body event)))

(defn- map->response [m]
  (clj->js {:statusCode 200
            :body (js/JSON.stringify (clj->js m))}))

(defn- post-to-connection [gateway payload source-conn-id conn-id]
  (let [params #js {:ConnectionId conn-id
                    :Data (js/JSON.stringify #js {:source source-conn-id
                                                  :payload payload})}
        request ^js (.postToConnection gateway params)]
    (p/promise [resolve reject]
               (-> (.promise request)
                   (p/then resolve)
                   (p/catch* (fn [err]
                               (println err)
                               (resolve nil)))))))

(defn- broadcast-to-players [^js gateway payload source-conn-id players]
  (let [promises (map (partial post-to-connection gateway payload source-conn-id) players)]
    (p/all promises)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Lambda Handlers
;;;;;;;;;;;;;;;;;;;;;;;
(defn host-room-handler [^js event ^js context callback]
  (let [ddb (get-dynamo) 
        room-id (rand-str 4)
        connection-id (get-connection-id event)
        params {:Item {:Room {:S room-id}
                       :Host {:S connection-id}
                       :Players {:SS [connection-id]}}
                :ReturnConsumedCapacity "TOTAL"
                :TableName "Tetris100"}]
    (.putItem ddb (clj->js params)
              (fn [err data]
                (callback err (map->response {:roomId room-id}))))))
  
(defn join-room-handler [^js event  ^js context callback]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        room-id (.-roomId body)
        ddb (get-dynamo)
        update-params {:Key {:Room {:S room-id}}
                       :TableName "Tetris100"
                       :UpdateExpression "ADD Players :p"
                       :ExpressionAttributeValues {":p"  {:SS [connection-id]}}}]
    (.updateItem ddb (clj->js update-params)
                 (fn [err data]
                   (callback err (map->response {:roomId room-id
                                                 :data data}))))))

(defn broadcast-handler [^js event ^js context callback]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        payload (.-payload body)
        room-id (.-roomId body)
        ddb (get-dynamo)
        gateway (get-gateway-api event)
        query-params {:Key {:Room {:S room-id}}
                      :TableName "Tetris100"}]
    (.getItem ddb (clj->js query-params)
              (fn [err ^js data]
                (let [players (set (oget data "Item.Players.SS"))]
                  (if (players connection-id)
                    (p/then (broadcast-to-players gateway payload connection-id (disj players connection-id))
                            (fn []
                              (callback nil #js {:statusCode 200})))
                    (callback nil #js {:statusCode 200})))))))

(defn message-handler [^js event ^js context callback]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        target-connection-id (.-connectionId body)
        payload (.-payload body)
        room-id (.-roomId body)
        ddb (get-dynamo)
        gateway (get-gateway-api event)
        query-params {:Key {:Room {:S room-id}}
                      :TableName "Tetris100"}]
    (.getItem ddb (clj->js query-params)
              (fn [err ^js data]
                (let [players (set (oget data "Item.Players.SS"))]
                  (if (players connection-id)
                    (p/then (broadcast-to-players gateway payload connection-id #{target-connection-id})
                            (fn []
                              (callback nil #js {:statusCode 200})))
                    (callback nil #js {:statusCode 200})))))))


(defn message-host-handler [^js event ^js context callback]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        payload (.-payload body)
        room-id (.-roomId body)
        ddb (get-dynamo)
        gateway (get-gateway-api event)
        query-params {:Key {:Room {:S room-id}}
                      :TableName "Tetris100"}]
    (.getItem ddb (clj->js query-params)
              (fn [err ^js data]
                (let [players (set (oget data "Item.Players.SS"))
                      host (oget data "Item.Host.S")]
                  (if (players connection-id)
                    (p/then (broadcast-to-players gateway payload connection-id #{host})
                            (fn [] (callback nil #js {:statusCode 200})))
                    (callback nil #js {:statusCode 200})))))))
