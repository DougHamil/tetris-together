(ns tetris100.core
  (:require ["aws-sdk" :as AWS]
            [oops.core :refer [oget]]
            [promesa.core :as p]
            [cljs-lambda.macros :refer [deflambda]]))

(defn- log [m]
  (.log js/console m))

(defn- rand-str [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))

(defn- get-dynamo []
  ^js (new (.-DynamoDB AWS) #js {:apiVersion "2012-08-10"}))

(defn- get-gateway-api [event]
  (let [domain-name (get-in event [:requestContext :domainName])
        stage (get-in event [:requestContext :stage])]
    ^js (new (.-ApiGatewayManagementApi AWS)
             #js {:apiVersion "2018-11-29"
                  :endpoint (str domain-name "/" stage)})))

(defn- get-connection-id [event]
  (get-in event [:requestContext :connectionId] "unknown-connection-id"))

(defn- parse-body [^js event]
  (js/JSON.parse (:body event)))

(defn- map->response [m]
  {:statusCode 200
   :body (js/JSON.stringify (clj->js m))})

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
(deflambda host-room-handler [event context]
  (let [ddb (get-dynamo) 
        room-id (rand-str 4)
        connection-id (get-connection-id event)
        params {:Item {:Room {:S room-id}
                       :Host {:S connection-id}
                       :Players {:SS [connection-id]}}
                :ReturnConsumedCapacity "TOTAL"
                :TableName "Tetris100"}]
    (p/alet [data (p/await (.putItem ddb (clj->js params)))]
      (p/promise (map->response {:roomId room-id})))))
  
(deflambda join-room-handler [event context]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        room-id (.-roomId body)
        ddb (get-dynamo)
        update-params {:Key {:Room {:S room-id}}
                       :TableName "Tetris100"
                       :UpdateExpression "ADD Players :p"
                       :ExpressionAttributeValues {":p"  {:SS [connection-id]}}}]
    (p/let [data (.updateItem ddb (clj->js update-params))]
      (map->response {:roomId room-id
                      :data data}))))

(deflambda broadcast-handler [event context]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        payload (.-payload body)
        room-id (.-roomId body)
        ddb (get-dynamo)
        gateway (get-gateway-api event)
        query-params {:Key {:Room {:S room-id}}
                      :TableName "Tetris100"}]
    (p/alet [data (p/await (.getItem ddb (clj->js query-params)))
             players (set (oget data "Item.Players.SS"))]
      (if (players connection-id)
        (->>
          (broadcast-to-players gateway payload connection-id (disj players connection-id))
          (p/map (fn [] {:statusCode 200})))
        (p/promise {:statusCode 200})))))

(deflambda message-handler [event context]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        target-connection-id (.-connectionId body)
        payload (.-payload body)
        room-id (.-roomId body)
        ddb (get-dynamo)
        gateway (get-gateway-api event)
        query-params {:Key {:Room {:S room-id}}
                      :TableName "Tetris100"}]
    (p/alet [data (p/await (.getItem ddb (clj->js query-params)))
             players (set (oget data "Item.Players.SS"))]
            (if (players connection-id)
              (->>
               (broadcast-to-players gateway payload connection-id #{target-connection-id})
               (p/map (fn [] {:statusCode 200})))
              (p/promise {:statusCode 200})))))


(deflambda message-host-handler [event ctx]
  (let [connection-id (get-connection-id event)
        body (parse-body event)
        payload (.-payload body)
        room-id (.-roomId body)
        ddb (get-dynamo)
        gateway (get-gateway-api event)
        query-params {:Key {:Room {:S room-id}}
                      :TableName "Tetris100"}]
    (p/alet [data (p/await (.getItem ddb (clj->js query-params)))
             players (set (oget data "Item.Players.SS"))
             host (oget data "Item.Host.S")]
            (if (players connection-id)
              (->>
                (broadcast-to-players gateway payload connection-id #{host})
                (p/map (fn [] {:statusCode 200})))
              (p/promise {:statusCode 200})))))
