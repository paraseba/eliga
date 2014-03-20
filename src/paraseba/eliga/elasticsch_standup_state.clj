(ns paraseba.eliga.elasticsch-standup-state
  (:require [clojurewerkz.elastisch.native :as es]
            [clojurewerkz.elastisch.native.index :as esi]
            [clojurewerkz.elastisch.native.document :as doc]
            [clojurewerkz.elastisch.rest.response :as esrsp]
            [clojure.walk :refer [postwalk]]
            [paraseba.eliga.standup-state :as sstate])
  (:import org.joda.time.DateTime))

(defn- from-es
  [map]
  (postwalk #(if (integer? %) (DateTime. %) %) map))

(defn- to-es
  [map]
  (postwalk #(if (isa? DateTime (class %)) (.getMillis %) %) map))

(defn- map-vals
  [f m]
  (into (empty m) (map #(update-in % [1] f) m)))

(defn- find-statuses-query
  [standup-id]
  {:filtered
   {:query {"match_all" {}}
    :filter {:and [{:missing {:field :archived}}
                   {:term {:standup-id standup-id}}]}}})

(deftype ESStandupState [index]
  sstate/StandupState
  (add-status [_ standup-id user status-data]
    (assert (string? user))
    (assert (not (empty? status-data)))
    (assert (every? #(string? (:message %)) (vals status-data)))
    (assert (every? #(instance? DateTime (:timestamp %)) (vals status-data)))
    (assert (every? #{:yesterday :today} (keys status-data)))
    (assert (string? standup-id))

    ; {:yesterday {:message "foo" :timestamp DateTime.} :today {...}}

    (doc/put index "standup" nil (-> status-data (assoc :user user :standup-id standup-id :archived nil) to-es)))

  (standup-as-map [_ standup-id]
    (let [es-query (find-statuses-query standup-id)
          res  (doc/search index "standup" :query es-query)
          hits (esrsp/hits-from res)
          docs (group-by :user (map (comp from-es :_source) hits))]
      (map-vals (fn [statuses]
                  (->> statuses
                       (map #(dissoc % :user :standup-id :archived))
                       (sort-by #(or (-> % :today :timestamp) (-> % :yesterday :timestamp)))
                       (apply merge)))
                docs)))

  (standup-done
    [_ standup-id]
    (let [es-query (find-statuses-query standup-id)
          res  (doc/search index "standup" :query es-query)
          hits (esrsp/hits-from res)]
      (doseq [doc hits]
        (doc/put index "standup" (:_id doc) (assoc doc :archived (.getMillis (DateTime.))))))))

(defn make-elasticsch-standup-state
  [host port cluster-name index-name]
  (es/connect! [[host port]]
               {"cluster.name" cluster-name})
  (when-not (esi/exists? index-name)
    (esi/create
      index-name
      :mappings {:standup {:properties {:standup-id {:type "string" :store "yes" :index "not_analyzed"}
                                        :archived {:type "long"}}}}))
  (->ESStandupState index-name) )

(comment

  (def esstate ( make-elasticsch-standup-state "localhost" 9300 "elasticsearch" "eliga"))

  (sstate/standup-as-map esstate "my-team")
  (sstate/standup-done esstate "my-team")

  (sstate/add-status esstate "my-team" "nico" {:today {:message "xxx" :timestamp (DateTime.)}})
  (sstate/add-status esstate "my-team" "nico" {:yesterday {:message "yyy" :timestamp (DateTime.)}})
  (sstate/add-status esstate "my-team" "nico" {:yesterday {:message "foobar" :timestamp (DateTime.)} :today {:message "barfoo" :timestamp (DateTime.)}})

  )
