(ns paraseba.eliga.standup-state
  (:import org.joda.time.DateTime))

(defprotocol StandupState
  (add-status [_ standup-id user status-data])
  (standup-as-map [_ standup-id])
  (standup-done [_ standup-id]))

(deftype MemoryStandupState [state]
  StandupState
  (add-status [_ standup-id user status-data]
    (assert (or (string? user) (number? user)))
    (assert (not (empty? status-data)))
    (assert (every? #(string? (:message %)) (vals status-data)))
    (assert (every? #(instance? DateTime (:timestamp %)) (vals status-data)))
    (assert (every? #{:yesterday :today} (keys status-data)))
    (assert (string? standup-id))
    (swap! state update-in [standup-id user] merge status-data))
  (standup-as-map [_ standup-id ] (get @state standup-id))
  (standup-done [_ standup-id] (swap! state dissoc standup-id)))

(defn empty-memory-standup-state [] (->MemoryStandupState (atom {})))


(comment

  (-> (empty-memory-standup-state) ( add-status "foo-bar" "nico" {}))

  (-> ( empty-memory-standup-state) ( standup-done "foo-bar" ))


  )
