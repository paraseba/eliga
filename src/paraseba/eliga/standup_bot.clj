(ns paraseba.eliga.standup-bot
  (:import org.joda.time.DateTime)
  (:require
    [clojure.string :as string]
    [postal.core :as postal]
    [paraseba.eliga.bot :as bot]
    [paraseba.eliga.standup-state :as sstate]))

(defn- parse-message
  [message]
  (let [y-index (.lastIndexOf message "#yesterday")
        t-index (.lastIndexOf message "#today")
        now (DateTime.)]
    (merge
      (when (> y-index -1)
        (let [y-start (+ y-index (count "#yesterday "))
              y-end (if (> y-index t-index) (count message) t-index)]
        {:yesterday {:message (string/trim (subs message y-start y-end)) :timestamp now}}))
      (when (> t-index -1)
        (let [t-start (+ t-index (count "#today "))
              t-end (if (> t-index y-index) (count message) y-index)]
        {:today {:message (string/trim (subs message t-start t-end)) :timestamp now}})))))

(comment

  (parse-message "@eliga #yesterday nothing #today everything")
  (parse-message "@eliga #today everything")
  (parse-message "@eliga #today idk #today everything")
  (parse-message "@eliga #today idk #yesterday everything")
  (parse-message "@eliga sending an invalid update")
  )

(defn- ack-updates-received
  [group-chat session to updates]
  (let [update-titles (map #(str "#" (name %)) (-> updates keys sort reverse))
        message (str (clojure.string/join " & " update-titles) " update received")]
    (bot/private-message group-chat session to message)))

(defn- apply-message!
  [group-chat session state message]
  (when (or (= (:to message) (-> session :user-details :id))
            (:mention? message))
    (when-let [updates (parse-message (:body message))]
      ; TODO: define a better rule to get the standup-id
      (sstate/add-status state (or (:room message) (-> session :rooms first)) (:from message) updates)
      (ack-updates-received group-chat session (:from message) updates))))

(defn- status-ready?
  [[_ status]]
  (every? (set (keys status)) [:yesterday :today]))

(comment

  (status-ready? ["foo" {}])
  (status-ready? ["foo" {:yesterday "ssadas"}])
  (status-ready? ["foo" {:today "dadfdsf" :yesterday "ssadas"}])
  )

(defn- standup-ready?
  [users statuses]
  (->> statuses
       (filter status-ready?)
       (map key)
       set
       (= (set users))))

(comment

  (standup-ready? ["nico" "seba"] {})
  (standup-ready? ["nico" "seba"] {:standups {"nico" {}}})
  (standup-ready? ["nico" "seba"] {:standups {"nico" {:yesterday "dasds" :today "dafsdfsd"}}})
  (standup-ready? ["nico" "seba"] {:standups {"nico" {:yesterday "dasds" :today "dafsdfsd"} "seba" {:yesterday "dasdsf"}}})
  (standup-ready? ["nico" "seba"] {:standups {"nico" {:yesterday "dasds" :today "dafsdfsd"} "seba" {:yesterday "dasdsf" :today "ggfdfs"}}})

 )

(defn format-standup-message [statuses team-name team-members]
  (let [format-user
        (fn [[user status]]
          (str (format "%s:\n" (team-members user))
               (format "  Yesterday: %s\n" (-> status :yesterday :message))
               (format "  Today: %s\n" (-> status :today :message))))]

    (str (format "Standup updates for %s\n" team-name)
         (string/join "\n" (map format-user (sort-by key statuses))))))

(defn send-standup-email [statuses team-name team-members email-config]
  (let [date (java.util.Date.)
        subject (format "tag:standup %s standup %s" team-name date)
        body (format-standup-message statuses team-name team-members)]
    (postal/send-message (dissoc email-config :from :to)
                           {:from (:from email-config)
                            :to (:to email-config)
                            :subject subject
                            :body body})))

(defn start [group-chat users config]
  (let [state (sstate/empty-memory-standup-state)]
    (bot/connect group-chat config
                (fn [session message]
                  (apply-message! group-chat session state message)
                  (when (standup-ready? users (sstate/standup-as-map state (-> config :rooms first)))
                    ((:on-ready config) session (sstate/standup-as-map state (-> config :rooms first)))
                    (sstate/standup-done state (-> config :rooms first)))))))

(defn -main [& args]
  (start (bot/->Hipchat)
         ["Sebastian Galkin" "Nicolás Berger"]
         {:user "98902_725271" :password "thebot"
          :rooms ["98902_eliga"] :nick "Eliga bot"
          :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"})
  )
(comment

  (def hipchat (bot/->Hipchat))
  (def users [{:id 725263 :name "Nicolás Berger"}])
  (defn find-user-name
    [group-chat session user-id]
    (:name (bot/find-user group-chat session user-id)))


  (def session
    (start hipchat (map :id users)
           {:user-id "725271" :password "thebot"
            :rooms ["98902_eliga"]
            :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"
            :on-ready (fn [session statuses]
                        (bot/broadcast hipchat session "98902_eliga"
                                       (format-standup-message statuses "my team"
                                                               (partial find-user-name hipchat session)))
                        (send-standup-email statuses "my team" (partial find-user-name hipchat session)
                                            {:host "mailtrap.io"
                                             :port 2525
                                             :user "eliga-3ebf39ee87f3a841"
                                             :pass "d1ddbe937212347e"
                                             :to ["myteam@example.com"]
                                             :from "eliga@example.com"}))}))

  (bot/disconnect hipchat session)

  )
