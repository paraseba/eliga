(ns paraseba.eliga.standup-bot
  (:require
    [clojure.string :as string]
    [paraseba.eliga.bot :as bot]))

(defn- parse-message
  [message]
  (let [y-index (.lastIndexOf message "#yesterday")
        t-index (.lastIndexOf message "#today")]
    (merge
      (when (> y-index -1)
        (let [y-start (+ y-index (count "#yesterday "))
              y-end (if (> y-index t-index) (count message) t-index)]
        {:yesterday (string/trim (subs message y-start y-end))}))
      (when (> t-index -1)
        (let [t-start (+ t-index (count "#today "))
              t-end (if (> t-index y-index) (count message) y-index)]
        {:today (string/trim (subs message t-start t-end))})))))

(comment

  (parse-message "@eliga #yesterday nothing #today everything")
  (parse-message "@eliga #today everything")
  (parse-message "@eliga #today idk #today everything")
  (parse-message "@eliga #today idk #yesterday everything")
  )

(defn- apply-message!
  [state message]
  (when (:mention? message)
    (swap! state update-in
           [:standups (:from message)]
           merge (parse-message (:body message)))))

(defn- status-ready?
  [[_ status]]
  (every? (set (keys status)) [:yesterday :today]))

(comment

  (status-ready? ["foo" {}])
  (status-ready? ["foo" {:yesterday "ssadas"}])
  (status-ready? ["foo" {:today "dadfdsf" :yesterday "ssadas"}])
  )

(defn- standup-ready?
  [users state]
  (->> (:standups state)
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
               (format "  Yesterday: %s\n" (:yesterday status))
               (format "  Today: %s\n" (:today status))))]

    (str (format "Standup updates for %s\n" team-name)
         (string/join "\n" (map format-user (sort-by key statuses))))))

(defn start [group-chat users config]
  (let [state (atom {})]
    (bot/connect group-chat config
                (fn [session message]
                  (apply-message! state message)
                  (when (standup-ready? users @state)
                    ((:on-ready config) session (:standups @state))
                    (swap! state dissoc :standups))))))

(defn -main [& args]
  (start (bot/->Hipchat)
         ["Sebastian Galkin" "Nicolás Berger"]
         {:user "98902_725271" :password "thebot"
          :rooms ["98902_eliga"] :nick "Eliga bot"
          :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"})
  )


(comment

  (def hipchat (bot/->Hipchat))
  (def session
    (start hipchat  ["Sebastian Galkin" "Nicolás Berger"]
           {:user "98902_725271" :password "thebot"
            :rooms ["98902_eliga"] :nick "Eliga bot"
            :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"
            :on-ready (fn [session statuses]
                        (bot/write hipchat session "98902_eliga"
                                   (format-standup-message statuses "my team"
                                                           identity )))}))

  (bot/disconnect hipchat session)

  (def mybot (start-bot 
               (fn [bot message]
                 (group-chat bot (:room message)
                             (str "Te escuche " (:from message) ": "
                                  (:body message))))))

  )
