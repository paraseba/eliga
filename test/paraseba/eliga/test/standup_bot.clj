(ns paraseba.eliga.test.standup-bot
  (:require [clojure.test :refer :all]
            [paraseba.eliga.bot :refer :all]
            )
  )

(deftype StubHipchat [state]
  GroupChat
  (connect [this config handler]
    {:config config :handler handler})

  (disconnect [this session])

  (write [this {:keys [config handler]} room msg]
    (swap! state update-in [:messages room] conj {(-> config :user :name) msg})
    (handler {:from (-> config :user :name)
              :body msg :mention? false :room room})))

(defn get-all-messages [hipchat room]
  (-> hipchat .state deref :messages (get room) reverse))

(defn stub-hipchat []
  (->StubHipchat (atom {})))

(deftest standup-info-gathering
  (let [hipchat (stub-hipchat)
        bot (start {:pwd :user}
                   ["nico" "seba" "foo"]
                   "standup@example.com")
        noop (fn [& _])
        nico-session (connect hipchat {:user {:name "Nicolas Foo"}} noop)
        seba-session (connect hipchat {:user {:name "Sebastian Bar"}} noop)
        foo-session  (connect hipchat {:user {:name "Foo Bar"}} noop)
        ]

    (write hipchat nico-session "team" "@eliga #yesterday I didn't do much")
    (write hipchat nico-session "team" "@eliga #today I'll do a lot")

    (write hipchat seba-session "team" "@eliga #today hard stuff #yesterday easy stuff")

    (write hipchat foo-session  "team" "#yesterday foo did X #today foo will do Y")

    (= (last (get-all-messages hipchat "team"))
       {"eliga"
        (str "Standup updates for team:\n"
             "Foo Bar:\n"
             "  Yesterday: foo did X\n"
             "  Today: foo will do Y\n"
             "\n"
             "nico:\n"
             "  Yesterday: I didn't do much\n"
             "  Today: I'll do a lot\n"
             "\n"
             "seba:\n"
             "  Yesterday: easy stuff\n"
             "  Today: hard stuff\n")})

    )
  )


(comment

  (let [hipchat (stub-hipchat)
        nico-session (connect hipchat {:user {:name "Nicolas Foo"}} noop)
        seba-session (connect hipchat {:user {:name "Sebastian Bar"}} noop)
        ]

    (write hipchat seba-session "team" "@eliga #today hard stuff #yesterday easy stuff")
    (write hipchat nico-session "team" "@eliga #yesterday I didn't do much")
    (prn (get-all-messages hipchat "team") )
    )

  )
