(ns paraseba.eliga.test.standup-bot
  (:require [clojure.test :refer :all]
            [paraseba.eliga.bot :refer :all]
            [paraseba.eliga.standup-bot :as standup]
            )
  )

(deftype StubHipchat [state]
  GroupChat
  (connect [this config handler]
    (let [session {:config config :handler handler}]
      (swap! state update-in [:sessions] conj session)
      session))

  (disconnect [this session])

  (write [this {:keys [config]} room msg]
    (swap! state update-in [:messages room] conj {(-> config :user :id) msg})
    (doseq [session (:sessions @state) :when (not= (-> config :user :id) (-> session :config :user :id))]
      ((:handler session)
       session
       {:from (-> config :user :id)
        :body msg
        :mention? (mentioned? (-> session :config :user :id) msg)
        :room room}))))

(defn get-all-messages [hipchat room]
  (-> hipchat .state deref :messages (get room) reverse))

(defn stub-hipchat []
  (->StubHipchat (atom {})))

(defn- noop [& _])

(deftest acknowledge-message
  (let [group-chat (stub-hipchat)
        bot (standup/start group-chat
                           ["nico"]
                           {:rooms ["team"] :user {:id "eliga" :name "Eliga"}
                            :on-ready noop})
        session (connect group-chat {:user {:id "nico" :name "Nicolas"}} noop)]
    (write group-chat session "team" "@eliga how are you?")
    (is (= (last (get-all-messages group-chat "team"))
           {"eliga" "@nico That didn't look like a standup update..."}))
    (write group-chat session "team" "@eliga #yesterday Way too much")
    (is (= (last (get-all-messages group-chat "team"))
           {"eliga" "@nico standup update received"}))))

(deftest standup-info-gathering
  (let [group-chat (stub-hipchat)
        done? (atom false)
        bot (standup/start group-chat
                           ["nico" "seba" "foo"]
                           {:rooms ["team"] :user {:id "eliga" :name "Eliga bot"}
                            :on-ready
                            (fn [session statuses]
                              (reset! done? true)
                              (is (not (nil? session)))
                              (is (= #{"nico" "seba" "foo"} (set (keys statuses))))
                              (is (= (-> statuses (get "nico") :yesterday :message)
                                     "I didn't do much"))
                              (is (= (-> statuses (get "nico") :today :message)
                                     "I'll do a lot"))
                              (is (= (-> statuses (get "seba") :yesterday :message)
                                     "easy stuff"))
                              (is (= (-> statuses (get "seba") :today :message)
                                     "hard stuff"))
                              (is (= (-> statuses (get "foo") :yesterday :message)
                                     "foo did X"))
                              (is (= (-> statuses (get "foo") :today :message)
                                     "foo will do Y")))})
        nico-session (connect group-chat {:user {:id "nico" :name "Nicolas Foo"}} noop)
        seba-session (connect group-chat {:user {:id "seba" :name "Sebastian Bar"}} noop)
        foo-session  (connect group-chat {:user {:id "foo" :name "Foo Bar"}} noop)]

    (write group-chat nico-session "team" "@eliga #yesterday I didn't do much")
    (write group-chat nico-session "team" "@eliga #today I'll do a lot")

    (write group-chat seba-session "team" "@eliga #today hard stuff #yesterday easy stuff")

    (write group-chat foo-session  "team" "@eliga #yesterday foo did X #today foo will do Y")

    (is @done?)))

(comment
  (is (= (last (get-all-messages group-chat "team"))
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
                 "  Today: hard stuff\n")}))

  (.printStackTrace *e)

  (standup-info-gathering)

  (acknowledge-message)

  (let [hipchat (stub-hipchat)
        nico-session (connect hipchat {:user {:id "nico" :name "Nicolas Foo"}} (fn [& rest] (prn "hola" rest)))
        ;seba-session (connect hipchat {:user {:name "Sebastian Bar"}} )
        ]

;    (write hipchat seba-session "team" "@eliga #today hard stuff #yesterday easy stuff")
    (write hipchat nico-session "team" "@eliga #yesterday I didn't do much")
    (prn (get-all-messages hipchat "team") )
    )

  )
