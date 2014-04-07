(ns paraseba.eliga.test.standup-bot
  (:require [clojure.test :refer :all]
            [paraseba.eliga.bot :refer :all]
            [paraseba.eliga.standup-bot :as standup]
            )
  )

(deftype StubHipchat [state]
  ;; state is an atom to a map with shape
  ;; {:sessions [{:config config :handler ...} ...]
  ;;  :messages {"room1" ({"user1" "msg1"} {"user2" "msg2"} ...)
  ;;             "room2" ({"user2" "msg2"} {"user3" "msg3"} ...)
  ;;             #{"user1" "user2"} ({"user1" "msg2"} {"user2" "msg3"} ...)}}
  GroupChat
  (connect [this config handler]
    (let [session {:config config :handler handler}]
      (swap! state assoc-in [:sessions (-> config :user :id)] session)
      session))

  (disconnect [this session])

  (broadcast [this {:keys [config]} room msg]
    (let [from (-> config :user :id)]
      (swap! state update-in [:messages room] conj {from msg})
      (doseq [session (-> state deref :sessions vals)
              :let [to (-> session :config :user :id)
                    handler (:handler session)]
              :when (not= (-> config :user :id) to)]
        (handler
         session
         {:from from :body msg :mention? (mentioned? to msg) :room room}))))

  (private-message [this {:keys [config]} user msg]
    (let [from (-> config :user :id)
          to user
          to-session (-> state deref :sessions (get to))
          to-handler (:handler to-session)]
      (swap! state update-in [:messages #{from to}] conj {from msg})
      (to-handler to-session {:from from :body msg :to to :mention? (mentioned? to msg)}))))

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
    (broadcast group-chat session "team" "@eliga how are you?")
    (is (= (last (get-all-messages group-chat "team"))
           {"nico" "@eliga how are you?"}))
    (broadcast group-chat session "team" "@eliga #yesterday Way too much")
    (is (= (last (get-all-messages group-chat #{"eliga" "nico"}))
           {"eliga" "#yesterday update received"}))
    (private-message group-chat session "eliga" "#today I'll do some stuff")
    (is (= (last (get-all-messages group-chat #{"eliga" "nico"}))
           {"eliga" "#today update received"}))
    (broadcast group-chat session "team" "@eliga #yesterday Way too much #today I'll work on that bug")
    (is (= (last (get-all-messages group-chat #{"eliga" "nico"}))
           {"eliga" "#yesterday & #today update received"}))))

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

    (broadcast group-chat nico-session "team" "@eliga #yesterday I didn't do much")
    (broadcast group-chat nico-session "team" "@eliga #today I'll do a lot")

    (broadcast group-chat seba-session "team" "@eliga #today hard stuff #yesterday easy stuff")

    (broadcast group-chat foo-session  "team" "@eliga #yesterday foo did X #today foo will do Y")

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

;    (broadcast hipchat seba-session "team" "@eliga #today hard stuff #yesterday easy stuff")
    (broadcast hipchat nico-session "team" "@eliga #yesterday I didn't do much")
    (prn (get-all-messages hipchat "team") )
    )

  )
