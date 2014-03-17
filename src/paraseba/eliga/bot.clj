(ns paraseba.eliga.bot
  (:require [clj-http.client :as http]
            [clojure.string :as string])
  (:import org.jivesoftware.smack.XMPPConnection
           org.jivesoftware.smackx.muc.MultiUserChat
           org.jivesoftware.smack.PacketListener
           org.jivesoftware.smack.keepalive.KeepAliveManager))

(defn- enter-room [{:keys [connection nick password] :as bot} room-name]
  (assoc-in bot [:chats room-name]
            (doto (MultiUserChat.
                    connection
                    (if (.endsWith room-name "@conf.hipchat.com")
                      room-name
                      (str room-name "@conf.hipchat.com")))
              (.join nick password))))

(defn- user-details [token user-id]
  (:body
    (http/get (str "https://api.hipchat.com/v2/user/"
                   (last (string/split user-id #"_")))
              {:as :json
               :query-params {:auth_token token}})))

(defn- extract-name [s]
  (last (string/split s #"/")))

(defn mentioned?
  [mention-name message]
  (re-find (java.util.regex.Pattern/compile
             (str "(^|\\s+)@" mention-name "(\\s+|$)"))
           message)
 )

(defn- packet->message [mention-name packet]
  {:from (extract-name (.getFrom packet))
   :body (.getBody packet)
   :mention? (boolean
               (mentioned? mention-name (.getBody packet)))})

(defn- handle? [msg bot]
  (not (= (:from msg) (:nick bot))))

(defn- process-packet [bot room packet]
  (let [msg (packet->message
              (-> bot :user-details :mention_name)
              packet)]
    (when (handle? msg bot)
      ((:handler bot) bot (assoc msg :room room)))))

(defn start-bot [{:keys [nick user password rooms api-token] :as config} handler]
  (let [connection (doto (XMPPConnection. "chat.hipchat.com")
                     .connect
                     (.login user password "bot"))
        pingmanager (doto (KeepAliveManager/getInstanceFor connection)
                      (.setPingInterval 60000))
        bot (reduce enter-room
                    (assoc config
                           :handler handler
                           :connection connection
                           :ping-manager pingmanager
                           :user-details (user-details api-token user))
                    rooms)]
    (doseq [[chat room] (map vector (vals (:chats bot)) rooms)]
      (.addMessageListener chat
                           (reify PacketListener
                             (processPacket [this packet]
                               (process-packet bot room packet)))))
    bot))

(defn stop-bot [bot]
  (.disconnect (:connection bot)))

(defn group-chat [bot room msg]
  (.sendMessage (-> bot :chats (get room)) msg))


(defprotocol GroupChat
  (connect [this config handler]
           ;; returns session
           )

  (disconnect [this session])

  (write [this session room msg]))

(deftype Hipchat []
  GroupChat
  (connect [_ config handler]
    (start-bot config handler))

  (disconnect [_ session]
    (stop-bot session))

  (write [_ session room msg]
   (group-chat session room msg)))


(comment
  (def mybot (start-bot {:user "98902_725271" :password "thebot"
                         :rooms ["98902_eliga"] :nick "Eliga bot"
                         :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"}
                        (fn [bot message]
                          (group-chat bot (:room message)
                                      (str "Te escuche " (:from message) ": "
                                           (:body message))))))

  )
