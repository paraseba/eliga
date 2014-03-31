(ns paraseba.eliga.bot
  (:require [clj-http.client :as http]
            [clojure.string :as string])
  (:import org.jivesoftware.smack.XMPPConnection
           org.jivesoftware.smackx.muc.MultiUserChat
           (org.jivesoftware.smack PacketListener)
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
  (-> (http/get (str "https://api.hipchat.com/v2/user/"
                     (last (string/split user-id #"_")))
                {:as :json
                 :query-params {:auth_token token}})
      :body))

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

(defn send-message [{:keys [connection api-token]} user msg]
  (let [details (user-details api-token user)
        jid (:xmpp_jid details)]
    (assert (string? jid))
    (.sendMessage (.createChat (.getChatManager connection)
                          jid
                          nil)
                  msg)))

(defprotocol GroupChat
   ;; handler is a function that takes
   ;; session and a message map with shape
   ;; {:from "user-name"
   ;;  :body "message body"
   ;;  :mention? false}
  (connect [this config handler])
  (disconnect [this session])
  (broadcast [this session room msg])
  (private-message [this session user msg]))

(deftype Hipchat []
  GroupChat
  (connect [_ config handler]
    (start-bot config handler))

  (disconnect [_ session]
    (stop-bot session))

  (broadcast [_ session room msg]
    (group-chat session room msg))

  (private-message [_ session user msg]
    (send-message session user msg)))


(comment
  (def mybot (start-bot {:user "98902_725271" :password "thebot"
                         :rooms ["98902_eliga"] :nick "Eliga bot"
                         :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"}
                        (fn [bot message]
                          (group-chat bot (:room message)
                                      (str "Te escuche " (:from message) ": "
                                           (:body message))))))

  (def hipchat (Hipchat.))
  (def mybot (connect hipchat
                      {:user "98902_725271" :password "thebot"
                       :rooms ["98902_eliga"] :nick "Eliga bot"
                       :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"}
                      (fn [bot message]
                        (group-chat bot (:room message)
                                    (str "Te escuche " (:from message) ": "
                                         (:body message))))))
  (broadcast hipchat mybot "98902_eliga" "hola")
  (private-message hipchat mybot "Nicolás Berger" "hola")
  (private-message hipchat mybot "NicolásBerger" "hola")
  (private-message hipchat mybot "nicoberger@gmail.com" "hola")
  (private-message hipchat mybot "98902_725263" "hola")
  (private-message hipchat mybot "98902_725263@chat.hipchat.com" "chau")

  (disconnect hipchat mybot)

(user-details "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P" "nicoberger@gmail.com" )

  )
