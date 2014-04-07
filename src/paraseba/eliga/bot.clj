(ns paraseba.eliga.bot
  (:require [clj-http.client :as http]
            [clojure.string :as string])
  (:import org.jivesoftware.smack.XMPPConnection
           org.jivesoftware.smackx.muc.MultiUserChat
           (org.jivesoftware.smack PacketListener MessageListener ChatManagerListener)
           org.jivesoftware.smack.keepalive.KeepAliveManager))

(defn- enter-room [{:keys [connection user-details password] :as bot} room-name]
  (assoc-in bot [:chats room-name]
            (doto (MultiUserChat.
                    connection
                    (if (.endsWith room-name "@conf.hipchat.com")
                      room-name
                      (str room-name "@conf.hipchat.com")))
              (.join (:name user-details) password))))

(defn- user-details
  "token: Hipchat api token
   user: id, email or mention_name"
  [token user-id]
  (assert token)
  (-> (http/get (str "https://api.hipchat.com/v2/user/" user-id)
                {:as :json
                 :query-params {:auth_token token}})
      :body))

(defn- find-users-in-room
  "token: Hipchat api token
   room: id or name
   returns list of users, with shape
   [{:id id :name name :mention_name mention_name} ... {...}]"
  [token room]
  (let [room-name (last (string/split room #"_"))
        api-url (str "https://api.hipchat.com/v2/room/" room-name)]
  (assert token)
  (-> (http/get api-url
                {:as :json
                 :query-params {:auth_token token}})
      :body
      :participants))
  )

(defn- extract-name [s]
  (last (string/split s #"/")))

(defn mentioned?
  "mention-name: user's mention_name
   message: a message where to look for @mention-name"
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

(defn- chat-id->user-id [chat-id]
  (-> chat-id
      (string/split #"@")
      first
      (string/split #"_")
      last
      Integer/parseInt))

(defn- chat-message->message [chat-message]
  {:from (chat-id->user-id (.getFrom chat-message))
   :body (.getBody chat-message)})

(defn- handle? [msg bot]
  (not (= (:from msg) (-> bot :user-details :name))))

(defn- find-user-in-room-by-name
  [api-token user-name room]
  (->> (find-users-in-room api-token room)
      (filter #(= (:name %) user-name))
      first))

(defn- process-packet [bot room packet]
  (let [msg (packet->message
              (-> bot :user-details :mention_name)
              packet)]
    (when (handle? msg bot)
      (let [from (find-user-in-room-by-name (:api-token bot)
                                    (:from msg)
                                    room)]
      ((:handler bot) bot (assoc msg
                                 :room room
                                 :from (:id from)))))))

(defn- process-chat-message
  [{:keys [handler user-details] :as bot} chat-message]
  (let [msg (chat-message->message chat-message)]
    (when (:body msg)
      (handler bot (assoc msg
                          :to (:id user-details))))))

(defn- subscribe-to-chat
  [bot chat]
  (.addMessageListener chat
                       (reify MessageListener
                         (processMessage [this chat message]
                           (process-chat-message bot message)))))

(defn start-bot [{:keys [user-id password rooms api-token] :as config} handler]
  (let [user-details (user-details api-token user-id)
        connection (doto (XMPPConnection. "chat.hipchat.com")
                     .connect
                     (.login (:xmpp_jid user-details) password "bot"))
        pingmanager (doto (KeepAliveManager/getInstanceFor connection)
                      (.setPingInterval 60000))
        bot (reduce enter-room
                    (assoc config
                           :handler handler
                           :connection connection
                           :ping-manager pingmanager
                           :user-details user-details)
                    rooms)]
    (doseq [[chat room] (map vector (vals (:chats bot)) rooms)]
      (.addMessageListener chat
                           (reify PacketListener
                             (processPacket [this packet]
                               (process-packet bot room packet)))))
    (-> connection
        .getChatManager
        (.addChatListener (reify ChatManagerListener
                            (chatCreated [this chat createdLocally]
                              (subscribe-to-chat bot chat)))))
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
  (private-message [this session user msg])
  (find-user [this session user-id]))

(deftype Hipchat []
  GroupChat
  (connect [_ config handler]
    (start-bot config handler))

  (disconnect [_ session]
    (stop-bot session))

  (broadcast [_ session room msg]
    (group-chat session room msg))

  (private-message [_ session user msg]
    (send-message session user msg))

  (find-user [_ session user-id]
    (user-details (:api-token session) user-id)))


(comment

  (def hipchat (Hipchat.))
  (def mybot (connect hipchat
                      {:user-id "725271" :password "thebot"
                       :rooms ["98902_eliga"]
                       :api-token "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P"}
                      (fn [bot message]
                        (if-let [room (:room message)]
                          (group-chat bot (:room message)
                                      (str "Te escuche " (:from message) ": "
                                           (:body message)))
                          (send-message bot (:from message)
                                           (str "Te escuche " (:from message) ": "
                                                (:body message)))))))

  (broadcast hipchat mybot "98902_eliga" "hola")
  (private-message hipchat mybot "@NicolasBerger" "hola")
  (private-message hipchat mybot "nicoberger@gmail.com" "hola")
  (private-message hipchat mybot "725263" "hola")

  (disconnect hipchat mybot)

  (find-user hipchat mybot "nicoberger@gmail.com" )
  (user-details "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P" "nicoberger@gmail.com" )
  (user-details "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P" "@NicolasBerger" )
  (user-details "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P" 725263 )
  (find-users-in-room "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P" "eliga" )
  (find-user-in-room-by-name "qjs7ceXYKzlzcARCj5GvrHoYhYG1ySLkDliZQd9P" "Nicolás Berger" "eliga")

  )
