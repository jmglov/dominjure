(ns dominjure
  (:require [clojure.string :as str]
            [dominjure.dom :as dom]
            [promesa.core :as p]))

(def statuses
  {:PENDING-FETCH-SETS "Waiting to fetch sets metadata"
   :PENDING-SET-SELECTION "Waiting for user to select a set"
   :PENDING-FETCH-CARDS "Waiting to fetch cards metadata for a set"
   :PENDING-START-GAME "Waiting for player to start game"
   :WAITING-FOR-PLAYER "Waiting for player to play a turn"})

(def initial-state
  {:status :PENDING-FETCH-SETS
   :pending-effect nil
   :pending-event nil
   :available-sets {}
   :set-name nil
   :cards []
   :board {:victory []
           :treasure []
           :supply []
           :trash []
           :p1 {:deck []
                :discard []
                :play-area []
                :hand []}
           :p2 {:deck []
                :discard []
                :play-area []
                :hand []}}})

(defonce game-state (atom initial-state))

(defonce event-handlers
  (atom {}))

(defn register-handler [handler-name f]
  (swap! event-handlers assoc handler-name f))

(defn log [msg obj]
  (cond
    (sequential? obj)
    (doseq [o obj]
      (log msg o))

    obj
    (js/console.log msg
                    (if (any? #(% obj) [map? vector? list? set? keyword?])
                      (pr-str obj)
                      obj))

    :default
    (js/console.log msg))
  obj)

(defn error [& msgs]
  (js/console.error (str/join msgs)))

(defn enqueue-effect [state effect]
  (assoc state :pending-effect effect))

(defn enqueue-event [state event args]
  (let [pending-event {:event event, :args args}]
    (log "Enqueuing event:" pending-event)
    (assoc state :pending-event pending-event)))

(defn process-effects
  ([]
   (process-effects @game-state))
  ([{:keys [pending-effect] :as state}]
   (if pending-effect
     (let [{:keys [effect event f args]} pending-effect]
       (log "Dispatching effect:" (cons effect args))
       (p/let [res (apply f args)]
         (-> state
             (enqueue-event event res)
             (assoc :pending-effect nil))))
     state)))

(defn process-events
  ([]
   (process-events @game-state))
  ([{:keys [pending-event] :as state}]
   (if pending-event
     (let [{:keys [event args]} pending-event]
       (log "Handling event:" pending-event)
       (if-let [f (@event-handlers event)]
         (-> state
             (merge (f state args))
             (assoc :pending-event nil))
         (do
           (error "No event handler registered for event: " (name event))
           (assoc state :pending-event nil))))
     state)))

(declare tick!)

(defn fire-event!
  ([event args]
   (fire-event! @game-state event args))
  ([state event args]
   (->> (enqueue-event state event args)
        tick!)))

(defn update-status [state status]
  (if (statuses status)
    (assoc state :status status)
    (error "Invalid status: " (name status) "\n"
           "Should be one of: " (-> (keys statuses) set pr-str))))

(defn populate-select! [id options]
  (let [select (dom/get-el id)
        cur-opts (->> (.-options select)
                      (map #(.-value %))
                      set)]
    (doseq [opt options]
      (when-not (contains? cur-opts (.-value opt))
        (.appendChild select opt)))
    (set! (.-onchange select)
          (fn [event]
            (fire-event! :SET-SELECTED event.target.value))))
  options)

(defn render-set-selection! [{:keys [status available-sets] :as state}]
  (dom/set-styles! "#load-set" {})  ; revert to default styles
  (case status
    :PENDING-FETCH-SETS
    (-> (dom/get-el "#set-select")
        dom/remove-children!
        (.appendChild (dom/mk-option "--" nil)))

    :PENDING-SET-SELECTION
    (populate-select! "#set-select"
                      (map (comp dom/mk-option :name) available-sets))

    (dom/set-styles! "#load-set" {:display "none"}))
  state)

(defn render-board! [{:keys [status] :as state}]
  (case status
    :PENDING-START-GAME
    (dom/set-children! "#p2-turn"
                       [(dom/mk-element "span" "Ready to go?")
                        (dom/mk-button "Start game"
                                       #(fire-event! :START-GAME []))])

    :default)
  state)

(defn render! [{:keys [status] :as state}]
  (-> state
      render-set-selection!
      render-board!))

(defn fetch-edn [path]
  (log "Fetching path:" path)
  (p/-> (js/fetch (js/Request. path))
        (.text)
        read-string))

(defn card-img [card]
  (let [filename (-> (:src card) (str/split #"/") last)]
    (str "img/" (:set card) "/" filename)))

(defn place-card!
  ([card div-or-id]
   (place-card! card div-or-id nil))
  ([card div-or-id click-handler]
   (let [div (if (string? div-or-id) (dom/get-el div-or-id) div-or-id)
         img-div (js/document.createElement "div")
         img (js/document.createElement "img")]
     (.appendChild div img-div)
     (when card
       (set! (.-src img) (card-img card))
       (set! (.-alt img) (:title card))
       (.appendChild img-div img)
       (when click-handler
         (.addEventListener img-div "click" click-handler))))))

(defn populate-supply! [cards]
  (let [parent-div (dom/get-el "#supply")]
    (doseq [row (partition-all 5 cards)]
      (let [div (js/document.createElement "div")]
        (.appendChild parent-div div)
        (doseq [card (reverse row)]
          (place-card! card div))))))

(defn select-cards
  ([pred cards]
   (select-cards pred (count cards) cards))
  ([pred n cards]
   (->> (vals cards)
        (filter pred)
        shuffle
        (take n)
        (sort-by :cost)
        reverse)))

(defn make-piles [cards]
  (->> cards
       (map (fn [{:keys [starting-amount] :as card}]
              (repeat starting-amount card)))))

(defn starting-deck [cards]
  (concat (repeat 7 (cards "Copper"))
          (repeat 3 (cards "Estate"))))

(defn draw [num-cards deck]
  {:hand (take num-cards deck)
   :deck (drop num-cards deck)})

(defn flip [{:keys [face-up src card-back card-front] :as card}]
  (when card
    (if face-up
      (merge card {:src card-back
                   :card-front src
                   :face-up false})
      (merge card {:src card-front
                   :face-up true}))))

(defn place-player-cards! [{:keys [player-name deck discard hand]}]
  (let [deck-id (str "#" player-name "-deck")
        discard-id (str "#" player-name "-discard")
        hand-id (str "#" player-name "-hand")]
    (doseq [id [deck-id discard-id hand-id]]
      (dom/remove-children! id))
    (place-card! (-> deck first flip) deck-id)
    (place-card! (-> discard first flip) discard-id)
    (doseq [card hand]
      (place-card! (flip card) hand-id
                   {:click #(str "Card clicked: " (:title card))
                    :contextmenu #(str "Help for card: " (:title card))}))))

(defn start-turn! [player-id]
  (let [player (@game-state player-id)]
    (->> (update player :hand #(map flip %))
         place-player-cards!)))

(defn add-sets [state sets]
  (log (str "Sets: " sets))
  (-> state
      (update-status :PENDING-SET-SELECTION)
      (assoc :available-sets sets)))

(defn select-set [state set-name]
  (if (not= "--" set-name)
    (do
      (log "Selecting set:" set-name)
      (-> state
          (update-status :PENDING-FETCH-CARDS)
          (assoc :set-name set-name)
          (enqueue-effect {:effect :FETCH-CARDS
                           :f fetch-edn
                           :args [(str "sets/" set-name ".edn")]
                           :event :CARDS-LOADED})))
    (do
      (log "No set selected")
      state)))

(defn start-game! [{:keys [card-back cards]}]
  (let [cards (->> cards
                   (map (fn [[k v]] [k (assoc v :card-back card-back)]))
                   (into {}))]
    (let [victory (select-cards (fn [{:keys [kingdom types]}]
                                  (and (or (:victory types) (:curse types))
                                       (not kingdom)))
                                cards)
          treasure (select-cards (fn [{:keys [kingdom types]}]
                                   (and (:treasure types)
                                        (not kingdom)))
                                 cards)
          kingdom (->> cards
                       (select-cards :kingdom 10))
          p1-cards (shuffle (starting-deck cards))
          p2-cards (shuffle (starting-deck cards))
          p1 (merge {:player-name "p1", :cards p1-cards} (draw 5 p1-cards))
          p2 (merge {:player-name "p2", :cards p2-cards} (draw 5 p2-cards))]
      (println "Treasure:" (map :title treasure))
      (println "Victory:" (map :title victory))
      (println "Kingdom:" (map :title kingdom))
      (doseq [card victory]
        (place-card! card "#victory"))
      (doseq [card treasure]
        (place-card! card "#treasure"))
      (populate-supply! kingdom)
      (place-player-cards! p1)
      (place-player-cards! p2)
      (let [state {:cards cards
                   :victory victory
                   :treasure treasure
                   :kingdom kingdom
                   :p1 p1
                   :p2 p2}]
        (reset! game-state state)
        state))))

(defn glue-card [card-back card]
  (assoc card :card-back card-back))

(defn assemble-cards [{:keys [card-back cards]}]
  (->> cards
       (map (fn [[card-name card]] [card-name (glue-card card-back card)]))
       (into {})))

(defn select-victory [{:keys [cards] :as state}]
  (assoc state :victory
         (->> cards
              (select-cards (fn [{:keys [kingdom types]}]
                              (and (or (:victory types) (:curse types))
                                   (not kingdom))))
              make-piles)))

(defn select-treasure [{:keys [cards] :as state}]
  (assoc state :treasure
         (->> cards
              (select-cards (fn [{:keys [kingdom types]}]
                              (and (:treasure types)
                                   (not kingdom))))
              make-piles)))

(defn select-kingdom [{:keys [cards] :as state}]
  (assoc state :kingdom
         (->> cards
              (select-cards :kingdom 10)
              make-piles)))

(defn deal-cards [{:keys [cards] :as state}]
  (let [p1-cards (shuffle (starting-deck cards))
        p2-cards (shuffle (starting-deck cards))
        p1 (merge {:player-name "p1", :cards p1-cards} (draw 5 p1-cards))
        p2 (merge {:player-name "p2", :cards p2-cards} (draw 5 p2-cards))]
    (merge state {:p1 p1, :p2 p2})))

(defn prepare-board [{:keys [set-name] :as state} game-set]
  (let [cards (assemble-cards game-set)]
    (log "Prepared board:"
         (-> state
             (assoc :cards cards)
             select-victory
             select-treasure
             select-kingdom
             deal-cards
             (update-status :PENDING-START-GAME)))))

(defn start-turn [state]
  (-> state
      (update :turn-num inc)
      (update-status :WAITING-FOR-PLAYER)))

(defn start-game [state]
  (log "Starting game!")
  (-> state
      (assoc :turn-num 0)
      start-turn))

(defn tick!
  ([]
   (tick! @game-state))
  ([state]
   (p/->> state
          process-events
          process-effects
          process-events
          render!
          (swap! game-state merge))))

(defn load-ui! []
  (register-handler :SETS-LOADED add-sets)
  (register-handler :SET-SELECTED select-set)
  (register-handler :CARDS-LOADED prepare-board)
  (register-handler :START-GAME start-game)
  (p/-> @game-state
        tick!
        (enqueue-effect {:effect :FETCH-SETS
                         :f fetch-edn
                         :args ["sets.edn"]
                         :event :SETS-LOADED})
        tick!))

#_(load-ui!)

(comment

  (load-ui!)
  ;; => #<Promise[~]>

  (swap! game-state update-status :PENDING-START-GAME)

  (reset! game-state initial-state)

  @game-state
  ;; => {:status :PENDING-FETCH-SETS, :available-sets {}, :set-name nil, :set-cards [], :pending-effects [], :pending-events []}

  (select-keys @game-state [:status :pending-effects :pending-events])
  ;; => {:status :PENDING-SET-SELECTION}
  ;; => {:status :PENDING-FETCH-CARDS}
  ;; => {:status :PENDING-FETCH-CARDS}
  ;; => {:status :PENDING-FETCH-CARDS}

  (-> @game-state
      (enqueue-effect {:effect :FETCH-CARDS
                       :f fetch-edn
                       :args [(str "sets/Base.edn")]
                       :event :CARDS-LOADED})
      tick!)

  )

#_(load-ui!)

(comment

  (load-ui!)

  @game-state
  ;; => {:status :PENDING-SET-SELECTION, :available-sets [{:name "Base"}], :set-name nil, :set-cards []}

  (reset! event-handlers {})

  @event-handlers
  ;; => {:SETS-LOADED #object[Function], :SET-SELECTED #object[Function]}

  (render! @game-state)

  (fire-event! :SETS-LOADED)

  (doseq [card (->> (:cards @game-state)
                    (select-cards (fn [{:keys [kingdom types]}]
                                    (and (:victory types)
                                         (not kingdom)))))]
    (place-card! card "#victory"))

  (reset-kingdom!)

  (->> (:selected-cards @game-state)
       :kingdom
       (partition-all 5)
       (map #(map (juxt :title :cost) %)))
  ;; => ((["Council Room" 5] ["Witch" 5] ["Festival" 5] ["Mine" 5] ["Market" 5]) (["Moneylender" 4] ["Remodel" 4] ["Harbinger" 3] ["Vassal" 3] ["Moat" 2]))

  (select-set!)

  (-> @game-state
      :selected-cards
      :kingdom
      populate-supply!)

  (place-card! {:set "Base", :src "Card_back.jpg"} "#p1-deck")
  (place-card! {:set "Base", :src "Card_back.jpg"} "#p2-deck")

  (dom/remove-children! "#p1-hand")

  (dotimes [_ 5]
    (place-card! {:set "Base", :src "Card_back.jpg"} "#p1-hand"))
  (dotimes [_ 5]
    (place-card! {:set "Base", :src "Card_back.jpg"} "#p2-hand"))

  (-> (get-in @game-state [:cards "Copper"])
      (place-card! "#p1-discard"))
  (-> (get-in @game-state [:cards "Copper"])
      (place-card! "#p2-discard"))

  (dom/set-styles! "#p1" {:gap "5px"})
  (dom/set-styles! "#p1-deck" {:width "10%"})
  (dom/set-styles! "#p1-discard" {:width "10%"})
  (dom/set-styles! "#p1-play-area" {:width "40%"})
  (dom/set-styles! "#p1-hand" {:width "40%"})

  (dom/set-styles! "#p2" {:display "flex", :justify-content "space-between", :gap "5px"})
  (dom/set-styles! "#p2-deck" {:width "10%"})
  (dom/set-styles! "#p2-discard" {:width "10%"})
  (dom/set-styles! "#p2-play-area" {:width "40%"})
  (dom/set-styles! "#p2-hand" {:display "flex", :width "40%", :gap "2px"})

  (let [{:keys [cards]} @game-state]
    (let [deck (shuffle (starting-deck cards))]
      {:hand (take 5 deck)
       :deck (drop 5 deck)}
      (flip (first deck))))
  ;; => {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "Card_back.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"}

  (:p1 @game-state)
  ;; => {:player-name "p1", :cards [{:card-back "Card_back.jpg", :kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"}], :hand ({:card-back "Card_back.jpg", :kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"}), :deck ({:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:card-back "Card_back.jpg", :kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"})}

  (place-player-cards! (:p2 @game-state))

  (->> (update (:p2 @game-state)
               :hand
               #(map flip %))
       place-player-cards!)

  (-> (dom/get-el "#p2-hand > div")
      (.addEventListener "click" #(log "clicked")))

  (set! (.-innerHTML (dom/get-el "#p2-turn")) "Play actions")

  (dom/set-styles! "#p2-turn" {:display "flex"
                           :justify-content "end"})

  (let [el (js/document.createElement "button")]
    (set! (.-innerHTML el) "End actions")
    (.appendChild (dom/get-el "#p2-turn") el))

  )
