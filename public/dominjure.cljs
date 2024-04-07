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

(defn card-img [card]
  (let [filename (-> (:src card) (str/split #"/") last)]
    (str "img/" (:set card) "/" filename)))

(defn place-card! [card id]
  (let [img-div (dom/mk-element "div")]
    (when card
      (let [img (dom/mk-img (card-img card) (:title card))]
        (when (:face-up card)
          (dom/add-event-listeners! img
                                    {:click #(log "Card clicked:" (:title card))
                                     :contextmenu #(log "Help for card:" (:title card))}))
        (dom/append-children! img-div [img])))
    (dom/append-children! id [img-div]))
  card)

(defn place-pile! [[card & _ :as pile] id]
  (when-not (place-card! card id)
    (error "Handle empty pile somehow"))
  pile)

(defn render-victory! [{:keys [victory] :as state}]
  (dom/remove-children! "#victory")
  (doseq [pile victory]
    (place-pile! pile "#victory"))
  state)

(defn render-treasure! [{:keys [treasure] :as state}]
  (dom/remove-children! "#treasure")
  (doseq [pile treasure]
    (place-pile! pile "#treasure"))
  state)

(defn render-supply! [{:keys [kingdom] :as state}]
  (dom/remove-children! "#supply")
  (doseq [row (partition-all 5 kingdom)]
    (let [row-div (dom/mk-element "div")]
      (dom/append-children! "#supply" row-div)
      (doseq [pile (reverse row)]
        (place-pile! pile row-div))))
  state)

(defn render-player! [{:keys [player-name deck discard hand]}]
  (let [deck-id (str "#" player-name "-deck")
        discard-id (str "#" player-name "-discard")
        hand-id (str "#" player-name "-hand")]
    (doseq [id [deck-id discard-id hand-id]]
      (dom/remove-children! id))
    (place-card! (first deck) deck-id)
    (place-card! (first discard) discard-id)
    (doseq [card hand]
      (place-card! card hand-id
                   {:click #(str "Card clicked: " (:title card))
                    :contextmenu #(str "Help for card: " (:title card))}))))

(defn render-players! [state]
  (doseq [player (map state [:p1 :p2])]
    (render-player! player))
  state)

(defn render-board! [{:keys [status] :as state}]
  (case status
    :PENDING-START-GAME
    (do
      (-> state
          render-victory!
          render-treasure!
          render-supply!
          render-players!)
      (dom/set-children! "#p2-turn"
                         [(dom/mk-element "span" "Ready to go?")
                          (dom/mk-button "Start game"
                                         #(fire-event! :START-GAME []))]))

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

#_(defn start-turn! [player-id]
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

(defn glue-card [card-back card]
  (assoc card
         :card-front (:src card)
         :card-back card-back
         :face-up true))

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
  (->> ["p1" "p2"]
       (reduce (fn [acc player-name]
                 (let [player-cards (shuffle (starting-deck cards))]
                   (assoc acc (keyword player-name)
                          {:player-name player-name
                           :cards player-cards
                           :deck (map flip player-cards)})))
               state)))

(comment

  (load-ui!)
  ;; => #<Promise[~]>

  (->> @game-state
       deal-cards
       render!)

  (-> @game-state
      (enqueue-effect {:effect :FETCH-CARDS
                       :f fetch-edn
                       :args [(str "sets/Base.edn")]
                       :event :CARDS-LOADED})
      tick!)

  (dom/set-styles! "#p2-turn" {:justify-content "right", :gap "10px"})

  (->> @game-state
       :cards
       vals
       (take 2))
  ;; => ({:card-back "Card_back.jpg", :kingdom true, :alt "Bureaucrat.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/4/4d/Bureaucrat.jpg/200px-Bureaucrat.jpg", :title "Bureaucrat", :types #{:action :attack}, :starting-amount 10, :card-front "https://wiki.dominionstrategy.com/images/thumb/4/4d/Bureaucrat.jpg/200px-Bureaucrat.jpg", :cost 4, :set "Base", :height "320", :text "Gain a Silver onto your deck. Each other player reveals a Victory card from their hand and puts it onto their deck (or reveals a hand with no Victory cards)."} {:card-back "Card_back.jpg", :kingdom true, :alt "Moneylender.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/7/70/Moneylender.jpg/200px-Moneylender.jpg", :title "Moneylender", :types #{:action}, :starting-amount 10, :card-front "https://wiki.dominionstrategy.com/images/thumb/7/70/Moneylender.jpg/200px-Moneylender.jpg", :cost 4, :set "Base", :height "320", :text "You may trash a Copper from your hand for +3"})

  )

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
  (js/document.addEventListener "contextmenu" #(.preventDefault %))
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

(comment

  (load-ui!)

  )
