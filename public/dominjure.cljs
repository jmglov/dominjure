(ns dominjure
  (:require [clojure.string :as str]
            [promesa.core :as p]))

(defonce game-state
  (atom
   {:supply []
    :cards nil}))

(defn log [msg obj]
  (if (sequential? obj)
    (doseq [o obj]
      (js/console.log msg o))
    (js/console.log msg obj))
  obj)

(defn get-el [selector]
  (if (instance? js/HTMLElement selector)
    selector  ; already an element; just return it
    (js/document.querySelector selector)))

(defn set-styles! [selector styles]
  (set! (.-style (get-el selector))
        (->> (map (fn [[k v]] (str (name k) ": " v)) styles)
             (str/join "; "))))

(defn remove-children! [id]
  (let [el (get-el id)]
    (while (.-firstChild el)
      (.removeChild el (.-lastChild el)))))

(defn fetch-edn [path]
  (p/-> (js/fetch (js/Request. path))
        (.text)
        read-string))

(defn mk-option
  ([text]
   (mk-option text text))
  ([text value]
   (let [opt (js/document.createElement "option")]
     (set! (.-text opt) text)
     (set! (.-value opt) value)
     opt)))

(defn card-img [card]
  (let [filename (-> (:src card) (str/split #"/") last)]
    (str "img/" (:set card) "/" filename)))

(defn place-card!
  ([card div-or-id]
   (place-card! card div-or-id nil))
  ([card div-or-id click-handler]
   (let [div (if (string? div-or-id) (get-el div-or-id) div-or-id)
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
  (let [parent-div (get-el "#supply")]
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

(defn starting-deck [cards]
  (concat (repeat 7 (cards "Copper"))
          (repeat 3 (cards "Estate"))))

(defn draw [num-cards deck]
  {:hand (take num-cards deck)
   :deck (drop num-cards deck)})

(defn flip [{:keys [src card-back card-front] :as card}]
  (when card
    (if (= card-back src)
      (assoc card :src card-front)
      (merge card {:src card-back
                   :card-front src}))))

(defn place-player-cards! [{:keys [player-name deck discard hand]}]
  (let [deck-id (str "#" player-name "-deck")
        discard-id (str "#" player-name "-discard")
        hand-id (str "#" player-name "-hand")]
    (doseq [id [deck-id discard-id hand-id]]
      (remove-children! id))
    (place-card! (-> deck first flip) deck-id)
    (place-card! (-> discard first flip) discard-id)
    (doseq [card hand]
      (place-card! (flip card) hand-id
                   {:click #(str "Card clicked: " (:title card))
                    :contextmenu #(str "Help for card: " (:title card))}))))

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

(defn start-turn! [player-id]
  (let [player (@game-state player-id)]
    (->> (update player :hand #(map flip %))
         place-player-cards!)))

(defn select-set! []
  (let [set-name (.-value (get-el "#set-select"))]
    (when-not (= "--" set-name)
      (log "Loading set:" set-name)
      (p/->> (fetch-edn (str "sets/" set-name ".edn"))
             start-game!)
      (set! (.-style (get-el "#load-set")) "display: none"))))

(defn populate-select! [id onchange-fn options]
  (let [select (get-el id)
        cur-opts (->> (.-options select)
                      (map #(.-value %))
                      set)]
    (doseq [opt options]
      (when-not (contains? cur-opts (.-value opt))
        (js/console.log "Adding option:" opt "to select" id)
        (.appendChild select opt)))
    (set! (.-onchange select) onchange-fn))
  options)

(defn load-ui! []
  (p/->> (fetch-edn "sets.edn")
         (map :name)
         (map mk-option)
         (populate-select! "#set-select" select-set!)))

(load-ui!)

(comment

  (load-ui!)

  (defn reset-kingdom! []
    (remove-children! "#victory")
    (remove-children! "#treasure")
    (remove-children! "#supply"))
  ;; => #'dominjure/reset-kingdom!

  (do
    (reset-kingdom!)
    (select-set!))

  (->> (:cards @game-state)
       (select-cards (fn [{:keys [kingdom types]}]
                       (and (:victory types)
                            (not kingdom)))))
  ;; => ({:kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"} {:kingdom false, :alt "Duchy.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/4/4a/Duchy.jpg/200px-Duchy.jpg", :title "Duchy", :types #{:victory}, :vp 3, :cost 5, :set "Base", :height "322"} {:kingdom false, :alt "Province.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/8/81/Province.jpg/200px-Province.jpg", :title "Province", :types #{:victory}, :vp 6, :cost 8, :set "Base", :height "320"})

  (def card {:kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"})

  (card-img card)
  ;; => "img/Base/200px-Estate.jpg"

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

  (remove-children! "#p1-hand")

  (dotimes [_ 5]
    (place-card! {:set "Base", :src "Card_back.jpg"} "#p1-hand"))
  (dotimes [_ 5]
    (place-card! {:set "Base", :src "Card_back.jpg"} "#p2-hand"))

  (-> (get-in @game-state [:cards "Copper"])
      (place-card! "#p1-discard"))
  (-> (get-in @game-state [:cards "Copper"])
      (place-card! "#p2-discard"))

  (set-styles! "#p1" {:gap "5px"})
  (set-styles! "#p1-deck" {:width "10%"})
  (set-styles! "#p1-discard" {:width "10%"})
  (set-styles! "#p1-play-area" {:width "40%"})
  (set-styles! "#p1-hand" {:width "40%"})

  (set-styles! "#p2" {:display "flex", :justify-content "space-between", :gap "5px"})
  (set-styles! "#p2-deck" {:width "10%"})
  (set-styles! "#p2-discard" {:width "10%"})
  (set-styles! "#p2-play-area" {:width "40%"})
  (set-styles! "#p2-hand" {:display "flex", :width "40%", :gap "2px"})

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

  (-> (get-el "#p2-hand > div")
      (.addEventListener "click" #(log "clicked")))

  (set! (.-innerHTML (get-el "#p2-turn")) "Play actions")

  (set-styles! "#p2-turn" {:display "flex"
                           :justify-content "end"})

  (let [el (js/document.createElement "button")]
    (set! (.-innerHTML el) "End actions")
    (.appendChild (get-el "#p2-turn") el))

  )
