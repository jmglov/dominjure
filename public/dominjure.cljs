(ns dominjure
  (:require [clojure.string :as str]
            [promesa.core :as p]))

(def game-state
  (atom
   {:supply []
    :cards nil}))

(defn log [msg obj]
  (if (sequential? obj)
    (doseq [o obj]
      (js/console.log msg o))
    (js/console.log msg obj))
  obj)

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

(defn add-card-to-supply! [card div-or-id]
  (let [div (if (string? div-or-id) (js/document.querySelector div-or-id) div-or-id)
        img (js/document.createElement "img")]
    (set! (.-src img) (card-img card))
    (set! (.-alt img) (:title card))
    (.appendChild div img)))

(defn populate-supply! [cards]
  (let [parent-div (js/document.querySelector "#supply")]
    (doseq [row (partition-all 5 cards)]
      (let [div (js/document.createElement "div")]
        (.appendChild parent-div div)
        (doseq [card row]
          (add-card-to-supply! card div))))))

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

(defn start-game! [cards]
  (swap! game-state assoc :cards cards)
  (let [victory (select-cards (fn [{:keys [kingdom types]}]
                                 (and (:victory types)
                                      (not kingdom)))
                              cards)
        treasure (select-cards (fn [{:keys [kingdom types]}]
                                 (and (:treasure types)
                                      (not kingdom)))
                               cards)
        kingdom (->> cards
                     (select-cards :kingdom 10))]
    (println "Treasure:" (map :title treasure))
    (println "Victory:" (map :title victory))
    (println "Kingdom:" (map :title kingdom))
    (swap! game-state assoc :selected-cards
           {:victory victory
            :treasure treasure
            :kingdom kingdom})
    (doseq [card victory]
      (add-card-to-supply! card "#victory"))
    (doseq [card treasure]
      (add-card-to-supply! card "#treasure"))
    (populate-supply! kingdom)))

(defn select-set! []
  (let [set-name (.-value (js/document.querySelector "#set-select"))]
    (when-not (= "--" set-name)
      (log "Loading set:" set-name)
      (p/->> (fetch-edn (str "sets/" set-name ".edn"))
             start-game!))))

(defn populate-select! [id onchange-fn options]
  (let [select (js/document.querySelector id)
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

  (defn remove-children! [id]
    (let [el (js/document.querySelector id)]
      (while (.-firstChild el)
        (.removeChild el (.-lastChild el)))))
  ;; => #'dominjure/remove-children!

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
    (add-card-to-supply! card "#victory"))

  (reset-kingdom!)

  (:selected-cards @game-state)
  ;; => {:victory ({:kingdom false, :alt "Province.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/8/81/Province.jpg/200px-Province.jpg", :title "Province", :types #{:victory}, :vp 6, :cost 8, :set "Base", :height "320"} {:kingdom false, :alt "Duchy.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/4/4a/Duchy.jpg/200px-Duchy.jpg", :title "Duchy", :types #{:victory}, :vp 3, :cost 5, :set "Base", :height "322"} {:kingdom false, :alt "Estate.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/9/91/Estate.jpg/200px-Estate.jpg", :title "Estate", :types #{:victory}, :vp 1, :cost 2, :set "Base", :height "322"}), :treasure ({:kingdom false, :alt "Gold.jpg", :value 3, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/5/50/Gold.jpg/200px-Gold.jpg", :title "Gold", :types #{:treasure}, :cost 6, :set "Base", :height "319"} {:kingdom false, :alt "Silver.jpg", :value 2, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/5/5d/Silver.jpg/200px-Silver.jpg", :title "Silver", :types #{:treasure}, :cost 3, :set "Base", :height "320"} {:kingdom false, :alt "Copper.jpg", :value 1, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/f/fb/Copper.jpg/200px-Copper.jpg", :title "Copper", :types #{:treasure}, :cost 0, :set "Base", :height "322"}), :kingdom ({:kingdom true, :alt "Artisan.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/1/1d/Artisan.jpg/200px-Artisan.jpg", :title "Artisan", :types #{:action}, :cost 6, :set "Base", :height "320", :text "Gain a card to your hand costing up to $5. Put a card from your hand onto your deck."} {:kingdom true, :alt "Council Room.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/e/e0/Council_Room.jpg/200px-Council_Room.jpg", :title "Council Room", :types #{:action}, :buys 1, :cards 4, :cost 5, :set "Base", :height "320", :text "Each other player draws a card."} {:kingdom true, :alt "Militia.jpg", :value 2, :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/a/a0/Militia.jpg/200px-Militia.jpg", :title "Militia", :types #{:action :attack}, :cost 4, :set "Base", :height "320", :text "Each other player discards down to 3 cards in hand."} {:kingdom true, :alt "Throne Room.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/d/d1/Throne_Room.jpg/200px-Throne_Room.jpg", :title "Throne Room", :types #{:action}, :cost 4, :set "Base", :height "320", :text "You may play an Action card from your hand twice."} {:kingdom true, :alt "Gardens.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/8/8c/Gardens.jpg/200px-Gardens.jpg", :title "Gardens", :types #{:victory}, :vp {:vp 1, :cards 10}, :cost 4, :set "Base", :height "319", :text "Worth 1 VP per 10 cards you have (round down)."} {:kingdom true, :alt "Smithy.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/3/36/Smithy.jpg/200px-Smithy.jpg", :title "Smithy", :types #{:action}, :cards 3, :cost 4, :set "Base", :height "321", :text ""} {:kingdom true, :alt "Workshop.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/5/50/Workshop.jpg/200px-Workshop.jpg", :title "Workshop", :types #{:action}, :cost 3, :set "Base", :height "321", :text "Gain a card costing up to $4."} {:kingdom true, :alt "Village.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/5/5a/Village.jpg/200px-Village.jpg", :actions 2, :title "Village", :types #{:action}, :cards 1, :cost 3, :set "Base", :height "321", :text ""} {:kingdom true, :alt "Merchant.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/7/78/Merchant.jpg/200px-Merchant.jpg", :actions 1, :title "Merchant", :types #{:action}, :cards 1, :cost 3, :set "Base", :height "321", :text "The first time you play a Silver this turn, +$1."} {:kingdom true, :alt "Chapel.jpg", :width "200", :src "https://wiki.dominionstrategy.com/images/thumb/2/29/Chapel.jpg/200px-Chapel.jpg", :title "Chapel", :types #{:action}, :cost 2, :set "Base", :height "320", :text "Trash up to 4 cards from your hand."})}

  (-> @game-state
      :selected-cards
      :kingdom
      populate-supply!)

  (->> @game-state
       :cards
       vals
       (remove :set)
       (map :title))
  ;; => ("Market")

  )
