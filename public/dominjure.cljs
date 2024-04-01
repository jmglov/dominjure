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
        img-div (js/document.createElement "div")
        img (js/document.createElement "img")]
    (.appendChild div img-div)
    (set! (.-src img) (card-img card))
    (set! (.-alt img) (:title card))
    (.appendChild img-div img)))

(defn populate-supply! [cards]
  (let [parent-div (js/document.querySelector "#supply")]
    (doseq [row (partition-all 5 cards)]
      (let [div (js/document.createElement "div")]
        (.appendChild parent-div div)
        (doseq [card (reverse row)]
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

  (->> (:selected-cards @game-state)
       :kingdom
       (partition-all 5)
       (map #(map (juxt :title :cost) %)))
  ;; => ((["Council Room" 5] ["Witch" 5] ["Festival" 5] ["Mine" 5] ["Market" 5]) (["Moneylender" 4] ["Remodel" 4] ["Harbinger" 3] ["Vassal" 3] ["Moat" 2]))

  (remove-children! "#board")
  (select-set!)

  (-> @game-state
      :selected-cards
      :kingdom
      populate-supply!)

  )
