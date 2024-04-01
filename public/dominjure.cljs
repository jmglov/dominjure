(ns dominjure
  (:require [promesa.core :as p]))

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

(defn populate-select! [id options]
  (let [select (js/document.querySelector id)
        cur-opts (->> (.-options select)
                      (map #(.-value %))
                      set)]
    (doseq [opt options]
      (when-not (contains? cur-opts (.-value opt))
        (js/console.log "Adding option:" opt "to select" id)
        (.appendChild select opt))))
  options)

(comment

  (p/->> (fetch-edn "sets.edn")
         (map :name)
         (map mk-option)
         (log "Created option")
         (populate-select! "#set-select"))


  )
