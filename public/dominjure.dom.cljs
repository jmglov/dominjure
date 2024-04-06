(ns dominjure.dom
  (:require [clojure.string :as str]))

(defn get-el [selector]
  (if (instance? js/HTMLElement selector)
    selector  ; already an element; just return it
    (js/document.querySelector selector)))

(defn mk-option
  ([text]
   (mk-option text text))
  ([text value]
   (let [opt (js/document.createElement "option")]
     (set! (.-text opt) text)
     (when value
       (set! (.-value opt) value))
     opt)))

(defn set-styles! [selector styles]
  (let [el (get-el selector)]
    (set! (.-style el)
          (->> (map (fn [[k v]] (str (name k) ": " v)) styles)
               (str/join "; ")))
    el))

(defn remove-children! [id]
  (let [el (get-el id)]
    (while (.-firstChild el)
      (.removeChild el (.-lastChild el)))
    el))
