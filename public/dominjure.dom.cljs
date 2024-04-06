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

(defn mk-element
  ([el-type]
   (mk-element el-type nil))
  ([el-type inner-html]
   (let [el (js/document.createElement el-type)]
     (when inner-html
       (set! (.-innerHTML el) inner-html))
     el)))

(defn mk-button [text click-handler]
  (let [button (mk-element "button" text)]
    (when click-handler
      (.addEventListener button "click" click-handler))
    button))

(defn set-styles! [selector styles]
  (let [el (get-el selector)]
    (set! (.-style el)
          (->> (map (fn [[k v]] (str (name k) ": " v)) styles)
               (str/join "; ")))
    el))

(defn append-children! [id children]
  (let [el (get-el id)]
    (doseq [child children]
      (.appendChild el child))
    el))

(defn remove-children! [id]
  (let [el (get-el id)]
    (while (.-firstChild el)
      (.removeChild el (.-lastChild el)))
    el))

(defn set-children! [id children]
  (-> (remove-children! id)
      (append-children! children)))
