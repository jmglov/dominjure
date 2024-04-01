(ns fetcher
  (:require [babashka.http-client :as http]
            [babashka.fs :as fs]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.zip :as zip]))

(defn img-link? [z]
  (let [node (zip/node z)]
    (and (associative? node)
         (= (:tag node) :a)
         (contains? (:attrs node) :title)
         (zip/down z)
         (let [child-node (zip/node (zip/down z))]
           (and (associative? child-node)
                (= (:tag child-node) :img))))))

(defn download-file! [base-dir url]
  (let [filename (fs/file-name url)
        {:keys [body]} (http/get url {:as :stream})]
    (io/copy body (fs/file base-dir filename))))

(defn download-image! [base-dir {:keys [src] :as image}]
  (download-file! base-dir src)
  image)

(defn ->image [base-url link-node]
  (-> link-node :content first :attrs
      (assoc :title (get-in link-node [:attrs :title]))
      (update :src #(str base-url %))))

(defn ->images [base-url xml]
  (->> xml
       (iterate zip/next)
       (take-while (complement zip/end?))
       (filter img-link?)
       (map (comp (partial ->image base-url) zip/node))))

(defn get-xml [base-url path]
  (-> (str base-url path)
      http/get
      :body
      xml/parse-str
      zip/xml-zip))

(defn extract-images [base-url path]
  (->images (get-xml base-url path)))

(defn write-edn! [base-dir expansion-name images]
  (pprint/pprint (->> images
                      (map (fn [{:keys [title] :as image}]
                             [title image]))
                      (into {}))
                 (io/writer (fs/file base-dir (format "%s.edn" expansion-name)))))

(comment

  (def base-url "https://wiki.dominionstrategy.com")
  ;; => #'fetcher/base-url

  (def path "/index.php/Dominion_(Base_Set)")
  ;; => #'fetcher/path

  (def xml (get-xml base-url path))
  ;; => #'fetcher/xml

  (write-edn! "/tmp" "Base" (->images base-url xml))
  ;; => nil

  (->> (->images base-url xml)
       (map (fn [{:keys [src]}]
              (let [filename (fs/file-name src)]
                (fs/move (fs/file "/tmp" filename)
                         (fs/file "/home/jmglov/Documents/code/dominjure/public/img/Base"
                                  filename))))))

  )
