(ns cc-paintbot.bot
  (:require [org.httpkit.client :as http]
            [clojure.string :as str])
  (:import (java.net URLEncoder URLDecoder)))

(def url (or (System/getenv "PAINTBOTS_URL")
             "http://localhost:31173"))

(def integer-fields #{:x :y})

;; Bot interface to server
(defn- post [& {:as args}]
  (let [_ (println "args " args)
        {:keys [status body headers] :as _resp} @(http/post url {:form-params args :as :text})
        _ (println "status " _resp)]
    (if (>= status 400)
      (throw (ex-info "Unexpected status code" {:status status :body body}))
      (if (= (:content-type headers) "application/x-www-form-urlencoded")
        (into {}
              (for [field (str/split body #"&")
                    :let [[k v] (str/split field #"=")
                          kw (keyword (URLDecoder/decode k))
                          v (URLDecoder/decode v)]]
                [kw (if (integer-fields kw)
                      (Integer/parseInt v)
                      v)]))
        body))))

(defn register [name]
  {:name name :id (post :register name)})

(defn move [bot dir]
  (merge
    bot
    (post :id (:id bot) :move dir)))

(defn paint [bot]
  (merge bot (post :id (:id bot) :paint "1")))

(defn color [bot c]
  (merge bot (post :id (:id bot) :color c)))

(defn- rotate [dir]
  (case dir
    "LEFT" "DOWN"
    "DOWN" "RIGHT"
    "RIGHT" "UP"
    "UP" "LEFT"))

(defn dragon [c]
  ;; concatenate rotated c to c
  ;; add marker between iterations to change color
  (into (conj c :change-col)
        (map rotate)
        (remove #(= % :change-col) (reverse c))))

(def ^:const palette "23489abcef")

(defn draw [bot n c]
  (let [curve (nth (iterate dragon ["LEFT"]) n)]
    (loop [bot bot
           [dir & curve] curve

           ;; start with a random color in palette
           colors (drop (rand-int 10) (cycle palette))]
      (cond
        (nil? dir) :done

        (= :change-col dir)
        (recur (color bot (str (first colors)))
               curve
               (rest colors))
        :else
        (recur (reduce (fn [bot _]
                         (-> bot paint (move dir)))
                       bot (range c))
               curve colors)))))

(defn move-to [bot to-x to-y]
  (let [dx (- (:x bot) to-x)
        dy (- (:y bot) to-y)

        ;; move randomly either x or y (to look cool ;)
        r (rand-int 2)]
    (cond
      (and (zero? dx) (zero? dy))
      bot

      (and (= 0 r) (not= 0 dx))
      (recur (move bot (if (neg? dx) "RIGHT" "LEFT")) to-x to-y)

      (and (= 1 r) (not= 0 dy))
      (recur (move bot (if (neg? dy) "DOWN" "UP")) to-x to-y)

      :else
      (recur bot to-x to-y))))

(defn -main [& args]
  (-> (register (str "dragon" (System/currentTimeMillis)))
      (move "LEFT")
      (move-to (+ 75 (rand-int 10)) (+ 45 (rand-int 10)))
      (draw 9 4)))