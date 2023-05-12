(ns cc-paintbot.bot
  (:require [org.httpkit.client :as http]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math :as math])
  (:import (java.net URLEncoder URLDecoder)))

(def url (or (System/getenv "PAINTBOTS_URL")
             ;;"http://172.24.245.53:31173"
             "http://localhost:31173"))

(def integer-fields #{:x :y})

(def bot-name "chachachaa")

;; Bot interface to server
(defn- post [& {:as args}]
  (let [{:keys [status body headers] :as _resp} @(http/post url {:form-params args :as :text})]
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

(defn- get-bot [name]
  (if (.exists (io/as-file (str name ".edn")))
    (edn/read-string (slurp (str name ".edn")))
    (do (->> (register (str name (System/currentTimeMillis)))
             prn-str
             (spit (str name ".edn")))
        (get-bot name))))

(defn move [bot dir]
  (merge
    bot
    (post :id (:id bot) :move dir)))

(defn paint [bot]
  (merge bot (post :id (:id bot) :paint "1")))

(defn color [bot c]
  (merge bot (post :id (:id bot) :color c)))

(defn say [msg & [bot name]]
  (let [bot (or bot (get-bot name))]
    (post :id (:id bot) :msg msg)))

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
        (nil? dir) bot

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

        ;; move randomly either x or y (to look cool ;)L
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

(defn line-to [bot to-x to-y]
  (let [dx (- (:x bot) to-x)
        dy (- (:y bot) to-y)

        ;; move randomly either x or y (to look cool ;)L
        r (rand-int 2)]
    (cond
      (and (zero? dx) (zero? dy))
      bot

      (and (= 0 r) (not= 0 dx))
      (recur (-> bot paint (move (if (neg? dx) "RIGHT" "LEFT"))) to-x to-y)

      (and (= 1 r) (not= 0 dy))
      (recur (-> bot paint (move (if (neg? dy) "DOWN" "UP"))) to-x to-y)

      :else
      (recur bot to-x to-y))))

;; x1 = r * cos(angle * PI / 180);
(defn circle [bot r]
  (let [sx (math/round (+ (:x bot) (-> (* 1 math/PI) (/ 18) math/cos (* r))))
        sy (math/round (+ (:y bot) (-> (* 1 math/PI) (/ 18) math/sin (* r))))]
    (loop [bot bot
           i 1]
      (if (= i 37)
        bot
        (recur (let [x (math/round (+ (:x bot) (-> (* i math/PI) (/ 18) math/cos (* r))))
                     y (math/round (+ (:y bot) (-> (* i math/PI) (/ 18) math/sin (* r))))]
                 (if (= i 36)
                   (-> bot paint (line-to sx sy))
                   (-> bot paint (line-to x y))))
               (inc i))))))

(defn masterpiece [bot]
  (-> bot
      (move "RIGHT")
      (move-to 50 50)
      ;(move-to (+ 75 (rand-int 10)) (+ 45 (rand-int 10)))
      (draw 9 4)))

(defn cha [bot]
  (-> bot
      (color "b")
      (move-to 90 50)
      (circle 1)
      (move-to 100 42)
      (circle 1)
      (move-to 110 36)
      (circle 1)
      (move-to 120 34)
      (circle 1)
      (move-to 130 36)
      (circle 1)
      (move-to 140 42)
      (circle 1)
      (move-to 150 50)
      (circle 1)

      ;;Head
      (move-to 120 15)
      (color "a")
      (circle 1.7)

      (color 1)
      (move-to 118 15)
      (line-to 118 18)
      (move "RIGHT")
      (line-to 119 19)
      (move "RIGHT")
      (line-to 120 17)
      (move "RIGHT")
      (line-to 121 17)
      (move "RIGHT")
      (line-to 122 19)
      ))

(defn save-bot-state [bot name]
  (spit (str name ".edn") (prn-str bot))
  :done)

(defn -main [& [args]]
  (-> (get-bot (or (:name args) bot-name))
      (cha)
      ;(move-to 20 20)
      ;(circle 3)
      (save-bot-state (or (:name args) bot-name))))