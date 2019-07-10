(ns card-game-war.game)

(require '[clojure.pprint :refer [pprint]])

(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(defrecord Card [suit rank])

(def cards
  (for [suit suits
        rank ranks]
    (Card. suit rank)))

(def rank-ordering
  (into {} (map-indexed #(-> [%2 %1]) ranks)))

(def cards-ordering
  (->> cards
       (map (fn [card] (vector card (-> card :rank rank-ordering))))
       (into {})))

(defn val-of [card]
  (get cards-ordering card))

(defn deal []
  (partition 26 (shuffle cards)))

;(defn compare [card-a card-b]
;  )

(defn play-round [player1-card player2-card])

(defn play-game [player1-cards player2-cards])


