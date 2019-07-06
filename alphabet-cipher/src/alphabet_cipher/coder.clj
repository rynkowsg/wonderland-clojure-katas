(ns alphabet-cipher.coder)

;;; Helper definitions

(def ^:private a-z
  (let [from (int \a)
        to   (inc (int \z))]
    (map char (range from to))))

(def ^:private substitution-chart
  (let [->c+rotated (fn [index c]
                      [c (->> (cycle a-z) (drop index) (take 26) ;; rotate a-z
                              (zipmap a-z))])]                   ;; zip keyword chars with cipher values (rotated a-z)
    (->> (map-indexed ->c+rotated a-z)
         (reduce (fn [lookup [key val]] (assoc lookup key val)) {}))))


;;; Helper functions

(defn find-repeating-seq [repeated-key]
  (let [len (count repeated-key)]
    (loop [l 1]
      (if (= l len)
        ;; if none of the smaller substrings was ok, it mean that the whole string is the key
        repeated-key
        ;; otherwise check is the sample l-length key is the one
        (let [candidate-key (take l repeated-key)
              repeated-candidate-key (take len (cycle candidate-key))]
          (if (= repeated-key repeated-candidate-key)
            candidate-key          ;; if repeated-candidate-key is equal to repeated-key we found it
            (recur (inc l))))))))  ;; otherwise check longer one


;;; Final methods

(defn encode [keyword message]
  (let [full-keyword (take (.length message) (cycle keyword))]
    (->> (map vector message full-keyword)
         (map #(get-in substitution-chart %))
         (apply str))))

(defn decode [keyword message]
  (let [full-keyword (take (.length message) (cycle keyword))]
    (->> (map vector full-keyword message)                  ;; for each pair (keyword-char, decipher-char)
         (map (fn [[keyword-char decipher-char]]            ;; (find the row with matching decipher-char for keyword-char)
                (->> (seq substitution-chart)               ;; transform chart to seq of key-values
                     (filter (fn [[key value]]              ;; find item that for the given keyword-char contains decipher-char
                               (= (get value keyword-char) decipher-char)))
                     first                                  ;; take first item of lazy seq
                     first)))                               ;; get key from the key value pair
         (apply str))))

(defn decipher [cipher message]
  (->> (map vector message cipher)
       (map (fn [[msg-char decipher-char]]
              (->> substitution-chart
                  (#(get % msg-char))
                  (keep #(when (= (val %) decipher-char) (key %))))))
       flatten
       find-repeating-seq
       (apply str)))
