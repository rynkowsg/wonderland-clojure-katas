(ns alphabet-cipher.coder
  (:require
    [clojure.pprint :refer [pprint]]))


;;; Helper methods

(defn ->index [char]
  """ Converts char to index """
  (-> char int (- (int \a))))

(defn ->char [index]
  """ Converts index to char """
  (-> (int \a) (+ index) char))

(defn extend-key [keyword length]
  """ Adjusts the key for given length.
  In most cases it extends the key, but if msg is shorter, the key is trimmed.
  """
  (let [x-factor (-> (quot length (.length keyword)) inc)
        repeated-key (apply str (repeat x-factor keyword))
        correct-key (subs repeated-key 0 length)]
    correct-key))

(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn find-repeating-str [repeated-key-str]
  (let [strlen (.length repeated-key-str)]
    (loop [l 1]
      (if (= l strlen)
        ;; if none of the smaller substrings was ok, it mean that the whole string is the key
        repeated-key-str
        ;; otherwise check is the sample l-length key is the one
        (let [sample-key (subs repeated-key-str 0 l)
              repeated-sample-key-str (extend-key sample-key strlen)]
            (if (= repeated-key-str repeated-sample-key-str)
              sample-key
              (recur (inc l))))))))


;;; Encoder & decoder

(defprotocol Encoder
  (encode-char [this msg-char key-char]))

(defprotocol Decoder
  (decode-char [this msg-char key-char]))

(defprotocol KeyCracker
  (find-key-char [this msg encoded-msg]))

(defrecord Chart [array2d]
  ;; vertical axis represent original character
  ;; horizontal axis represent key
  ;; the value in the chart represent decoded character
  Encoder
  (encode-char [_ msg-char key-char]
    """ Given original char and key character, it returns encrypted char. """
    (aget array2d (->index msg-char) (->index key-char)))
  Decoder
  (decode-char [_ encoded-msg-char key-char]
    """ Given encrypted char and key char, it returns original char. """
    """ Looks for x index (horizontal axis that represent key) that matches
    given encrypted character given the key."""
    (let [key-index (->index key-char)
          matching-key-char (fn [index row]
                              (if (= encoded-msg-char (aget row key-index))
                                index))
          matching-row (->> array2d
                            (keep-indexed matching-key-char)
                            first)
          decoded-char (->char matching-row)]
      decoded-char))
  KeyCracker
  (find-key-char [_ msg-char encoded-msg-char]
    """ Given original char and encrypted char, it return key char. """
    """ original key - vertical axis
        encoded key - value
        key char - horizontal axis """
    (let [row (aget array2d (->index msg-char))
          key-char-index (index-of encoded-msg-char row)
          key-char (->char key-char-index)]
      key-char)))


;;; Matrix generator

(def alphabet-length (-> (- (int \z) (int \a)) inc))

(defn indices->char [y x]
  """ Calculates the value of chart for the given coordinates """
  (let [res-index (mod (+ x y) alphabet-length)]
    (->char res-index)))

(defn create-matrix [size]
  """ Creates matrix of letters for given size """
  (let [array2d (to-array-2d (repeat size (make-array Character/TYPE size)))]
    (doseq [y (range size)
            x (range size)]
      (aset array2d y x (indices->char y x)))
    array2d))

(def chart (->Chart (create-matrix alphabet-length)))


;;; Final methods

(defn encode [enc-key msg]
  (let [len (.length msg)
        ext-enc-key (extend-key enc-key len)
        encoded-seq (for [i (range len)
                          :let [msg-char (get msg i)
                                key-char (get ext-enc-key i)]]
                      (encode-char chart msg-char key-char))
        encoded-str (apply str encoded-seq)]
    encoded-str))

(defn decode [dec-key msg]
  (let [len (.length msg)
        ext-dec-key (extend-key dec-key len)
        decoded-seq (for [i (range len)
                          :let [msg-char (get msg i)
                                key-char (get ext-dec-key i)]]
                      (decode-char chart msg-char key-char))
        decoded-str (apply str decoded-seq)]
    decoded-str))

(defn decipher [cipher msg]
  (let [len (.length msg)
        repeated-key-seq (-> (for [i (range len)
                               :let [msg-char (get msg i)
                                     cipher-char (get cipher i)]]
                               (find-key-char chart msg-char cipher-char))
                             seq)
        repeated-key-str (apply str repeated-key-seq)
        key-str (find-repeating-str repeated-key-str)
        ]
    key-str))
