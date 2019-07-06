(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))


;; Additional tests

(deftest test-extend-encrypting-key
  (testing "does it extend key properly"
    (let [message "Tom has a big house"
          exp-key "abcabcabcabcabcabca"]
      (is (= exp-key
             (extend-key "abc" (.length message)))))))

(deftest test-finding-repeating-str
  (testing "repeating str"
    (is (= "scones" (find-repeating-str "sconessconessconessco")))
    (is (= "a" (find-repeating-str "aaaaaaaaaaaaaaaa")))
    (is (= "abcdefghijkl" (find-repeating-str "abcdefghijkl")))))


;; Original tests

(deftest test-encode
  (testing "can encode a message with a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode a message given an encoded message and a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))

(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))
    (is (= "abcabcx"
           (decipher "hfnlphoontutufa" "hellofromrussia")))))
