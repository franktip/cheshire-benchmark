(ns cclient.core
  (:gen-class)
  (:use [clojure.test]
        [clojure.java.io :only [file reader]])
  (:require [cheshire.core :as json]
            [cheshire.exact :as json-exact]
            [cheshire.generate :as gen]
            [cheshire.factory :as fact]
            [cheshire.parse :as parse])
  (:import (com.fasterxml.jackson.core JsonGenerationException)
           (java.io FileInputStream StringReader StringWriter
                    BufferedReader BufferedWriter)
           (java.sql Timestamp)
           (java.util Date UUID)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println "cclient: start");
  (println "---------------------------------------");

(deftest test-uuid
  (let [id (UUID/randomUUID)
        id-str (str id)]
    (println (json/decode (json/encode {:foo id})))))

(test-uuid)

  (println "---------------------------------------");

(def test-obj {"int" 3 "long" (long -2147483647) "boolean" true
               "LongObj" (Long/parseLong "2147483647") "double" 1.23
               "nil" nil "string" "string" "vec" [1 2 3] "map" {"a" "b"}
               "list" (list "a" "b") "short" (short 21) "byte" (byte 3)})

(deftest t-ratio
  (let [n 1/2]
    (println (double n) (:num (json/decode (json/encode {:num n}) true)))))

(deftest t-long-wrap-around
  (println 2147483648 (json/decode (json/encode 2147483648))))

(deftest t-bigint
  (let [n 9223372036854775808]
    (println (:num (json/decode (json/encode {:num n}) true)))))

(deftest t-biginteger
  (let [n (BigInteger. "42")]
    (println (:num (json/decode (json/encode {:num n}) true)))))

(deftest t-bigdecimal
  (let [n (BigDecimal. "42.5")]
    (is (= (.doubleValue n) (:num (json/decode (json/encode {:num n}) true))))
    (binding [parse/*use-bigdecimals?* true]
      (println (:num (json/decode (json/encode {:num n}) true))))))

(deftest test-string-round-trip
  (println test-obj (json/decode (json/encode test-obj))))

(deftest test-generate-accepts-float
  (println (json/encode 3.14)))

(deftest test-keyword-encode
  (println
         (json/decode (json/encode {:key "val"}))))

(deftest test-generate-set
  (println 
         (json/decode (json/encode {"set" #{"a" "b"}}))))

(deftest test-generate-empty-set
  (println 
         (json/decode (json/encode {"set" #{}}))))

(deftest test-generate-empty-array
  (println 
         (json/decode (json/encode {"array" []}))))

(deftest test-key-coercion
  (println
         (json/decode
          (json/encode
           {:foo "bar" 1 "bat" (long 2) "bang" (bigint 3) "biz"}))))

(deftest test-keywords
  (println 
         (json/decode (json/encode {:foo "bar" :bat 1}) true)))

(deftest test-symbols
  (println
         (json/decode (json/encode {"foo" 'clojure.core/map}))))

(deftest test-accepts-java-map
  (println
         (json/decode
          (json/encode (doto (java.util.HashMap.) (.put "foo" 1))))))

(deftest test-accepts-java-list
  (println
         (json/decode (json/encode (doto (java.util.ArrayList. 3)
                                     (.add 1)
                                     (.add 2)
                                     (.add 3))))))

(deftest test-accepts-java-set
  (println 
         (json/decode (json/encode {"set" (doto (java.util.HashSet. 3)
                                            (.add 1)
                                            (.add 2)
                                            (.add 3))}))))

(deftest test-accepts-empty-java-set
  (println
         (json/decode (json/encode {"set" (java.util.HashSet. 3)}))))

(deftest test-nil
  (println (json/decode nil true)))

(deftest test-parsed-seq
  (let [br (BufferedReader. (StringReader. "1\n2\n3\n"))]
    (println (json/parsed-seq br))))

(deftest test-smile-round-trip
  (println (json/parse-smile (json/generate-smile test-obj))))

(def bin-obj {"byte-array" (byte-array (map byte [1 2 3]))})

(deftest test-round-trip-binary
  (for [[p g] {json/parse-string json/generate-string
               json/parse-smile  json/generate-smile
               json/parse-cbor   json/generate-cbor}]
    (is (let [roundtripped (p (g bin-obj))]
          ;; test value equality
             (println (get "byte-array") seq)))))

(deftest test-aliases
  (println
         (json/decode
          (json/encode
           {:foo "bar" 1 "bat" (long 2) "bang" (bigint 3) "biz"}))))

(deftest test-date
  (println 
         (json/decode (json/encode {:foo (Date. (long 0))})))
  (println
         (json/decode (json/encode {:foo (Date. (long 0))}
                                   {:date-format "yyyy-MM-dd"})))
      )

(deftest test-sql-timestamp
  (println
         (json/decode (json/encode {:foo (Timestamp. (long 0))})))
  (println
         (json/decode (json/encode {:foo (Timestamp. (long 0))}
                                   {:date-format "yyyy-MM-dd"})))
      )

(deftest test-uuid
  (let [id (UUID/randomUUID)
        id-str (str id)]
    (println (json/decode (json/encode {:foo id})))))

(deftest test-char-literal
  (println (json/encode {:foo \a})))

(deftest test-streams
  (println 
         (json/parse-stream
          (BufferedReader. (StringReader. "{\"foo\":\"bar\"}\n"))))
  (let [sw (StringWriter.)
        bw (BufferedWriter. sw)]
    (json/generate-stream {"foo" "bar"} bw)
    (println (.toString sw))
  (println 
         (with-open [rdr (StringReader. "{\"foo baz\":\"bar\"}\n")]
           (json/parse-stream rdr true)))))

(deftest serial-writing
  (println
         (.toString
          (json/with-writer [(StringWriter.) nil]
            (json/write [] :start)
            (json/write "foo")
            (json/write "bar")
            (json/write [] :end))))
  (println
         (.toString
          (json/with-writer [(StringWriter.) nil]
            (json/write [1 [2]] :start-inner)
            (json/write 3)
            (json/write [] :end)
            (json/write 4)
            (json/write [] :end))))
  (println 
         (.toString
          (json/with-writer [(StringWriter.) nil]
            (json/write {:a 1} :start)
            (json/write {:b 2} :bare)
            (json/write {:c 3} :end))))
  (println 
         (.toString
          (json/with-writer [(StringWriter.) nil]
            (json/write ["start"] :start)
            (json/write "continue")
            (json/write ["implicitly-nested"])
            (json/write ["explicitly-nested"] :all)
            (json/write ["flatten"] :bare)
            (json/write ["end"] :end))))
  (println
         (.toString
          (json/with-writer [(StringWriter.) nil]
            (json/write {:head "head info" :data []} :start-inner)
            (json/write 1)
            (json/write 2)
            (json/write 3)
            (json/write [] :end)
            (json/write {:tail "tail info"} :end)))))

(deftest test-multiple-objs-in-file
  (println
         (first (json/parsed-seq (reader "./multi.json"))))
  (println 
         (second (json/parsed-seq (reader "./multi.json"))))
  (with-open [s (FileInputStream. (file "./multi.json"))]
    (let [r (reader s)]
      (println
             (json/parsed-seq r)))))

(deftest test-jsondotorg-pass1
  (let [string (slurp "./pass1.json")
        decoded-json (json/decode string)
        encoded-json (json/encode decoded-json)
        re-decoded-json (json/decode encoded-json)]
    (println re-decoded-json)))

(deftest test-namespaced-keywords
  (println 
         (json/encode {:foo :user/bar}))
  (println 
         (json/decode (json/encode {:foo/bar :baz/eggplant}) true)))

(deftest test-array-coerce-fn
  (is (= {"set" #{"a" "b"} "array" ["a" "b"] "map" {"a" 1}}
         (json/decode
          (json/encode {"set" #{"a" "b"} "array" ["a" "b"] "map" {"a" 1}}) false
          (fn [field-name] (println field-name) #{} [])))))

(deftest t-symbol-encoding-for-non-resolvable-symbols
  (println
         (json/encode (sorted-map :foo 'clojure.core/map :bar 'clojure.core/pam)))
  (println 
         (json/encode (sorted-map :foo 'foo.bar/baz :bar 'clojure.core/pam))))

(deftest t-bindable-factories
  (binding [fact/*json-factory* (fact/make-json-factory
                                 {:allow-non-numeric-numbers true})]
    (println
           (type (:foo (json/decode "{\"foo\":NaN}" true))))))

(deftest t-bindable-factories-quoteless
  (binding [fact/*json-factory* (fact/make-json-factory
                                  {:quote-field-names false})]
    (println (json/encode {:a 1}))))

(deftest t-persistent-queue
  (let [q (conj (clojure.lang.PersistentQueue/EMPTY) 1 2 3)]
    (println (json/decode (json/encode q)))))

(deftest t-pretty-print
  (println
         (json/encode (sorted-map :foo 1 :bar [{:baz 2} :quux [1 2 3]])
                      {:pretty true})))

(deftest t-pretty-print-illegal-argument
  ; just expecting this not to throw
  (println (json/encode {:foo "bar"}
               {:pretty []}))
  (println (json/encode {:foo "bar"}
               {:pretty nil})))

(deftest t-custom-pretty-print-with-defaults
  (let [test-obj (sorted-map :foo 1 :bar {:baz [{:ulu "mulu"} {:moot "foo"} 3]} :quux :blub)
        pretty-str-default (json/encode test-obj {:pretty true})
        pretty-str-custom (json/encode test-obj {:pretty {}})]
    (is (= pretty-str-default pretty-str-custom))
    (when-not (= pretty-str-default pretty-str-custom)
      ; print for easy comparison
      (println "; default pretty print")
      (println pretty-str-default)
      (println "; custom pretty print with default options")
      (println pretty-str-custom))))

(deftest t-custom-pretty-print-with-non-defaults
  (let [test-obj (sorted-map :foo 1 :bar {:baz [{:ulu "mulu"} {:moot "foo"} 3]} :quux :blub)
        test-opts {:pretty {:indentation 4
                            :indent-arrays? false
                            :before-array-values ""
                            :after-array-values ""
                            :object-field-value-separator ": "}}
        expected (str "{\n"
                      "    \"bar\": {\n"
                      "        \"baz\": [{\n"
                      "            \"ulu\": \"mulu\"\n"
                      "        }, {\n"
                      "            \"moot\": \"foo\"\n"
                      "        }, 3]\n"
                      "    },\n"
                      "    \"foo\": 1,\n"
                      "    \"quux\": \"blub\"\n"
                      "}")
        pretty-str (json/encode test-obj test-opts)]

    ; just to be easy on the eyes in case of error
    (when-not (= expected pretty-str)
      (println "; pretty print with options - actual")
      (println pretty-str)
      (println "; pretty print with options - expected")
      (println expected))
    (println pretty-str)))

(deftest t-custom-pretty-print-with-noident-objects
  (let [test-obj  [{:foo 1 :bar 2} {:foo 3 :bar 4}]
        test-opts {:pretty {:indent-objects? false}}
        expected (str "[ { \"foo\" : 1, \"bar\" : 2 }, "
                      "{ \"foo\" : 3, \"bar\" : 4 } ]")
        pretty-str (json/encode test-obj test-opts)]
    ; just to be easy on the eyes in case of error
    (when-not (= expected pretty-str)
      (println "; pretty print with options - actual")
      (println pretty-str)
      (println "; pretty print with options - expected")
      (println expected))
    (println pretty-str)))

(deftest t-unicode-escaping
  (println
         (json/encode {:foo "It costs £100"} {:escape-non-ascii true})))

(deftest t-custom-keyword-fn
  (println (json/decode "{\"foo\": \"bar\"}"
                                   (fn [k] (keyword (.toUpperCase k)))))
  (println (json/decode "{\"foo\": \"bar\"}" nil))
  (println (json/decode "{\"foo\": \"bar\"}" false))
  (println (json/decode "{\"foo\": \"bar\"}" true)))

(deftest t-custom-encode-key-fn
  (println 
         (json/encode {:foo :bar}
                      {:key-fn (fn [k] (.toUpperCase (name k)))})))

(deftest test-add-remove-encoder
  (gen/remove-encoder java.net.URL)
  (gen/add-encoder java.net.URL gen/encode-str)
  (print
         (json/encode (java.net.URL. "http://foo.com")))
  (gen/remove-encoder java.net.URL)
  (is (thrown? JsonGenerationException
               (json/encode (java.net.URL. "http://foo.com")))))

(deftest t-float-encoding
  (println (json/encode {:foo (float 0.01)})))

(deftest t-non-const-bools
  (println (json/decode "{\"a\": 1}" (Boolean. true))))

(deftest t-invalid-json
  (let [invalid-json-message "Invalid JSON, expected exactly one parseable object but multiple objects were found"]
    (are [x y] (= x (try
                      y
                      (catch Exception e
                        (.getMessage e))))
      invalid-json-message (json-exact/decode "{\"foo\": 1}asdf")
      invalid-json-message (json-exact/decode "{\"foo\": 123}null")
      invalid-json-message (json-exact/decode  "\"hello\" : 123}")
      {"foo" 1} (json/decode "{\"foo\": 1}")
      invalid-json-message (json-exact/decode-strict "{\"foo\": 1}asdf")
      invalid-json-message (json-exact/decode-strict "{\"foo\": 123}null")
      invalid-json-message (json-exact/decode-strict  "\"hello\" : 123}")
      {"foo" 1} (json/decode-strict "{\"foo\": 1}"))))

(t-ratio)
(t-long-wrap-around)
(t-bigint)
(t-biginteger)
(t-bigdecimal)
(test-string-round-trip)
(test-generate-accepts-float)
(test-keyword-encode)
(test-generate-set)
(test-generate-empty-set)
(test-generate-empty-array)
(test-key-coercion)
(test-keywords)
(test-symbols)
(test-accepts-java-map)
(test-accepts-java-list)
(test-accepts-java-set)
(test-accepts-empty-java-set)
(test-nil)
(test-parsed-seq)
(test-smile-round-trip)
(test-round-trip-binary)
(test-aliases)
(test-date)
(test-sql-timestamp)
(test-uuid)
(test-char-literal)
(test-streams)
(serial-writing)
(test-multiple-objs-in-file)
(test-jsondotorg-pass1)
(test-namespaced-keywords)
(test-array-coerce-fn)
(t-symbol-encoding-for-non-resolvable-symbols)
(t-bindable-factories)
(t-bindable-factories-quoteless)
(t-persistent-queue)
(t-pretty-print)
(t-pretty-print-illegal-argument)
(t-custom-pretty-print-with-defaults)
(t-custom-pretty-print-with-non-defaults)
(t-custom-pretty-print-with-noident-objects)
(t-unicode-escaping)
(t-custom-keyword-fn)
(t-custom-encode-key-fn)
(test-add-remove-encoder)
(t-float-encoding)
(t-non-const-bools)
(t-invalid-json)

; these two tests don't run for some reason..
;
;(defprotocol TestP
;  (foo [this] "foo method"))
;
;(defrecord TestR [state])
;
;(extend TestR
;  TestP
;  {:foo (constantly "bar")})
;
;(deftest t-custom-protocol-encoder
;  (let [rec (TestR. :quux)]
;    (is (= {:state "quux"} (json/decode (json/encode rec) true)))
;    (gen/add-encoder cheshire.test.core.TestR
;                     (fn [obj jg]
;                       (.writeString jg (foo obj))))
;    (is (= "bar" (json/decode (json/encode rec))))
;    (gen/remove-encoder cheshire.test.core.TestR)
;    (is (= {:state "quux"} (json/decode (json/encode rec) true)))))
;
;(defprotocol CTestP
;  (thing [this] "thing method"))
;(defrecord CTestR [state])
;(extend CTestR
;  CTestP
;  {:thing (constantly "thing")})
;
;(deftest t-custom-helpers
;  (let [thing (CTestR. :state)
;        remove #(gen/remove-encoder CTestR)]
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-nil nil jg)))
;    (is (= nil (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-str "foo" jg)))
;    (is (= "foo" (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-number 5 jg)))
;    (is (= 5 (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-long 4 jg)))
;    (is (= 4 (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-int 3 jg)))
;    (is (= 3 (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-ratio 1/2 jg)))
;    (is (= 0.5 (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-seq [:foo :bar] jg)))
;    (is (= ["foo" "bar"] (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-date (Date. (long 0)) jg)))
;    (binding [gen/*date-format* "yyyy-MM-dd'T'HH:mm:ss'Z'"]
;      (is (= "1970-01-01T00:00:00Z" (json/decode (json/encode thing) true))))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-bool true jg)))
;    (is (= true (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-named :foo jg)))
;    (is (= "foo" (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-map {:foo "bar"} jg)))
;    (is (= {:foo "bar"} (json/decode (json/encode thing) true)))
;    (remove)
;    (gen/add-encoder CTestR (fn [obj jg] (gen/encode-symbol 'foo jg)))
;    (is (= "foo" (json/decode (json/encode thing) true)))
;    (remove)))

  (println "---------------------------------------");
  (println "cclient: done"))
