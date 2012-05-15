(ns stanford-nlp-tools.postagger
  (:import [edu.stanford.nlp.tagger.maxent MaxentTagger TaggerConfig]
           [edu.stanford.nlp.ling HasWord Word]
           [java.util Properties])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^{:dynamic true} *tagger*)

(defmacro with-pos-tagger
  [model & body]
  `(let [tagger# (make-pos-tagger ~model)]
     (binding [*tagger* tagger#]
       ~@body)))

(defn make-pos-tagger
  [model & [config]]
  (if config
    (MaxentTagger. model (TaggerConfig.
                          (doto (Properties.)
                            (.load (io/reader config)))))
    (MaxentTagger. model)))

(defprotocol Wordable
  (make-word [word] "creates a Word"))

(extend-protocol Wordable
  String
  (make-word [word] (Word. word))
  HasWord
  (make-word [word] word)
  clojure.lang.Associative
  (make-word [{:keys [word start end] :or {start -1 end -1}}] (Word. word start end)))

(defn pos-tag
  ([tagger toks]
     (let [ws (map #(array-map :word (.word %) :tag (.tag %)
                               :start (.beginPosition %)
                               :end (.endPosition %))
                   (.tagSentence tagger (map make-word toks)))]
       (if (map? (first toks))
         (loop [[h-tok & rest-tok] toks
                [h-w & rest-w] ws
                sent (transient [])]
           (if h-tok
             (recur rest-tok rest-w (conj! sent (merge h-w h-tok)))
             (persistent! sent)))
         ws)))
  ([toks] (pos-tag *tagger* toks)))
