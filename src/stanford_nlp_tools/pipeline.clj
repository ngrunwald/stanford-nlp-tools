(ns stanford-nlp-tools.pipeline
  (:use [stanford-nlp-tools.core])
  (:require [clojure.string :as str])
  (:import [edu.stanford.nlp.ling TaggedWord]
           [edu.stanford.nlp.util Timing]
           [edu.stanford.nlp.pipeline Annotation]
           [edu.stanford.nlp.tagger.maxent MaxentTagger]))

(defmacro with-timing
  [id & body]
  `(if (empty? ~id)
     ~@body
     (let [t (Timing.)]
       (.doing t (str "Beginning operation " (name id)))
       ~@body
       (.done t))))

(defn make-pipeline
  [specs]
  (let [annotators (for [[fun conf] specs]
                     (fun conf))
        pipeline (apply comp annotators)]
    pipeline))

(defn make-pos-tagger
  [conf]
  (let [model-path (get conf :model-path (get conf :pos.model))
        max-length (get conf :max-length (get conf :pos.maxlen))
        verbose  (get conf :verbose)
        model (MaxentTagger. model-path)]
    (fn [document]
      (with-timing (if verbose (str "pos-tagging with model " model-path))
        (let [tokens (get-ts ann :tokens)
              words (.apply model tokens)]
          (doseq [idx (range 0 (dec (count tokens)))
                  :let [token (.get tokens idx)
                        ^TaggedWord word (.get words idx)]]
            (assoc-ts! token :part-of-speech (.tag word)))
          document)))))