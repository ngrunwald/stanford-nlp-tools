(ns stanford-nlp-tools.pipeline
  (:use [stanford-nlp-tools.core])
  (:require [clojure.string :as str])
  (:import [edu.stanford.nlp.ling TaggedWord]
           [edu.stanford.nlp.util Timing]
           [edu.stanford.nlp.pipeline
            Annotation TokensRegexAnnotator$Options
            PTBTokenizerAnnotator WordsToSentencesAnnotator
            MorphaAnnotator]
           [edu.stanford.nlp.tagger.maxent MaxentTagger]
           [edu.stanford.nlp.ling.tokensregex
            TokenSequencePattern
            CoreMapExpressionExtractor]
           [java.util ArrayList Collection]))

(defmacro with-timing
  [id & body]
  `(if (empty? ~id)
     (do ~@body)
     (let [t# (Timing.)]
       (.doing t# (str "Beginning operation " (name ~id)))
       (let [return# (do ~@body)]
         (.stop t# "done!")
         return#))))

(defn make-pipeline
  [{:keys [verbose]} specs]
  (let [annotators (for [[fun conf] specs]
                     (fun conf))
        pipeline (apply comp (reverse annotators))
        full-pipeline (if verbose
                        (fn [doc] (with-timing "pipeline" (pipeline doc)))
                        pipeline)]
    full-pipeline))

;;
;; Tokenizers
;;

(defn make-ptb-tokenizer
  [{:keys [options verbose] :as conf
    :or {:verbose false}}]
  (let [tokenizer (if options
                    (PTBTokenizerAnnotator. false options)
                    (PTBTokenizerAnnotator. false))]
    (fn [text-or-document]
      (with-timing (if verbose "ptb-tokenization")
        (let [document (if (string? text-or-document)
                         (Annotation. text-or-document)
                         text-or-document)]
          (.annotate tokenizer document)
          document)))))

;;
;; Sentence Splitter
;;

(defn make-sentence-splitter
  [{:keys [verbose] :as conf
    :or {:verbose false}}]
  (let [splitter (WordsToSentencesAnnotator. false)]
    (fn [^Annotation document]
      (with-timing (if verbose "sentence-splitting")
        (.annotate splitter document)
        document))))

;;
;; MAXENT POS Tagger
;;

(defn tag-sentence!
  [^MaxentTagger model ^ArrayList tokens]
  (let [words (.apply model tokens)]
    (doseq [idx (range 0 (dec (count tokens)))
            :let [token (.get tokens idx)
                  ^TaggedWord word (.get words idx)]]
      (assoc-ts! token :part-of-speech (.tag word)))))

(defn tag-tokens!
  ([model cmap max-length]
     (let [^ArrayList all-tokens (get-ts cmap :tokens)
           text-size (count all-tokens)]
       (if (< text-size max-length)
         (tag-sentence! model all-tokens)
         (loop [idx 0
                ^ArrayList chunk (ArrayList.)]
           (if (< idx text-size)
             (let [token (.get all-tokens idx)]
               (.add chunk token)
               (if (= max-length (count chunk))
                 (do
                   ;; add a token... find out why
                   (.add chunk (.get all-tokens (inc idx)))
                   (tag-sentence! model chunk)
                   (recur (inc idx) (ArrayList.)))
                 (recur (inc idx) chunk)))
             (if-not (empty? chunk)
               (tag-sentence! model chunk)))))
       nil)))

(defn make-pos-tagger
  [conf]
  (let [^String model-path (get conf :model (get conf :pos.model))
        max-length (get conf :max-length (get conf :pos.maxlen 200))
        verbose (get conf :verbose)
        model (MaxentTagger. model-path)]
    (fn [^Annotation document]
      (with-timing (if verbose (str "pos-tagging with model " model-path))
        (if-let [sentences (get-ts document :sentences)]
          (doseq [sentence sentences]
            (tag-tokens! model sentence max-length))
          (tag-tokens! model document max-length)))
      document)))

;;
;; Morphology
;;

(defn make-english-lemmatizer
  [{:keys [verbose] :as conf
    :or {:verbose false}}]
  (let [lemmatizer (MorphaAnnotator. false)]
    (fn [^Annotation document]
      (with-timing (if verbose "morphology processing")
        (.annotate lemmatizer document)
        document))))

;;
;; Tokens Regexp
;;

(defn add-token-offsets!
  [annotation]
  (let [^ArrayList tokens (get-ts annotation :tokens)]
    (doseq [idx (range 0 (dec (count tokens)))]
      (assoc-ts! (.get tokens idx) :token-begin idx :token-end idx))
    annotation))

(defn apply-tokens-regexp!
  [^CoreMapExpressionExtractor extractor cmap]
  (let [cms (.extractCoreMapsMergedWithTokens extractor cmap)]
    (assoc-ts! cmap :tokens cms)))

(defn make-tokens-regexp
  [{:keys [rules verbose add-token-offsets]}]
  (let [^Collection rules-paths (if (coll? rules)
                                  rules
                                  [rules])
        env (TokenSequencePattern/getNewEnv)
        extractor (CoreMapExpressionExtractor/createExtractorFromFiles
                   env (ArrayList. rules-paths))
        options (TokensRegexAnnotator$Options.)]
    (.bind env "options" options)
    (fn [document]
      (with-timing
        (if verbose
          (str "regexp token extraction with rules " (str/join ", " rules-paths)))
        (if-let [sentences (get-ts document :sentences)]
          (doseq [sentence sentences]
            (if add-token-offsets
              (add-token-offsets! sentence))
            (apply-tokens-regexp! extractor sentence))
          (do
            (if add-token-offsets
              (add-token-offsets! document))
            (apply-tokens-regexp! extractor document))))
      document)))