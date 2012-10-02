(ns stanford-nlp-tools.core
  (:require [clojure.string :as str])
  (:import [edu.stanford.nlp.ling
            CoreAnnotations]
           [edu.stanford.nlp.pipeline
            StanfordCoreNLP
            Annotation
            Annotator]
           [edu.stanford.nlp.ling.tokensregex.types
            Tags
            Tags$TagsAnnotation]
           [edu.stanford.nlp.util TypesafeMap]
           [java.util Properties]))

(defn- handle-case
  [w]
  (if (re-find #"^\p{Upper}\p{Lower}" w)
    (str/lower-case w)
    w))

(defn- class->kw
  [^Class klass]
  (let [nam (.getName klass)
        ann-name (let [[parent & all] (str/split nam #"\$")]
                   (if all
                     (last all)
                     parent))
        ann-type (str/replace ann-name #"Annotation$" "")
        access (str/join "-"
                         (map handle-case
                              (str/split (str/replace ann-type #"(\p{Lower})(\p{Upper})" "$1-$2") #"[\-_]")))]
    (keyword access)))

(defn- reverse-map
  [m]
  (apply hash-map
        (interleave (vals m)
                    (keys m))))

(def kw->annotations
  (let [classes (.getDeclaredClasses CoreAnnotations)
        others [Tags$TagsAnnotation]
        anns (concat classes others)
        ks (map class->kw anns)]
    (apply hash-map (interleave ks anns))))

(def annotations->kw
  (reverse-map kw->annotations))

(defn get-ts
  ([^TypesafeMap ts-map k default]
     (if-let [klass (get kw->annotations k)]
       (or (.get ts-map klass) default)))
  ([ts-map k] (get-ts ts-map k nil)))

(defn keys-ts
  [^TypesafeMap ts-map]
  (map #(get annotations->kw % %) (.keySet ts-map)))

(defn count-ts
  [^TypesafeMap ts-map]
  (.size ts-map))

(defn assoc-ts!
  [^TypesafeMap ts-map k v & kvs]
  (.set ts-map (get kw->annotations k k) v)
  (if (empty? kvs)
    ts-map
    (let [[nk nv & others] kvs]
      (recur ts-map nk nv others))))

(defn dissoc-ts!
  [^TypesafeMap ts-map k & ks]
  (.remove ts-map (get kw->annotations k k))
  (if (empty? ks)
    ts-map
    (recur ts-map (first ks) (rest ks))))

(defn contains-ts?
  [^TypesafeMap ts-map k]
  (.has (get kw->annotations k k)))

(defn get-tags
  [tok]
  (if-let [^Tags ts (get-ts tok :tags)]
    (into #{} (.getTags ts))
    #{}))
