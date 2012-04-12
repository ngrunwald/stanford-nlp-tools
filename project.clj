(defproject stanford-nlp-tools "0.1.0-SNAPSHOT"
  :description "Clojure wrapper around the Stanford NLP tools"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [edu.stanford.nlp/stanford-postagger "3.1.1"]]
  :repositories {"local" ~(str (.toURI (java.io.File. "mvn_repo")))})