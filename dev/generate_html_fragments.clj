(ns generate-html-fragments
  "This script writes test/File.edn files, backing docstring-related Elisp tests."
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [orchard.java])
  (:import
   (java.io File)))

;; Ensure that it is requireable - if Orchard internally falls back to other ns, this script won't work properly:
(require 'orchard.java.parser-next)

(defn -main [& _]
  (doseq [class-symbol [`Thread `String `Object `File 'java.util.Map]
          :let [{:keys [members] :as x} (orchard.java/source-info class-symbol)
                members (->> members vals (map vals) (reduce into))
                all (conj members x)
                filename (str "../test" File/separator (-> class-symbol eval .getSimpleName) ".edn")]]
    (-> filename io/file .delete)
    (with-open [w (io/writer filename :append true)]
      (.write w "[")
      (doseq [{:keys [doc-fragments doc-first-sentence-fragments doc-block-tags-fragments] :as i} all
              :when (or (seq doc-fragments)
                        (seq doc-first-sentence-fragments)
                        (seq doc-block-tags-fragments))]
        (.write w (with-out-str
                    (pprint (select-keys i [:doc-fragments
                                            :doc-first-sentence-fragments
                                            :doc-block-tags-fragments]))))
        (.write w "\n"))
      (.write w "]"))
    (println "Processed" (-> filename io/file str)))
  (shutdown-agents))
