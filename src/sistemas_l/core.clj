(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn read-file [file]
  "Recibe un archivo por parametro, si existe, lee su contenido y escribe como un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn gen-patron [axioma remplazos it]
  "Se recibe el axioma y como se va remplazado"
  )

(defn -main [inputFile it outputFile]
  (let [
        info (read-file inputFile)
        patron (gen-patron (first (rest info)) (vec (rest (rest info))) it)
        ]
    (print info)))