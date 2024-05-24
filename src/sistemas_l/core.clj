(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn read-file [file]
  "Recibe un archivo por parametro, si existe, lee su contenido y escribe como un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn gen-patron [axioma reemp it]
  (if (zero? it)
    axioma
    (gen-patron (apply str (sequence (replace reemp (vec axioma)))) reemp (dec it))
    )
  )

(defn hash-create [remp]
  (println remp)
  (if (empty? remp) {}
                   (let [key (first (seq (apply str (first remp))))
                         value (apply str (nnext (seq (apply str (first remp)))))
                         ]

                     (merge (hash-map key value) (hash-create (vec (rest remp)))))
    )
  )

(defn gen-texto-svg [angulo patron]
  (println angulo)
  patron
  )

(defn write-file [outputFile texto]
  (println texto)
  )

(defn -main [inputFile it outputFile]
  (let [info (read-file inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (drop 2 info))) it)
        ]
    (write-file outputFile (gen-texto-svg angulo patron))
    ))
