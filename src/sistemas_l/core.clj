(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn read-file [file]
  "Recibe un archivo por parametro, si existe, lee su contenido y escribe como un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn gen-patron [axioma remp it]
  (if (zero? it) axioma
                 (gen-patron (apply str (sequence (replace remp (vec axioma)))) remp (dec it))
                 )
  )

(defn hash-create [remp]
  "Recibe un vector con los remplazos (remp) y lo separa en un hash, con key el caracter asociado al remplazo
  y value el remplazo en si"
  (if (empty? remp) {}
                    (let [key (first (seq (apply str (first remp))))
                          value (apply str (nnext (seq (apply str (first remp)))))
                          ]
                     (merge (hash-map key value) (hash-create (vec (rest remp)))))
                    )
  )

(defn gen-texto-svg [angulo patron])

(defn write-file [outputFile texto]
  )

(defn -main [inputFile it outputFile]
  "Recibe el archio .sl, la cantidad de iteraciones a realizar y el archivo .xml donde se escribe el resultado"
  (let [info (read-file inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (nnext info))) it)
        ]
    (write-file outputFile (gen-texto-svg angulo patron))
    ))
