(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.math :as math]))

(def LAMBDA 10)
(def MOVE-PLUMA #{\F \G \f \g})
(def ROTAR-PLUMA #{\+ \- \|})
(def PILA-TORTUGA #{\[ \]})

(defn read-file! [file]
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

(defn parse [n]
  "Recibe un numero n y lo parsea de tal modo que sea un float
  con 4 decimales despues de la coma"
  (let [n-str (str (math/copy-sign n 0))]
    (if (clojure.string/includes? n-str "E") 0.0000 (float (with-precision 4 (/ n 1M))))
    )
  )

(defn generar-coordenada [x y angulo]
  "Recibe dos coordenadas y un angulo. A partir de eso genera un punto"
  (let [u (+ (* LAMBDA (math/sin (math/to-radians angulo))) x)
        v (+ (* LAMBDA (math/cos (math/to-radians angulo))) y)
        ]
    (hash-map :x u :y v)
    )
  )

(defn gen-svg [punto simbolo]
  "Recibe un punto y un simbolo. Con esto genera un texto del tipo M x y ó L x y"
  (apply str (concat (str simbolo) " " (str (get punto :x)) " " (str (get punto :y))))
  )

(defn tortuga-create [x y angulo]
  "Crea una Tortuga donde guarda en un hash-map de donde empezo a dibujar, un hash-map
  que guarda donde esta y el angulo con el que tiene que dibujar"
  (hash-map :x x :y y :angulo angulo)
  )

(defn tortuga-rotar [tortuga rotar angulo-default]
  "Recibe una tortuga y un simbolo que indica hacia donde rotar"
  (if (= rotar \+)
    (update tortuga :angulo + angulo-default)
    (update tortuga :angulo - angulo-default)
    )
  )

(defn gen-text [patron pila-tortuga angulo-default texto]
  (if (empty? patron)
    texto
    (let [simbolo (first patron)
          tortuga (first pila-tortuga)
          rest-patron (rest patron)
          rest-pila (rest pila-tortuga)
          ]
      (cond
        (contains? MOVE-PLUMA simbolo) (let [new-punto (generar-coordenada (get tortuga :x) (get tortuga :y) (get tortuga :angulo))
                                             new-tortuga (tortuga-create (get new-punto :x) (get new-punto :y) (get tortuga :angulo))
                                             text-svg (gen-svg new-punto (if (contains? #{\F \G} simbolo) \L \M))
                                             ]
                                         (gen-text rest-patron (cons new-tortuga rest-pila) angulo-default (apply str (concat texto " " text-svg))))
        (contains? ROTAR-PLUMA simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= \| simbolo) 180 angulo-default))]
                                          (gen-text rest-patron (cons new-tortuga rest-pila) angulo-default texto))
        (= \[ simbolo) (gen-text rest-patron (cons tortuga pila-tortuga) angulo-default texto)
        (and (not (empty? rest-pila)) (= simbolo \])) (let [text-svg (gen-svg (first rest-pila) \M)]
                                                        (gen-text rest-patron rest-pila angulo-default (apply str (concat texto " " text-svg ))))
        :else (gen-text rest-pila pila-tortuga angulo-default texto)
        )
      )
    )
  )

(defn write-file! [outputFile text]
  (println text)
  )

(defn -main [inputFile it outputFile]
  "Recibe el archio .sl, la cantidad de iteraciones a realizar y el archivo .xml donde se escribe el resultado"
  (let [info (read-file! inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (nnext info))) it)
        pila-tortuga (list (tortuga-create 0 0 0))
        text-svg (apply str (concat "M 0 0 "(gen-text (seq patron) pila-tortuga angulo " ")))
        ]
    (write-file! outputFile text-svg)
    )
  )