(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.math :as math]))

(def LAMBDA 10)
(def MOVE-PLUMA #{\F \G \f \g})
(def ROTAR-PLUMA #{\+ \- \|})
(def APILAR \[)
(def DESAPILAR \])

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

(defn generar-punto [x y angulo]
  "Recibe dos coordenadas y un angulo. A partir de eso genera un punto"
  (let [u (+ (* LAMBDA (math/cos (math/to-radians (- angulo 90)))) x)
        v (+ (* LAMBDA (math/sin (math/to-radians (- angulo 90)))) y)
        ]
    (hash-map :x u :y v)
    )
  )

(defn gen-path [punto simbolo]
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

(defn gen-text [patron pila-tortuga angulo-default]
  (if (empty? patron) ""
    (let [simbolo (first patron)
          tortuga (first pila-tortuga)
          rest-patron (rest patron)
          rest-pila (rest pila-tortuga)
          ]
      (cond
        (contains? MOVE-PLUMA simbolo) (let [new-punto (generar-punto (get tortuga :x) (get tortuga :y) (get tortuga :angulo))
                                             new-tortuga (tortuga-create (get new-punto :x) (get new-punto :y) (get tortuga :angulo))
                                             text-svg (gen-path new-punto (if (contains? #{\F \G} simbolo) \L \M))
                                             ]
                                         (apply str (concat text-svg " " (gen-text rest-patron (cons new-tortuga rest-pila) angulo-default))))

        (contains? ROTAR-PLUMA simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= \| simbolo) 180 angulo-default))]
                                          (gen-text rest-patron (cons new-tortuga rest-pila) angulo-default))

        (= APILAR simbolo) (gen-text rest-patron (cons tortuga pila-tortuga) angulo-default)

        (and (not (empty? rest-pila)) (= simbolo DESAPILAR)) (let [text-svg (gen-path (first rest-pila) \M)]
                                                        (apply str (concat text-svg " " (gen-text rest-patron rest-pila angulo-default))))
        :else (gen-text rest-patron pila-tortuga angulo-default)
        )
      )
    )
  )

(defn write-file! [outputFile text]
  (println text)
  )

(defn -main [inputFile it outputFile]
  "Recibe el archivo.sl, la cantidad de iteraciones a realizar y el archivo .xml donde se escribe el resultado"
  (let [info (read-file! inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (nnext info))) it)
        pila-tortuga (list (tortuga-create 0 0 0))
        text-svg (apply str (concat "M 0 0 "(gen-text (seq patron) pila-tortuga angulo)))
        ]
    (println patron)
    (write-file! outputFile text-svg)
    )
  )