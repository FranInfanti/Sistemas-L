(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.math :as math]))

(def lambda 10)
(def move-pluma #{\F \G \f \g})
(def dibujar #{\F \G})
(def rotar-pluma #{\+ \- \|})
(def rotar-derecha \+)
(def rotar-180 \|)
(def apilar \[)
(def desapilar \])
(def angulo-corrector 90)
(def up-pluma \M)
(def down-pluma \L)
(def svg "<svg viewBox=\"%viewbox\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"%svg\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/></svg>")
(def replace-viewbox #"%viewbox")
(def replace-text #"%svg")

(defn read-file! [file]
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn hash-create [remplazos]
  (if (empty? remplazos)
    {}
    (let [key (first (seq (first remplazos)))
          value (apply str (nnext (seq (first remplazos))))]
      (merge (hash-map key value) (hash-create (rest remplazos))))))

(defn gen-patron [axioma remp it]
  (if (zero? it) axioma (recur (apply str (sequence (replace remp (vec axioma)))) remp (dec it))))

(defn datos-create [text] (hash-map :xmin 0 :ymin 0 :xmax 0 :ymax 0 :text text))

(defn new-datos [datos tortuga text-svg]
  (hash-map
    :xmin (min (get datos :xmin) (get tortuga :x))
    :ymin (min (get datos :ymin) (get tortuga :y))
    :xmax (max (get datos :xmax) (get tortuga :x))
    :ymax (max (get datos :ymax) (get tortuga :y))
    :text (str (get datos :text) " " text-svg)))

(defn gen-nueva-pos [tortuga]
  (let [u (+ (* lambda (math/cos (math/to-radians (- (get tortuga :angulo) angulo-corrector)))) (get tortuga :x))
        v (+ (* lambda (math/sin (math/to-radians (- (get tortuga :angulo) angulo-corrector)))) (get tortuga :y))]
    (hash-map :x u, :y v, :angulo (get tortuga :angulo))))

(defn gen-svg [tortuga simbolo] (clojure.string/join " " [simbolo (get tortuga :x) (get tortuga :y)]))

(defn tortuga-rotar [tortuga rotar angulo-default]
  (if (= rotar rotar-derecha) (update tortuga :angulo + angulo-default) (update tortuga :angulo - angulo-default)))

(defn gen-text [patron pila-tortuga angulo-default datos]
  (if (empty? patron)
    datos
    (let [simbolo (first patron)
          tortuga (peek pila-tortuga)
          rest-pila (pop pila-tortuga)]
      (cond
        (contains? move-pluma simbolo) (let [new-tortuga (gen-nueva-pos tortuga)
                                             text-svg (gen-svg new-tortuga (if (contains? dibujar simbolo) down-pluma up-pluma))]
                                         (recur (rest patron) (conj rest-pila new-tortuga) angulo-default (new-datos datos new-tortuga text-svg)))
        (contains? rotar-pluma simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= rotar-180 simbolo) 180 angulo-default))]
                                          (recur (rest patron) (conj rest-pila new-tortuga) angulo-default datos))
        (= apilar simbolo) (recur (rest patron) (conj pila-tortuga tortuga) angulo-default datos)
        (= desapilar simbolo) (recur (rest patron) rest-pila angulo-default (new-datos datos tortuga (gen-svg (peek rest-pila) up-pluma)))
        :else (recur (rest patron) pila-tortuga angulo-default datos)))))

(defn calcular-extremos [extremo x y max]
  (if (nil? max) (+ extremo (double (/ (abs (- x y)) lambda))) (- extremo (double (/ (abs (- x y)) lambda)))))

(defn gen-viewbox [datos]
  (let [x-min (calcular-extremos (get datos :xmin) (get datos :xmax) (get datos :xmin) 0)
        y-min (calcular-extremos (get datos :ymin) (get datos :ymax) (get datos :ymin) 0)
        x-max (calcular-extremos (get datos :xmax) (get datos :xmax) (get datos :xmin) nil)
        y-max (calcular-extremos (get datos :ymax) (get datos :ymax) (get datos :ymin) nil)
        ancho (abs (- x-max x-min))
        alto (abs (- y-max y-min))]
    (clojure.string/join " " [x-min y-min ancho alto])))

(defn format-svg [viewbox text-svg]
  (let [final-svg svg] (-> final-svg (clojure.string/replace replace-viewbox viewbox) (clojure.string/replace replace-text text-svg))))

(defn write-file! [outputFile text]
  (try
    (with-open [w (io/writer outputFile)] (.write w (str text)))
    (catch Exception _)))

(defn -main [inputFile it outputFile]
  (let [info (read-file! inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (nnext info)) it)
        pila-tortuga (list (hash-map :x 0 :y 0 :angulo 0))
        text-svg (gen-text patron pila-tortuga angulo (datos-create (gen-svg (peek pila-tortuga) up-pluma)))]
    (write-file! outputFile (format-svg (gen-viewbox text-svg) (get text-svg :text)))))