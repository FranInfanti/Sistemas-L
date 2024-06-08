(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.math :as math]))

(def LAMBDA 10)
(def MOVE-PLUMA #{\F \G \f \g})
(def DIBUJAR #{\F \G})
(def ROTAR-PLUMA #{\+ \- \|})
(def ROTAR-DERECHA \+)
(def ROTAR-180 \|)
(def APILAR \[)
(def DESAPLIAR \])
(def ANGULO-CORRECTOR 90)
(def UP-PLUMA \M)
(def DOWN-PLUMA \L)
(def PUNTO-INICIAL "M 0 0 ")
(def SVG "<svg viewBox=\"%viewbox\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"%svg\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/></svg>")
(def REPLACE-VIEWBOX #"%viewbox")
(def REPLACE-TEXT #"%svg")

(defn read-file! [file]
  "Recibe un archivo por parametro, si existe, lee su contenido y lo escribe como un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn gen-patron [axioma remp it]
  "Recibe un axioma, los remplazos posibles de cada caracter del axioma y la cantidad de iteraciónes a realizar"
  (if (zero? it) axioma (recur (apply str (sequence (replace remp (vec axioma)))) remp (dec it))))

(defn hash-create [remplazos]
  "Recibe un vector con los remplazos y lo separa en un hash, con :key el caracter asociado al remplazo
  y value el remplazo en si"
  (if (empty? remplazos)
    {}
    (let [key (first (seq (apply str (first remplazos))))
          value (apply str (nnext (seq (apply str (first remplazos)))))]
      (merge (hash-map key value) (hash-create (vec (rest remplazos)))))))

(defn gen-coordenada [x y angulo]
  "Recibe dos coordenadas (x,y) y un angulo. A partir de eso genera un punto"
  (let [u (+ (* LAMBDA (math/cos (math/to-radians (- angulo ANGULO-CORRECTOR)))) x)
        v (+ (* LAMBDA (math/sin (math/to-radians (- angulo ANGULO-CORRECTOR)))) y)]
    (hash-map :x u, :y v)))

(defn gen-svg [tortuga simbolo]
  "Recibe un punto y un simbolo. Con esto genera un texto del tipo 'M x y' o 'L x y'"
  (str simbolo " " (get tortuga :x) " " (get tortuga :y)))

(defn tortuga-create [x y angulo]
  "Crea una Tortuga en la que guarda, en un hash-map, de donde empezo a dibujar, donde esta y el angulo con el que tiene que dibujar"
  (hash-map :x x, :y y, :angulo angulo))

(defn tortuga-rotar [tortuga rotar angulo-default]
  "Recibe una tortuga y un simbolo que indica hacia donde rotar"
  (if (= rotar ROTAR-DERECHA) (update tortuga :angulo + angulo-default) (update tortuga :angulo - angulo-default)))

(defn datos-create [text] (hash-map :xmin 0 :ymin 0 :xmax 0 :ymax 0 :text text))

(defn new-datos [datos tortuga text-svg]
  (hash-map
    :xmin (min (get datos :xmin) (get tortuga :x))
    :ymin (min (get datos :ymin) (get tortuga :y))
    :xmax (max (get datos :xmax) (get tortuga :x))
    :ymax (max (get datos :ymax) (get tortuga :y))
    :text (str (get datos :text) " " text-svg)))

(defn gen-text [patron pila-tortuga angulo-default datos]
  (if (empty? patron)
    datos
    (let [simbolo (first patron)
          tortuga (peek pila-tortuga)
          rest-pila (pop pila-tortuga)]
      (cond
        (contains? MOVE-PLUMA simbolo) (let [new-punto (gen-coordenada (get tortuga :x) (get tortuga :y) (get tortuga :angulo))
                                             new-tortuga (tortuga-create (get new-punto :x) (get new-punto :y) (get tortuga :angulo))
                                             text-svg (gen-svg new-punto (if (contains? DIBUJAR simbolo) DOWN-PLUMA UP-PLUMA))]
                                         (recur (rest patron) (conj rest-pila new-tortuga) angulo-default (new-datos datos new-tortuga text-svg)))

        (contains? ROTAR-PLUMA simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= ROTAR-180 simbolo) 180 angulo-default))]
                                          (recur (rest patron) (conj rest-pila new-tortuga) angulo-default datos))

        (= APILAR simbolo) (recur (rest patron) (conj pila-tortuga tortuga) angulo-default datos)

        (= DESAPLIAR simbolo) (recur (rest patron) rest-pila angulo-default (new-datos datos tortuga (gen-svg (peek rest-pila) UP-PLUMA)))

        :else (recur (rest patron) pila-tortuga angulo-default datos)))))

(defn calcular-extremos [extremo x y max]
  (if (nil? max) (+ extremo (double (/ (abs (- x y)) LAMBDA))) (- extremo (double (/ (abs (- x y)) LAMBDA)))))

(defn gen-viewbox [datos]
  "Caclula los parametros del viewbox para que la imagen se vea como corresponde"
  (let [x-min (calcular-extremos (get datos :xmin) (get datos :xmax) (get datos :xmin) 0)
        y-min (calcular-extremos (get datos :ymin) (get datos :ymax) (get datos :ymin) 0)
        x-max (calcular-extremos (get datos :xmax) (get datos :xmax) (get datos :xmin) nil)
        y-max (calcular-extremos (get datos :ymax) (get datos :ymax) (get datos :ymin) nil)
        ancho (abs (- x-max x-min))
        alto (abs (- y-max y-min))]
    (str x-min " " y-min " " ancho " " alto)))

(defn format-svg [viewbox text-svg]
  "Genera el texto svg completo para poder ya escribirlo en el archivo"
  (let [final-svg SVG] (-> final-svg (clojure.string/replace REPLACE-VIEWBOX viewbox) (clojure.string/replace REPLACE-TEXT text-svg))))

(defn write-file! [outputFile text]
  "Recibe el archivo de salida y el texto a escribir en este archivo"
  (try
    (with-open [w (io/writer outputFile)] (.write w (str text)))
    (catch Exception _)))

(defn -main [inputFile it outputFile]
  "Recibe el archio .sl, la cantidad de iteraciones a realizar y el archivo .xml donde se escribe el resultado"
  (let [info (read-file! inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (nnext info))) it)
        pila-tortuga (list (tortuga-create 0 0 0))
        text-svg (gen-text patron pila-tortuga angulo (datos-create PUNTO-INICIAL))]
    (write-file! outputFile (format-svg (gen-viewbox text-svg) (get text-svg :text)))))