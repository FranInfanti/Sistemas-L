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

(def SVG "<svg viewBox=\"%viewbox\" xmlns=\"http://www.w3.org/2000/svg\"> %svg </svg>")
(def SVG-LINE "<line x1=\"%x1\" y1=\"%y1\" x2= \"%x2\" y2=\"%y2\" stroke-width=\"%grosor\" stroke=\"%color\" />")
(def SVG-CIRCLE "<circle cx=\"%x\" cy=\"%y\" r=\"%radio\" fill=\"%color\"/>")
(def COLORES #{\a \b \c})
(def GROSOR #{\1 \2 \3 \4 \5 \6 \7 \8 \9})
(def GROSOR-DEFAULT \1)
(def COLOR-DEFAULT "black")
(def COLOR-OPTION1 "grey")

(defn read-file! [file]
  "Recibe un archivo por parametro, si existe, lee su contenido y escribe como un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn gen-patron [axioma remp it]
  (if (zero? it) axioma (recur (apply str (sequence (replace remp (vec axioma)))) remp (dec it))))

(defn hash-create [remp]
  "Recibe un vector con los remplazos (remp) y lo separa en un hash, con key el caracter asociado al remplazo
  y value el remplazo en si"
  (if (empty? remp) {}
                    (let [key (first (seq (apply str (first remp))))
                          value (apply str (nnext (seq (apply str (first remp)))))]
                      (merge (hash-map key value) (hash-create (vec (rest remp)))))))

(defn gen-coordenada [x y angulo]
  "Recibe dos coordenadas y un angulo. A partir de eso genera un punto"
  (let [u (+ (* LAMBDA (math/cos (math/to-radians (- angulo 90)))) x)
        v (+ (* LAMBDA (math/sin (math/to-radians (- angulo 90)))) y)]
    (hash-map :x u :y v)))

(defn gen-svg [inicio final simbolo datos]
  "Recibe un punto y un simbolo. Con esto genera un texto del tipo M x y ó L x y"
  (if (contains? DIBUJAR simbolo)
    (let [svg-line SVG-LINE]
      (-> svg-line
          (clojure.string/replace #"%x1" (str (get inicio :x)))
          (clojure.string/replace #"%x2" (str (get final :x)))
          (clojure.string/replace #"%y1" (str (get inicio :y)))
          (clojure.string/replace #"%y2" (str (get final :y)))
          (clojure.string/replace #"%grosor" (str (get datos :grosor)))
          (clojure.string/replace #"%color" (str (get datos :color))))) ""))

(defn gen-circle-svg [posicion datos]
  (let [svg-line SVG-CIRCLE]
    (-> svg-line
        (clojure.string/replace #"%x" (str (get posicion :x)))
        (clojure.string/replace #"%y" (str (get posicion :y)))
        (clojure.string/replace #"%color" (get datos :color))
        (clojure.string/replace #"%radio" (str (double  (/ (Double/parseDouble (str (get datos :grosor))) 2)))))))

(defn tortuga-create [x y angulo]
  "Crea una Tortuga donde guarda en un hash-map de donde empezo a dibujar, un hash-map
  que guarda donde esta y el angulo con el que tiene que dibujar"
  (hash-map :x x :y y :angulo angulo))

(defn tortuga-rotar [tortuga rotar angulo-default]
  "Recibe una tortuga y un simbolo que indica hacia donde rotar"
  (if (= rotar ROTAR-DERECHA) (update tortuga :angulo + angulo-default) (update tortuga :angulo - angulo-default)))

(defn new-color [datos simbolo]
  (case simbolo
    \a (assoc datos :color COLOR-DEFAULT)
    \b (assoc datos :color COLOR-OPTION1)))

(defn new-grosor [datos simbolo] (assoc datos :grosor simbolo))

(defn new-datos [datos coordenadas text-svg]
  (hash-map :xmin (min (get datos :xmin) (get coordenadas :x))
            :ymin (min (get datos :ymin) (get coordenadas :y))
            :xmax (max (get datos :xmax) (get coordenadas :x))
            :ymax (max (get datos :ymax) (get coordenadas :y))
            :text (str (get datos :text) " " text-svg)
            :color (get datos :color)
            :grosor (get datos :grosor)))

(defn datos-create [text] (hash-map :xmin 0 :ymin 0 :xmax 0 :ymax 0 :text text :grosor GROSOR-DEFAULT :color COLOR-DEFAULT))

(defn gen-text [patron pila-tortuga angulo-default datos]
  (if (empty? patron)
    datos
    (let [simbolo (first patron)
          tortuga (peek pila-tortuga)
          rest-patron (rest patron)
          rest-pila (pop pila-tortuga)]
      (cond
        (contains? MOVE-PLUMA simbolo) (let [new-punto (gen-coordenada (get tortuga :x) (get tortuga :y) (get tortuga :angulo))
                                             new-tortuga (tortuga-create (get new-punto :x) (get new-punto :y) (get tortuga :angulo))
                                             text-svg (gen-svg tortuga new-tortuga simbolo datos)]
                                         (recur rest-patron (conj rest-pila new-tortuga) angulo-default (new-datos datos new-tortuga text-svg)))
        (contains? ROTAR-PLUMA simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= ROTAR-180 simbolo) 180 angulo-default))]
                                          (recur rest-patron (conj rest-pila new-tortuga) angulo-default datos))
        (contains? COLORES simbolo) (recur rest-patron pila-tortuga angulo-default (new-color datos simbolo))
        (contains? GROSOR simbolo)  (recur rest-patron pila-tortuga angulo-default (new-grosor datos simbolo))
        (= \L simbolo) (let [circle-svg (gen-circle-svg tortuga datos)]
                         (recur rest-patron pila-tortuga angulo-default (new-datos datos tortuga circle-svg)))
        (= APILAR simbolo) (recur rest-patron (conj pila-tortuga tortuga) angulo-default datos)
        (and (not (empty? rest-pila)) (= simbolo DESAPLIAR)) (recur rest-patron rest-pila angulo-default datos)
        :else (recur rest-patron pila-tortuga angulo-default datos)))))

(defn calcular-extremos [extremo x y max]
  (if (nil? max) (+ extremo (double (/ (abs (- x y)) LAMBDA))) (- extremo (double (/ (abs (- x y)) LAMBDA)))))

(defn gen-viewbox [datos]
  (let [x-min (calcular-extremos (get datos :xmin) (get datos :xmax) (get datos :xmin) 0)
        y-min (calcular-extremos (get datos :ymin) (get datos :ymax) (get datos :ymin) 0)
        x-max (calcular-extremos (get datos :xmax) (get datos :xmax) (get datos :xmin) nil)
        y-max (calcular-extremos (get datos :ymax) (get datos :ymax) (get datos :ymin) nil)
        ancho (abs (- x-max x-min))
        alto (abs (- y-max y-min))]
    (str x-min " " y-min " " ancho " " alto)))

(defn complete-svg [viewbox text-svg]
  (let [svg SVG] (-> svg (clojure.string/replace #"%viewbox" viewbox) (clojure.string/replace #"%svg" text-svg))))

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
        text-svg (gen-text patron pila-tortuga angulo (datos-create " "))]
    (write-file! outputFile (complete-svg (gen-viewbox text-svg) (get text-svg :text)))))