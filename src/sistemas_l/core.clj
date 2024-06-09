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
(def svg-complete "<svg viewBox=\"%viewbox\" xmlns=\"http://www.w3.org/2000/svg\"> %svg </svg>")
(def svg-line "<line x1=\"%x1\" y1=\"%y1\" x2= \"%x2\" y2=\"%y2\" stroke-width=\"%grosor\" stroke=\"%color\" />")
(def svg-circle "<circle cx=\"%x1\" cy=\"%y1\" r=\"%grosor\" fill=\"%color\"/>")
(def colores #{\a \b})
(def grosor #{\1 \2 \3 \4 \5 \6 \7 \8 \9})
(def circulo \L)
(def grosor-default \1)
(def color-default "black")
(def color-option1 "grey")
(def replace-viewbox #"%viewbox")
(def replace-text #"%svg")
(def replace-x1 #"%x1")
(def replace-x2 #"%x2")
(def replace-y1 #"%y1")
(def replace-y2 #"%y2")
(def replace-grosor #"%grosor")
(def replace-color #"%color")

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

(defn datos-create [text] (hash-map :xmin 0 :ymin 0 :xmax 0 :ymax 0 :text text :grosor grosor-default :color color-default))

(defn new-datos [datos coordenadas text-svg]
  (hash-map :xmin (min (get datos :xmin) (get coordenadas :x))
            :ymin (min (get datos :ymin) (get coordenadas :y))
            :xmax (max (get datos :xmax) (get coordenadas :x))
            :ymax (max (get datos :ymax) (get coordenadas :y))
            :text (str (get datos :text) " " text-svg)
            :color (get datos :color)
            :grosor (get datos :grosor)))

(defn gen-nueva-pos [tortuga]
  (let [u (+ (* lambda (math/cos (math/to-radians (- (get tortuga :angulo) angulo-corrector)))) (get tortuga :x))
        v (+ (* lambda (math/sin (math/to-radians (- (get tortuga :angulo) angulo-corrector)))) (get tortuga :y))]
    (hash-map :x u, :y v, :angulo (get tortuga :angulo))))

(defn gen-svg [inicio final simbolo datos]
  (if (contains? dibujar simbolo)
    (let [svg svg-line]
      (-> svg
          (clojure.string/replace replace-x1 (str (get inicio :x)))
          (clojure.string/replace replace-x2 (str (get final :x)))
          (clojure.string/replace replace-y1 (str (get inicio :y)))
          (clojure.string/replace replace-y2 (str (get final :y)))
          (clojure.string/replace replace-grosor (str (get datos :grosor)))
          (clojure.string/replace replace-color (str (get datos :color))))) ""))

(defn gen-circle-svg [posicion datos]
  (let [svg svg-circle]
    (-> svg
        (clojure.string/replace replace-x1 (str (get posicion :x)))
        (clojure.string/replace replace-y1 (str (get posicion :y)))
        (clojure.string/replace replace-color (get datos :color))
        (clojure.string/replace replace-grosor (str (double  (/ (Double/parseDouble (str (get datos :grosor))) 2)))))))

(defn tortuga-rotar [tortuga rotar angulo-default]
  (if (= rotar rotar-derecha) (update tortuga :angulo + angulo-default) (update tortuga :angulo - angulo-default)))

(defn new-color [datos simbolo]
  (case simbolo
    \a (assoc datos :color color-default)
    \b (assoc datos :color color-option1)))

(defn new-grosor [datos simbolo] (assoc datos :grosor simbolo))

(defn gen-text [patron pila-tortuga angulo-default datos]
  (if (empty? patron)
    datos
    (let [simbolo (first patron)
          tortuga (peek pila-tortuga)
          rest-patron (rest patron)
          rest-pila (pop pila-tortuga)]
      (cond
        (contains? move-pluma simbolo) (let [new-tortuga (gen-nueva-pos tortuga)
                                             text-svg (gen-svg tortuga new-tortuga simbolo datos)]
                                         (recur rest-patron (conj rest-pila new-tortuga) angulo-default (new-datos datos new-tortuga text-svg)))
        (contains? rotar-pluma simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= rotar-180 simbolo) 180 angulo-default))]
                                          (recur rest-patron (conj rest-pila new-tortuga) angulo-default datos))
        (contains? colores simbolo) (recur rest-patron pila-tortuga angulo-default (new-color datos simbolo))
        (contains? grosor simbolo) (recur rest-patron pila-tortuga angulo-default (new-grosor datos simbolo))
        (= circulo simbolo) (let [circle-svg (gen-circle-svg tortuga datos)]
                         (recur rest-patron pila-tortuga angulo-default (new-datos datos tortuga circle-svg)))
        (= apilar simbolo) (recur rest-patron (conj pila-tortuga tortuga) angulo-default datos)
        (and (not (empty? rest-pila)) (= simbolo desapilar)) (recur rest-patron rest-pila angulo-default datos)
        :else (recur rest-patron pila-tortuga angulo-default datos)))))

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

(defn complete-svg [viewbox text-svg]
  (let [svg svg-complete] (-> svg (clojure.string/replace replace-viewbox viewbox) (clojure.string/replace replace-text text-svg))))

(defn write-file! [outputFile text]
  (try
    (with-open [w (io/writer outputFile)] (.write w (str text)))
    (catch Exception _)))

(defn -main [inputFile it outputFile]
  (let [info (read-file! inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (nnext info))) it)
        pila-tortuga (list (hash-map :x 0 :y 0 :angulo 0))
        text-svg (gen-text patron pila-tortuga angulo (datos-create " "))]
    (write-file! outputFile (complete-svg (gen-viewbox text-svg) (get text-svg :text)))))