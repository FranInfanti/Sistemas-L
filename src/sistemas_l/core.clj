(ns sistemas-l.core
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]))

(def lambda 10)
(def move-pluma #{\F \G \f \g})
(def dibujar #{\F \G})
(def rotar-pluma #{\+ \- \|})
(def rotar-derecha \+)
(def rotar-180 \|)
(def apilar \[)
(def desapilar \])
(def svg-complete "<svg viewBox=\"%s\" xmlns=\"http://www.w3.org/2000/svg\"> %s </svg>")
(def svg-line "<line x1=\"%.4f\" y1=\"%.4f\" x2= \"%.4f\" y2=\"%.4f\" stroke-width=\"%.4f\" stroke=\"%s\" />")
(def svg-circle "<circle cx=\"%.4f\" cy=\"%.4f\" r=\"%.4f\" fill=\"%s\"/>")
(def colores #{\a \b})
(def simbolo-color-option1 \b)
(def grosor #{\1 \2 \3 \4 \5 \6 \7 \8 \9})
(def circulo \L)
(def color-default "black")
(def color-option1 "#e5bf10")
(def tortuga-inicial {:x 0.0 :y 0.0 :angulo 0.0})
(def datos-inicial {:xmin 0.0 :ymin 0.0 :xmax 0.0 :ymax 0.0 :text "" :grosor 1.0 :color color-default})

(defn read-file! [file]
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn hash-create [remp]
  "Crea un hash con :key el caracter asociado al remplazo y :value el remplazo en si"
  (when-let [key (ffirst remp)]
    (let [value (apply str (nnext (first remp)))]
      (merge (hash-map key value) (hash-create (rest remp))))))

(defn gen-patron [axioma remp it]
  (if (zero? it)
    axioma
    (recur (apply str (replace remp axioma)) remp (dec it))))

(defn new-datos [datos posicion text-svg]
  (-> datos
      (update :xmin min (:x posicion))
      (update :ymin min (:y posicion))
      (update :xmax max (:x posicion))
      (update :ymax max (:y posicion))
      (update :text str " " text-svg)))

(defn calcular-pos [pos angulo x]
  (if x
    (+ (* lambda (math/cos (math/to-radians (- angulo 90)))) pos)
    (+ (* lambda (math/sin (math/to-radians (- angulo 90)))) pos)))

(defn gen-new-pos [tortuga]
  (-> tortuga
      (update :x calcular-pos (:angulo tortuga) 0)
      (update :y calcular-pos (:angulo tortuga) nil)))

(defn gen-svg [inicio final simbolo datos]
  (when (contains? dibujar simbolo)
    (format svg-line (:x inicio) (:y inicio) (:x final) (:y final) (:grosor datos) (:color datos))))

(defn gen-circle-svg [posicion datos]
  (format svg-circle (:x posicion) (:y posicion) (double (/ (:grosor datos) 2)) (:color datos)))

(defn tortuga-rotar [tortuga rotar angulo-default]
  (if (= rotar rotar-derecha)
    (update tortuga :angulo + angulo-default)
    (update tortuga :angulo - angulo-default)))

(defn new-color [datos simbolo]
  (if (= simbolo simbolo-color-option1)
    (assoc datos :color color-option1)
    (assoc datos :color color-default)))

(defn new-grosor [datos simbolo] (assoc datos :grosor (Double/parseDouble (str simbolo))))

(defn gen-text [patron pila-tortuga angulo datos]
  (if-let [simbolo (first patron)]
    (let [tortuga (peek pila-tortuga)
          rest-pila (pop pila-tortuga)]
      (cond
        (contains? move-pluma simbolo)
        (let [new-tortuga (gen-new-pos tortuga)
              text-svg (gen-svg tortuga new-tortuga simbolo datos)]
          (recur (rest patron) (conj rest-pila new-tortuga) angulo (new-datos datos new-tortuga text-svg)))

        (contains? rotar-pluma simbolo)
        (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= rotar-180 simbolo) 180 angulo))]
          (recur (rest patron) (conj rest-pila new-tortuga) angulo datos))

        (contains? colores simbolo)
          (recur (rest patron) pila-tortuga angulo (new-color datos simbolo))

        (contains? grosor simbolo)
          (recur (rest patron) pila-tortuga angulo (new-grosor datos simbolo))

        (= circulo simbolo)
          (recur (rest patron) pila-tortuga angulo (new-datos datos tortuga (gen-circle-svg tortuga datos)))

        (= apilar simbolo)
          (recur (rest patron) (conj pila-tortuga tortuga) angulo datos)

        (and (= simbolo desapilar) (seq rest-pila))
          (recur (rest patron) rest-pila angulo datos)

        :else
          (recur (rest patron) pila-tortuga angulo datos)))
    datos))

(defn calcular-extremos [x1 x2 max]
  (if max
    (+ x2 (double (/ (abs (- x1 x2)) lambda)))
    (- x1 (double (/ (abs (- x1 x2)) lambda)))))

(defn gen-viewbox [datos]
  "Calcula los parametros del viewbox para que la imagen se vea como corresponde"
  (let [x-min (calcular-extremos (:xmin datos) (:xmax datos) nil)
        y-min (calcular-extremos (:ymin datos) (:ymax datos) nil)
        x-max (calcular-extremos (:xmin datos) (:xmax datos) 0)
        y-max (calcular-extremos (:ymin datos) (:ymax datos) 0)
        ancho (abs (- x-max x-min))
        alto (abs (- y-max y-min))]
    (str/join " " [x-min y-min ancho alto])))

(defn format-svg [viewbox text-svg] (format svg-complete viewbox text-svg))

(defn write-file! [outputFile text]
  (try
    (with-open [w (io/writer outputFile)] (.write w (str text)))
    (catch Exception _)))

(defn -main [inputFile it outputFile]
  (when-let [info (seq (read-file! inputFile))]
    (let [angulo (Double/parseDouble (first info))
          patron (gen-patron (second info) (hash-create (nnext info)) it)
          pila-tortuga (list tortuga-inicial)
          text-svg (gen-text patron pila-tortuga angulo datos-inicial)]
      (write-file! outputFile (format-svg (gen-viewbox text-svg) (:text text-svg))))))