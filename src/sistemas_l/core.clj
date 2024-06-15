(ns sistemas-l.core
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.string :as str]))

(def largo-linea 10)

(defn read-file! [file]
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _ nil)))

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

(defn calcular-pos [pos angulo]
  [(+ (* largo-linea (math/cos (math/to-radians (- angulo 90)))) pos)
   (+ (* largo-linea (math/sin (math/to-radians (- angulo 90)))) pos)])

(defn gen-new-pos [tortuga]
  (-> tortuga
      (assoc :x (first (calcular-pos (:x tortuga) (:angulo tortuga))))
      (assoc :y (second (calcular-pos (:y tortuga) (:angulo tortuga))))))

(defn gen-svg [inicio final simbolo datos]
  (let [svg "<line x1=\"%.4f\" y1=\"%.4f\" x2= \"%.4f\" y2=\"%.4f\" stroke-width=\"%.4f\" stroke=\"%s\" />"
        dibujar #{\F \G}]
    (when (contains? dibujar simbolo)
      (format svg (:x inicio) (:y inicio) (:x final) (:y final) (:grosor datos) (:color datos)))))

(defn gen-circle-svg [posicion datos]
  (let [svg "<circle cx=\"%.4f\" cy=\"%.4f\" r=\"%.4f\" fill=\"%s\"/>"]
    (format svg (:x posicion) (:y posicion) (double (/ (:grosor datos) 2)) (:color datos))))

(defn tortuga-rotar [tortuga simbolo angulo-default]
  (let [rotar {\+ angulo-default \- (- angulo-default) \| 180}]
    (update tortuga :angulo + (get rotar simbolo))))

(defn new-color [datos simbolo]
  (let [colores {\a "black" \b "yellow"}]
    (assoc datos :color (get colores simbolo))))

(defn new-grosor [datos simbolo] (assoc datos :grosor (Double/parseDouble (str simbolo))))

(defn gen-text [patron pila-tortuga angulo datos]
  (if-let [simbolo (first patron)]
    (let [tortuga (peek pila-tortuga)
          rest-pila (pop pila-tortuga)]
      (case simbolo
        (\F \G \f \g) (let [new-tortuga (gen-new-pos tortuga)
                         text-svg (gen-svg tortuga new-tortuga simbolo datos)]
                     (recur (rest patron) (conj rest-pila new-tortuga) angulo (new-datos datos new-tortuga text-svg)))

        (\+ \- \|) (let [new-tortuga (tortuga-rotar tortuga simbolo angulo)]
                      (recur (rest patron) (conj rest-pila new-tortuga) angulo datos))

        (\a \b) (recur (rest patron) pila-tortuga angulo (new-color datos simbolo))

        (\1 \2 \3 \4 \5 \6 \7 \8 \9) (recur (rest patron) pila-tortuga angulo (new-grosor datos simbolo))

        \L (recur (rest patron) pila-tortuga angulo (new-datos datos tortuga (gen-circle-svg tortuga datos)))

        \[ (recur (rest patron) (conj pila-tortuga tortuga) angulo datos)

        \] (if (seq rest-pila)
                    (recur (rest patron) rest-pila angulo datos)
                    (recur (rest patron) pila-tortuga angulo datos))

        (recur (rest patron) pila-tortuga angulo datos)))
    datos))

(defn calcular-extremos
  "Calcula el extremo entre x1 x2 y usa se les agrega un 10% del ancho y alto"
  [x1 x2]
  [(+ x2 (double (/ (abs (- x1 x2)) 10)))
   (- x1 (double (/ (abs (- x1 x2)) 10)))])

(defn gen-viewbox
  "Calcula los parametros del viewbox para que la imagen se vea como corresponde"
  [datos]
  (let [x-min (second (calcular-extremos (:xmin datos) (:xmax datos)))
        y-min (second (calcular-extremos (:ymin datos) (:ymax datos)))
        x-max (first (calcular-extremos (:xmin datos) (:xmax datos)))
        y-max (first (calcular-extremos (:ymin datos) (:ymax datos)))
        ancho (abs (- x-max x-min))
        alto (abs (- y-max y-min))]
    (str/join " " [x-min y-min ancho alto])))

(defn format-svg [viewbox text-svg]
  (let [svg "<svg viewBox=\"%s\" xmlns=\"http://www.w3.org/2000/svg\"> %s </svg>"]
    (format svg viewbox text-svg)))

(defn write-file! [outputFile text]
  (try
    (with-open [w (io/writer outputFile)] (.write w (str text)))
    (catch Exception _ nil)))

(defn -main [inputFile it outputFile]
  (when-let [info (seq (read-file! inputFile))]
    (let [angulo (Double/parseDouble (first info))
          patron (gen-patron (second info) (hash-create (nnext info)) (Integer/parseInt it))
          pila-tortuga (list {:x 0.0 :y 0.0 :angulo 0.0})
          text-svg (gen-text patron pila-tortuga angulo
                             {:xmin 0.0 :ymin 0.0 :xmax 0.0 :ymax 0.0 :text "" :grosor 1.0 :color "black"})]
      (write-file! outputFile (format-svg (gen-viewbox text-svg) (:text text-svg))))))