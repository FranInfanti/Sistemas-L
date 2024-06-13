(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.math :as math])
  (:require [clojure.string :as str]))

(def lambda 10)
(def mover-pluma #{\F \G \f \g})
(def dibujar #{\F \G})
(def rotar-pluma #{\+ \- \|})
(def rotar-derecha \+)
(def rotar-180 \|)
(def apilar \[)
(def desapilar \])
(def angulo-corrector 90)
(def up-pluma \M)
(def down-pluma \L)
(def svg "<svg viewBox=\"%s\" xmlns=\"http://www.w3.org/2000/svg\"><path d=\"%s\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/></svg>")
(def svg-path "%s %.4f %.4f")
(def tortuga-inicial {:x 0.0 :y 0.0 :angulo 0.0})

(defn read-file! [file]
  "Recibe un archivo por parametro, si existe y puede abrilo, lee su contenido y lo escribe en un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn hash-create [remplazos]
  "Crea un hash con :key el caracter asociado al remplazo y :value el remplazo en si"
  (when-let [key (ffirst remplazos)]
    (let [value (apply str (nnext (first remplazos)))]
      (merge (hash-map key value) (hash-create (rest remplazos))))))

(defn gen-patron [axioma remp it]
  "Devuelve el patron de simbolos sobre el cual se va a basar el dibujo"
  (if (zero? it)
    axioma
    (recur (apply str (replace remp axioma)) remp (dec it))))

(defn datos-create [text] {:xmin 0 :ymin 0 :xmax 0 :ymax 0 :text text})

(defn new-datos [datos posicion text-svg]
  "Crea un nuevo dato actualizando el text y los maximos y minimos valores de x e y"
  (-> datos
      (update :xmin min (:x posicion))
      (update :ymin min (:y posicion))
      (update :xmax max (:x posicion))
      (update :ymax max (:y posicion))
      (update :text str " " text-svg)))

(defn calcular-pos [pos angulo x]
  (if x
    (+ (* lambda (math/cos (math/to-radians (- angulo angulo-corrector)))) pos)
    (+ (* lambda (math/sin (math/to-radians (- angulo angulo-corrector)))) pos)))

(defn gen-new-posicion [tortuga]
  "Calcula una nueva posicion en base a la posicion actual de la tortuga y el angulo actual de la tortuga"
  (-> tortuga
      (update :x calcular-pos (:angulo tortuga) 0)
      (update :y calcular-pos (:angulo tortuga) nil)))

(defn gen-svg [tortuga simbolo] (format svg-path simbolo (:x tortuga) (:y tortuga)))

(defn tortuga-rotar [tortuga rotar angulo-default]
  "Rota la tortuga para la derecha o izquierda"
  (if (= rotar rotar-derecha)
    (update tortuga :angulo + angulo-default)
    (update tortuga :angulo - angulo-default)))

(defn gen-text [patron pila-tortuga angulo datos]
  (if-let [simbolo (first patron)]
    (let [tortuga (peek pila-tortuga)
          rest-pila (pop pila-tortuga)]
      (cond
        (contains? mover-pluma simbolo) (let [new-tortuga (gen-new-posicion tortuga)
                                              text-svg (gen-svg new-tortuga (if (contains? dibujar simbolo) down-pluma up-pluma))]
                                          (recur (rest patron) (conj rest-pila new-tortuga) angulo (new-datos datos new-tortuga text-svg)))

        (contains? rotar-pluma simbolo) (let [new-tortuga (tortuga-rotar tortuga simbolo (if (= rotar-180 simbolo) 180 angulo))]
                                          (recur (rest patron) (conj rest-pila new-tortuga) angulo datos))

        (= apilar simbolo) (recur (rest patron) (conj pila-tortuga tortuga) angulo datos)

        (and (= simbolo desapilar) (seq rest-pila)) (recur (rest patron) rest-pila angulo (new-datos datos tortuga (gen-svg (peek rest-pila) up-pluma)))

        :else (recur (rest patron) pila-tortuga angulo datos)))
    datos))

(defn calcular-extremos [x1 x2 max]
  (if max (+ x2 (double (/ (abs (- x1 x2)) lambda))) (- x1 (double (/ (abs (- x1 x2)) lambda)))))

(defn gen-viewbox [datos]
  "Calcula los parametros del viewbox para que la imagen se vea como corresponde"
  (let [x-min (calcular-extremos (:xmin datos) (:xmax datos) nil)
        y-min (calcular-extremos (:ymin datos) (:ymax datos) nil)
        x-max (calcular-extremos (:xmin datos) (:xmax datos) 0)
        y-max (calcular-extremos (:ymin datos) (:ymax datos) 0)
        ancho (abs (- x-max x-min))
        alto (abs (- y-max y-min))]
    (str/join " " [x-min y-min ancho alto])))

(defn format-svg [viewbox text-svg] (format svg viewbox text-svg))

(defn write-file! [outputFile text]
  "Recibe el nombre de un archivo y un texto a escribir en este"
  (try
    (with-open [w (io/writer outputFile)] (.write w (str text)))
    (catch Exception _)))

(defn -main [inputFile it outputFile]
  (when-let [info (seq (read-file! inputFile))]
    (let [angulo (Double/parseDouble (first info))
          patron (gen-patron (second info) (hash-create (nnext info)) it)
          pila-tortuga (list tortuga-inicial)
          text-svg (gen-text patron pila-tortuga angulo (datos-create (gen-svg (peek pila-tortuga) up-pluma)))]
      (write-file! outputFile (format-svg (gen-viewbox text-svg) (get text-svg :text))))))