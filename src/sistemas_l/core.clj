(ns sistemas-l.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn read-file [file]
  "Recibe un archivo por parametro, si existe, lee su contenido y escribe como un vector"
  (try
    (with-open [reader (io/reader file)] (apply conj [] (line-seq reader)))
    (catch Exception _)))

(defn gen-patron [axioma reemp it]
  (if (zero? it)
    axioma
    (gen-patron (apply str (sequence (replace reemp (vec axioma)))) reemp (dec it))
    )
  )

(defn hash-create [remp]
  (println remp)
  (if (empty? remp) {}
                   (let [key (first (seq (apply str (first remp))))
                         value (apply str (nnext (seq (apply str (first remp)))))
                         ]

                     (merge (hash-map key value) (hash-create (vec (rest remp)))))
    )
  )

(defn tortuga [x y angulo]
  {:x x,  :y y ,:angulo angulo})


(defn tortuga+angulo [tortuga angulo]
  (update tortuga :angulo + angulo)
  )

(defn tortuga-angulo [tortuga angulo]
  (update tortuga :angulo - angulo)
  )

(defn gen-texto-svg [patron pila-tortuga angulo]
  (println pila-tortuga)
  (if (empty? patron)
    " "
    (let [letra (first patron)
          resto (rest patron)
          tortuga (first pila-tortuga)
          resto-pila (rest pila-tortuga)]
      (cond
        (= letra \F) (str "Avanzo. "           (gen-texto-svg resto (cons tortuga resto-pila) angulo)) ;una nueva lista con la tortuga modificada en pos en vez de la tortuga
        (= letra \f) (str "Avanzo dibujando. " (gen-texto-svg resto (cons tortuga resto-pila) angulo)) ;lo mismo
        (= letra \+) (gen-texto-svg resto (cons (tortuga+angulo tortuga angulo) resto-pila) angulo);una nueva lista con la tortuga modificada en angulo en vez de la tortuga
        (= letra \-) (gen-texto-svg resto (cons (tortuga-angulo tortuga angulo) resto-pila) angulo) ;lo mismo
        (= letra \|) (gen-texto-svg resto (cons (tortuga+angulo tortuga 180) resto-pila) angulo) ;lo mismo
        (= letra \[) (str "Apilo Tortuga. "    (gen-texto-svg resto (cons tortuga pila-tortuga) angulo))
        (and (not (empty? resto-pila)) (= letra  \])) (str "Desapilo Tortuga. " (gen-texto-svg resto resto-pila angulo))
        :else (gen-texto-svg resto pila-tortuga angulo))
        )
      )
  )



(defn write-file [outputFile texto]
  (println texto)
  )

(defn -main [inputFile it outputFile]
  (let [info (read-file inputFile)
        angulo (Double/parseDouble (first info))
        patron (gen-patron (first (rest info)) (hash-create (vec (drop 2 info))) it)
        pila-tortuga (list (tortuga 0 0 0))
        ]
    (write-file outputFile (gen-texto-svg patron pila-tortuga angulo))
    ))
