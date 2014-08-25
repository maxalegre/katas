(ns code-katas-1.core)

(defn filter-odd
  "Escribir una funcion que retorne solamente los numeros impares de
   una secuencia"
  [s]
  (for [x s :when (odd? x)] x)
  )

(defn nil-key
  "Escribir una funcion que dada una clave y un mapa, devuelva true, solamente si el mapa
   contiene una entrada con esa clave, y su valor es nil"
  [k m]
  (if (contains? m k) (if (nil? (k m)) true false) false)
  )

(defn range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  (if (< start end) (take (- end start) (iterate inc start)) "Inicio debe ser mas pequeño que final")
  )

(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  
  (defn secuencia [x sec temp]
    (if (= x "")
      (secuencia (aget sec 0) (to-array (into [] (drop 1 sec))) (conj temp (aget sec 0)))
      (if (not (empty? sec))
        (if (not= x (aget sec 0)) 
          (secuencia (aget sec 0) (to-array (into [] (drop 1 sec))) (conj temp (aget sec 0))) 
          (secuencia x (to-array (into [] (drop 1 sec))) temp))
        temp
        )
      )
    )
  (secuencia "" (to-array s) [])
  )

(defn max-value
  "Escribir una funcion que reciba un numero variable de parametros
   y retorne el que tenga el valor mayor
   Restricciones: max y max-key"
  [& args]
  (defn max-value2 [maxim pmts]
   (if (not (empty? pmts))
     (if (> (first pmts) maxim) 
       (max-value2 (first pmts) (into [] (drop 1 pmts))) 
       (max-value2 maxim (into [] (drop 1 pmts))) 
       )
     maxim
   )
   )
   (max-value2 0 args)
 )

(defn split-two
  "Escribir una funcion que parta una secuencia en dos partes
   Restricciones: split-at"
  [length s]
  (into [] (into [] (for [x (take length s)] x)) (into [] (drop length s)) )
  )

(defn inter-two
  "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  
    (reduce
      (fn [a b]
        (if (not= (- b 1) 0)
          (concat (into [] a) (vector (aget (to-array s1) (- b 1)) (aget (to-array s2) (- b 1))))
          (list (aget (to-array s1) (- b 1)) (aget (to-array s2) (- b 1)))
          )
        )
      (range (+ 1 (if (< (count s1) (count s2)) (count s1) (count s2))))
      )
  )

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  (reduce (fn [a b] (if (= (str b) (clojure.string/upper-case b)) (str a b) a)) (to-array text))
  )

(defn find-truth
  "Escribir una funcion que tome un numero variable de booleans, y devuelva true
   solamente si alguno de los parametros son true, pero no todos son true. En otro
   caso debera retornar false"
  [& xs]
  (if (some false? xs)
     (if (some true? xs) true false)
     false
     )
  )

(defn zip-map
  "Escribir una funcion que reciba un vector de claves y un vector de valores, y
   construya un mapa a partir de ellos.
   Restricciones: zipmap"
  [k v]
  (reduce 
        (fn [a b] 
          (if (not= 0 a) 
            (merge (into {} (vector(vector (nth k (- b 1)) (nth v (- b 1))))) a) 
            (into {} (vector(vector (nth k 0) (nth v 0))))
            )
          ) 
        (range 0 (+ 1 (count k)))
        )
  )
