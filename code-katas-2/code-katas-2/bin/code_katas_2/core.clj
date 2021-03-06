(ns code-katas-2.core)


(defn unpartial
  "Escribir una funcion que acepte una funcion parcial con cantidad de argumentos desconocida,
   retornar una funcion equivalente de n argumentos"
  [f]
  (partial (fn [f & args]
             (if (fn? (f (first args)))
               (recur (f (first args)) (rest args))
               (f (first args))
               )
             )
           f
           )
  )


(defn search
  "Dado un numero cualquiera de secuencias, cada una ya ordenada de menor a mayor, encontrar el numero
   mas chico que aparezca en todas las secuencias, las secuencias pueden ser infinitas."
  [& seqs]
  
  (defn true-false [x secuencia]
    (for [y secuencia]
      (contains? (set y) x)
      )
    )
  
  (defn search2 [secuencias]
    (if (some false? (true-false (first (first secuencias)) (rest secuencias)))
      #(search2 (concat (list (rest (first secuencias))) (rest secuencias)))
      (first (first secuencias))
      )
    )
  
  (trampoline search2 seqs)
  )


(defn intercalar
  "Escriba una funcion que tome un predicado de 2 argumentos, un valor y una coleccion, y
   retorne una nueva coleccion donde el valor es insertado intercalado cada dos argumentos
   que cumplan el predicado"
  [predicado valor secuencia]
  (lazy-seq
    (if (not= (count secuencia) 1)
      (if (predicado (first secuencia) (second secuencia))
        (concat (list (first secuencia) valor) (intercalar predicado valor (rest secuencia)))
        (cons (first secuencia) (intercalar predicado valor (rest secuencia)))
        )
      secuencia
      )
    )
  )


(defn tartamudeo
  "Escriba una funcion que retorne una secuencia lazy que comprima el tartamudeo de una secuencia de numeros.
   Comprimir el tartamudeo se refiere a que [1 1 1] se exprese como [3 1] y a su vez [3 1] se exprese como [1 3 1 1].

   La funcion debe aceptar una secuencia inicial de numeros, y devolver una secuencia infinita de compresiones, donde
   cada nuevo elemento es el elemento anterior comprimido."
  [secuencia]
  
  (defn tarta [x y sec temp]
    (if (not (empty? sec))
      (if (= x (first sec))
        (tarta x (+ 1 y) (rest sec) temp)
        (tarta (first sec) 1 (rest sec) (concat temp (list y x)))
        )
      (concat temp (list y x))
      )
    )
  
  (defn tarta2 [secuencia2] (tarta (first secuencia2) 1 (rest secuencia2) '()))
  
  (iterate tarta2 secuencia)
    
  )