; beatriz de assumpção zardo
; 1. Utilizando  a  linguagem  Clojure,  crie  uma  função  chamada  ultimo  que  receba  uma  lista  e devolva o último elemento desta lista sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn ultimo [lista]
  (let [tamanho (count lista)]
      (nth lista (- tamanho 1))))

; 2. Utilizando a linguagem Clojure, crie uma função chamada penultimo que receba uma lista e  devolva  o  penúltimo  elemento  desta  lista  usar as  funções  já  prontas  e disponíveis para esta mesma finalidade na linguagem Clojure. 

(defn penultimo [lista]
  (let [tamanho (count lista)]
      (nth lista (- tamanho 2))))

; 3. Utilizando a linguagem Clojure, crie uma função chamada elementoN que receba uma lista e um inteiro N e devolva o  elemento que  está na  posição N desta lista usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn elementoN [lista indice]
  (nth lista indice))

; 4. Utilizando  a  linguagem Clojure,  crie  uma função  chamada  inverso  que  receba uma  lista  e devolva esta lista com as posições dos elementos invertidas. Por exemplo recebe [1,2,3] e devolve [3,2,1]. Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.

(defn inverso [lista listarev]
    (if (> (count lista) 0)
       (recur (pop lista) (conj listarev (last lista)))           
      listarev))

; 5. Utilizando a  linguagem Clojure, crie uma função chamada  mdc que receba  dois inteiros e devolve o mínimo divisor comum entre eles.  Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn mdc [primeiro segundo]
  (if (= segundo 0)
    primeiro
    (recur segundo (mod primeiro segundo))))

(println "Func 1 entrada [1 2 3 4 5] resultado: " (ultimo [1 2 3 4 5]))
(println "Func 2 entrada [1 2 3 4 5] resultado: " (penultimo [1 2 3 4 5]))
(println "Func 3 entrada [1 2 3 4 5] 2 resultado: " (elementoN [1 2 3 4 5] 2))
(println "Func 4 entrada [1 2 3 4 ] [] resultado: " (inverso [1 2 3 4] []))
(println "Func 5 entrada 9 3 resultado: " (mdc 3 4 ))