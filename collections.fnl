; fennelib/collections.fnl
;
; functions (re)implemented in/efficiently
; for handling collections
;
; created on : 2022.06.09.
; last update: 2022.06.21.

(local collections {})

; Returns if given sequential table `col` is empty or not
(fn collections.empty? [col]
  (or (= nil col) (= (length col) 0)))

; Returns the first element of given sequential table `col`
(fn collections.head [col]
  (match (length col)
    0 nil
    _ (. col 1)))

; Returns elements except the first one
(fn collections.rest [col]
  (match (length col)
    0 nil
    1 []
    _ [(table.unpack col 2)]))

; Returns the last element of given sequential table `col`
(fn collections.tail [col]
  (let [count (length col)]
    (match count
      0 nil
      _ (. col count))))

; Returns a new sequential table with `e` as the head and `col` as the rest
(fn collections.cons [e col]
  (let [new col]
    (table.insert new 1 e)
    new))

; Returns a new sequential table with `e` as the new tail of `col`
(fn collections.conj [col e]
  (let [new col]
    (table.insert new e)
    new))

; Returns a reversed sequential table of `col`
;(fn collections.reverse [col]
;  (local out [])
;  (each [_ e (ipairs col)]
;    (table.insert out 1 e))
;  out)
(fn collections.reverse [col]
  (if (collections.empty? col)
    col
    (let [h (collections.head col)
          r (collections.rest col)]
      (collections.conj (collections.reverse r) h))))

; Returns a concatenated sequential table with given sequential tables
(fn collections.concat [...]
  (let [col [...]]
    (local out [])
    (each [_ t (ipairs col)]
      (each [_ e (ipairs t)]
        (table.insert out e)))
    out))

; Returns a range of numbers as a sequential table:
;
; (collections.range 5 => [1 2 3 4]
; (collections.range 10 15) => [10 11 12 13 14]
; (collections.range 20 30 3) => [20 23 26 29]
(fn collections.range [...]
  (fn _range [start end step acc]
    (if (or (and (> step 0) (< start end))
            (and (< step 0) (> start end)))
      (_range (+ start step) end step (collections.conj acc start))
      acc))
  (let [args (table.pack ...)]
    (match (. args :n)
      0 nil
      1 (let [end (. args 1)]
          (_range 1 end 1 []))
      2 (let [start (. args 1)
              end (. args 2)]
          (_range start end 1 []))
      3 (let [start (. args 1)
              end (. args 2)
              step (. args 3)]
          (_range start end step []))
      4 (let [start (. args 1)
              end (. args 2)
              step (. args 3)
              acc (. args 4)]
          (_range start end step acc))
      _ nil)))

; Returns the `i`th element from given sequential table `col`
(fn collections.nth [col i]
  (let [count (length col)]
    (if (> i count)
      nil
      (match i
        0 nil
        1 (match (length col)
            0 nil
            _ (. col 1))
        _ (collections.nth (collections.rest col) (- i 1))))))

; Returns the first `n` elements from given sequential table `col`
(fn collections.take [n col]
  (if (collections.empty? col)
    col
    (if (> n 0)
      (let [h (collections.head col)
            r (collections.rest col)]
        (collections.cons h (collections.take (- n 1) r)))
      [])))

; Drops first `n` elements from given sequential table `col` and returns the remaining
(fn collections.drop [n col]
  (if (collections.empty? col)
    col
    (if (= n 0)
      col
      (collections.drop (- n 1) (collections.rest col)))))

; Returns a sequential map with each element applied with function `f`
;(fn collections.map [f col]
;  (icollect [_ e (ipairs col)]
;   (f e)))
(fn collections.map [f col]
  (if (collections.empty? col)
    col
    (let [h (collections.head col)
          r (collections.rest col)]
      (collections.cons (f h) (collections.map f r)))))

; Returns an accumulated value
; which is calculated by function `f` (which takes two parameters)
; with initial value `acc` and each element of sequential table `col`
(fn collections.reduce [f acc col]
  (if (collections.empty? col)
    acc
    (let [h (collections.head col)
          r (collections.rest col)
          acc2 (f acc h)]
        (collections.reduce f acc2 r))))

; Filter elements from `col` which evaluates to true with function `f` (which takes one parameter)
(fn collections.filter [f col]
  (if (collections.empty? col)
    col
    (let [h (collections.head col)
          r (collections.rest col)
          filtered (collections.filter f r)]
      (if (f h)
        (collections.cons h filtered)
        filtered))))

; Sort elements of `col` with function `f`
; (function `f` takes two parameters,
;  returns true when the first parameter is bigger than or equal to the second one, and
;  returns false when the second one is bigger)
(fn collections.sort [f col]
  (if (collections.empty? col)
    col
    (let [pivot (collections.head col)
          r (collections.rest col)
          ls (collections.filter #(f $1 pivot) r)
          rs (collections.filter #(not (f $1 pivot)) r)]
      (collections.concat (collections.sort f ls) [pivot] (collections.sort f rs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finally, return this module for requiring from the outer world
collections

