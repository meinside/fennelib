;; fennelib/collections.fnl
;;
;; functions (re)implemented in/efficiently
;; for handling collections
;;
;; created on : 2022.06.09.
;; last update: 2022.06.22.

(local collections {})

;; Returns if given sequential table `coll` is empty or not
(fn collections.empty? [coll]
  (or (= nil coll) (= (length coll) 0)))

;; Returns the first element of given sequential table `coll`
(fn collections.head [coll]
  (match (length coll)
    0 nil
    _ (. coll 1)))

;; Returns elements except the first one
(fn collections.rest [coll]
  (if (collections.empty? coll)
    coll
    (match (length coll)
      0 nil
      1 []
      _ [(table.unpack coll 2)])))

;; Returns the last element of given sequential table `coll`
(fn collections.tail [coll]
  (let [count (length coll)]
    (match count
      0 nil
      _ (. coll count))))

;; Returns a new sequential table with `e` as the head and `coll` as the rest
(fn collections.cons [e coll]
  (let [new coll]
    (table.insert new 1 e)
    new))

;; Returns a new sequential table with `e` as the new tail of `coll`
(fn collections.conj [coll e]
  (let [new coll]
    (table.insert new e)
    new))

;; Returns a sequential table with `e` repeated `n` times
(fn collections.repeat [n e]
  (if (<= n 0)
    []
    (collections.cons e (collections.repeat (- n 1) e))))

;; Returns a reversed sequential table of `coll`
;(fn collections.reverse [coll]
;  (local out [])
;  (each [_ e (ipairs coll)]
;    (table.insert out 1 e))
;  out)
(fn collections.reverse [coll]
  (if (collections.empty? coll)
    coll
    (let [[h & r] coll]
      (collections.conj (collections.reverse r) h))))

;; Returns a concatenated sequential table with given sequential tables
(fn collections.concat [...]
  (let [coll [...]]
    (local out [])
    (each [_ t (ipairs coll)]
      (each [_ e (ipairs t)]
        (table.insert out e)))
    out))

;; Returns a range of numbers as a sequential table:
;;
;; (collections.range 5 => [0 1 2 3 4] ; (starts from 0)
;; (collections.range 10 15) => [10 11 12 13 14]
;; (collections.range 20 30 3) => [20 23 26 29]
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
          (_range 0 end 1 []))
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

;; Returns the `n`th element from given sequential table `coll`
;; (`n` starts from 1, not 0)
(fn collections.nth [coll n]
  (let [count (length coll)]
    (if (> n count)
      nil
      (match n
        0 nil
        1 (match count
            0 nil
            _ (. coll 1))
        _ (collections.nth (collections.rest coll) (- n 1))))))

;; Returns the `n`th rest of given sequential table `coll`
;; (`n` starts from 1, not 0)
(fn collections.nthrest [coll n]
  (if (< n 1)
    nil
    (match n
      1 coll
      _ (collections.nthrest (collections.rest coll) (- n 1)))))

;; Returns the first `n` elements from given sequential table `coll`
(fn collections.take [n coll]
  (if (collections.empty? coll)
    coll
    (if (> n 0)
      (let [[h & r] coll]
        (collections.cons h (collections.take (- n 1) r)))
      [])))

;; Returns successive items from sequential table `coll` while each item returns true with function `f` (which takes one parameter)
(fn collections.take-while [f coll]
  (fn _take-while [f coll acc]
    (if (collections.empty? coll)
      acc
      (let [[h & r] coll
            evaluated (f h)]
        (if evaluated
          (_take-while f r (collections.conj acc h))
          acc))))
  (_take-while f coll []))

;; Drops first `n` elements from given sequential table `coll` and returns the remaining
(fn collections.drop [n coll]
  (if (collections.empty? coll)
    coll
    (if (= n 0)
      coll
      (collections.drop (- n 1) (collections.rest coll)))))

;; Returns a sequential map with each element applied with function `f`
;(fn collections.map [f coll]
;  (icollect [_ e (ipairs coll)]
;   (f e)))
(fn collections.map [f coll]
  (if (collections.empty? coll)
    coll
    (let [[h & r] coll]
      (collections.cons (f h) (collections.map f r)))))

;; Returns an accumulated value
;; which is calculated by function `f` (which takes two parameters)
;; with initial value `acc` and each element of sequential table `coll`
(fn collections.reduce [f acc coll]
  (if (collections.empty? coll)
    acc
    (let [[h & r] coll
          acc2 (f acc h)]
      (collections.reduce f acc2 r))))

;; Filter elements from `coll` which evaluates to true with function `f` (which takes one parameter)
(fn collections.filter [f coll]
  (if (collections.empty? coll)
    coll
    (let [[h & r] coll
          filtered (collections.filter f r)]
      (if (f h)
        (collections.cons h filtered)
        filtered))))

;; Returns if each element in `coll` returns true with function `f` (which takes one parameter)
(fn collections.every? [f coll]
  (if (collections.empty? coll)
    true
    (let [[h & r] coll
          evaluated (f h)]
      (if evaluated
        (collections.every? f r)
        false))))

;; Returns the first element in `coll` that evaluates to true with function `f` (which takes one parameter),
;; nil if none
(fn collections.some [f coll]
  (if (collections.empty? coll)
    nil
    (let [[h & r] coll
          evaluated (f h)]
      (if evaluated
        h
        (collections.some f r)))))

;; Sort elements of `coll` with function `f`
;; (function `f` takes two parameters,
;;  returns true when the first parameter is bigger than or equal to the second one, and
;;  returns false when the second one is bigger)
(fn collections.sort [f coll]
  (if (collections.empty? coll)
    coll
    (let [[pivot & r] coll
          ls (collections.filter #(f $1 pivot) r)
          rs (collections.filter #(not (f $1 pivot)) r)]
      (collections.concat (collections.sort f ls) [pivot] (collections.sort f rs)))))

;; Partition elements of `coll` with count `n`:
;;
;; (collections.partition 4 (collections.range 8)) => [[0 1 2 3] [4 5 6 7]]
;; (collections.partition 4 (collections.range 10)) => [[0 1 2 3] [4 5 6 7]] ; drops immature partitions
;; (collections.partition 2 4 (collections.range 10)) => [[0 1] [4 5] [8 9]]
;; (collections.partition 3 3 [:x] (collections.range 10)) => [[0 1 2] [3 4 5] [6 7 8] [9 "x"]] ; fill immature partitions with pad
;; (collections.partition 3 3 [:x :y :z :w] (collections.range 10)) => [[0 1 2] [3 4 5] [6 7 8] [9 "x" "y"]]
(fn collections.partition [n ...]
  (fn _partition [n step pad coll acc]
    (if (collections.empty? coll)
      acc
      (let [p (collections.take n coll)
            l (length p)]
        (if (= n l)
          (_partition n step pad (collections.nthrest coll (+ step 1)) (collections.conj acc p))
          (if (collections.empty? pad)
            acc
            (_partition n step pad (collections.nthrest coll (+ step 1)) (collections.conj acc (collections.take n (collections.concat p pad)))))))))
  (let [args (table.pack ...)]
    (match (. args :n)
      0 nil
      1 (let [coll (. args 1)]
          (_partition n n nil coll []))
      2 (let [step (. args 1)
              coll (. args 2)]
          (_partition n step nil coll []))
      3 (let [step (. args 1)
              pad (. args 2)
              coll (. args 3)]
          (_partition n step pad coll []))
      _ nil)))

;; Returns if given `key` exists in the table `coll`
(fn collections.contains? [coll key]
  (let [val (. coll key)]
    (not (= nil val))))

;; Returns keys of given table `coll`
(fn collections.keys [coll]
  (local out [])
  (each [k _ (pairs coll)]
    (table.insert out k))
  out)

;; Returns values of given table `coll`
(fn collections.vals [coll]
  (local out [])
  (each [_ v (pairs coll)]
    (table.insert out v))
  out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
collections

