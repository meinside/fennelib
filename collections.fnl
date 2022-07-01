;; fennelib/collections.fnl
;;
;; functions (re)implemented in/efficiently
;; for handling collections
;;
;; created on : 2022.06.09.
;; last update: 2022.07.01.

(local collections {})

;; for requiring my own packages
(local n (let [in-fennelib (string.match (os.getenv "PWD") "/fennelib$")
               sep (package.config:sub 1 1)]
           (if in-fennelib
             (require :num)
             (require (.. :fennelib sep :num)))))

;; Return the argument.
(fn collections.identity [x]
  {:fnl/docstring "Return the argument."
   :fnl/arglist [x]}
  x)

;; Return the count of elements in given table `coll`.
(fn collections.count [coll]
  {:fnl/docstring "Return the count of elements in given table `coll`."
   :fnl/arglist [coll]}
  (var count 0)
  (each [_ _ (pairs coll)]
    (set count (n.inc count)))
  count)

;; Return if `x` is a table.
(fn collections.table? [x]
  {:fnl/docstring "Return if `x` is a table."
   :fnl/arglist [x]}
  (= "table" (type x)))

;; Return if `coll` is empty or not.
(fn collections.empty? [coll]
  {:fnl/docstring "Return if `coll` is empty or not."
   :fnl/arglist [coll]}
  (or (= nil coll) (= (collections.count coll) 0)))

;; Return the first element of sequential table `coll`.
(fn collections.head [coll]
  {:fnl/docstring "Return the first element of sequential table `coll`."
   :fnl/arglist [coll]}
  (match (length coll)
    0 nil
    _ (. coll 1)))

;; Return elements of sequential table `coll` except the first one.
(fn collections.rest [coll]
  {:fnl/docstring "Return elements of sequential table `coll` except the first one."
   :fnl/arglist [coll]}
  (if (collections.empty? coll)
    coll
    (match (length coll)
      0 nil
      1 []
      _ [(table.unpack coll 2)])))

;; Return the last element of sequential table `coll`.
(fn collections.tail [coll]
  {:fnl/docstring "Return the last element of sequential table `coll`."
   :fnl/arglist [coll]}
  (let [count (length coll)]
    (match count
      0 nil
      _ (. coll count))))

;; Return a new sequential table with `e` as the head and `coll` as the rest.
(fn collections.cons [e coll]
  {:fnl/docstring "Return a new sequential table with `e` as the head and `coll` as the rest."
   :fnl/arglist [e coll]}
  (let [new coll]
    (table.insert new 1 e)
    new))

;; Return a new sequential table with `e` as the new tail of `coll`.
(fn collections.conj [coll e]
  {:fnl/docstring "Return a new sequential table with `e` as the new tail of `coll`."
   :fnl/arglist [coll e]}
  (let [new coll]
    (table.insert new e)
    new))

;; Return a sequential table with `e` repeated `n` times.
(fn collections.repeat [n e]
  {:fnl/docstring "Return a sequential table with `e` repeated `n` times."
   :fnl/arglist [n e]}
  (if (<= n 0)
    []
    (collections.cons e (collections.repeat (- n 1) e))))

;; Return a reversed sequential table of `coll`.
;(fn collections.reverse [coll]
;  (local out [])
;  (each [_ e (ipairs coll)]
;    (table.insert out 1 e))
;  out)
(fn collections.reverse [coll]
  {:fnl/docstring "Return a reversed sequential table of `coll`."
   :fnl/arglist [coll]}
  (if (collections.empty? coll)
    coll
    (let [[h & r] coll]
      (collections.conj (collections.reverse r) h))))

;; Return a concatenated sequential table with given sequential tables.
(fn collections.concat [...]
  {:fnl/docstring "Return a concatenated sequential table with given sequential tables."
   :fnl/arglist [xs1 xs2 ...]}
  (let [coll [...]]
    (local out [])
    (each [_ t (ipairs coll)]
      (each [_ e (ipairs t)]
        (table.insert out e)))
    out))

;; Return a range of numbers as a sequential table.
;;
;; (collections.range 5 => [0 1 2 3 4] ; (starts from 0)
;; (collections.range 10 15) => [10 11 12 13 14]
;; (collections.range 20 30 3) => [20 23 26 29]
(fn collections.range [...]
  {:fnl/docstring "Return a range of numbers as a sequential table."
   :fnl/arglist [start end step]}
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

;; Return the `n`th element from sequential table `coll`.
;; NOTE: `n` starts from 1, not 0.
(fn collections.nth [coll n]
  {:fnl/docstring "Return the `n`th element from sequential table `coll`."
   :fnl/arglist [coll n]}
  (let [count (length coll)]
    (if (> n count)
      nil
      (match n
        0 nil
        1 (match count
            0 nil
            _ (. coll 1))
        _ (collections.nth (collections.rest coll) (- n 1))))))

;; Return the `n`th rest of `coll`.
;; NOTE: `n` starts from 1, not 0.
(fn collections.nthrest [coll n]
  {:fnl/docstring "Return the `n`th rest of `coll`."
   :fnl/arglist [coll n]}
  (if (< n 1)
    nil
    (match n
      1 coll
      _ (collections.nthrest (collections.rest coll) (- n 1)))))

;; Return the first `n` elements from `coll`.
(fn collections.take [n coll]
  {:fnl/docstring "Return the first `n` elements from `coll`."
   :fnl/arglist [n coll]}
  (if (collections.empty? coll)
    coll
    (if (> n 0)
      (let [[h & r] coll]
        (collections.cons h (collections.take (- n 1) r)))
      [])))

;; Return last `n` elements from sequential table `coll`.
(fn collections.take-last [n coll]
  {:fnl/docstring "Return last `n` elements from sequential table `coll`."
   :fnl/arglist [n coll]}
  (let [taken (length (collections.take n coll))
        dropped (- (length coll) taken)]
    (collections.drop dropped coll)))

;; Return successive items from `coll` while each item returns true with function `f` (which takes one parameter).
(fn collections.take-while [f coll]
  {:fnl/docstring "Return successive items from `coll` while each item returns true with function `f` (which takes one parameter)."
   :fnl/arglist [f coll]}
  (fn _take-while [f coll acc]
    (if (collections.empty? coll)
      acc
      (let [[h & r] coll
            evaluated (f h)]
        (if evaluated
          (_take-while f r (collections.conj acc h))
          acc))))
  (_take-while f coll []))

;; Drop first `n` elements of `coll` and return the remaining.
(fn collections.drop [n coll]
  {:fnl/docstring "Drop first `n` elements of `coll` and return the remaining."
   :fnl/arglist [n coll]}
  (if (collections.empty? coll)
    coll
    (if (= n 0)
      coll
      (collections.drop (- n 1) (collections.rest coll)))))

;; Return all but the last `n`(default 1) elements from sequential table `coll`.
;;
;; (collections.drop-last [1 2 3 4]) => [1 2 3]
;; (collections.drop-last 2 [1 2 3 4]) => [1 2]
;; (collections.drop-last 4 [1 2]) => []
(fn collections.drop-last [...]
  {:fnl/docstring "Return all but the last `n`(default 1) elements from sequential table `coll`."
   :fnl/arglist [n coll]}
  (fn _drop-last [n coll]
    (let [count (length (collections.drop n coll))]
      (collections.take count coll)))
  (let [args (table.pack ...)]
    (match (. args :n)
      1 (let [n 1
              coll (. args 1)]
          (_drop-last n coll))
      2 (let [n (. args 1)
              coll (. args 2)]
          (_drop-last n coll))
      _ nil)))

;; Return a sequential table with items in `coll`,
;; starting from the first item which evaluates to false with function `f` (which takes one parameter).
(fn collections.drop-while [f coll]
  {:fnl/docstring "Return a sequential table with items in `coll`, starting from the first item which evaluates to false with function `f` (which takes one parameter)."
   :fnl/arglist [f coll]}
  (let [[h & r] coll
        evaluated (f h)]
    (if evaluated
      (collections.drop-while f r)
      coll)))

;; Return splitted lists of sequential table `coll` with split position `n` (0-based).
(fn collections.split-at [n coll]
  {:fnl/docstring "Return splitted lists of sequential table `coll` with split position `n` (0-based)."
   :fnl/arglist [n coll]}
  [(collections.take n coll) (collections.drop n coll)])

;; Return splitted lists of sequential table `coll`, split at the first position where function `f` returns false.
(fn collections.split-with [f coll]
  {:fnl/docstring "Return splitted lists of sequential table `coll`, split at the first position where function `f` returns false."
   :fnl/arglist [f coll]}
  [(collections.take-while f coll) (collections.drop-while f coll)])

;; Return a table of which rest of `...` are merged into the first one.
(fn collections.merge [...]
  {:fnl/docstring "Return a table of which rest of `...` are merged into the first one."
   :fnl/arglist [xs1 xs2 ...]}
  (let [coll [...]]
    (collections.reduce #(let [in (or $2 {})
                               out $1]
                           (each [k v (pairs in)]
                             (if (collections.array? in)
                               (table.insert out v) ; sequential table (array)
                               (tset out k v))) ; table (map)
                           out) {} coll)))

;; Return a table with given keys and values.
(fn collections.zipmap [keys vals]
  {:fnl/docstring "Return a table with given keys and values."
   :fnl/arglist [keys vals]}
  (fn _zipmap [keys vals acc]
    (if (or (collections.empty? keys) (collections.empty? vals))
      acc
      (let [[k & ks] keys
            [v & vs] vals]
        (_zipmap ks vs (collections.tset acc k v)))))
  (_zipmap keys vals {}))

;; Return if all items in `coll` are different.
(fn collections.distinct? [coll]
  {:fnl/docstring "Return if all items in `coll` are different."
   :fnl/arglist [coll]}
  (fn _distinct? [coll hash]
    (if (collections.empty? coll)
      true
      (let [[h & r] coll]
        (if (collections.contains? hash h)
          false
          (_distinct? r (collections.tset hash h true))))))
  (_distinct? coll {}))

;; Return a sequential table `coll` with duplicated items removed.
(fn collections.distinct [coll]
  {:fnl/docstring "Return a sequential table `coll` with duplicated items removed."
   :fnl/arglist [coll]}
  (fn _distinct [coll hash acc]
    (if (collections.empty? coll)
      acc
      (let [[h & r] coll]
        (if (collections.contains? hash h)
          (_distinct r hash acc)
          (_distinct r (collections.tset hash h true) (collections.conj acc h))))))
  (_distinct coll {} []))

;; Return a table of the elements of sequential table `coll`,
;; each key is the result of function `f` (which takes one element) on each element,
;; and each value is the sequential tables of the elements grouped by each key.
(fn collections.group-by [f coll]
  {:fnl/docstring "Return a table of the elements of sequential table `coll`, each key is the result of function `f` (which takes one element) on each element, and each value is the sequential tables of the elements grouped by each key."
   :fnl/arglist [f coll]}
  (fn _group-by [f coll acc]
    (if (collections.empty? coll)
      acc
      (let [[h & r] coll
            key (f h)]
        (match (. acc key)
          value (_group-by f r (collections.tset acc key (collections.conj value h)))
          _ (_group-by f r (collections.tset acc key [h]))))))
  (_group-by f coll {}))

;; Return a table of which keys are distinct items in table `coll`
;; and values are the number of appearances.
;;
;; NOTE: keys are converted to string with `tostring` due to the confusion with sequential tables.
(fn collections.frequencies [coll]
  {:fnl/docstring "Return a table of which keys are distinct items in table `coll`, and values are the number of appearances."
   :fnl/arglist [coll]}
  (collections.reduce #(let [hash $1
                             value (tostring $2)
                             count (or (. hash value) 0)]
                         (collections.tset hash value (n.inc count))) {} coll))

;; Return a sequential map with each element of `coll` evaluated with function `f`.
;(fn collections.map [f coll]
;  (icollect [_ e (ipairs coll)]
;   (f e)))
(fn collections.map [f coll]
  {:fnl/docstring "Return a sequential map with each element of `coll` evaluated with function `f`."
   :fnl/arglist [f coll]}
  (if (collections.empty? coll)
    coll
    (let [[h & r] coll]
      (collections.cons (f h) (collections.map f r)))))

;; Return an accumulated value
;; which is evaluated by function `f` (which takes two parameters)
;; with initial value `acc` and each element of `coll`.
(fn collections.reduce [f acc coll]
  {:fnl/docstring "Return an accumulated value which is evaluated by function `f` (which takes two parameters) with initial value `acc` and each element of `coll`."
   :fnl/arglist [f acc coll]}
  (if (collections.empty? coll)
    acc
    (let [[h & r] coll
          applied (f acc h)]
      (collections.reduce f applied r))))

;; Filter elements from `coll` which evaluates to true with function `f` (which takes one parameter).
(fn collections.filter [f coll]
  {:fnl/docstring "Filter elements from `coll` which evaluates to true with function `f` (which takes one parameter)."
   :fnl/arglist [f coll]}
  (if (collections.empty? coll)
    coll
    (let [[h & r] coll
          filtered (collections.filter f r)]
      (if (f h)
        (collections.cons h filtered)
        filtered))))

;; Return if each element in `coll` returns true with function `f` (which takes one parameter).
(fn collections.every? [f coll]
  {:fnl/docstring "Return if each element in `coll` returns true with function `f` (which takes one parameter)."
   :fnl/arglist [f coll]}
  (if (collections.empty? coll)
    true
    (let [[h & r] coll
          evaluated (f h)]
      (if evaluated
        (collections.every? f r)
        false))))

;; Return the first element in `coll` that evaluates to true with function `f` (which takes one parameter),
;; nil if none.
(fn collections.some [f coll]
  {:fnl/docstring "Return the first element in `coll` that evaluates to true with function `f` (which takes one parameter), nil if none."
   :fnl/arglist [f coll]}
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
;;  returns false when the second one is bigger).
(fn collections.sort [f coll]
  {:fnl/docstring "Sort elements of `coll` with function `f` (function `f` takes two parameters, returns true when the first parameter is bigger than or equal to the second one, and returns false when the second one is bigger)."
   :fnl/arglist [f coll]}
  (if (collections.empty? coll)
    coll
    (let [[pivot & r] coll
          ls (collections.filter #(f $1 pivot) r)
          rs (collections.filter #(not (f $1 pivot)) r)]
      (collections.concat (collections.sort f ls) [pivot] (collections.sort f rs)))))

;; Partition elements of sequential table `coll` with count `n`.
;;
;; (collections.partition 4 (collections.range 8)) => [[0 1 2 3] [4 5 6 7]]
;; (collections.partition 4 (collections.range 10)) => [[0 1 2 3] [4 5 6 7]] ; drops immature partitions
;; (collections.partition 2 4 (collections.range 10)) => [[0 1] [4 5] [8 9]]
;; (collections.partition 3 3 [:x] (collections.range 10)) => [[0 1 2] [3 4 5] [6 7 8] [9 "x"]] ; fill immature partitions with pad
;; (collections.partition 3 3 [:x :y :z :w] (collections.range 10)) => [[0 1 2] [3 4 5] [6 7 8] [9 "x" "y"]]
(fn collections.partition [n ...]
  {:fnl/docstring "Partition elements of sequential table `coll` with count `n`."
   :fnl/arglist [n step pad coll]}
  (fn _partition [n step pad coll acc]
    (if (collections.empty? coll)
      acc
      (let [p (collections.take n coll)
            l (length p)]
        (if (= n l)
          (_partition n step pad (collections.nthrest coll (n.inc step)) (collections.conj acc p))
          (if (collections.empty? pad)
            acc
            (_partition n step pad (collections.nthrest coll (n.inc step)) (collections.conj acc (collections.take n (collections.concat p pad)))))))))
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

;; Return items of `coll` splitted whenever each one evaluates to a new value with function `f` (which takes one parameter).
;;
;; (collections.partition-by #(= 0 (% $1 2)) [1 3 5 7 8 10 12 13 14]) => [[1 3 5 7] [8 10 12] [13] [14]]
(fn collections.partition-by [f coll]
  {:fnl/docstring "Return items of `coll` splitted whenever each one evaluates to a new value with function `f` (which takes one parameter)."
   :fnl/arglist [f coll]}
  (fn _partition-by [f coll acc]
    (if (collections.empty? coll)
      acc
      (let [[h & r] coll
            evaluated (f h)
            run (collections.cons h (collections.take-while #(= evaluated (f $1)) r))
            remaining (collections.drop (length run) coll)]
        (_partition-by f remaining (collections.conj acc run)))))
  (_partition-by f coll []))

;; Return if given `key` exists in the table `coll`.
(fn collections.contains? [coll key]
  {:fnl/docstring "Return if given `key` exists in the table `coll`."
   :fnl/arglist [coll key]}
  (let [val (. coll key)]
    (not (= nil val))))

;; Return keys of given table `coll`.
(fn collections.keys [coll]
  {:fnl/docstring "Return keys of given table `coll`."
   :fnl/arglist [coll]}
  (local out [])
  (each [k _ (pairs coll)]
    (table.insert out k))
  out)

;; Return values of given table `coll`.
(fn collections.vals [coll]
  {:fnl/docstring "Return values of given table `coll`."
   :fnl/arglist [coll]}
  (local out [])
  (each [_ v (pairs coll)]
    (table.insert out v))
  out)

;; Return if `x` is a sequential table.
;; (https://stackoverflow.com/questions/6077006/how-can-i-check-if-a-lua-table-contains-only-sequential-numeric-indices)
(fn collections.array? [x]
  {:fnl/docstring "Return if `x` is a sequential table."
   :fnl/arglist [x]}
  (if (collections.table? x)
    (if (next x)
      (let [indices (collections.range 1 (n.inc (collections.count x)))]
        (= nil (collections.some #(= nil (. x $1)) indices)))
      true)
    false))

;; Return if `x` is a table but not sequential.
(fn collections.map? [x]
  {:fnl/docstring "Return if `x` is a table but not sequential."
   :fnl/arglist [x]}
  (if (collections.table? x)
    (if (next x)
      (not (collections.array? x))
      true)
    false))

;; Return a new collection consisting of `to` with items of `from` conjoined.
(fn collections.into [to from]
  {:fnl/docstring "Return a new collection consisting of `to` with items of `from` conjoined."
   :fnl/arglist [to from]}
  (collections.merge to from))

;; Return a table `coll` with key `k` and value `v` applied (just apply `tset` and return it).
(fn collections.tset [coll k v]
  {:fnl/docstring "Return a table `coll` with key `k` and value `v` applied (just apply `tset` and return it)."
   :fnl/arglist [coll k v]}
  (let [tbl coll]
    (tset tbl k v)
    tbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
collections

