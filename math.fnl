;; fennelib/math.fnl
;;
;; functions (re)implemented in/efficiently
;; for dealing with numbers
;;
;; created on : 2022.06.24.
;; last update: 2022.07.01.

(local math {})

;; XXX: import original math functions
(let [builtins (require :math)]
  (each [name f (pairs builtins)]
    (tset math name f)))

;; Return a number `n` increased by 1.
(fn math.inc [n]
  {:fnl/docstring "Return a number `n` increased by 1."
   :fnl/arglist [n]}
  (+ n 1))

;; Return a number `n` decreased by 1.
(fn math.dec [n]
  {:fnl/docstring "Return a number `n` decreased by 1."
   :fnl/arglist [n]}
  (- n 1))

;; Return the maximum value from given sequential table `coll`.
(fn math.max [coll]
  {:fnl/docstring "Return the maximum value from given sequential table `coll`."
   :fnl/arglist [coll]}
  (fn _max [coll mx]
    (if (> (length coll) 0)
      (let [[h & r] coll]
        (if (> h mx)
          (_max r h)
          (_max r mx)))
      mx))
  (if (or (= nil coll) (= (length coll) 0))
    nil
    (let [[h & r] coll] 
      (_max r h))))

;; Return the minimum value from given sequential table `coll`.
(fn math.min [coll]
  {:fnl/docstring "Return the minimum value from given sequential table `coll`."
   :fnl/arglist [coll]}
  (fn _min [coll mn]
    (if (> (length coll) 0)
      (let [[h & r] coll]
        (if (< h mn)
          (_min r h)
          (_min r mn)))
      mn))
  (if (or (= nil coll) (= (length coll) 0))
    nil
    (let [[h & r] coll] 
      (_min r h))))

;; Return if given number `n` is positive.
(fn math.pos? [n]
  {:fnl/docstring "Return if given number `n` is positive."
   :fnl/arglist [n]}
  (> n 0))

;; Return if given number `n` is negative.
(fn math.neg? [n]
  {:fnl/docstring "Return if given number `n` is negative."
   :fnl/arglist [n]}
  (< n 0))

;; Return if given number `n` is zero.
(fn math.zero? [n]
  {:fnl/docstring "Return if given number `n` is zero."
   :fnl/arglist [n]}
  (= n 0))

;; Return if given number `n` is even.
(fn math.even? [n]
  {:fnl/docstring "Return if given number `n` is even."
   :fnl/arglist [n]}
  (= 0 (% n 2)))

;; Return if given number `n` is odd.
(fn math.odd? [n]
  {:fnl/docstring "Return if given number `n` is odd."
   :fnl/arglist [n]}
  (not (math.even? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
math

