;; fennelib/num.fnl
;;
;; functions (re)implemented in/efficiently
;; for dealing with numbers
;;
;; created on : 2022.06.24.
;; last update: 2022.07.01.

(local num {})

;; XXX: merge original math functions for convenience
(let [builtins (require :math)]
  (each [name f (pairs builtins)]
    (tset num name f)))

;; Return a number `n` increased by 1.
(fn num.inc [n]
  {:fnl/docstring "Return a number `n` increased by 1."
   :fnl/arglist [n]}
  (+ n 1))

;; Return a number `n` decreased by 1.
(fn num.dec [n]
  {:fnl/docstring "Return a number `n` decreased by 1."
   :fnl/arglist [n]}
  (- n 1))

;; Return the maximum value from given sequential table `coll`.
(fn num.max [coll]
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
(fn num.min [coll]
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
(fn num.pos? [n]
  {:fnl/docstring "Return if given number `n` is positive."
   :fnl/arglist [n]}
  (> n 0))

;; Return if given number `n` is negative.
(fn num.neg? [n]
  {:fnl/docstring "Return if given number `n` is negative."
   :fnl/arglist [n]}
  (< n 0))

;; Return if given number `n` is zero.
(fn num.zero? [n]
  {:fnl/docstring "Return if given number `n` is zero."
   :fnl/arglist [n]}
  (= n 0))

;; Return if given number `n` is even.
(fn num.even? [n]
  {:fnl/docstring "Return if given number `n` is even."
   :fnl/arglist [n]}
  (= 0 (% n 2)))

;; Return if given number `n` is odd.
(fn num.odd? [n]
  {:fnl/docstring "Return if given number `n` is odd."
   :fnl/arglist [n]}
  (not (num.even? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
num

