;; fennelib/math.fnl
;;
;; functions (re)implemented in/efficiently
;; for dealing with numbers
;;
;; created on : 2022.06.24.
;; last update: 2022.06.24.

(local math {})

;; Returns a number increased by 1
(fn math.inc [n]
  (+ n 1))

;; Returns a number decreased by 1
(fn math.dec [n]
  (- n 1))

;; Return the maximum value from given sequential table
(fn math.max [coll]
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

;; Return the minimum value from given sequential table
(fn math.min [coll]
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

;; Returns if given number is positive
(fn math.pos? [n]
  (> n 0))

;; Returns if given number is positive
(fn math.neg? [n]
  (< n 0))

;; Returns if given number is zero
(fn math.zero? [n]
  (= n 0))

;; Returns if given number is even
(fn math.even? [n]
  (= 0 (% n 2)))

;; Returns if given number is odd
(fn math.odd? [n]
  (not (math.even? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
math

