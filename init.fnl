;; fennelib/init.fnl
;;
;; all functions in `fennelib`
;;
;; created on : 2022.06.23.
;; last update: 2022.07.01.

(local fennelib {})

;; for merging every function to `fennelib`
(fn all-merged [packages]
  (local fennelib {})
  (each [_ package (ipairs packages)]
    (each [name f (pairs package)]
      (tset fennelib name f)))
  fennelib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return all merged things
(all-merged [
             (require :fennelib/collections)
             (require :fennelib/num)
             (require :fennelib/test)
             ;; TODO: add more packages here
             ])

