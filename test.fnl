; fennelib/test.fnl
;
; functions (re)implemented in/efficiently
; for debugging and testing
;
; created on : 2022.06.15.
; last update: 2022.06.15.

(local test {})

; Prints contents of given table `t` with a number of indentation `indent`
;
; (referenced: https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console)
(fn test.dump-table [t indent]
  (let [indent (or indent 0)]
    (each [k v (pairs t)]
      (let [fmt (.. (string.rep " " (* 2 indent)) k ": ")]
        (match (type v)
          "table" (do
                    (print fmt)
                    (test.dump-table v (+ 1 indent)))
          _ (print (.. fmt (tostring v))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finally, return this module for requiring from the outer world
test

