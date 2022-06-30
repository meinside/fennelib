;; fennelib/test.fnl
;;
;; functions (re)implemented in/efficiently
;; for debugging and testing
;;
;; created on : 2022.06.15.
;; last update: 2022.06.30.

(local test {})

;; Returns pretty-printed contents of given table `t` with a number of indentation `indent`.
;;
;; (referenced: https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console)
(fn test.prettify [t indent]
  {:fnl/docstring "Returns pretty-printed contents of given table `t` with a number of indentation `indent`."
   :fnl/arglist [table indent]}
  (fn _table->lines [t indent]
    (local out [])
    (each [k v (pairs t)]
      (let [indented (string.rep " " (* 2 indent))
            line (.. indented k ": ")]
        (match (type v)
          "table" ; recurse on tables
          (do
            (table.insert out (.. line "{"))

            (let [recursed (_table->lines v (+ 1 indent))]
              (each [_ v (ipairs recursed)]
                (table.insert out v)))

            (table.insert out (.. indented "}")))
          _
          (do
            (table.insert out (.. line (tostring v)))))))
    out)
  (let [lines (_table->lines t (or indent 0))]
    (table.concat lines "\n"))) ; return all lines joined with "\n"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
test

