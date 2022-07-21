;; fennelib/convert.fnl
;;
;; functions (re)implemented in/efficiently
;; for converting things
;;
;; created on : 2022.07.21.
;; last update: 2022.07.21.
;;
;; $ luarocks install json-lua
;; $ luarocks install ftcsv

(local convert {})

(local json (require "JSON"))
(local csv (require "ftcsv"))

;; Return a table converted from given JSON string `str`.
(fn convert.json->table [str]
  {:fnl/docstring "Return a table converted from given json string `str`."
   :fnl/arglist [str]}
  (let [decoded (json:decode str)]
    decoded))

;; Return a JSON string converted from given table `coll`.
(fn convert.table->json [coll]
  {:fnl/docstring "Return a JSON string converted from given table `coll`."
   :fnl/arglist [coll]}
  (let [encoded (json:encode coll)]
    encoded))

;; Return a table converted from given CSV string `str`.
(fn convert.csv->table [str has-header? delimiter]
  {:fnl/docstring "Return a table converted from given CSV string `str` delimited by `delimiter`."
   :fnl/arglist [str has-header? delimiter]}
  (let [has-header? (or has-header? false)
        delimiter (or delimiter ",")]
    (csv.parse str delimiter {:loadFromString true
                              :headers has-header?})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
convert

