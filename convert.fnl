;; fennelib/convert.fnl
;;
;; functions (re)implemented in/efficiently
;; for converting things
;;
;; created on : 2022.07.21.
;; last update: 2022.07.22.
;;
;; $ luarocks install json-lua
;; $ luarocks install ftcsv

(local convert {})

(local json (require "JSON"))
(local csv (require "ftcsv"))

;; Return a table converted from given JSON string `str`. On error, print it and return nil instead.
(fn convert.json->table [str]
  {:fnl/docstring "Return a table converted from given json string `str`. On error, print it and return nil instead."
   :fnl/arglist [str]}
  (let [(ok? decoded) (pcall #(json:decode $1) str)] ; FIXME: cannot use multisyms in pcall?
    (if ok?
      decoded
      (print decoded)))) ; on error, print it and return nil

;; Return a JSON string converted from given table `coll`. On error, print it and return nil instead.
(fn convert.table->json [coll]
  {:fnl/docstring "Return a JSON string converted from given table `coll`. On error, print it and return nil instead."
   :fnl/arglist [coll]}
  (let [(ok? encoded) (pcall #(json:encode $1) coll)] ; FIXME: cannot use multisyms in pcall?
    (if ok?
      encoded
      (print encoded)))) ; on error, print it and return nil

;; Return a table converted from given CSV string `str`. On error, print it and return nil instead.
(fn convert.csv->table [str has-header? delimiter]
  {:fnl/docstring "Return a table converted from given CSV string `str` delimited by `delimiter`. On error, print it and return nil instead."
   :fnl/arglist [str has-header? delimiter]}
  (let [has-header? (or has-header? false)
        delimiter (or delimiter ",")
        (ok? converted) (pcall csv.parse str delimiter {:loadFromString true
                                                        :headers has-header?})]
    (if ok?
      converted
      (print converted))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
convert

