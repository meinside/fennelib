;; fennelib/init.fnl
;;
;; all functions in `fennelib`
;;
;; created on : 2022.06.23.
;; last update: 2022.07.22.

(local fennelib {})
(local local-module-names [:collections
                           :convert
                           :http
                           :num
                           :test]) ; TODO: add more packages here

;; for requiring module without error
(fn requirex [name]
  (let [in-fennelib (string.match (os.getenv "PWD") "/fennelib$")
        sep (package.config:sub 1 1)]
    (if in-fennelib
      (require name)
      (require (.. :fennelib sep name)))))

;; merge all functions to `fennelib`
(each [_ name (ipairs local-module-names)]
  (let [package (requirex name)]
    (each [name f (pairs package)]
      (tset fennelib name f))))

;; Finally, return all merged things
fennelib

