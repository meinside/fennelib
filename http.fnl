;; fennelib/http.fnl
;;
;; functions (re)implemented in/efficiently
;; for handling http resquests and responses
;;
;; created on : 2022.06.10.
;; last update: 2022.07.19.
;;
;; $ luarocks install http

(local http {})

(local hr (require :http.request))
(local hh (require :http.headers))

;; create new headers with http method and existing key-values
(fn new-http-headers [method headers]
  (let [headers (hh.new)]
    (headers:upsert ":method" method) ; http method
    (each [k v (pairs headers)] ; headers
      (headers:upsert k v))
    headers))

;; Return response of HTTP GET request.
(fn http.get [url headers timeout]
  {:fnl/docstring "Return response of HTTP GET request."
   :fnl/arglist [url headers timeout]}
  (let [request (hr.new_from_uri url (new-http-headers "GET" headers))
        timeout (or timeout 10)
        (headers stream errno) (request:go timeout)]
    (if (not= nil errno)
      {:error errno}
      {:status (headers:get ":status")
       :body (stream:get_body_as_string timeout)})))

;; Return response of HTTP POST request.
(fn http.post [url params headers timeout]
  {:fnl/docstring "Return response of HTTP POST request."
   :fnl/arglist [url params headers timeout]}
  (let [params (or params "")
        request (hr.new_from_uri url (new-http-headers "POST" headers))
        timeout (or timeout 10)]
    (request:set_body params) ; set http request body

    (let [(headers stream errno) (request:go timeout)]
      (if (not= nil errno)
        {:error errno}
        {:status (headers:get ":status")
         :body (stream:get_body_as_string timeout)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
http

