;; fennelib/http.fnl
;;
;; functions (re)implemented in/efficiently
;; for handling http resquests and responses
;;
;; created on : 2022.06.10.
;; last update: 2022.07.20.
;;
;; $ luarocks install http

(local http {})

(local hr (require :http.request))
(local hh (require :http.headers))
(local default-timeout 10)

;; create new headers with http method and existing key-values
(fn new-http-headers [method headers]
  (let [headers (hh.new)]
    (headers:upsert ":method" method) ; http method
    (each [k v (pairs headers)] ; headers
      (headers:upsert k v))
    headers))

(fn headers->table [headers]
  (local table {})
  (each [k v _ (headers:each)]
    (tset table k v))
  table)

;; Return response of HTTP GET request.
(fn http.get [url headers timeout]
  {:fnl/docstring "Return response of HTTP GET request."
   :fnl/arglist [url headers timeout]}
  (let [request (hr.new_from_uri url (new-http-headers "GET" headers))
        timeout (or timeout default-timeout)
        (headers stream errno) (request:go timeout)]
    (if (not= nil errno)
      {:error errno}
      {:status (tonumber (headers:get ":status")) ; status code number
       :headers (headers->table headers) ; all headers
       :body (stream:get_body_as_string timeout)}))) ; and body

;; Return response of HTTP POST request.
(fn http.post [url body headers timeout]
  {:fnl/docstring "Return response of HTTP POST request."
   :fnl/arglist [url body headers timeout]}
  (let [request (hr.new_from_uri url (new-http-headers "POST" headers))
        timeout (or timeout default-timeout)]
    (if (not= nil body)
     (request:set_body body)) ; set http request body

    (let [(headers stream errno) (request:go timeout)]
      (if (not= nil errno)
        {:error errno}
        {:status (tonumber (headers:get ":status")) ; status code number
         :headers (headers->table headers) ; all headers
         :body (stream:get_body_as_string timeout)})))) ; and body

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally, return this module for requiring from the outer world
http

