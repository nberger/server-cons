(ns server-cons.server
  (:require [server-cons.site :as s]
            [ring.adapter.jetty :as jetty]
            [environ.core :refer [env]]))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty s/site {:port port :join? false})))
