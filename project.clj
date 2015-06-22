(defproject server-cons "0.2.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-RC2"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/test.check "0.7.0"]
                 [compojure "1.2.1"]
                 [ring/ring-core "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-jetty-adapter "1.3.1"]
                 [environ "0.5.0"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler server-cons.site/site}
  :min-lein-version "2.0.0"
  :uberjar-name "server-cons.jar")
