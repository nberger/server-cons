(defproject server-cons "0.2.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.logic "0.8.8"]
                 [midje "1.6.3"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
