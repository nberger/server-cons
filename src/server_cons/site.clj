(ns server-cons.site
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer :all]
            [server-cons.core :as c]
            [clojure.edn :as edn]
            [hiccup.form :as f])
  (:use [hiccup.core]))

(defn consolidation-form
  [{:keys [machines max-cpu]}]
  (f/form-to [:post "/"]
             [:fieldset
              [:div.input
               [:label {:for "max-cpu"} "Max avg cpu"]
               [:input#max-cpu {:name "max-cpu" :value max-cpu}]]
              [:div.input
               [:label {:for "machines"} "Machines list (edn)"]
               [:textarea#machines {:name "machines"
                                    :rows 12
                                    :cols 20} (str machines)]]]
             [:input {:type "submit"}]))

(def new-consolidation
  (html (consolidation-form {:max-cpu 60
                             :machines [{:id 1 :cpu-avg 22}
                                        {:id 2 :cpu-avg 17}
                                        {:id 3 :cpu-avg 22}
                                        {:id 4 :cpu-avg 3}
                                        {:id 5 :cpu-avg 6}
                                        {:id 6 :cpu-avg 11}
                                        {:id 7 :cpu-avg 35}
                                        {:id 8 :cpu-avg 26}
                                        {:id 9 :cpu-avg 29}
                                        {:id 10 :cpu-avg 7}]})))

(defn show-allocated-machines
  [machines allocated-machines]
  [:div.result
   [:h1 "Result"]
   [:p "Original machines: " [:bold (count machines)]]
   [:p "Resulting machines: " [:bold (count allocated-machines)]]
   (for [[i group] (map-indexed vector allocated-machines)
         :let [group-cpu (reduce + (map #(Integer. (:cpu-avg %)) group))
               i (inc i)]]
     [:div.group
      [:h1 "Server " i]
      [:p "Machine count: " [:bold (count group)]]
      [:table {:style "text-align: center"}
       [:thead
        [:tr
         [:th "id"]
         [:th "cpu avg"]]]
       [:tbody
        (for [{:keys [id cpu-avg]} group]
          [:tr
           [:td id]
           [:td cpu-avg]])]
       [:tfoot
        [:tr
         [:th "Total"]
         [:th group-cpu]]]]])])

(defn create-consolidation
  [{:keys [machines max-cpu] :as params}]
  (let [machines (edn/read-string machines)
        max-cpu (Integer. max-cpu)
        allocated-machines (c/allocate-machines machines max-cpu)]
    (html (consolidation-form params)
          (show-allocated-machines machines allocated-machines))))

(defroutes app
  (GET "/" [] new-consolidation)
  (POST "/" {params :params} (create-consolidation params))
  (route/not-found "<h1>Page not found</h1>"))

(def site
  (wrap-defaults app (-> site-defaults
                         (assoc-in [:security :anti-forgery] false)
                         (assoc-in [:params :keywordize] true)
                         (assoc-in [:params :urlencoded] true))))
