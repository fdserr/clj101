(ns clj101.core
  (:require
   [cljs.pprint :refer [pprint]]
   cljsjs.react
   cljsjs.react.dom
   sablono.core
   [clj101.db :as db]
   [clj101.ui :as ui])
  (:require-macros
   [sablono.core :refer [html]]))

(enable-console-print!)

(declare render-app)

(defonce app-state (atom db/people))

(defonce state-history (atom {:history [] :step 0}))

(add-watch app-state :render
           (fn [k a o n] (render-app a)))

(add-watch app-state :history
           (fn [k a o n] (swap! state-history update-in [:history] conj n)))

(add-watch state-history :step
           (fn [k a o n] (let [old (:step o)
                               new (:step n)]
                           (if (not= old new)
                             (reset! app-state (nth (:history n) (:step n)))))))

(defn on-js-reload []
  (swap! app-state update-in [:__figwheel_counter] inc))

(defn render-app [s]
  (ReactDOM.render (ui/form-persons s)
                   (.getElementById js/document "app")))


(render-app app-state)
