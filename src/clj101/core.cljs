(ns clj101.core
  (:require
   [cljs.pprint :refer [pprint]]
   cljsjs.react
   cljsjs.react.dom
   sablono.core
   [clj101.persons :as persons])
  (:require-macros
   [sablono.core :refer [html]]))

(enable-console-print!)

(declare render-app)

; (defn ui-input []
;   (React.createElement "div" "MYDIV"))

(defonce app-state (atom persons/people))

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

(defn set-step [state-history step]
  (swap! state-history assoc :step step))

;;;;; UI

(def throttle-input
  (let [h (volatile! nil)]
    (fn [app-state mutation & args]
      ; (when-let [t @h]
      ;   (.clearTimeout js/window t))
      (vreset! h
        (.setTimeout js/window
                    #(swap! app-state (apply mutation args))
                    200)))))

(defn ui-debug [s h]
  (let [state @s
        state (with-out-str (pprint state))
        history @h
        history (with-out-str (pprint history))]
    (html
     [:div
      [:code state]
      [:code history]
      [:br]
      [:input {:on-change #(set-step h (js/parseInt (-> % .-target .-value)))}]])))


(defn edit-person [app-state person]
  (swap! app-state assoc :edit person))

(defn ui-person [app-state index person]
  (html
   [:li {:key (str index)
         :on-click #(edit-person app-state person)}
    (str person)]))

(defn ui-persons-list [app-state]
  (let [db-value @app-state
        persons (:persons db-value)]
    (html
       [:div {} "Persons"
        [:ul {}
         (map-indexed #(ui-person app-state %1 %2) (vals persons))]])))

(defn ui-form-persons [app-state]
  (let [db-value @app-state
        person (get db-value :edit {:name "" :address "" :phone ""})]
    (html
      [:div
       (ui-debug app-state state-history)
       [:div
        [:input {:type "text"
                 :placeholder "Name"
                 :value (:name person)
                 :on-change #(throttle-input
                               app-state
                               (fn [e]
                                 (-> e
                                     (assoc :name (-> e .-target .-value))
                                     (assoc :address (:address person))
                                     (assoc :phone (:phone person))))
                               %)}]
        [:br]
        [:input {:type "text"
                 :placeholder "Address"
                 :value (:address person)
                 :on-change #(swap! app-state
                                    update-in [:edit]
                                    (fn [e]
                                      (-> e
                                          (assoc :address (-> % .-target .-value))
                                          (assoc :name (:name person))
                                          (assoc :phone (:phone person)))))}]
        [:br]
        [:input {:type "text"
                 :placeholder "Phone"
                 :value (:phone person)
                 :on-change #(swap! app-state
                                    update-in [:edit]
                                    (fn [e]
                                      (-> e
                                          (assoc :phone (-> % .-target .-value))
                                          (assoc :name (:name person))
                                          (assoc :address (:address person)))))}]
        [:br]
        [:button {:on-click #(swap! app-state
                                   (fn [s]
                                      (-> s
                                          (persons/update-person (:edit s))
                                          (dissoc :edit))))}
         "UPDATE"]
        [:br]
        [:button {:on-click #(swap! app-state
                                   (fn [s]
                                      (-> s
                                          (persons/add-person (:edit s))
                                          (dissoc :edit))))}
         "ADD"]
        [:button {:on-click #(swap! app-state
                                   (fn [s]
                                      (-> s
                                          (persons/delete-person (get-in s [:edit :name]))
                                          (dissoc :edit))))}
         "DELETE"]
        [:br]
        (ui-persons-list app-state)]])))



(defn ui-div [s]
  (html
    [:div
     {:style {}}
     [:p {} (:value @s)]
     [:button {:on-click #(swap! s update-in [:value] dec)} "PUSH ME"]]))

;;;;;;;;;;

(defn render-app [s]
  (ReactDOM.render (html [:p (:text @s)])
                   (.getElementById js/document "app")))


(defn on-js-reload []
  (swap! app-state update-in [:__figwheel_counter] inc))

(render-app app-state)
