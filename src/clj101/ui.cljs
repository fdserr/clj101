(ns clj101.ui
  (:require
   [cljs.pprint :refer [pprint]]
   [clj101.db :as db]
   sablono.core)
  (:require-macros
   [sablono.core :refer [html]]))

(defn set-step [state-history step]
  (swap! state-history assoc :step step))

(defn debug [s h]
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


(def throttle-input
  (let [h (volatile! nil)]
    (fn [s mutation & args]
      ; (when-let [t @h]
      ;   (.clearTimeout js/window t))
      (vreset! h
        (.setTimeout js/window
                    #(swap! s (apply mutation args))
                    200)))))

(defn edit-person [app-state person]
  (swap! app-state assoc :edit person))

(defn person [app-state index person]
  (html
   [:li {:key (str index)
         :on-click #(edit-person app-state person)}
    (str person)]))

(defn persons-list [app-state]
  (let [db-value @app-state
        persons (:persons db-value)]
    (html
       [:div {} "Persons"
        [:ul {}
         (map-indexed #(person app-state %1 %2) (vals persons))]])))

;;;;;;;;;;;;;;;;;;; pers form

;; home
;;;; persons
;;;; companies
;;;; roles

(defn navigation [store]
  (html [:ul
         [:li {:on-click #(db/dispatch! swap! store assoc :screen :home)} "HOME"]
         [:li {:on-click #(db/dispatch! swap! store assoc :screen :persons)} "PERSONS"]
         [:li {:on-click #(db/dispatch! swap! store assoc :screen :companies)} "COMPANIES"]
         [:li {:on-click #(db/dispatch! swap! store assoc :screen :roles)} "ROLES"]]))

(defmulti screen
  "doc"
  (fn [store]
    (get @store :screen :home))
  :default :home)

(defmethod screen :home
  [store]
  (html [:h1 "HOME"]))

(defmethod screen :persons
  [store]
  (html [:h1 "PERSONS"]))

(defmethod screen :companies
  [store]
  (html [:h1 "COMPANIES"]))

(defmethod screen :roles
  [store]
  (html [:h1 "ROLES"]))

(defn app [store]
  (html
   [:div
    [:h1 "APP"]
    (screen store)
    (navigation store)]))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn form-persons [app-state]
  (let [db-value @app-state
        person (get db-value :edit {:name "" :address "" :phone ""})]
    (html
      [:div
      ;  (debug app-state state-history)
       [:div
        [:input {:type "text"
                 :placeholder "Name"
                 :value (:name person)
                ;  :on-change #(throttle-input)
                 :on-change (fn [e]
                                (-> (.persist e)
                                    (assoc :name (-> e .-target .-value))
                                    (assoc :address (:address person))
                                    (assoc :phone (:phone person))))}]
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
                                          (db/update-person (:edit s))
                                          (dissoc :edit))))}
         "UPDATE"]
        [:br]
        [:button {:on-click #(swap! app-state
                                   (fn [s]
                                      (-> s
                                          (db/add-person (:edit s))
                                          (dissoc :edit))))}
         "ADD"]
        [:button {:on-click #(swap! app-state
                                   (fn [s]
                                      (-> s
                                          (db/delete-person (get-in s [:edit :name]))
                                          (dissoc :edit))))}
         "DELETE"]
        [:br]
        (persons-list app-state)]])))



(defn div [s]
  (html
    [:div
     {:style {}}
     [:p {} (:value @s)]
     [:button {:on-click #(swap! s update-in [:value] dec)} "PUSH ME"]]))
