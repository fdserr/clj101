(ns clj101.db)

(def people {:persons {"Peter"
                       {:name "Peter"
                        :address "SG"
                        :phone "911"}
                       "John"
                       {:name "John"
                        :address "HK"
                        :phone "852911"}
                       "Mary"
                       {:name "Mary"
                        :address "CN"
                        :phone "86911"}}
              :companies {"Palo"
                          {:name "Palo"
                           :address "SG"
                           :phone "911"}
                          "Dokmee"
                          {:name "Dokmee"
                           :address "US"
                           :phone "852911"}}
              :work-for [{:person-name "Peter"
                          :company-name "Palo"}]
              :roles ["Admin" "Accountant" "User"]
              :role-in-company [{:person-name "Peter"
                                 :company-name "Palo"
                                 :role "Accountant"}]})

(defn wrap-exception [f args]
  (try
    (apply f args)
    (catch js/Error e
      (do
        (println "EXCEPTION: " (.-message e))
        (throw e)))))


(defn dispatch! [f & args]
  (let [r (wrap-exception f args)]
    (println (str "dispatch: " f " : " args))
    (println (str "result: " r))
    r))

(defn add-person [db {:keys [name address phone] :as person}]
  (if
    (every? #(not= "" %) [name address phone])
    (update-in db [:persons] conj [(:name person) person])
    (throw (js/Error. "Person info incomplete %s" person))))

(defn add-company [db {company-name :name :as company}]
  (update-in db [:companies] conj [company-name company]))

(defn person-exists? [db person-name]
  (some? (get-in db [:persons person-name])))

(defn company-exists? [db company-name]
  (some? (get-in db [:companies company-name])))

(defn role-exists? [db role]
  (some? (seq (filter #(= role %) (:roles db)))))

(defn update-person [db {name :name :as person}]
  (if (person-exists? db name)
    (assoc-in db [:persons name] person)
    (throw (js/Error. "Person not found"))))

(defn delete-person [db person-name]
  (update-in db [:persons] dissoc person-name))

(defn add-work-for [db person-name company-name]
  (if (and (company-exists? db company-name)
           (person-exists? db person-name))
    (update-in db [:work-for]
               #(into [] (conj % {:person-name person-name :company-name company-name})))
    (throw (js/Error. "Error adding work-for"))))

(defn add-role-for [db person-name company-name role]
  (if (and (company-exists? db company-name)
           (person-exists? db person-name)
           (role-exists? db role))
    (update-in db [:role-in-company]
               #(into [] (conj % {:person-name person-name :company-name company-name :role role})))
    (throw (js/Error. "Error adding role-for"))))
