(ns whopper.elasticsearch
  (:require [clojure.data.json :as json]
            [whopper.generator :as gen]))


(defn batch
  [n]
  (partition-all n))

(def base {:first-name :jim :second-name :spence :amount 100 :ccy :usd})

(def id (atom 0))
(defn tx-id
  []
  (str (swap! id inc)))

(defn add-type
  [m t]
  (map json/write-str [{:_index :acp
                        :_type t
                        :_id (tx-id)}
                       m]))

(defn ->person
  [{:keys [person]}]
  (when-let [[first-name second-name] person]
    (add-type {:prename first-name :surname second-name} :person)))

(defn ->standing-order
  [m]
  (when (-> m
            :transaction-type
            (= :standing-order))
    (add-type (dissoc m :person) :standing-order)))

(defn ->payment
  [m]
  (add-type m :payment))

(defn ->* [tx]
  (mapcat #(% tx) [->person ->standing-order ->payment]))

(def xform
  (comp (mapcat ->*)
        (remove nil?)
        (partition-all 10)))

(defn doit []
  (->> (gen/transactions)
       (take 100)
       (transduce xform conj)))