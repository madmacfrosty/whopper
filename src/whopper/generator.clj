(ns whopper.generator
  (:require [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.math.numeric-tower :as math]))

(s/def ::first-name #{:anne :alan :brenda :bert :basil :beth :colin :christine :dan :deirdre :stevie :martin :oli
                      :louise :lisa :angie :helen :kirsty :isla :brian :frank :efe :archie :eva :albert :james})
(s/def ::second-name
  #{:smith :frost :doherty :howarth :mirhadi :cook :mcduff :mckie :macdonald :mcstay :gemmill
    :samson :foot :johnson :macron :laurel :maxwell :fleming :carnegie :baird :bell :einstein
    :wright :hanlon :gray :bartley :kamberi :boyle :maclaren :mallan :ambrose :shaw})
(s/def ::person (s/cat :first-name ::first-name :second-name ::second-name))
(s/def ::persons (s/coll-of ::user :distinct true))

(s/def ::adjective #{:smooth :rough :ticklish :angry :mad :soothing :hot :cold :big :small})
(s/def ::noun #{:cup :desk :pen :ferret :cat :mouse :computer :gobstopper :monitor :coat :bean :sandwich :sausage})
(s/def ::system-name (s/cat :adjective ::adjective :noun ::noun))
(s/def ::tenant-name ::system-name)
(s/def ::tenant-id pos-int?)


(s/def ::tenant (s/with-gen
                  (s/keys :req [::system-
                                name ::tenant-id])
                  (fn []
                    (let [a (atom 0)]                      
                      (gen/fmap #(hash-map ::tenant-name %
                                           ::tenant-id (swap! a inc))
                                (s/gen ::system-name))))))

(s/coll-of ::tenant distinct true)

(defn tenant-generator
  [size {:keys [head-tail]}]
  (if (= size 0)
    (repeatedly nil)
    (let [[x & xs] (gen/sample (s/gen ::tenant) size)]
      (gen/frequency [[head-tail (gen/return (:tenant-id x))] [(- 100 head-tail) (gen/elements xs)]]))))

(s/def ::amount (s/with-gen number?
                  #(let [scale-up (partial * 100)
                         scale-down (fn [x] (double (/ x 100)))]
                     (gen/frequency [[6 (gen/fmap scale-down (s/gen (s/int-in 1 (scale-up 100))))]
                                     [3 (gen/fmap scale-down (s/gen (s/int-in 1 (scale-up 1000))))]
                                     [1 (gen/fmap scale-down (s/gen (s/int-in 1 (scale-up 999999))))]]))))
(s/def ::currency #{:gbp :chf :eur :usd})
(s/def ::reference string?)
(s/def ::user uuid?)
(s/def ::user-id pos-int?)
(s/def ::tenant-id pos-int?)
(s/def ::transaction (s/keys :req-un [::person ::currency ::amount ::reference ::user-id ::tenant-id]))


#_(defn transaction-generator
  [{:keys [user-distribution tenant-distribution]}]
  (let [tenant-gen (apply head-tail-distribution tenant-distribution)]
    (gen/bind tenant-gen
              (fn [id]
                (gen/fmap #(assoc % ::tenant-id id) (s/gen ::transaction))))))

;; A person typically uses tx

;; Generate the transactions and fill the users on demand
;; Need to work out the ratio of tenant 

(defn head-tail-distribution
  "Creates a deterministic function taking a positive integer and returning another following a head-tail distro"
  [n n-head ratio]
  {:pre [(< 0 n-head n)
         (rational? ratio)
         (< 0 ratio 1)]}
  (let [head-share (/ ratio n-head)
        tail-share (/ (- 1 ratio) (- n n-head))
        array-size (math/lcm (denominator head-share) (denominator tail-share))
        head-occurrences (* head-share array-size)
        tail-occurrences (* tail-share array-size)
        mapping (vec (concat (mapcat #(repeat head-occurrences %) (range 0 n-head))
                             (mapcat #(repeat tail-occurrences %) (range n-head n))))]
    (fn [x]
      (let [index (-> x (* 65537) (mod array-size))]
        (get mapping index)))))

(def tx-gen (s/gen ::transaction))

(defn create-transaction
  [transaction-id {:keys [transaction->user user->tenant]}]
  (let [user-id (transaction->user transaction-id)
        tenant-id (user->tenant user-id)]
    (-> tx-gen
        gen/generate
        (assoc :transaction-id transaction-id
               :user-id user-id
               :tenant-id tenant-id))))

(defn transactions
  []
  (let [user->tenant (head-tail-distribution 20 2 4/5)
        transaction->user (head-tail-distribution 100000 1000 1/2)]
    (map #(create-transaction % {:user->tenant user->tenant
                                 :transaction->user transaction->user}) (range))))
