(ns bf.impl.runtime)

(defonce ^:private a-id
  (atom 0))

(defn next-id
  "generates a unique execution point id, a positive number"
  []
  (swap! a-id inc))

(defonce db
  (atom {:execution-points {}}))


