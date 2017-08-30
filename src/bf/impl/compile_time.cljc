(ns bf.impl.compile-time)

(defonce ^:private a-id
  (atom 0))

(defn next-id
  "generates a unique code site id, a negative integer"
  []
  (swap! a-id dec))

(defonce db
  (atom {:code-sites {}}))

