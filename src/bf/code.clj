(ns bf.code)

(defonce next-id (atom 0))

(defn get-id
  []
  (swap! next-id inc))

(defonce runtime-db
  (atom {}))

(defn last-scope-id []
  (apply max (keys @runtime-db)))

(defn scope-info [scope-id]
  (get @runtime-db scope-id))

(defn default-log-save-scope
  [scope form-meta id]
  (prn "Saving scope with locals" (keys scope) "with id" id))

(defn save-scope
  [{:as opts
    :keys [log-save-scope]}
   scope form-meta]
  (let [id (get-id)]
    (log-save-scope scope form-meta id)
    (swap! runtime-db
      assoc id
      {:scope scope
       :form-meta form-meta})))

(defn default-log-compile-sv
  [locals form-meta]
  (prn "Will save scopes with locals" locals))

(defmacro sv
  ([opts expr]
    (let [{:keys [log-compile-sv log-save-scope]
           :or {log-compile-sv `default-log-compile-sv
                log-save-scope `default-log-save-scope}} opts
          locals (keys &env)
          form-meta (merge
                      {:form `(quote ~&form)
                       :expr `(quote ~expr)}
                      (select-keys (meta &form) [:line :column]))]
      ((eval log-compile-sv) locals form-meta)
      `(do
         (save-scope
           {:log-save-scope ~log-save-scope}
           ~(into {}
              (map (fn [l]
                     [`(quote ~l) l]))
              locals)
           ~form-meta)
         ~expr)
      ))
  ([expr]
    ;; CAVEAT will lose &form meta
    `(sv {} ~expr)))

(defmacro defsc
  [scope-id]
  (let [scs (gensym "scope")]
    `(let [~scs (:scope (get @runtime-db ~scope-id))]
       ~(into []
          (map (fn [l]
                 `(def ~l (get ~scs (quote ~l)))))
          (-> (get @runtime-db scope-id) :scope keys))
       )))

(defmacro withsc
  [scope-id & body]
  (let [scs (gensym "scope")]
    `(let
       ~(into
          `[~scs (:scope (get @runtime-db ~scope-id))]
          (mapcat (fn [l]
                    [l `(get ~scs (quote ~l))]
                    ))
          (-> (get @runtime-db scope-id) :scope keys))
       ~@body
       )))
