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

(defn saved-value
  [scope-id]
  (-> (scope-info scope-id) :value :v))

(defn default-log-save-scope
  [scope form-meta id]
  (println "BF" (str "[" id "]") "saving scope with locals" (pr-str (keys scope))))

(defn save-scope
  [{:as opts
    :keys [log-save-scope]}
   scope-id scope form-meta]
  (let [id scope-id]
    (log-save-scope scope form-meta id)
    (swap! runtime-db
      assoc id
      {:scope scope
       :form-meta form-meta})))

(defn save-v
  [scope-id err? v]
  (swap! runtime-db
    assoc-in [scope-id :value]
    {:error? err?
     :v v}))

(defn default-log-compile-sv
  [locals form-meta]
  (println "BF: at" (str (get form-meta :file "(Unknown file)") ":" (:line form-meta))
    "\n"
    "will save scope with locals" (pr-str locals))
  )

(defn default-log-v
  [scope-id form-meta error? v]
  (println "BF" (str "[" scope-id "]") (str (get form-meta :file "(Unknown file)") ":" (:line form-meta))
    (str
      "\n"
      (pr-str (:expr form-meta))
      "\n=>\n"
      (pr-str v))))

(defn save-and-log-v
  [log-v scope-id error? v]
  (save-v scope-id error? v)
  (try
    (let [form-meta (:form-meta (scope-info scope-id))]
      (log-v scope-id form-meta error? v))
    (catch Throwable err nil))
  nil)

(defn sv-emit
  [amp-form amp-env opts expr]
  (let [{:keys [log-compile-sv log-save-scope log-v]
         :or {log-compile-sv `default-log-compile-sv
              log-save-scope `default-log-save-scope
              log-v `default-log-v}} opts
        locals (keys amp-env)
        form-meta (merge
                    {:form `(quote ~amp-form)
                     :expr `(quote ~expr)
                     :file *file*
                     :ns (str *ns*)}
                    (select-keys (meta amp-form) [:line :column]))
        scope-id-s (gensym "scope-id")]
    ((eval log-compile-sv) locals form-meta)
    `(let [~scope-id-s (get-id)]
       (save-scope
         {:log-save-scope ~log-save-scope}
         ~scope-id-s
         ~(into {}
            (map (fn [l]
                   [`(quote ~l) l]))
            locals)
         ~form-meta)
       ~(when expr
          `(try
             (let [v# ~expr]
               (save-and-log-v ~log-v ~scope-id-s false v#)
               v#)
             (catch Throwable err#
               (save-and-log-v ~log-v ~scope-id-s true err#)
               (throw err#)))))
    ))

(defmacro sv
  ([opts expr]
    (sv-emit &form &env opts expr))
  ([expr]
    ;; CAVEAT will lose &form meta
   (sv-emit &form &env nil expr)))

(defmacro defsc
  [scope-id]
  (let [scs (gensym "scope")]
    `(let [~scs (:scope (get @runtime-db ~scope-id))]
       ~(into []
          (map (fn [l]
                 `(def ~l (get ~scs (quote ~l)))))
          (-> (get @runtime-db scope-id) :scope keys))
       )))

(defmacro letsc
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
