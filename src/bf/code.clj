(ns bf.code
  (:require [clojure.core.async :as a]
            [bf.impl.compile-time :as ct]
            [bf.impl.runtime :as rt]))



(defn last-scope-id []
  (apply max (keys (:execution-points @rt/db))))

(defn scope-info [scope-id]
  (get-in @rt/db [:execution-points scope-id]))

(defn saved-value
  [scope-id]
  (-> (scope-info scope-id) :value :v))

(defn default-log-save-scope
  [scope form-meta id]
  (println "BF"
    (pr-str [id (:code-site-id form-meta)]) "saving scope with locals" (pr-str (:locals form-meta))))

(defn save-scope
  [{:as opts
    :keys [log-save-scope]}
   scope-id scope form-meta]
  (let [id scope-id]
    (log-save-scope scope form-meta id)
    (swap! rt/db
      update-in [:execution-points id] merge
      {:scope scope
       :form-meta form-meta})))

(defn save-v
  [scope-id err? v]
  (swap! rt/db
    assoc-in [:execution-points scope-id :value]
    {:error? err?
     :v v}))

(defn default-log-compile-sv
  [locals form-meta]
  (println "BF"
    (str "<" (:code-site-id form-meta) ">")
    (str (get form-meta :file "(Unknown file)") ":" (:line form-meta))
    "\n"
    "will save scope with locals" (pr-str locals))
  )

(defn default-log-v
  [scope-id form-meta error? v]
  (println "BF"
    (pr-str [scope-id (:code-site-id form-meta)])
    (str (get form-meta :file "(Unknown file)") ":" (:line form-meta))
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

(defn make-form-meta
  [code-site-id amp-form amp-env expr]
  (merge
    {:code-site-id code-site-id
     :form amp-form
     :locals (vec (keys amp-env))
     :expr expr
     :file *file*
     :ns (ns-name *ns*)}
    (select-keys (meta amp-form) [:line :column])))

(defn spy-emit
  [amp-form amp-env opts expr]
  (let [{:keys [log-compile-sv log-save-scope log-v]
         :or {log-compile-sv `default-log-compile-sv
              log-save-scope `default-log-save-scope
              log-v `default-log-v}} opts
        code-site-id (ct/next-id)
        form-meta (make-form-meta code-site-id amp-form amp-env expr)
        locals (:locals form-meta)
        rt-id-s (gensym "execution-point-id")]
    (swap! ct/db update-in [:code-sites code-site-id]
      merge form-meta)
    ((eval log-compile-sv) locals form-meta)
    `(let [~rt-id-s (rt/next-id)]
       (save-scope
         {:log-save-scope ~log-save-scope}
         ~rt-id-s
         ~(into {}
            (map (fn [l]
                   [`(quote ~l) l]))
            locals)
         (quote ~form-meta))
       ~(when expr
          `(try
             (let [v# ~expr]
               (save-and-log-v ~log-v ~rt-id-s false v#)
               v#)
             (catch Throwable err#
               (save-and-log-v ~log-v ~rt-id-s true err#)
               (throw err#)))))
    ))

(defmacro spy
  ([opts expr]
    (spy-emit &form &env opts expr))
  ([expr]
   (spy-emit &form &env nil expr)))

(defn save-brk
  [{:as opts
    :keys [log-save-scope]}
   scope-id scope form-meta
   =ch=]
  (let [id scope-id]
    (log-save-scope scope form-meta id)
    (swap! rt/db
      update-in [:execution-points id] merge
      {:scope scope
       :form-meta form-meta
       :breakpoint-chan =ch=})))

(defmacro brk
  ;; TODO custom logging functions
  ;; TODO enable / disable breakpoint at call site
  [opts expr]
  (let [{:keys [log-compile-sv log-save-scope log-v]
         :or {log-compile-sv `default-log-compile-sv
              log-save-scope `default-log-save-scope}} opts
        code-site-id (ct/next-id)
        scope-id-s (gensym "scope-id")
        form-meta (make-form-meta code-site-id &form &env expr)]
    (swap! ct/db update-in [:code-sites code-site-id]
      merge form-meta)
    ((eval log-compile-sv) (:locals form-meta) form-meta)
    `(let [ch# (a/chan 8)
           ~scope-id-s (rt/next-id)]
       (save-brk {:log-save-scope ~log-save-scope}
         ~scope-id-s
         ~(into {}
            (map (fn [l]
                   [`(quote ~l) l]))
            (:locals form-meta))
         (quote ~form-meta)
         ch#)
       (loop []
         (let [msg# (a/<!! ch#)]
           (case (:bf.brk/type msg#)
             :bf.brk.type/loose-with
             (if (:bf.brk/throw? msg#)
               (throw (:bf.brk/loose-value msg#))
               (:bf.brk/loose-value msg#))

             (:bf.brk.type/loose nil)
             (do
               (println "loosing breakpoint" ~scope-id-s)
               ~expr)

             (do
               (println "WARNING: unknown message type" msg#)
               (recur))
             )))
       )))

(defn loose!
  [scope-id]
  (let [=ch= (:breakpoint-chan (scope-info scope-id))]
    (a/>!! =ch= {:bf.brk/type :bf.brk.type/loose})
    (a/close! =ch=)))

(defn loose-with!
  [scope-id v]
  (let [=ch= (:breakpoint-chan (scope-info scope-id))]
    (a/>!! =ch= {:bf.brk/type :bf.brk.type/loose-with
                 :bf.brk/throw? false
                 :bf.brk/loose-value v})
    (a/close! =ch=)))

(defn loose-with-ex!
  [scope-id err]
  (let [=ch= (:breakpoint-chan (scope-info scope-id))]
    (a/>!! =ch= {:bf.brk/type :bf.brk.type/loose-with
                 :bf.brk/throw? true
                 :bf.brk/loose-value err})
    (a/close! =ch=)))

(defn resolve-code-site
  [id]
  (let [code-site-id
        (cond
          (vector? id)
          (second id)

          (integer? id)
          (cond
            (pos? id)
            (if-let [{:keys [code-site-id]} (get-in @rt/db [:execution-points id :form-meta])]
              code-site-id
              (throw (ex-info
                       (str "Could not resolve execution point " id "from the compile-side."
                         " Maybe you passed a wrong value, or maybe you're running in a Clojure setup with separate compilation and execution phases?")
                       {:execution-point-id id})))

            (neg? id)
            id)
          )]
    (get-in @ct/db [:code-sites code-site-id])))

(defn resolve-execution-point-id
  [runtime-id]
  (cond
    (vector? runtime-id)
    (first runtime-id)

    (integer? runtime-id)
    runtime-id
    ))

(defmacro defsc
  [scope-id]
  (let [scs (gensym "scope")
        execution-point-id (resolve-execution-point-id scope-id)
        {:keys [locals]} (resolve-code-site scope-id)
        ]
    `(let [~scs (:scope (get-in @rt/db [:execution-points ~execution-point-id]))]
       ~(into []
          (map (fn [l]
                 `(def ~l (get ~scs (quote ~l)))))
          locals)
       )))

(defmacro letsc
  [scope-id & body]
  (let [scs (gensym "scope")
        execution-point-id (resolve-execution-point-id scope-id)
        {:keys [locals]} (resolve-code-site scope-id)]
    `(let
       ~(into
          `[~scs (:scope (get-in @rt/db [:execution-points ~execution-point-id]))]
          (mapcat (fn [l]
                    [l `(get ~scs (quote ~l))]
                    ))
          locals)
       ~@body
       )))

(defmacro undefsc
  [id]
  (let [{:keys [ns locals]} (resolve-code-site id)]
    (into []
      (map (fn [l]
             `(ns-unmap (quote ~ns) (quote ~l))))
      locals)))

(defn clear-ep
  [epid]
  (swap! rt/db update :execution-points dissoc epid))

(defn clear-eps-of-site
  [csid]
  (swap! rt/db update :execution-points
    (fn [eps]
      (apply dissoc eps
        (->> eps
          (filter (fn [[epid v]]
                    (-> v :form-meta :code-site-id (= csid))))
          (map first))))))

(defmacro clear
  [id]
  (cond
    (vector? id)
    `(clear-ep ~(first id))

    (integer? id)
    (cond
      (pos? id)
      `(clear-ep ~id)

      (neg? id)
      (do
        (swap! ct/db update :code-sites dissoc id)
        `(clear-eps-of-site ~id)))
    ))


