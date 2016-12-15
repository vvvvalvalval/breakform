(ns breakform.core
  (:require [clojure.core.async :as a]))

;; debugging utitlities
;; ideas:
;; - ability to add a breakpoint w/ (brk name? expr)
;; - running into breakpoint blocks before evaluating expr, prompts in the REPL that the breakpoint was hit.
;; in the REPL, can call several functions / macros
;; - getting the environment at break point; current file, ns, line as well (get-env)
;; - continuing, optionally redefining part of the env (continue {bindings}?). Returns the value
;; - simulating evaluation of the expression (using eval?). (bpeval {bindings}) - evals in same namespace
;; - evaluating an expression in the context of the breakpoint. (in-bp)
;; TODO unique ids of breakpoints-thread pairs used as key in weakhashmap? Could avoid race conditions for stuff like in-bp and sim. (Val, 15 Dec 2016)
;; TODO concurrency control: don't want other breakpoints popping up while we're dealing with the current breakpoint, do we? (Val, 15 Dec 2016)
;; concurrency control is only about determining which breakpoint gets to seize the current prompt.
;; priority is probably best done as a FIFO queue in which the current Thread has priority.

(defrecord BreakPointState
  [locals
   local-names expr ns-name form-meta stacktrace
   p_new-locals p_ret])

(def breakpoints-stack (atom ()))

(defn current-breakpoint []
  (first @breakpoints-stack))

(defn brk*
  "returns a map binding local names to their (potentially redefined) values."
  [locals local-names expr ns-name form-meta stracktrace]
  (let [p_new-locals (promise)
        bps (->BreakPointState locals local-names expr ns-name form-meta stracktrace
              p_new-locals (promise))]
    (swap! breakpoints-stack #(cons bps %))
    bps))

(defmacro brk
  "Sets a breakpoint at the beginning of the `expr` form.
  Will block the current thread before evaluating `expr` until `continue` is called for this breakpoint."
  [expr]
  (let [env &env
        local-names (vec (keys env))
        nsn (ns-name *ns*)
        form-meta (select-keys (meta &form) [:line :column :file :name])
        new-locals-sym (gensym "new-locals")]
    `(let [locals# ~(into {} (for [ln local-names] [(list 'quote ln) ln]))
           stack-trace# (.. Thread (currentThread) (getStackTrace))
           bps# (brk* locals#
                  (quote ~local-names) (quote ~expr) (quote ~nsn) (quote ~form-meta) stack-trace#)
           ~new-locals-sym @(:p_new-locals bps#)]
       ;; overriding
       (let ~(into [] cat (for [ln local-names]
                            [ln `(get ~new-locals-sym (quote ~ln) ~ln)]))
         (let [ret# (try ~expr
                      (catch Throwable err
                        ;; in case of an Exception, the value returned to continue will be the exception itself.
                        (deliver (:p_ret bps#) err)
                        (throw err)))]
           (deliver (:p_ret bps#) ret#)
           ret#)))))

(defn env []
  "Returns the local environment of the current breakpoint, as a map mapping local names (as symbols) to their value."
  (:locals (current-breakpoint)))

(defn quote-keys [m]
  (into {} (for [[k v] m] [`(quote ~k) v])))

(defmacro in-bp
  "Evaluates an expression in the context of the current breakpoint, i.e in the same namespace with the same local env.
  Optionally, a map from (unquoted) local names to value may be provided to override some of the local names in the breakpoint env."
  ([expr] `(in-bp nil ~expr))
  ([env-overrides expr]
   (let [new-locals-sym (gensym "new-locals")]
     (binding [*ns* (find-ns (:ns-name (current-breakpoint)))]
       (clojure.core/eval
         `(let [{locals# :locals} (current-breakpoint)
                ~new-locals-sym (merge locals# ~(quote-keys env-overrides))]
            (let ~(into [] cat (for [ln (:local-names (current-breakpoint))]
                                 [ln `(get ~new-locals-sym (quote ~ln))]))
              ~expr)))))))

(defmacro sim
  "Simulates evaluating the form in the current breakpoint without actually releasing the breakpoint.
  Optionally, a map from (unquoted) local names to value may be provided to override some of the local names in the breakpoint env."
  ([] `(sim {}))
  ([env-overrides]
   (let [new-locals-sym (gensym "new-locals")
         bp (current-breakpoint)]
     (binding [*ns* (find-ns (:ns-name bp))]
       (clojure.core/eval
         `(let [bp# (current-breakpoint)
                {locals# :locals} bp#
                ~new-locals-sym (merge locals# ~(quote-keys env-overrides))]
            (let ~(into [] cat (for [ln (:local-names bp)]
                                 [ln `(get ~new-locals-sym (quote ~ln))]))
              ~(:expr bp))))))))

(defn continue* [new-locals]
  (when-let [bp (current-breakpoint)]
    (deliver (:p_new-locals bp) new-locals)
    (swap! breakpoints-stack rest)
    @(:p_ret bp)))

(defmacro continue
  "Continues the current breakpoint, returns the value of the form.
  Optionally, a map from (unquoted) local names to value may be provided to override some of the local names in the breakpoint env."
  ([] `(continue {}))
  ([env-overrides]
   `(continue* ~(quote-keys env-overrides))))
