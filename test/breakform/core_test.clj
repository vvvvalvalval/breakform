(ns breakform.core-test
  (:require [clojure.test :refer :all]
            [breakform.core :as bf]
            [bf.code :as code :refer :all]))

(def a 38)

(let [b 52]
  (defn myfun
    [x y]
    (* 3
      (code/spy {} (+ a b x y)))))

(def fut (future
           (myfun 1 12)))

(loose-with! 4
  42)

(macroexpand-1
  '(code/brk {} (+ a b x y)))

(scope-info 1)


