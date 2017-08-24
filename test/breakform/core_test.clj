(ns breakform.core-test
  (:require [clojure.test :refer :all]
            [breakform.core :as bf]
            [bf.code :as code :refer :all]))

(def a 38)

(let [b 52]
  (defn myfun
    [x y]
    (* 3
      (code/sv (+ a b x y)))))

(myfun 1 25)

(scope-info 8)



(comment
  (deftest a-test
    (testing "FIXME, I fail."
      (is (= 0 1))))

  (def v 12)

  ;; FIXME clean
  (defn foo-fn [x y]
    (let [z (+ x y)
          {:keys [a b]} {:a (inc 32) :b "coucou"}]
      (bf/brk [x y v z a b])
      (println "done")))

  (comment
    (future (foo-fn 2 3))

    (bf/env)
    => {x 2, y 3, z 5, map__17496 {:a 33, :b "coucou"}, a 33, b "coucou"}

    (bf/sim)
    => [2 3 12 5 33 "coucou"]

    (bf/sim {x -1000})
    => [-1000 3 12 5 33 "coucou"]

    (bf/in-bp (+ x y v))
    => 17
    (bf/in-bp {x 100} (+ x y v))
    => 115
    (bf/in-bp v)
    => 12

    (bf/continue)
    => [2 3 12 5 33 "coucou"]
    (bf/continue {x 100})
    => [100 3 12 5 33 "coucou"]
    ))

