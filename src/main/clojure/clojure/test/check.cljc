;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check
  (:require [clojure.test.check2 :as ctc2]
            [clojure.test.check.results :as results]
            [clojure.test.check.rose-tree :as rose]))

(defn- complete
  [property num-trials seed reporter-fn]
  (reporter-fn {:type :complete
                :property property
                :result true
                :num-tests num-trials
                :seed seed})

  {:result true :num-tests num-trials :seed seed})

(defn reporter-fn->step-fn
  [reporter-fn]
  (fn [{:keys [step property num-tests so-far-tests seed size shrunk]
        :as qc-state}]
    (case step
      :started
      qc-state

      :succeeded
      (complete property so-far-tests seed reporter-fn)

      :trying
      (do
        (reporter-fn {:type :trial
                            :property property
                            :so-far so-far-tests
                            :num-tests num-tests})
        qc-state)

      :failed
      (let [{:keys [result args]} (rose/root (:result-map-rose qc-state))]
        (do
          (reporter-fn {:type :failure
                        :property property
                        :result (results/passing? result)
                        :result-data (results/result-data result)
                        :trial-number so-far-tests
                        :failing-args args})
          qc-state))

      :shrinking
      (let [{:keys [result args pass? smallest]} shrunk]
        (do
          (reporter-fn {:type :shrink-step
                        :result result
                        :args args
                        :pass? pass?
                        :current-smallest smallest})
          qc-state))

      :shrunk
      (let [{:keys [result args]} (rose/root (:result-map-rose qc-state))]
        (reporter-fn {:type :shrunk
                      :property property
                      :trial-number so-far-tests
                      :failing-args args
                      :shrunk shrunk})
        {:result (results/passing? result)
         :result-data (results/result-data result)
         :seed seed
         :failing-size size
         :num-tests so-far-tests
         :fail (vec args)
         :shrunk shrunk}))))

(defn quick-check
  "Tests `property` `num-tests` times.

  Takes several optional keys:

  `:seed`
    Can be used to re-run previous tests, as the seed used is returned
    after a test is run.

  `:max-size`.
    can be used to control the 'size' of generated values. The size will
    start at 0, and grow up to max-size, as the number of tests increases.
    Generators will use the size parameter to bound their growth. This
    prevents, for example, generating a five-thousand element vector on
    the very first test.

  `:reporter-fn`
    A callback function that will be called at various points in the test
    run, with a map like:

      ;; called after a passing trial
      {:type      :trial
       :property  #<...>
       :so-far    <number of tests run so far>
       :num-tests <total number of tests>}

      ;; called after each failing trial
      {:type         :failure
       :property     #<...>
       :result       ...
       :trial-number <tests ran before failure found>
       :failing-args [...]}

    It will also be called on :complete, :shrink-step and :shrunk.

  Examples:

      (def p (for-all [a gen/pos-int] (> (* a a) a)))

      (quick-check 100 p)
      (quick-check 200 p
                   :seed 42
                   :max-size 50
                   :reporter-fn (fn [m]
                                  (when (= :failure (:type m))
                                    (println \"Uh oh...\"))))"
  [num-tests property & {:keys [seed max-size reporter-fn]
                         :or {max-size 200, reporter-fn (constantly nil)}}]
  (ctc2/quick-check num-tests property {:seed seed
                                        :max-size max-size
                                        :step-fn (reporter-fn->step-fn reporter-fn)}))
