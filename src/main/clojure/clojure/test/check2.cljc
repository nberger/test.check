(ns clojure.test.check2
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.results :as results]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.impl :refer [get-current-time-millis]]))

(defrecord QuickCheckState
  [step num-tests so-far-tests shrink-total-steps result smallest abort?])

(defn- mk-qc-state [options]
  (merge {:num-tests 100
          :so-far-tests 0
          :shrunk {:total-nodes-visited 0
                   :depth 0}
          :abort? false}
         options))

(def steps
  "state flow diagram:

       started
          v
  trial, trial, [...]
          v
succeeded | failure
                 v
    shrinking, shrinking, [...]
                 v
               shrunk"
  #{:started :trying :succeeded :failed :shrinking :shrunk :aborted})

(defn- make-rng
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (get-current-time-millis)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn- shrink
  [{:keys [result-map-rose] :as qc-state} step-fn]
  (let [shrinks-this-depth (rose/children result-map-rose)]
    (loop [qc-state qc-state
           nodes shrinks-this-depth
           current-smallest (rose/root result-map-rose)]
      (if (empty? nodes)
        (let [shrink-result (:result current-smallest)]
          (-> qc-state
              (assoc :step :shrunk)
              (assoc-in [:shrunk :result] (results/passing? shrink-result))
              (assoc-in [:shrunk :result-data] (results/result-data shrink-result))
              (assoc-in [:shrunk :smallest] (:args current-smallest))
              step-fn))
        (let [;; can't destructure here because that could force
              ;; evaluation of (second nodes)
              head (first nodes)
              tail (rest nodes)
              result (:result (rose/root head))
              args (:args (rose/root head))
              qc-state (-> qc-state
                            (assoc :step :shrinking)
                            (assoc-in [:shrunk :args] args)
                            (assoc-in [:shrunk :result] result)
                            (update-in [:shrunk :total-nodes-visited] inc))]
          (if (results/passing? result)
            ;; this node passed the test, so now try testing its right-siblings
            (-> qc-state
                (assoc-in [:shrunk :pass?] true)
                (assoc-in [:shrunk :smallest] current-smallest)
                step-fn
                (recur tail current-smallest))
            ;; this node failed the test, so check if it has children,
            ;; if so, traverse down them. If not, save this as the best example
            ;; seen now and then look at the right-siblings
            ;; children
            (let [new-smallest (rose/root head)
                  qc-state (-> qc-state
                                (assoc-in [:shrunk :pass?] false)
                                (assoc-in [:shrunk :smallest] new-smallest))]
              (if-let [children (seq (rose/children head))]
                (recur (step-fn (update-in qc-state [:shrunk :depth] inc)) children new-smallest)
                (recur (step-fn qc-state) tail new-smallest)))))))))

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

  `:step-fn`
    A callback function that will be called at various points in the test
    run, with a QuickCheckState record. This function is intended to provide feedback
    to the user. It must return the (possibly modified) QC state record.

  Examples:

      (def p (for-all [a gen/pos-int] (> (* a a) a)))

      (quick-check 100 p)
      (quick-check 200 p {:seed 42
                          :max-size 50
                          :step-fn (fn [state]
                                     (when (= :failure (:step m))
                                       (println \"Uh oh...\"))
                                     state)})"
  [num-tests property {:keys [seed max-size step-fn]
                       :or {max-size 200 step-fn identity}}]
  (let [[created-seed rng] (make-rng seed)]
    (loop [{:keys [so-far-tests step]
            :as qc-state} (step-fn
                             (mk-qc-state {:num-tests num-tests
                                           :step :started
                                           :seed created-seed
                                           :property property}))
           size-seq (gen/make-size-range-seq max-size)
           rstate rng]
      (if (== so-far-tests num-tests)
        (step-fn (assoc qc-state :step :succeeded))
        (let [[size & rest-size-seq] size-seq
              [r1 r2] (random/split rstate)
              result-map-rose (gen/call-gen property r1 size)
              result (:result (rose/root result-map-rose))
              qc-state (-> qc-state
                            (update :so-far-tests inc)
                            (assoc :size size
                                   :step :trying
                                   :result-map-rose result-map-rose
                                   :result result)
                            step-fn)]
          (if (results/passing? result)
            (recur qc-state rest-size-seq r2)
            (-> qc-state
                (assoc :step :failed)
                step-fn
                (shrink step-fn))))))))
