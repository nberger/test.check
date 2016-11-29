(ns clojure.test.check2
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.results :as results]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.impl :refer [get-current-time-millis]]
            ))

(defrecord QuickCheckState
  [state num-tests so-far-tests shrink-total-steps result smallest abort?])

(defn mk-qc-result [options]
  (merge {:num-tests 100
          :so-far-tests 0
          :shrunk {:total-nodes-visited 0
                   :depth 0}
          :abort? false}
         options))

(def states
  #{:started :trying :succeeded :failed :shrinking :shrunk :aborted})

"
  |
  |
  v
  trial, trial, trial
  |
  |
  v
  complete     |     failure
                       |
                       |
                       v 
                       shrink-step, shrink-step, shrink-step
                       |
                       v
                       shrunk
"

(defn- make-rng
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (get-current-time-millis)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn shrink
  [{:keys [result-map-rose] :as qc-result} step-fn]
  (let [shrinks-this-depth (rose/children result-map-rose)]
    (loop [qc-result qc-result
           nodes shrinks-this-depth
           current-smallest (rose/root result-map-rose)]
      (if (empty? nodes)
        (let [shrink-result (:result current-smallest)]
          (-> qc-result
              (assoc :state :shrunk)
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
              qc-result (-> qc-result
                            (assoc :state :shrinking)
                            (assoc-in [:shrunk :args] args)
                            (assoc-in [:shrunk :result] result)
                            (update-in [:shrunk :total-nodes-visited] inc))]
          (if (results/passing? result)
            ;; this node passed the test, so now try testing its right-siblings
            (-> qc-result
                (assoc-in [:shrunk :pass?] true)
                (assoc-in [:shrunk :smallest] current-smallest)
                step-fn
                (recur tail current-smallest))
            ;; this node failed the test, so check if it has children,
            ;; if so, traverse down them. If not, save this as the best example
            ;; seen now and then look at the right-siblings
            ;; children
            (let [new-smallest (rose/root head)
                  qc-result (-> qc-result
                                (assoc-in [:shrunk :pass?] false)
                                (assoc-in [:shrunk :smallest] new-smallest))]
              (if-let [children (seq (rose/children head))]
                (recur (update-in qc-result [:shrunk :depth] inc) children new-smallest)
                (recur qc-result tail new-smallest)))))))))

(defn quick-check
  [num-tests property {:keys [seed max-size step-fn]
                       :or {max-size 200 step-fn identity}}]
  (let [[created-seed rng] (make-rng seed)]
    (loop [{:keys [so-far-tests state]
            :as qc-result} (step-fn
                             (mk-qc-result {:num-tests num-tests
                                            :state :started
                                            :seed created-seed
                                            :property property}))
           size-seq (gen/make-size-range-seq max-size)
           rstate rng]
      (if (== so-far-tests num-tests)
        (step-fn (assoc qc-result :state :succeeded))
        (let [[size & rest-size-seq] size-seq
              [r1 r2] (random/split rstate)
              result-map-rose (gen/call-gen property r1 size)
              result (:result (rose/root result-map-rose))
              qc-result (-> qc-result
                            (update :so-far-tests inc)
                            (assoc :size size
                                   :state :trying
                                   :result-map-rose result-map-rose
                                   :result result)
                            step-fn)]
          (if (results/passing? result)
            (recur qc-result rest-size-seq r2)
            (-> qc-result
                (assoc :state :failed)
                step-fn
                (shrink step-fn))))))))
