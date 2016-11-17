;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.stats
  (:refer-clojure :exclude [print])
  (:require [clojure.string :as string]
            #?(:cljs [goog.string.format])
            [clojure.test.check.generators :as gen]))

(defn- add-label-to-result-map [result-map label]
  (update result-map :labels conj label))

(defn collect
  "Returns a property that adds the label resulting from applying label-fn
  to the args in the result-map returned by prop.

  Example:

  (-> (prop/for-all [v (gen/vector gen/pos-int)] (>= (first v) 0))
      (stats/collect count))"
  ([prop] (collect prop (fn [& args] args)))
  ([prop label-fn]
   (gen/fmap
     (fn [{:keys [args] :as result-map}]
       (add-label-to-result-map result-map (apply label-fn args)))
     prop)))

(defn classify
  "Returns a property that adds label to the result-map returned by prop
  when pred applied to the args in the result-map returns a truthy value

  Example:

  (-> (prop/for-all [i gen/pos-int] (>= i 0))
      (stats/classify (fn [x] (> 10 x)) :lt-10)
      (stats/classify (fn [x] (> 20 x)) :lt-20))"
  [prop pred label]
  (gen/fmap
    (fn [{:keys [args] :as result-map}]
      (if (apply pred args)
        (add-label-to-result-map result-map label)
        result-map))
    prop))

(defn- format* [fmt & args]
  (apply #?(:clj format
            :cljs goog.string.format)
         fmt
         args))

(defn print
  "Prints the percentage of occurence for each set of labels"
  [num-tests labels]
  (doseq [[labels-set q] (->> (map set labels)
                              frequencies
                              (sort-by second)
                              reverse)
          :when (seq labels-set)
          :let [percentage (double (* 100 (/ q num-tests)))
                labels-str (string/join ", " (sort labels-set))]]
    (println (format* "%.1f%% %s" percentage labels-str))))
