(ns clj-punkt.core-test
  (:require [expectations :refer :all]
            [clj-punkt.core :refer :all]
            [clojure.java.io :as io]))

(def par "About a century ago, the fine structure of the retina was discovered. The retina is the light-sensitive part of the eye, lining the inside of the eyeball. The back of the retina contains cones to sense the colours red, green and blue. Spread among the cones are rods, which are much more light-sensitive than cones, but which are colour-blind.")

(defn model->data
  [model]
  (reduce (fn [acc [k v]]
            (assoc acc k (deref v)))
          {}
          model))

(let [trainer (make-new-trainer)]
  (expect clj_punkt.core.Trainer trainer)
  (let [corpus (slurp "test/pg2600.txt")
        model (finalize-trainer! (train! trainer corpus :clean true))]
    (expect clj_punkt.core.Trainer trainer)
    (expect {} (deref (:type-fdist model)))
    (let [tokens (extract-tokens model par)
          sentences (extract-sentences model par)]
      (expect 4 (count tokens))
      (expect 4 (count sentences)))
    (try
      (store-model model "test.mod")
      (let [loaded-mod (load-model "test.mod")]
        (expect (model->data model) (model->data loaded-mod)))
      (finally
        (io/delete-file "test.mod")))))
