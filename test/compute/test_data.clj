(ns compute.test-data
  (:require
    [clojure.test :refer :all]
    [clojure.spec.alpha :as s]))

(def schema-test-data
  [{:name     "empty map"
    :expected {`s/conform      {}
               `s/explain-data nil}
    :actual   [::user {}]}

   {:name     "no nesting map"
    :expected {`s/conform      {::id 1}
               `s/explain-data nil}
    :actual   [::user {::id 1}]}

   {:name     "map with nesting"
    :expected {`s/conform      {::id   1
                                ::addr {::zip 99999}}
               `s/explain-data nil}
    :actual   [::user {::id   1
                       ::addr {::zip 99999}}]}

   {:name     "invalid, no nesting"
    :expected {`s/conform      ::s/invalid
               `s/explain-data '#:clojure.spec.alpha{:problems ({:path [:compute.spec1-select-test/id],
                                                                 :pred clojure.core/int?,
                                                                 :val  "not-id",
                                                                 :via  [:compute.spec1-select-test/user :compute.spec1-select-test/id],
                                                                 :in   [:compute.spec1-select-test/id]}),
                                                     :spec     :compute.spec1-select-test/user,
                                                     :value    #:compute.spec1-select-test{:id "not-id"}}}
    :actual   [::user {::id "not-id"}]}

   {:name     "invalid, nesting"
    :expected {`s/conform ::s/invalid}
    :actual   [::user {::id   1
                       ::addr {::zip "invalid"}}]}])

(defmacro expand-test-data
  [test-data spec-fn]
  (let [test-forms (map (fn [{:keys [name expected actual]}]
                          `(testing ~name
                             (is (= ~(get expected spec-fn) (~spec-fn ~@actual)))))
                        (eval test-data))]
    `(do ~@test-forms)))