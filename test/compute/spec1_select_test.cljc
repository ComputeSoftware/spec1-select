(ns compute.spec1-select-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [clojure.spec.alpha :as s]
    [clojure.test.check.generators :as gen]
    [compute.spec1-select :as ssel]))

(s/def ::street string?)
(s/def ::city string?)
(s/def ::state string?)
(s/def ::zip int?)
(s/def ::addr (ssel/schema [::street ::city ::state ::zip]))

(s/def ::company string?)
(s/def ::suite string?)
(s/def ::company-addr (ssel/union ::addr [::company ::suite]))

(s/def ::company-addr-req (ssel/select ::company-addr '[*]))

(s/def ::food-name string?)
(s/def ::food (ssel/schema [::food-name]))
(s/def ::foods (s/coll-of ::food))

(s/def ::id int?)
(s/def ::first string?)
(s/def ::last string?)
(s/def ::user (ssel/schema [::id ::first ::last ::addr ::foods]))

(s/def ::user-or-id
  (s/or :id ::id
        :user ::user))


(s/def ::movie-times-user (ssel/select ::user [::id ::addr {::addr [::zip]}]))
(s/def ::foods-user (ssel/select ::user [::id {::foods [::food-name]}]))
(s/def ::user-all-addr-partial (ssel/select ::user '[* {::addr [::zip]}]))

(deftest schema-conform-test
  (testing "empty map"
    (is (= {} (s/conform ::user {}))))
  (testing "no nesting map"
    (is (= {::id 1}
           (s/conform ::user {::id 1}))))
  (testing "map with nesting"
    (is (= {::id   1
            ::addr {::zip 99999}}
           (s/conform ::user
                      {::id   1
                       ::addr {::zip 99999}}))))
  (testing "invalid no nesting"
    (is (= ::s/invalid
           (s/conform ::user {::id "not-id"}))))
  (testing "invalid nesting"
    (is (= ::s/invalid
           (s/conform ::user
                      {::id   1
                       ::addr {::zip "invalid"}}))))
  (testing "valid or"
    (is (= [:id 1]
           (s/conform ::user-or-id 1))))
  (testing "invalid or"
    (is (= ::s/invalid
           (s/conform ::user-or-id "")))))

(deftest schema-explain-test
  (testing "empty map"
    (is (= nil (s/explain-data ::user {}))))
  (testing "no nesting map"
    (is (= nil
           (s/explain-data ::user {::id 1}))))
  (testing "map with nesting"
    (is (= nil
           (s/explain-data ::user
                           {::id   1
                            ::addr {::zip 99999}}))))
  (testing "invalid no nesting"
    (is (= {::s/problems (list
                           {:in   [::id]
                            :path [::id]
                            :pred `int?
                            :val  "not-id"
                            :via  [::user ::id]})
            ::s/spec     ::user
            ::s/value    {::id "not-id"}}
           (s/explain-data ::user {::id "not-id"}))))
  (testing "invalid nesting"
    (is (= {::s/problems (list {:in   [::addr ::zip]
                                :path [::addr ::zip]
                                :pred `clojure.core/int?
                                :val  "invalid"
                                :via  [::user ::addr ::zip]})
            ::s/spec     ::user
            ::s/value    {::addr {::zip "invalid"}
                          ::id   1}}
           (s/explain-data ::user
                           {::id   1
                            ::addr {::zip "invalid"}})))))

(deftest select-conform-test
  (testing "empty map is invalid"
    (is (= ::s/invalid
           (s/conform ::movie-times-user {}))))
  (testing "valid map"
    (is (= {::id   1
            ::addr {::zip 9999}}
           (s/conform ::movie-times-user {::id   1
                                          ::addr {::zip 9999}}))))
  (testing "nested map missing key"
    (is (= ::s/invalid
           (s/conform ::movie-times-user {::id   1
                                          ::addr {}}))))
  (testing "wildcard invalid"
    (is (= ::s/invalid
           (s/conform ::user-all-addr-partial
                      {::id   1
                       ::addr {}}))))
  (testing "wildcard valid"
    (is (= {::id    1
            ::first "k"
            ::last  "w"
            ::foods []
            ::addr  {::zip 9999}}
           (s/conform ::user-all-addr-partial
                      {::id    1
                       ::first "k"
                       ::last  "w"
                       ::foods []
                       ::addr  {::zip 9999}}))))

  (testing "nested collection valid"
    (is (= {::id    1
            ::foods [{::food-name "foo"}]}
           (s/conform ::foods-user
                      {::id    1
                       ::foods [{::food-name "foo"}]}))))
  (testing "nested collection invalid"
    (is (= ::s/invalid
           (s/conform ::foods-user
                      {::id    1
                       ::foods [{::food-name 1}]})))))


(deftest select-explain-test
  (testing "empty map is invalid"
    (is (= {::s/problems (list {:in   []
                                :path []
                                :pred (list
                                        `fn
                                        ['%]
                                        (list `contains? '% ::id))
                                :val  {}
                                :via  [::movie-times-user]}
                               {:in   []
                                :path []
                                :pred (list
                                        `fn
                                        ['%]
                                        (list `contains? '% ::addr))
                                :val  {}
                                :via  [::movie-times-user]})
            ::s/spec     ::movie-times-user
            ::s/value    {}}
           (s/explain-data ::movie-times-user {}))))

  (testing "valid map"
    (is (= nil
           (s/explain-data ::movie-times-user {::id   1
                                               ::addr {::zip 9999}}))))

  (testing "nested map missing key"
    (is (= {::s/problems (list
                           {:in   [::addr]
                            :path [::addr]
                            :pred (list
                                    `fn
                                    ['%]
                                    (list `contains? '% ::zip))
                            :val  {}
                            :via  [::movie-times-user ::addr]})
            ::s/spec     ::movie-times-user
            ::s/value    #:compute.spec1-select-test{:addr {}
                                                     :id   1}}
           (s/explain-data ::movie-times-user {::id   1
                                               ::addr {}}))))

  (testing "wildcard invalid"
    (is (= {::s/problems (list
                           {:in   []
                            :path []
                            :pred (list
                                    `fn
                                    ['%]
                                    (list `contains? '% ::last))
                            :val  {::addr  {}
                                   ::first ""
                                   ::foods []
                                   ::id    1}
                            :via  [::user-all-addr-partial]})
            ::s/spec     ::user-all-addr-partial
            ::s/value    {::addr  {}
                          ::first ""
                          ::foods []
                          ::id    1}}
           (s/explain-data ::user-all-addr-partial
                           {::id    1
                            ::first ""
                            ::foods []
                            ::addr  {}}))))

  (testing "wildcard valid"
    (is (= nil
           (s/explain-data ::user-all-addr-partial
                           {::id    1
                            ::first "k"
                            ::last  "w"
                            ::foods []
                            ::addr  {::zip 9999}}))))

  (testing "nested collection valid"
    (is (= nil
           (s/explain-data ::foods-user
                           {::id    1
                            ::foods [{::food-name "foo"}]}))))

  (testing "nested collection invalid"
    (is (= {::s/problems (list
                           {:in   [::foods 0 ::food-name]
                            :path [::foods ::food-name]
                            :pred `string?
                            :val  1
                            :via  [::foods-user
                                   ::foods
                                   ::food
                                   ::food-name]})
            ::s/spec     ::foods-user
            ::s/value    {::foods [{::food-name 1}]
                          ::id    1}}
           (s/explain-data ::foods-user
                           {::id    1
                            ::foods [{::food-name 1}]})))))


(deftest schema-gen
  (testing ""
    (is (= {::addr {::zip -1 ::state "" ::city "" ::street "Q"}
            ::last "8"
            ::id   -1}
           (gen/generate (s/gen ::user) 1 100)))))

(deftest select-gen
  (testing ""
    (is (= #?(:clj  {::addr {::zip -1} ::id 0}
              :cljs {::addr {::zip 0 ::street "G"} ::id 0})
           (gen/generate (s/gen ::movie-times-user) 1 1))))
  (testing "wildcard"
    (is (= #?(:clj  {::foods [{::food-name ""}
                              {::food-name ""}]
                     ::addr  {::zip -1 ::street ""}
                     ::last  ""
                     ::first ""
                     ::id    0}
              :cljs {::id    -1,
                     ::first "",
                     ::last  "",
                     ::addr  {::zip 0},
                     ::foods [{} {::food-name ""}]})
           (gen/generate (s/gen ::user-all-addr-partial) 0 #?(:clj 0 :cljs 11))))))