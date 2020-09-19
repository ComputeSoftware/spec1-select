(ns example
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [compute.spec1-select :as ssel]))

(s/def ::street string?)
(s/def ::city string?)
(s/def ::state string?)                                     ;; simplified
(s/def ::zip int?)                                          ;; simplified
(s/def ::addr (ssel/schema [::street ::city ::state ::zip]))

(s/def ::id int?)
(s/def ::first string?)
(s/def ::last string?)
(s/def ::user (ssel/schema [::id ::first ::last ::addr]))

(s/def ::company string?)
(s/def ::suite string?)
(s/def ::company-addr (ssel/union ::addr [::company ::suite]))

(comment
  (gen/sample (s/gen ::user) 5)
  (gen/sample (s/gen ::company-addr) 5)
  )


;; https://github.com/clojure/spec-alpha2/wiki/Schema-and-select#get-movie-times
(s/def ::movie-times-user (ssel/select ::user [::id ::addr {::addr [::zip]}]))

(s/valid? ::movie-times-user {::id 1 ::addr {::zip 90210}})

(comment
  (gen/sample (s/gen ::movie-times-user) 5)
  )


;; https://github.com/clojure/spec-alpha2/wiki/Schema-and-select#place-order
(s/def ::place-order
  (ssel/select ::user [::first ::last ::addr
                       {::addr [::street ::city ::state ::zip]}]))

(s/valid? ::place-order {::first "Alex" ::last "Miller"
                         ::addr  {::street "123 Elm" ::city "Springfield"
                                  ::state  "IL" ::zip 12345}})

(comment
  (s/explain-data ::place-order {::first "Alex" ::last "Miller" ::addr {::state "IL"}})
  (gen/sample (s/gen ::place-order) 3)
  )


;; https://github.com/clojure/spec-alpha2/wiki/Schema-and-select#wildcard

(comment
  (gen/sample (s/gen (ssel/select ::user ['*])) 3)
  (gen/sample (s/gen (ssel/select ::user ['* {::addr ['*]}])) 3)
  )