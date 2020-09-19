# spec1-select 
[![CircleCI](https://circleci.com/gh/ComputeSoftware/spec1-select.svg?style=svg)](https://circleci.com/gh/ComputeSoftware/spec1-select)

Adds support for Spec2's [`schema` and `select`](https://github.com/clojure/spec-alpha2/wiki/Schema-and-select) to Spec1. 

## Install

```clojure
compute/spec1-select {:git/url "https://github.com/ComputeSoftware/spec1-select.git"
                      :sha     "<most recent sha>"}
```

### Usage with CLJS

To use this library in CLJS, you must add [spec-tools](https://github.com/metosin/spec-tools) to your deps. spec-tools allows us to easily define Specs at runtime. Happy to accept PRs to remove this dependency. When used in Clojure, spec-tools is **not** required.


## Rationale

Looking for schema/select rationale? Go [here](https://github.com/clojure/spec-alpha2/wiki/Schema-and-select). 

This library simply exists as a stopgap until Spec2 is released. Now that we know the approach Spec2 is taking with required and optional keys, it feels so wrong to declare required keys up front with Spec1's `s/keys`. 

An alternative approach is to declare all of your Spec's using only `:opt` and `:opt-un`. This sort of works. Often functions require nested keys to be required in order to run correctly. With Spec1, there is no way to make these keys explicitly required when they are nested. This will make generative tests fail due to the missing keys. You could write custom generators to generate the required keys each function needs, regardless of nesting. This feels quite tedious and we can do better. 

Additionally, when transacting to a database, we often need to ensure a particular arbitrarily nested set of keys are required. If you followed the `:opt`/`:opt-un` method, the only way to do this would be to write custom predicates to check for these arbitrarily nested keys. This works but does not give great error messages. Again, we can do better.

## Usage

We'll copy the example code written in Spec2's [wiki](https://github.com/clojure/spec-alpha2/wiki/Schema-and-select#schema-forms). Note the alias `ssel` for `compute.spec1-select`.

All of these examples are available in the `env/user.clj` namespace.

```clojure
(require '[compute.spec1-select :as ssel])

(s/def ::street string?)
(s/def ::city string?)
(s/def ::state string?) ;; simplified
(s/def ::zip int?) ;; simplified
(s/def ::addr (ssel/schema [::street ::city ::state ::zip]))

(s/def ::id int?)
(s/def ::first string?)
(s/def ::last string?)
(s/def ::user (ssel/schema [::id ::first ::last ::addr]))
```

### Unions 

```clojure
(s/def ::company string?)
(s/def ::suite string?)
(s/def ::company-addr (ssel/union ::addr [::company ::suite]))
```

### Schema gen

```clojure
(gen/sample (s/gen ::user) 5)
=>
({}
 #:example{:addr #:example{:zip -1, :city "A"}, :last "", :first ""}
 #:example{:addr #:example{:city "Br", :street ""}}
 #:example{:addr #:example{:zip 0, :street "p"}, :first "56", :id -1}
 {})
```

### Unqualified keys

Not supported yet... Might be easy to add?

### get-movie-times

```clojure
(s/def ::movie-times-user (ssel/select ::user [::id ::addr {::addr [::zip]}]))

(gen/sample (s/gen ::movie-times-user) 5)
=>
(#:example{:addr #:example{:zip -1, :state "", :city ""}, :first "", :id -1}
 #:example{:addr #:example{:zip -1, :state "", :city "L", :street "f"}, :last "", :first "", :id 0}
 #:example{:addr #:example{:zip 1}, :id 0}
 #:example{:addr #:example{:zip -1, :state "cO3", :city "", :street "5"}, :last "", :first "Y", :id 0}
 #:example{:addr #:example{:zip 0, :city "lC9", :street "i"}, :id 0})
```

### place-order

```clojure
(s/def ::place-order
  (ssel/select ::user [::first ::last ::addr
                       {::addr [::street ::city ::state ::zip]}]))
```


```clojure
(s/explain ::place-order {::first "Alex" ::last "Miller" ::addr {::state "IL"}})
#:example{:state "IL"} - failed: (contains? % :user/street) in: [:user/addr] at: [:user/addr] spec: :user/addr
#:example{:state "IL"} - failed: (contains? % :user/city) in: [:user/addr] at: [:user/addr] spec: :user/addr
#:example{:state "IL"} - failed: (contains? % :user/zip) in: [:user/addr] at: [:user/addr] spec: :user/addr
```


```clojure
(gen/sample (s/gen ::place-order) 3)
=>
(#:example{:addr #:example{:zip 0, :state "", :city "", :street ""}, :last "", :first ""}
 #:example{:addr #:example{:zip -1, :state "e", :city "", :street ""}, :last "", :first "Z", :id -1}
 #:example{:addr #:example{:zip -1, :state "R", :city "8", :street "G8"}, :last "X", :first "", :id 0})
```

### Wildcard

```clojure
(gen/sample (s/gen (ssel/select ::user ['*])) 3)
=>
(#:example{:addr #:example{:zip 0, :state "", :city "", :street ""}, :last "", :first "", :id 0}
 #:example{:addr #:example{:zip 0, :state "", :street "2"}, :last "", :first "0", :id -1}
 #:example{:addr #:example{:zip -2}, :last "Qm", :first "", :id -2})
```

```clojure
(gen/sample (s/gen (ssel/select ::user ['* {::addr ['*]}])) 3)
=>
(#:example{:addr #:example{:zip 0, :state "", :city "", :street ""}, :last "", :first "", :id 0}
 #:example{:addr #:example{:zip 0, :state "S", :city "", :street "i"}, :last "", :first "5", :id -1}
 #:example{:addr #:example{:zip 1, :state "ni", :city "1l", :street ""}, :last "C4", :first "fT", :id 1})
```

## License

Copyright © 2020 Compute Software

Distributed under the Eclipse Public License either version 2.0 or (at
your option) any later version.
