(ns compute.spec1-select
  #?(:cljs (:require-macros [compute.spec1-select]))
  (:require
    [clojure.walk :as walk]
    [clojure.set :as sets]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    ;; We need spec-tools to dynamically define specs at runtime
    #?(:cljs [spec-tools.data-spec :as ds])))

(def ^:dynamic *req-paths* nil)
(def ^:dynamic *conform-path* [])

(defn- get-spec-object
  "Returns the s/Spec object for the given spec-form. Recurses until either nil
  or a s/Spec implementation is returned."
  [spec-form]
  (if (satisfies? s/Spec spec-form)
    spec-form
    (let [s (s/get-spec spec-form)]
      (if s
        (get-spec-object s)
        nil))))

(defprotocol Schema
  (keyspecs* [spec] "Returns map of key to symbolic spec"))

(defprotocol Select
  "Marker protocol for selects")

(defn select?
  [x]
  (satisfies? Select x))

(defn- make-keys-spec
  [req opt]
  #?(:clj
     (eval `(s/keys ~@(when *req-paths* [:req req])
                    :opt ~opt))
     ;; TODO: Remove dependency on spec-tools
     ;; We need a way to dynamically define specs. Spec-tools lets us do that with
     ;; its data-spec API. This lets us easily support CLJS.
     :cljs
     (let [opt-ks (into {} (map (fn [k] [(ds/opt k) k])
                                (sets/difference (set opt) (set req))))
           req-ks (into opt-ks (map (fn [k] [(ds/req k) k]) req))]
       (ds/spec {:spec req-ks}))))

(defn schema-impl
  [{::keys [keys-vec gfn]}]
  ;; As per Spec2, schemas cannot have nested select specs
  ;; https://github.com/clojure/spec-alpha2/blob/495e5ac3238be002b4de72d1c48479f6bec06bb3/src/main/clojure/clojure/alpha/spec/impl.clj#L421
  (assert (every? #(not (select? (s/get-spec %))) keys-vec)
          "Schemas cannot contain nested select specs.")
  (let [path->req-ks (fn []
                       (reduce
                         (fn [path->req-ks path]
                           (update path->req-ks (or (butlast path) []) (fnil conj []) (last path)))
                         {} *req-paths*))
        req-ks-f #(get (path->req-ks) %)

        get-spec (fn [path]
                   (make-keys-spec (req-ks-f path) keys-vec))]
    (reify
      Schema
      (keyspecs* [_]
        (into {} (map #(vector % %)) keys-vec))

      s/Specize
      (specize* [s] s)
      (specize* [s _] s)

      s/Spec
      (conform* [_ m]
        (let [req-ks (set (req-ks-f *conform-path*))]
          (cond
            (not (map? m))
            ::s/invalid
            (not (sets/subset? req-ks (set (keys m))))
            ::s/invalid
            :else
            (reduce-kv
              (fn [m attr v]
                (let [c (binding [*conform-path* (conj *conform-path* attr)]
                          (if (s/get-spec attr)
                            (s/conform attr v)
                            v))]
                  (if (s/invalid? c)
                    (reduced c)
                    (assoc m attr c))))
              {} m))))
      (unform* [_ m]
        (s/unform (get-spec []) m))
      (explain* [_ path via in x]
        (s/explain* (get-spec path) path via in x))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (s/gen* (get-spec path) overrides path rmap)))
      (with-gen* [_ gfn]
        (schema-impl {::keys-vec keys-vec
                      ::gfn      gfn}))
      (describe* [_]
        `(schema ~keys-vec)))))

#?(:clj
   (defmacro schema
     [keys-vec]
     `(schema-impl {::keys-vec ~keys-vec})))

(defn schema-keys
  [spec]
  (-> spec
      (get-spec-object)
      (keyspecs*)
      (keys)))

(defn merge-schemas
  [schemas]
  (into []
        (comp
          (mapcat (fn [keyset]
                    (if (vector? keyset)
                      keyset
                      (schema-keys keyset))))
          (distinct))
        schemas))

#?(:clj
   (defmacro union
     [& schemas]
     `(schema-impl (merge-schemas ~schemas))))

(defn flatten-selection
  ([selection] (flatten-selection [] selection))
  ([base-path selection]
   (cond
     (keyword? selection)
     [(conj base-path selection)]
     (vector? selection)
     (into [] (mapcat #(flatten-selection base-path %)) selection)
     (map? selection)
     (into []
           (mapcat (fn [[next-path inner-selection]]
                     (flatten-selection
                       (conj base-path next-path)
                       inner-selection)))
           selection))))

(comment
  (flatten-selection [::b])
  (flatten-selection [::a {::a [::b]}])
  (flatten-selection '[::a])
  (flatten-selection [::a {::a [::b ::d {::b [::c]}]}])
  )

(defn expand-wildcard-selection
  "Returns the selection without a wildcard."
  [start-attr selection]
  (let [*cur-attr (atom start-attr)]
    (walk/prewalk
      (fn [form]
        (cond
          (and
            (sequential? form)
            (not-empty (filter #(= '* %) form)))
          (vec (concat
                 (schema-keys @*cur-attr)
                 (filter #(not= '* %) form)))
          (map-entry? form)
          (do
            (reset! *cur-attr (key form))
            form)
          :else
          form))
      selection)))

(comment
  (expand-wildcard-selection ::user [::id])
  (expand-wildcard-selection ::user '[* {::addr [*]}])
  )

(defn select-spec-impl
  [{::keys [schema selection gfn]}]
  (let [selection (expand-wildcard-selection schema selection)
        flat-paths (flatten-selection selection)
        schema-spec (get-spec-object schema)]
    (reify
      Select

      s/Specize
      (specize* [s] s)
      (specize* [s _] s)

      s/Spec
      (conform* [_ m]
        (binding [*req-paths* flat-paths]
          (s/conform* schema-spec m)))
      (unform* [_ m]
        (binding [*req-paths* flat-paths]
          (s/unform* schema-spec m)))
      (explain* [_ path via in x]
        (binding [*req-paths* flat-paths]
          (s/explain* schema-spec path via in x)))
      (gen* [_ overrides path rmap]
        ;; goal here is to generate each layer, starting from the top, by merging
        ;; in the required keys
        (binding [*req-paths* flat-paths]
          (if gfn
            (gfn)
            (s/gen* schema-spec overrides path rmap))))
      (with-gen* [_ gfn]
        (select-spec-impl {::schema    schema
                           ::selection selection
                           ::gfn       gfn}))
      (describe* [_]
        `(select ~schema ~selection)))))

#?(:clj
   (defmacro select
     [schema selection]
     `(select-spec-impl {::schema ~schema ::selection ~selection})))

(comment
  (s/def ::name string?)
  (s/def ::user (s/keys :req [::name]))

  (s/explain-data ::user {::name 1})

  (do
    (s/def ::food-name string?)
    (s/def ::food (schema [::food-name]))
    (s/def ::foods (s/coll-of ::food))
    (s/def ::user (schema [::name ::foods]))

    (s/def ::user-req (select ::user [::name ::foods {::foods [::food-name]}])))

  (gen/generate (s/gen ::user))
  (gen/sample (s/gen ::user-req))

  (s/conform ::user-req {::name "a"})
  (s/conform ::user-req {::name  "a"
                         ::foods [{::food-name "asd"}]})
  (s/explain-data ::user-req {::name  ""
                              ::foods [{::food-name "ads"}]})
  )