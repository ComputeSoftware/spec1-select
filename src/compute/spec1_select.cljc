(ns compute.spec1-select
  #?(:cljs (:require-macros [compute.spec1-select]))
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

(defprotocol Schema
  (keyspecs* [spec] "Returns map of key to symbolic spec"))

(defprotocol Select
  "Marker protocol for selects")

(defn- resolve-spec
  "Returns the s/Spec object for the given spec-form. Recurses until either nil
  or a s/Spec implementation is returned."
  [spec-form]
  (if (satisfies? s/Spec spec-form)
    spec-form
    (let [s (s/get-spec spec-form)]
      (if s
        (resolve-spec s)
        nil))))

(defn select?
  [x]
  (satisfies? Select ^js x))

(defn- recur-limit? [rmap id path k]
  (and (> (get rmap id) (::s/recursion-limit rmap))
       (contains? (set path) k)))

(defn- inck [m k]
  (assoc m k (inc (or (get m k) 0))))

(declare or-k-gen and-k-gen)

(defn- k-gen
  "returns a generator for form f, which can be a keyword or a list
  starting with 'or or 'and."
  [f]
  (cond
    (keyword? f) (gen/return f)
    (= `or (first f)) (or-k-gen 1 (rest f))
    (= `and (first f)) (and-k-gen (rest f))))

(defn- or-k-gen
  "returns a tuple generator made up of generators for a random subset
  of min-count (default 0) to all elements in s."
  ([s] (or-k-gen 0 s))
  ([min-count s]
   (gen/bind (gen/tuple
               (gen/choose min-count (count s))
               (gen/shuffle (map k-gen s)))
             (fn [[n gens]]
               (apply gen/tuple (take n gens))))))

(defn- and-k-gen
  "returns a tuple generator made up of generators for every element
  in s."
  [s]
  (apply gen/tuple (map k-gen s)))

(defn schema-impl
  [coll gfn]
  (let [coll (if (map? coll) [coll] coll)
        ks (filter keyword? coll)
        qks (zipmap ks ks)
        unq-map (apply merge (filter map? coll))
        unq-specs (map resolve-spec (vals unq-map))
        uqks (zipmap (keys unq-map) unq-specs)
        key-specs (merge uqks qks)
        lookup #(or (s/get-spec %) (get key-specs %))]
    ;; schemas cannot contain nested select specs
    (assert (every? #(if-let [sp (s/get-spec %)] (not (select? sp)) true) ks))
    (assert (every? #(not (select? %)) unq-specs))
    (reify
      s/Spec
      (conform* [_ x]
        (if (or (not (map? x))
                #_(and
                    (-> settings (get :closed) (contains? settings-key))
                    (not (set/subset? (-> x keys set) (set ks)))))
          ::s/invalid
          (loop [ret x                                      ;; either conformed map or ::s/invalid
                 [[k v] & ks :as m] x]
            (if k
              (let [conformed (if-let [sp (lookup k)] (s/conform* sp v) v)]
                (if (s/invalid? conformed)
                  (recur ::s/invalid nil)
                  (recur (if-not (identical? v conformed) (assoc ret k conformed) ret) ks)))
              ret))))
      (unform* [_ x]
        (loop [ret x, [[k v] & ks :as m] x]
          (if-not m
            ret
            (if-let [sp (lookup k)]
              (let [uv (s/unform sp v)]
                (recur (if (identical? uv v) ret (assoc ret k uv)) ks))
              (recur ret ks)))))
      (explain* [_ path via in x]
        (if (not (map? x))
          [{:path path :pred `map? :val x :via via :in in}]
          (if false #_(and (-> settings (get :closed) (contains? settings-key))
                           (not (set/subset? (-> x keys set) (set ks))))
            (let [form `(fn [~'%] (set/subset? (set (keys ~'%)) ~(set ks)))]
              [{:path path :pred form :val x :via via :in in}])
            (reduce-kv
              (fn [p k v]
                (if-let [sp (lookup k)]
                  (into p (#'s/explain-1 (s/form sp) sp (conj path k) via (conj in k) v))
                  p))
              [] x))))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (let [ogen (fn [k s]
                       [k (gen/delay (#'s/gensub s overrides (conj path k) rmap k))])
                opt-kset (keys key-specs)
                opt-specs (->> opt-kset (map lookup) (remove nil?))
                opts (remove nil? (map ogen opt-kset opt-specs))]
            (when (every? identity (map second opts))
              (gen/bind
                (or-k-gen opt-kset)
                (fn [opt-ks]
                  (let [qks (flatten opt-ks)]
                    (->> opts
                         (filter #((set qks) (first %)))
                         (apply concat)
                         (apply gen/hash-map)))))))))
      (with-gen* [_ gfn] (schema-impl coll gfn))
      (describe* [_] `(schema ~coll))

      Schema
      (keyspecs* [_] key-specs))))

(defn schema?
  [x]
  (satisfies? Schema x))

(defn- keyspecs
  [schema]
  (keyspecs* schema))

(defn schema*
  "Returns a schema object given a fully-qualified schema definition.
 If needed use 'explicate' to qualify forms."
  [sform]
  (cond
    (keyword? sform) (resolve-spec sform)
    (vector? sform) (schema-impl sform nil)
    ;(map? sform) (create-spec `{:clojure.spec/op schema, :schemas [~sform]})
    ;(c/or (list? sform) (seq? sform)) (create-spec sform)
    (nil? sform) nil
    :else (throw #?(:clj (IllegalArgumentException. (str "Unknown schema op of type: " (class sform)))
                    :cljs (js/Error (str "Unknown schema op of type: " (type sform)))))))

(defmacro schema
  [schema-form]
  `(schema-impl ~schema-form nil))

(comment
  (s/def ::food-name string?)
  (s/def ::food (schema* [::food-name]))
  (s/explain-data
    (schema-impl [::food-name] nil)
    {::food-name 1})
  )

(defn- mk-uuid
  []
  #?(:clj (java.util.UUID/randomUUID) :cljs (random-uuid)))

(defn select-impl
  [schema-form selection gfn]
  (let [id (mk-uuid)
        schema (schema* schema-form)
        key-specs (keyspecs schema)
        req-kset (if (->> selection (filter symbol?) (filter #(= (name %) "*")) seq)
                   (set (keys key-specs))
                   (->> selection (filter keyword?) set))
        sub-selects (->> selection (filter map?) (apply merge))
        lookup #(if (qualified-keyword? %)
                  (let [schema-obj (s/get-spec %)]
                    (if (schema? schema-obj)
                      (let [sub-schema (vec (some-> schema-obj keyspecs keys))
                            sub-selection (get sub-selects % [])]
                        (select-impl sub-schema sub-selection nil)
                        #_(resolve-spec `(s/select ~sub-schema ~sub-selection)))
                      schema-obj))
                  (get key-specs %))
        opt-kset (set/difference (set/union (-> key-specs keys set)
                                            (-> sub-selects keys set))
                                 req-kset)]
    (reify
      Select
      s/Spec
      (conform* [_ x]
        (if (or (not (map? x))
                (not (set/subset? req-kset (-> x keys set))))
          ::s/invalid
          (loop [ret x                                      ;; either conformed map or ::s/invalid
                 [[k v] & ks :as m] x]
            (if k
              (let [conformed (if-let [sp (lookup k)]
                                (s/conform* sp v)
                                v)]
                (if (s/invalid? conformed)
                  (recur ::s/invalid nil)
                  (if-let [sub-spec (lookup k)]
                    (if (not (s/valid? sub-spec (get x k)))
                      (recur ::s/invalid nil)
                      (recur (if-not (identical? v conformed) (assoc ret k conformed) ret) ks))
                    (recur (if-not (identical? v conformed) (assoc ret k conformed) ret) ks))))
              ret))))
      (unform* [_ x]
        (loop [ret x, [[k v] & ks :as m] x]
          (if-not m
            ret
            (if-let [sp (lookup k)]
              (let [uv (s/unform sp v)]
                (recur (if (identical? uv v) ret (assoc ret k uv)) ks))
              (recur ret ks)))))
      (explain* [_ path via in x]
        (if (not (map? x))
          [{:path path :pred `map? :val x :via via :in in}]
          (-> (concat
                ;; required key missing
                (->> req-kset
                     (keep (fn [k]
                             (when-not (contains? x k)
                               {:path path :pred `(fn [~'m] (contains? ~'m ~k)) :val x :via via :in in}))))
                ;; registered key has conforming v
                (->> x
                     (keep
                       (fn [[k v]]
                         (when-let [sp (lookup k)]
                           (#'s/explain-1 (s/form sp) sp (conj path k) via (conj in k) v))))
                     (mapcat identity))
                ;; nested select matches
                (->> sub-selects
                     (keep
                       (fn [[k _]]
                         (when (contains? x k)
                           (when-let [sp (lookup k)]
                             (#'s/explain-1 (s/form sp) sp (conj path k) via (conj in k) (get x k))))))
                     (mapcat identity)))
              distinct
              vec)))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (let [rmap (inck rmap id)
                rgen (fn [k s] [k (#'s/gensub s overrides (conj path k) rmap k)])
                ogen (fn [k s]
                       (when-not (recur-limit? rmap id path k)
                         [k (gen/delay (#'s/gensub s overrides (conj path k) rmap k))]))
                req-specs (->> req-kset (map lookup) (remove nil?))
                reqs (map rgen req-kset req-specs)
                opt-specs (->> opt-kset (map lookup) (remove nil?))
                opts (remove nil? (map ogen opt-kset opt-specs))]
            (when (every? identity (concat (map second reqs) (map second opts)))
              (gen/bind
                (gen/tuple (and-k-gen req-kset) (or-k-gen opt-kset))
                (fn [[req-ks opt-ks]]
                  (let [qks (flatten (concat req-ks opt-ks))]
                    (->> (into reqs opts)
                         (filter #((set qks) (first %)))
                         (apply concat)
                         (apply gen/hash-map)))))))))
      (with-gen* [_ gfn] (select-impl schema-form selection gfn))
      (describe* [_] `(select ~schema-form ~selection)))))

(defmacro select
  [schema-form selection]
  `(select-impl ~schema-form ~selection nil))

(defn union-impl
  [schemas gfn]
  (let [ks (->> schemas (map schema*) (map keyspecs) (apply merge))
        qk (filterv qualified-keyword? (keys ks))
        ukv (reduce-kv (fn [m k v]
                         (if (simple-keyword? k)
                           (assoc m k (if (keyword? v) v (s/form v)))
                           m))
                       {} ks)
        impl (schema-impl (if (seq ukv) (conj qk ukv) qk) nil)]
    (reify
      s/Spec
      (conform* [_ x] (s/conform* impl x))
      (unform* [_ x] (s/unform* impl x))
      (explain* [_ path via in x] (s/explain* impl path via in x))
      (gen* [_ overrides path rmap] (s/gen* impl overrides path rmap))
      (with-gen* [_ gfn] (union-impl schemas gfn))
      (describe* [_] `(union ~@schemas))

      Schema
      (keyspecs* [_] (keyspecs* impl)))))

(defmacro union
  [& schemas]
  `(union-impl ~schemas nil))

(comment
  (s/def ::name string?)
  (s/def ::user (s/keys :req [::name]))

  (s/explain-data ::user {::name 1})

  (do
    (s/def ::food-name string?)
    (s/def ::food (schema-impl [::food-name] nil))
    (s/def ::foods (s/coll-of ::food))
    (s/def ::user (schema-impl [::name ::foods] nil))

    (s/def ::user-req (select-impl ::user [::name ::foods {::foods [::food-name]}] nil)))

  (gen/generate (s/gen ::user))
  (gen/sample (s/gen ::user-req))

  (s/explain-data ::user-req {::name "a"})
  (s/conform ::user-req {::name  "a"
                         ::foods [{::food-name "asd"}]})
  (s/explain-data ::user-req {::name  ""
                              ::foods [{::food-name "ads"}]})
  )