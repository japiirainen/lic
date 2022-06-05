#!/usr/bin/env bb
(refer-clojure :exclude [eval read read-string])
(require '[clojure.edn :refer [read read-string]])

(def closure? (partial seq-starts-with? 'clo))

(def literal?
  (some-fn string? number? boolean? char? nil? fn? closure?))

(defn seq-starts-with? [starts-with form]
  (and (seqable? form) (= (first form) starts-with)))

(def quote? (partial seq-starts-with? 'quote))

(def def? (partial seq-starts-with? 'def))

(def if? (partial seq-starts-with? 'if))

(def env (atom {:globe {'+     + '= = 'list list 'map map 'concat concat
                        'first first 'second second 'not not 'fav-num 41}
                :scope {}}))

(defn lookup-symbol [env-atom sym]
  (let [{:keys [globe scope]} @env-atom
        v (get (merge globe scope {'scope scope}) sym)]
    (assert v (format "expected value for sym = %s" sym))
    v))

(defn eval-many [env forms] (map (partial eval env) forms))

(defn assign-vars [symbols args]
  (assert (= (count symbols) (count args))
          (format "symbols and args must match. symbols: %s args: %s"
                  (vec symbols) (vec args)))
  (into {} (map vector symbols args)))

(defn eval-closure [env [_ scope symbols body] args]
  (user/eval (atom (swap! env assoc :scope (merge scope (assign-vars symbols args)))) body))

(defn eval-application [env [f & args]]
  (let [f-evaluated (user/eval env f)]
    (cond
      (fn? f-evaluated) (apply f-evaluated (eval-many env args))
      (closure? f-evaluated) (eval-closure env f-evaluated (eval-many env args)))))

(defn eval-def [env [_ k v]]
  (assert (symbol? k) (format "expected k = %s to be a symbol" k))
  (swap! env assoc-in [:globe k] (user/eval env v)))

(defn eval-if [env [_ tf t f]]
  (let [et (user/eval env tf)]
    (user/eval env (if et t f))))

(defn eval [env form]
  (cond
    (literal? form) form
    (quote? form) (second form)
    (symbol? form) (lookup-symbol env form)
    (def? form) (eval-def env form)
    (if? form) (eval-if env form)
    :else (eval-application env form)))

(comment
  (eval env (read-string "12"))
  (eval env (read-string "(quote (+ 1 1))"))
  (eval env (read-string "fav-num"))
  (eval env (read-string "(+ 1 1)"))
  (eval env (read-string "(+ 1 (+ 1 fav-num))"))
  (do
    (eval env (read-string "(def second-fav (+ 1 fav-num))"))
    (eval env (read-string "second-fav")))
  (eval env (read-string "(if (= fav-num 41) (quote yes) (quote no))"))
  (eval env (read-string "(((clo nil (x) (list (quote clo) scope (quote (y)) (quote (+ y x)))) 2) 4)"))
  )