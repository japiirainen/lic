#!/usr/bin/env bb
(refer-clojure :exclude [eval read read-string])
(require '[clojure.edn :refer [read read-string]])

(def literal?
  (some-fn string? number? boolean? char? nil? fn?))

(defn seq-starts-with? [starts-with form]
  (and (seqable? form) (= (first form) starts-with)))

(def quote? (partial seq-starts-with? 'quote))

(def def? (partial seq-starts-with? 'def))

(def env (atom {:globe {'+ + 'fav-num 41}}))

(defn lookup-symbol [env-atom sym]
  (let [{:keys [globe]} @env-atom
        v (get globe sym)]
    (assert v (format "expected value for sym = %s" sym))
    v))

(defn eval-many [env forms] (map (partial eval env) forms))

(defn eval-application [env [f & args]]
  (let [f-evaluated (user/eval env f)]
    (cond
      (fn? f-evaluated) (apply f-evaluated (eval-many env args)))))

(defn eval-def [env [_ k v]]
  (assert (symbol? k) (format "expected k = %s to be a symbol" k))
  (swap! env assoc-in [:globe k] (user/eval env v)))

(defn eval [env form]
  (cond
    (literal? form) form
    (quote? form) (second form)
    (symbol? form) (lookup-symbol env form)
    (def? form) (eval-def env form)
    :else (eval-application env form)))

(comment
  (eval env (read-string "12"))
  (eval env (read-string "(quote (+ 1 1))"))
  (eval env (read-string "fav-num"))
  (eval env (read-string "(+ 1 1)"))
  (eval env (read-string "(+ 1 (+ 1 fav-num))"))
  (eval env (read-string "(def second-fav (+ 1 fav-num))"))
  (eval env (read-string "second-fav"))
  )