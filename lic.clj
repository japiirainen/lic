(ns lic
  (:refer-clojure :exclude [read])
  (:require [clojure.edn :refer [read]]))

(defn seq-starts-with? [starts-with form]
  (and (seqable? form) (= (first form) starts-with)))
(def closure? (partial seq-starts-with? 'clo))
(def macro? (partial seq-starts-with? 'mac))
(def literal?
  (some-fn string? number? boolean? char? nil? fn? closure? macro?))
(def quote? (partial seq-starts-with? 'quote))
(def def? (partial seq-starts-with? 'def))
(def if? (partial seq-starts-with? 'if))

(defn lookup-symbol [env-atom sym]
  (let [{:keys [globe scope]} @env-atom
        v (get (merge {'scope scope} globe scope) sym)]
    (assert v (format "expected value for symbol = %s" sym))
    v))

(declare evaluate eval-many)

(defn assign-vars [symbols args]
  (assert (= (count symbols) (count args))
          (format "symbols and args must match. symbols: %s args: %s"
                  (vec symbols) (vec args)))
  (into {} (map vector symbols args)))

(defn eval-closure [env [_ scope symbols body] args]
  (evaluate (atom (assoc @env :scope (merge scope (assign-vars symbols args)))) body))

(defn eval-macro [env [_ clo] args]
  (evaluate env
            (evaluate env
                      (concat [clo] (map (partial list 'quote) args)))))

(defn eval-def [env [_ k v]]
  (assert (symbol? k) (format "expected k = %s to be a symbol" k))
  (swap! env assoc-in [:globe k] (evaluate env v)))

(defn eval-if [env [_ tf t f]]
  (let [et (evaluate env tf)]
    (evaluate env (if et t f))))

(defn eval-application [env [f & args]]
  (let [f-evaluated (evaluate env f)]
    (cond
      (fn? f-evaluated) (apply f-evaluated (eval-many env args))
      (closure? f-evaluated) (eval-closure env f-evaluated (eval-many env args))
      (macro? f-evaluated) (eval-macro env f-evaluated args))))

(defn evaluate [env form]
  (cond
    (literal? form) form
    (quote? form) (second form)
    (symbol? form) (lookup-symbol env form)
    (def? form) (eval-def env form)
    (if? form) (eval-if env form)
    :else (eval-application env form)))

(defn eval-many [env forms] (map (partial evaluate env) forms))

(def env (atom {:globe {'+     + '= = 'list list 'map map 'concat concat
                        'first first 'second second 'not not 'fav-num 41}
                :scope {}}))

(defn -main [& args]
  (println "welcome to lic")
  (loop []
    (print "lic> ")
    (flush)
    (println (evaluate env (read)))
    (recur)))

(comment
  ; we have basic primitive support
  (evaluate env "foo")
  (evaluate env 1.2)

  ; we also have closures
  (evaluate env '(clo nil (x) (+ 1 x)))

  ; we can also define variables via `def`
  (let [e env]
    (evaluate e '(def foo 'bar))
    (evaluate e 'foo))

  ; we have `if` statements
  (evaluate env '(if true 'foo 'bar))

  ; we have higher order functions like `map`
  (evaluate env '(map first '((a b))))

  ; we have macro support via `mac`
  (evaluate env '(def unless (mac (clo nil (test v))
                                  (list 'if (list 'not test) v))))
  ; we can define `defmacro` to ease writing new macros
  (evaluate env '(def defmacro
                   (mac (clo nil (n p e)
                             (list 'def n
                                   (list 'mac (list 'clo nil p e)))))))
  ; with `defmacro` we can write `unless` more concisely
  (evaluate env '(defmacro unless (test v)
                   (list 'if (list 'not test) v)))

  ; you call macros just like functions
  (evaluate env '(unless (= fav-num 41) (throw-error)))

  ; let's write a `fn` macro
  (evaluate env '(defmacro fn (args body)
                   (list 'list ''clo 'scope
                         (list 'quote args)
                         (list 'quote body))))

  ; now we can do stuff like this!
  (evaluate env '(def make-adder (clo nil (n)
                                      (fn (y) (+ y n)))))
  (evaluate env '(def add-two (make-adder 2)))
  (evaluate env '(add-two 5))

  ; currently, to define functions we have to use this rather verbose syntax
  (evaluate env '(def add-fav-num (fn (x) (+ x fav-num))))

  ; let's introduce `defn`
  (evaluate env '(defmacro defn (n args body)
                   (list 'def n (list 'fn args body))))

  ; one more cool macro
  (evaluate env '(defmacro let (pairs body)
                   (concat (list (list 'fn (map first pairs) body))
                           (map second pairs))))

  (evaluate env '(let ((x 1) (y 2))
                   (+ x y)))

  )
