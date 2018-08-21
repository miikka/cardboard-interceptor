(ns straw-interceptor.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [straw-interceptor.core :as interceptor]))

(def inc-enter-interceptor
  {:enter (fn [ctx] (update ctx :a inc))})

(def inc-leave-interceptor
  {:leave (fn [ctx] (update ctx :a inc))})

(def inc-dec-interceptor
  {:enter (fn [ctx] (update ctx :a inc))
   :leave (fn [ctx] (update ctx :a dec))})

(def throw-enter-interceptor
  {:enter (fn [ctx] (throw (Exception. "test")))})

(def throw-leave-interceptor
  {:leave (fn [ctx] (throw (Exception. "Test")))})

(defn conj-interceptor
  [x]
  {:enter (fn [ctx] (update ctx :a conj x))})

(defn catch-interceptor
  [error-a]
  {:error (fn [ctx] (reset! error-a (:error ctx)) (dissoc ctx :error))})

(defn exception? [e] (instance? Exception e))

(deftest execute-test
  (testing ":enter only"
    (is (= {:a 1} (interceptor/execute [inc-enter-interceptor] {:a 0})))
    (is (= {:a 2} (interceptor/execute (repeat 2 inc-enter-interceptor) {:a 0}))))
  (testing ":leave only"
    (is (= {:a 1} (interceptor/execute [inc-leave-interceptor] {:a 0})))
    (is (= {:a 2} (interceptor/execute (repeat 2 inc-leave-interceptor) {:a 0}))))
  (testing ":enter and :leave"
    (is (= {:a 0} (interceptor/execute [inc-dec-interceptor] {:a 0}))))
  (testing "ordering"
    (is (= {:a [:a :b]}
           (interceptor/execute [(conj-interceptor :a) (conj-interceptor :b)]
                                {:a []}))))
  (testing "unhandled exceptions are thrown"
    (is (thrown? Exception (interceptor/execute [throw-enter-interceptor] {}))))
  (testing "exceptions are caught on :enter"
    (let [error-a (atom nil)]
      (is (= {:a 0}
             (interceptor/execute [(catch-interceptor error-a) throw-enter-interceptor]
                                  {:a 0})))
      (is (exception? @error-a))))
  (testing "exceptions are caught on :leave"
    (let [error-a (atom nil)]
      (is (= {:a 0}
             (interceptor/execute [(catch-interceptor error-a) throw-leave-interceptor]
                                  {:a 0})))
      (is (exception? @error-a)))))
