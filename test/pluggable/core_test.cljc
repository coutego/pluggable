(ns pluggable.core-test
  (:require [pluggable.core :as sut]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true]))
  (:import clojure.lang.ExceptionInfo))

(deftest basic-plugin-functionality
  (testing "Minimal plugin is loaded"
    (let [p  {:id :plugin1}
          db (sut/load-plugins [p])]
      (is (= {} db))))

  (testing "Plugin without id throws exception"
    (let [p {:no-id :foo}]
      (is (thrown? ExceptionInfo (sut/load-plugins [p]))))))


(deftest extensions
  (testing "Basic extensions work"
    (let [h1  (fn [db ext-vals] (assoc db :test ext-vals))
          h2  (fn [db ext-vals] (assoc db :h2 (first (or ext-vals []))))
          p1  {:id   :p1
               :extensions
               [{:key     :test
                 :handler h1
                 :doc     "This extension loads keys ':test' and puts them on a list"
                 :spec    string?}]
               :test "Tp1"
               :h2   "p1"}
          p2  {:id         :p2
               :extensions [{:key     :h2
                             :handler h2
                             :doc     "..."}]
               :test       "Tp2"
               :h2         "p2"}
          p3  {:id   :p3
               :test "Tp3"
               :h2   "p3"}
          res (sut/load-plugins [p1 p2 p3])]
      (is (= ["Tp1" "Tp2" "Tp3"] (:test res)))
      (is (= "p2" (:h2 res)))))

  (testing "Specs are checked"
    (let [h1 (fn [db ext-vals] (assoc db :test ext-vals))
          p1 {:id   :p1
              :extensions
              [{:key     :test
                :handler h1
                :doc     "This extension loads keys ':test' and puts them on a list"
                :spec    string?}]
              :test "Tp1"
              :h2   "p1"}
          p2 {:id   :p2
              :test 42}]
      (is (thrown? ExceptionInfo (sut/load-plugins [p1 p2])))))
  (testing "Nils are filtered out"
    (let [h1  (fn [db ext-vals] (assoc db :test ext-vals))
          h2  (fn [db ext-vals] (assoc db :h2 (first (or ext-vals []))))
          p1  {:id   :p1
               :extensions
               [{:key     :test
                 :handler h1
                 :doc     ""
                 :spec    string?}]
               :h2   "p1"}
          p2  {:id         :p2
               :extensions [{:key     :h2
                             :handler h2
                             :doc     "..."}]
               :test       "Tp2"
               :h2         "p2"}
          p3  {:id   :p3
               :h2   "p3"}
          res (sut/load-plugins [p1 p2 p3])]
      (is (= ["Tp2"] (:test res)))
      (is (= "p2" (:h2 res))))))


