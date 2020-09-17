(ns pluggable.container-test
  (:require [pluggable.container :as sut]
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

(deftest dependency-management
  (testing "Duplicate ids are detected"
    (let [p1 {:id :p1}
          p2 {:id :p1 :foo :bar}]
      (is (thrown? ExceptionInfo (sut/load-plugins [p1 p2])))))

  (testing "Circular dependencies are detected"
    (let [p1 {:id :p1}
          p3 {:id :p3 :deps [p1]}
          p2 {:id :p2 :deps [p3]}
          p1 {:id :p1 :deps [p2]}]
      (is (thrown? ExceptionInfo (sut/process-plugin-deps [p1 p2 p3])))))

  (testing "Plugins dependencies are taken into account"
    (let [p1 {:id :p1}
          p2 {:id :p2 :deps [p1]}
          p3 {:id :p3 :deps [p2]}]
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p1 p2 p3]))))
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p1 p3 p2]))))
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p2 p1 p3]))))
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p2 p3 p1]))))
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p3 p2 p1]))))
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p3 p1 p2]))))))

  (testing "Plugins dependencies & plugin order are taken into account"
    (let [p1 {:id :p1}
          p2 {:id :p2 :deps [p1]}
          p3 {:id :p3 :deps [p1]}
          p4 {:id :p4 :deps [p2]}
          p5 {:id :p5 :deps [p2 p4]}
          p6 {:id :p6 :deps [p1]}]
      (is (= [p1 p2 p3] (into [] (sut/process-plugin-deps [p1 p2 p3]))))
      (is (= [p1 p3 p2] (into [] (sut/process-plugin-deps [p1 p3 p2]))))
      (is (= [p1 p2 p4 p3] (into [] (sut/process-plugin-deps [p4 p2 p1 p3]))))
      (is (= [p1 p6 p2 p4 p5 p3] (into [] (sut/process-plugin-deps [p6 p5 p3 p1 p3])))))))

(deftest loaders
  (comment)
  (testing "Check whether loaders are invoked"
    (let [p1-loader (fn [db plugins] (update db :p1 #(inc (or % 0))))
          p2-loader (fn [db plugins] (update db :p2 #(inc (or % 0))))
          p1        {:id :p1 :loader p1-loader}
          p2        {:id :p2 :loader p2-loader}
          db        (sut/load-plugins [p1 p2] {})]
      (is (= 1 (:p1 db)))
      (is (= 1 (:p2 db)))))

  (testing "Check whether loaders can process extensions"
    (let [p1-loader
          (fn [db plugins]
            (let [foo (reduce
                       (fn [acc p]
                         (if-let [n (:foo p)]
                           (+ acc n)
                           acc))
                       0
                       plugins)]
              (assoc db :foo-acc foo)))
          p2-loader
          (fn [db plugins]
            (let [bar (reduce
                       (fn [acc p]
                         (if-let [n (:bar p)]
                           (+ acc n)
                           acc))
                       0
                       plugins)]
              (assoc db :bar-acc bar)))
          p1        {:id :p1 :loader p1-loader :foo 1 :bar 1}
          p2        {:id :p2 :loader p2-loader :foo 2 :bar 2}
          db        (sut/load-plugins [p1 p2] {})]
        (is (= 3 (:foo-acc db)))
        (is (= 2 (:bar-acc db))))))
