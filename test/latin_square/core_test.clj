(ns latin-square.core-test
  (:require [clojure.test :refer :all]
            [latin-square.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defn generate [size]
  (let [values (range 1 (inc size))]
    (mapv (fn [v] (->> (cycle values) (drop v) (take size) (into []))) values)))

(deftest column-and-row-values
  (testing "valid latin square"
    (is (= {:row-values (repeat 3 #{1 2 3})
            :column-values (repeat 3 #{1 2 3})}
           (square-values [[1 2 3]
                           [2 3 1]
                           [3 1 2]]))))

  (testing "too few columns, no duplicates, values in range"
    (is (thrown? ExceptionInfo
                 (square-values [[1 2] [2 3] [3 1]]))))

  (testing "Value exceeds number of rows"
    (is (thrown? ExceptionInfo
                 (square-values [[1 2 3] [2 3 1]]))))

  (testing "Value out of range"
    (is (thrown? ExceptionInfo
                 (square-values [[1 2 3] [2 3 1] [3 1 0]]))))

  (testing "Duplicate values in second row and first column"
    (is (= {:row-values [#{1 2 3} #{1 3} #{1 2 3}]
            :column-values [#{1 3} #{1 2 3} #{1 2 3}]}
           (square-values [[1 2 3] [1 3 1] [3 1 2]]))))

  (testing "Duplicate values in first row and second column"
    (is (= {:row-values [#{1 3} #{1 2 3} #{1 2 3}]
            :column-values [#{1 2 3} #{1 3} #{1 2 3}]}
           (square-values [[1 1 3] [2 3 1] [3 1 2]]))))

  (testing "Duplicate values in second row and third column"
    (is (= {:row-values [#{1 2 3} #{2 3} #{1 2 3}]
            :column-values [#{1 2 3} #{1 2 3} #{2 3}]}
           (square-values [[1 2 3] [2 3 2] [3 1 2]]))))

  (testing "1x1 square is allowed"
    (is (= {:row-values [#{1}] :column-values [#{1}]}
           (square-values [[1]]))))

  (testing "Empty square throws an exception"
    (is (thrown? ExceptionInfo (square-values [[]]))))

  (testing "Time taken for 1000x1000 square"
    (let [input (generate 500)
          before (System/currentTimeMillis)
          result (square-values input)
          after (System/currentTimeMillis)]
      (println "Took" (- after before) "ms")
      (is (= [500 500] (map count (vals result))))
      (comment is (< (- after before) 1000)))))

(deftest test-predicate
  (testing "3x3 example"
    (is (latin-square?
          [[1 2 3]
           [2 3 1]
           [3 1 2]])))

  (testing "values out of range for square"
    (is (not (latin-square?
               [[4 2 3]
                [2 3 4]
                [3 4 2]]))))

  (testing "duplicate in column"
    (is (not (latin-square?
               [[1 2 3]
                [1 3 1]
                [3 1 2]]))))

  (testing "duplicate col2 and row3"
    (is (not (latin-square?
               [[1 2 3]
                [2 3 1]
                [3 2 2]]))))

  (testing "too many columns"
    (is (not (latin-square?
               [[1 2 3 1]
                [2 3 1 2]
                [3 1 2 3]]))))

  (testing "too many rows"
    (is (not (latin-square?
               [[1 2 4 3]
                [2 3 1 4]
                [4 2 2 1]]))))

  (testing "larger square"
    (let [square (generate 100)]
      (is (latin-square? square))))

  (testing "empty square is not valid"
    (is (not (latin-square? []))))

  (testing "single element square is valid"
    (is (latin-square? [[1]]))))
