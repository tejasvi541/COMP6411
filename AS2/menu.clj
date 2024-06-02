(ns menu
  (:require [db :as db]))

(defn display-table [table]
  (doseq [record table]
    (println record)))

(defn display-sales-table []
  (doseq [record db/sales]
    (let [customer (db/get-customer (second record))
          product (db/get-product (nth record 2))]
      (println (list (first record) (second customer) (second product) (last record))))))

(defn menu []
  (println "*** Sales Menu ***")
  (println "------------------")
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit")
  (print "Enter an option? ")
  (flush)
  (let [option (read-line)]
    (cond
      (= option "1") (display-table db/customers)
      (= option "2") (display-table db/products)
      (= option "3") (display-sales-table)
      (= option "4") (do (print "Enter customer name: ")(flush)
                          (println (db/total-sales-for-customer (read-line))))
      (= option "5") (do (print "Enter product description: ")(flush)
                          (println (db/total-count-for-product (read-line))))
      (= option "6") (println "Good Bye")
      :else (println "Invalid option")))
  (unless (= option "6") (menu)))

(defn -main []
  (menu))