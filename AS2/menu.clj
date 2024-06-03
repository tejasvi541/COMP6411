(ns menu
  (:require [clojure.string :as string]
            [db :as db]))

(defn clear-screen []
  (print "\u001b[2J"))

(defn prompt []
  (println "*** Sales Menu ***")
  (println "------------------")
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit\n")
  (print "Enter an option? ")
  (flush))

(defn get-option []
  (let [option (read-line)]
    (if (some #(= option %) ["1" "2" "3" "4" "5" "6"])
      (Integer. option)
      (do (println "Invalid option, try again.")
          (recur)))))

(defn main-loop [customers products sales]
  (loop []
    (clear-screen)
    (prompt)
    (let [option (get-option)]
      (case option
        1 (do (db/display-customer-table customers)
              (read-line))
        2 (do (db/display-product-table products)
              (read-line))
        3 (do (db/display-sales-table sales customers products)
              (read-line))
        4 (do (print "Enter customer name: ")
              (flush)
              (let [name (read-line)]
                (db/total-sales-for-customer name customers sales products))
              (read-line))
        5 (do (print "Enter product name: ")
              (flush)
              (let [name (read-line)]
                (db/total-count-for-product name products sales))
              (read-line))
        6 (do (println "Good Bye") (System/exit 0))))
    (recur)))

(defn -main []
  (let [customers (db/build-customer-table)
        products (db/build-product-table)
        sales (db/build-sales-table)]
    (main-loop customers products sales)))

;; To run the program directly
(-main)
