(ns menu
  (:require [clojure.string :as string]
            [db :as db]))

(defn display-Clean []
  (print "\u001b[2J"))

(defn prompt []
  (println "***** Sales Menu *****")
  (println "1. Display Customers")
  (println "2. Display Products")
  (println "3. Display Sales")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit")
  (print "Enter an option? ")
  (flush))

(defn select []
  (let [option (read-line)]
    (if (some #(= option %) ["1" "2" "3" "4" "5" "6"])
      (Integer. option)
      (do (println "Invalid request. Please try again.")
          (recur)))))

(defn main-loop [customers products sales]
  (loop []
    (display-Clean)
    (prompt)
    (let [option (select)]
      (case option
        1 (do (db/display-customer-table customers)
              (println "Press Enter to continue...") (read-line))
        2 (do (db/display-product-table products)
              (println "Press Enter to continue...") (read-line))
        3 (do (db/display-sales-table sales customers products)
              (println "Press Enter to continue...") (read-line))
        4 (do (print "Enter customer name: ")
              (flush)
              (let [name (read-line)]
                (db/total-sales-for-customer name customers sales products))
              (println "Press Enter to continue...") (read-line))
        5 (do (print "Enter product name: ")
              (flush)
              (let [name (read-line)]
                (db/total-count-for-product name products sales))
              (println "Press Enter to continue...") (read-line))
        6 (do (println "Byee....") (System/exit 0))))
    (recur)))

(defn -main []
  (let [customers (db/build-customer-table)
        products (db/build-product-table)
        sales (db/build-sales-table)]
    (main-loop customers products sales)))

(-main)
