(ns db
  (:require [clojure.string :as string]))

;; using slurp to read the file as mentioned in the assignment
(defn load-file [filename]
  (slurp filename))

;; using string/split to split the line by the delimiter
(defn parse-line [line]
  (string/split line #"\|"))

;; using map to parse the line and return the list of lists
(defn load-data [filename]
  (->> (load-file filename)
       (string/split-lines)
       (map parse-line)))

;; using into to convert the list of lists to a map to build the customer table
(defn build-customer-table []
  (into {} (map (fn [[custID name address phoneNumber]]
                  [(Integer. custID) [name address phoneNumber]])
                (load-data "cust.txt"))))

;; using into to convert the list of lists to a map to build the product table
(defn build-product-table []
  (into {} (map (fn [[prodID itemDescription unitCost]]
                  [(Integer. prodID) [itemDescription (Double. unitCost)]])
                (load-data "prod.txt"))))

;; using map to convert the list of lists to a list of lists to build the sales table
(defn build-sales-table []
  (map (fn [[salesID custID prodID itemCount]]
         [(Integer. salesID) [(Integer. custID) (Integer. prodID) (Integer. itemCount)]])
       (load-data "sales.txt")))

;; Functions for display-customer-table recursively
(defn display-customer-table-helper [sorted-customers]
  (when (seq sorted-customers)
    (let [[id details] (first sorted-customers)]
      (println id ":" details)
      (recur (rest sorted-customers)))))
(defn display-customer-table [customers]
  (display-customer-table-helper (sort-by first customers)))


;; Functions for display-product-table recursively
(defn display-product-table-helper [sorted-products]
  (when (seq sorted-products)
    (let [[id details] (first sorted-products)]
      (println id ":" details)
      (recur (rest sorted-products)))))
(defn display-product-table [products]
  (display-product-table-helper (sort-by first products)))

;; Functions for display-sales-table recursively  
(defn display-sales-table-helper [sorted-sales customers products]
  (when (seq sorted-sales)
    (let [[id [custID prodID itemCount]] (first sorted-sales)]
      (println id ":" [(first (customers custID)) (first (products prodID)) (str itemCount)])
      (recur (rest sorted-sales) customers products))))
(defn display-sales-table [sales customers products]
  (display-sales-table-helper (sort-by first sales) customers products))

;; Functions for total-sales-for-customer
(defn find-customer-id [customer-name customers]
  (some (fn [[id [name _ _]]]
          (when (= name customer-name) id))
        customers))

(defn calculate-total-sales [customer-id sales products]
  (reduce (fn [acc [_ [custID prodID itemCount]]]
            (if (= custID customer-id)
              (+ acc (* itemCount (second (products prodID))))
              acc))
          0
          sales))

(defn total-sales-for-customer [customer-name customers sales products]
  (let [customer-id (find-customer-id customer-name customers)]
    (if customer-id
      (let [total (calculate-total-sales customer-id sales products)]
        (println customer-name ": $" total))
      (println "Customer not found"))))


;; Functions for total-count-for-product
(defn find-product-id [product-name products]
  (some (fn [[id [description _]]]
          (when (= description product-name) id))
        products))

(defn calculate-total-count [product-id sales]
  (reduce (fn [acc [_ [custID prodID itemCount]]]
            (if (= prodID product-id)
              (+ acc itemCount)
              acc))
          0
          sales))

(defn total-count-for-product [product-name products sales]
  (let [product-id (find-product-id product-name products)]
    (if product-id
      (let [total (calculate-total-count product-id sales)]
        (println product-name ": " total))
      (println "Product not found"))))
