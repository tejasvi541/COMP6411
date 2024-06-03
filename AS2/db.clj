(ns db
  (:require [clojure.string :as string]))

(defn load-file [filename]
  (slurp filename))

(defn parse-line [line]
  (string/split line #"\|"))

(defn load-data [filename]
  (->> (load-file filename)
       (string/split-lines)
       (map parse-line)))

(defn build-customer-table []
  (into {} (map (fn [[custID name address phoneNumber]]
                  [(Integer. custID) [name address phoneNumber]])
                (load-data "cust.txt"))))

(defn build-product-table []
  (into {} (map (fn [[prodID itemDescription unitCost]]
                  [(Integer. prodID) [itemDescription (Double. unitCost)]])
                (load-data "prod.txt"))))

(defn build-sales-table []
  (map (fn [[salesID custID prodID itemCount]]
         [(Integer. salesID) [(Integer. custID) (Integer. prodID) (Integer. itemCount)]])
       (load-data "sales.txt")))

(defn display-customer-table [customers]
  (doseq [[id details] (sort-by first customers)]
    (println id ":" details)))

(defn display-product-table [products]
  (doseq [[id details] (sort-by first products)]
    (println id ":" details)))

(defn display-sales-table [sales customers products]
  (doseq [[id [custID prodID itemCount]] (sort-by first sales)]
    (println id ":" [(first (customers custID)) (first (products prodID)) (str itemCount)])))

(defn total-sales-for-customer [customer-name customers sales products]
  (let [customer-id (some (fn [[id [name _ _]]]
                            (when (= name customer-name) id))
                          customers)]
    (if customer-id
      (let [total (reduce (fn [acc [_ [custID prodID itemCount]]]
                            (if (= custID customer-id)
                              (+ acc (* itemCount (second (products prodID))))
                              acc))
                          0
                          sales)]
        (println customer-name ": $" total))
      (println "Customer not found"))))

(defn total-count-for-product [product-name products sales]
  (let [product-id (some (fn [[id [description _]]]
                           (when (= description product-name) id))
                         products)]
    (if product-id
      (let [total (reduce (fn [acc [_ [custID prodID itemCount]]]
                            (if (= prodID product-id)
                              (+ acc itemCount)
                              acc))
                          0
                          sales)]
        (println product-name ": " total))
      (println "Product not found"))))
