(ns db
  (:require [clojure.string :as str]))

(defn load-data [filename]
  (map (fn [line] (str/split line #"\|"))
       (str/split-lines (slurp filename))))

(def customers (load-data "cust.txt"))
(def products (load-data "prod.txt"))
(def sales (load-data "sales.txt"))

(defn get-customer [id]
  (first (filter #(= (first %) id) customers)))

(defn get-product [id]
  (first (filter #(= (first %) id) products)))

(defn total-sales-for-customer [name]
  (reduce + (map #(Integer/parseInt (last %))
                 (filter #(= (second (get-customer (second %))) name) sales))))

(defn total-count-for-product [description]
  (reduce + (map #(Integer/parseInt (last %))
                 (filter #(= (second (get-product (nth % 2))) description) sales))))