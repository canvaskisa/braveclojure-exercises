(ns chapter-4.core)

;; --------------------------------------------------------------
;; Chapter 4 http://www.braveclojure.com/core-functions-in-depth/
;;
;; A Vampire Data Analysis Program for the FWPD

(def filename "src/chapter_4/suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  "Converts string to Integer"
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

;; ----------
;; Exercise 1.
;; Turn the result of your glitter filter into a list of names.
;; ----------
(defn glittered-persons
  "Expects a seq of maps like {:name \"\" :glitter-index 10} and converts it into a list"
  [persons]
  (map (fn [person]
         (:name person))
       persons))
