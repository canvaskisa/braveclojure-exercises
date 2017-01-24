(ns chapter-3.core)

;; --------------------------------------
;; http://www.braveclojure.com/do-things/

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  "Expects a map of :name and :size.
   Replaces left- to the right- in the name value"
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

;; -----------------------
;; Recursion implemetation
(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size.
   Clones all :name values, which starts with left- to right-"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

;; --------------------
;; Reduce implemetation
(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size.
   Clones all :name values, which starts with left- to right-"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  "Expects a seq of maps that have a :name and :size.
   Randomly chooses one map, based on it's size & index."
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

;; ----------
;; Exercise 1.
;; Use the str, vector, list, hash-map, and hash-set functions.
;; ----------
;; Use str.
(defn greet
  "Expects a string with a name to greet"
  [name]
  (str "Hello " name))

;; Use vector.
(defn inc-seq
  "Increment seq and convert to vector"
  [s]
  (into (vector) (map inc s)))

;; Use list
(defn dec-seq
  "Decrement seq and convert to list"
  [s]
  (into (list) (map dec s)))

;; Use hash-map
(defn str-to-map
  "Converts str to map"
  [s]
  (apply hash-map (.split s "")))

;; Use hash-set
(defn uniq-seq
  "Converts seq to unique set"
  [s]
  (apply hash-set s))

;; ----------
;; Exercise 2.
;; Write a function that takes a number and adds 100 to it.
;; ----------
(defn add100
  "Adds 100 to provided number"
  [x]
  (+ x 100))

;; ----------
;; Exercise 3.
;; Write a function, dec-maker, that works exactly like the function inc-maker except with subtraction.
;;
;; (def dec9 (dec-maker 9))
;; (dec9 10) => 1
;; ----------
(defn dec-maker
  "Creates a custom decrementator"
  [dec-by]
  #(- % dec-by))

;; ----------
;; Exercise 4.
;; Write a function, mapset, that works like map except the return value is a set:
;;
;; (mapset inc [1 1 2 2]) => #{2 3}
;; ----------
(defn mapset
  "Works like map except the return value is a set"
  [f l]
  (set (map f l)))

;; ----------
;; Exercise 5.
;; Create a function that’s similar to symmetrize-body-parts except that it has to work
;; with weird space aliens with radial symmetry.
;; Instead of two eyes, arms, legs, and so on, they have five.
;; ----------
(def asym-alien-body-parts [{:name "head" :size 3}
                            {:name "0-deg-eye" :size 1}
                            {:name "0-deg-ear" :size 1}
                            {:name "mouth" :size 1}
                            {:name "nose" :size 1}])
(defn matching-weird-part
  "Expects a map of :name and :size.
   Replaces left- to the right- in the name value"
  [part to]
  {:name (clojure.string/replace (:name part) #"^0" to)
   :size (:size part)})

(defn weird-symmetrize-body-parts
  "Symmetrizes body parts radially"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part
                                         (matching-weird-part part (str 72))
                                         (matching-weird-part part (str (* 72 2)))
                                         (matching-weird-part part (str (* 72 3)))
                                         (matching-weird-part part (str (* 72 4)))
                                         (matching-weird-part part (str (* 72 5)))])))
          []
          asym-body-parts))

;; ----------
;; @TODO Exercise 6.
;; Create a function that generalizes symmetrize-body-parts and the function you created in Exercise 5.
;; The new function should take a collection of body parts and the number of matching body parts to add.
;; If you’re completely new to Lisp languages and functional programming, it probably won’t be obvious how to do this.
;; If you get stuck, just move on to the next chapter and revisit the problem later.
;; ----------
