;; 1
(= true true)

;; 2
(= (- 10 (* 2 3)) 4)

;; 3
(= "HELLO WORLD" (.toUpperCase "hello world"))

;; 4
(= (list :a :b :c) '(:a :b :c))

;; 5
(= '(1 2 3 4) (conj '(2 3 4) 1))
(= '(1 2 3 4) (conj '(3 4) 2 1))

;; 6
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

;; 7
(= [1 2 3 4] (conj [1 2 3] 4))
(= [1 2 3 4] (conj [1 2] 3 4))

;; 8
(= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))
(= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))

;; 9
(= #{1 2 3 4} (conj #{1 4 3} 2))

;; 10
(= 20 ((hash-map :a 10, :b 20, :c 30) :b))
(= 20 (:b {:a 10, :b 20, :c 30}))

;; 11
(= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))

;; 12
(= 3 (first '(3 2 1)))
(= 3 (second [2 3 4]))
(= 3 (last (list 1 2 3)))

;; 13
(= [20 30 40] (rest [10 20 30 40]))

;; 14
(= 8 ((fn add-five [x] (+ x 5)) 3))
(= 8 ((fn [x] (+ x 5)) 3))
(= 8 (#(+ % 5) 3))
(= 8 ((partial + 5) 3))

;; 15
(= (* 2 2) 4)
(= (* 2 3) 6)
(= (* 2 11) 22)
(= (* 2 7) 14)

;; 16
(defn hello [s]
  (str "Hello, " s "!"))

(= (hello "Dave") "Hello, Dave!")
(= (hello "Jenn") "Hello, Jenn!")
(= (hello "Rhea") "Hello, Rhea!")

;; 17
(= '(6 7 8) (map #(+ % 5) '(1 2 3)))

;; 18
(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

;; 19
(= ((comp first reverse) [1 2 3 4 5]) 5)
(= ((comp first reverse) '(5 4 3)) 3)
(= ((comp first reverse) ["b" "c" "d"]) "d")

;; 20
(= ((comp second reverse) (list 1 2 3 4 5)) 4)
(= ((comp second reverse) ["a" "b" "c"]) "b")
(= ((comp second reverse) [[1 2] [3 4]]) [1 2])

;; 21
(defn my-nth [coll n]
  (if (= n 0)
    (first coll)
    (recur (rest coll) (dec n))))

(= (my-nth '(4 5 6 7) 2) 6)
(= (my-nth [:a :b :c] 0) :a)
(= (my-nth [1 2 3 4] 1) 2)
(= (my-nth '([1 2] [3 4] [5 6]) 2) [5 6])

;; 22
(= (reduce (fn [acc _] (inc acc)) 0 '(1 2 3 3 1)) 5)
(= (reduce (fn [acc _] (inc acc)) 0 "Hello World") 11)
(= (reduce (fn [acc _] (inc acc)) 0 [[1 2] [3 4] [5 6]]) 3)
(= (reduce (fn [acc _] (inc acc)) 0 '(13)) 1)
(= (reduce (fn [acc _] (inc acc)) 0 '(:a :b :c)) 3)

;; 23
(= (reduce #(conj %1 %2) '() [1 2 3 4 5]) [5 4 3 2 1])
(= (reduce #(conj %1 %2) '() (sorted-set 5 7 2 7)) '(7 5 2))
(= (reduce #(conj %1 %2) '() [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])

;; 24
(= (reduce + [1 2 3]) 6)
(= (reduce + (list 0 -2 5 5)) 8)
(= (reduce + #{4 2 1}) 7)
(= (reduce + '(0 0 -1)) -1)
(= (reduce + '(1 10 3)) 14)

;; 25
(= (filter odd? #{1 2 3 4 5}) '(1 3 5))
(= (filter odd? [4 2 1 6]) '(1))
(= (filter odd? [2 2 4 6]) '())
(= (filter odd? [1 1 1 3]) '(1 1 1 3))

;; 26
(do ; 4Clojure requires the solution to be just one Sexp
  (def fibs (lazy-cat [1 1] (map + fibs (rest fibs))))
  (defn n-fibs [n]
    (take n fibs)))

(= (n-fibs 3) '(1 1 2))
(= (n-fibs 6) '(1 1 2 3 5 8))
(= (n-fibs 8) '(1 1 2 3 5 8 13 21))

;; 27
(defn palindrome? [s]
  (let [len             (count s)
        mid             (quot len 2)
        first-half      (take mid s)
        second-half-rev (reverse (take-last mid s))]
    (= first-half second-half-rev)))

(false? (palindrome? '(1 2 3 4 5)))
(true?  (palindrome? "racecar"))
(true?  (palindrome? [:foo :bar :foo]))
(true?  (palindrome? '(1 1 3 3 1 1)))
(false? (palindrome? '(:a :b :c)))

;; 28
(defn my-flatten [coll]
  (mapcat #(if (sequential? %) (my-flatten %) (list %)) coll))

(= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c"))
(= (my-flatten '((((:a))))) '(:a))

;; 29
(defn get-caps [s]
  (clojure.string/replace s #"[^A-Z]" ""))

(=      (get-caps "HeLlO, WoRlD!") "HLOWRD")
(empty? (get-caps "nothing"))
(=      (get-caps "$#A(*&987Zf") "AZ")

;; 30
(defn remove-seq-dup [coll]
  (letfn [(iter [coll acc]
            (if (empty? coll)
              acc
              (recur (next coll)
                     (if (= (first coll) (last acc))
                       acc
                       (conj acc (first coll))))))]
    (iter coll [])))

(= (apply str (remove-seq-dup "Leeeeeerrroyyy")) "Leroy")
(=            (remove-seq-dup [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(=            (remove-seq-dup [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

;; 31
(defn compress [coll]
  (letfn [(iter [coll acc]
            (if (empty? coll)
              acc
              (recur (next coll)
                     (if (= (first coll) (first (last acc)))
                       (conj (pop acc) (conj (last acc) (first coll)))
                       (conj acc (list (first coll)))))))]
    (iter coll [])))

(= (compress [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (compress [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (compress [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))

;; 32
(defn duplicate [coll]
  (mapcat #(list % %) coll))

(= (duplicate [1 2 3]) '(1 1 2 2 3 3))
(= (duplicate [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (duplicate [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (duplicate [44 33]) [44 44 33 33])

;; 33
(defn n-plicate [coll n]
  (mapcat #(repeat n %) coll))

(= (n-plicate [1 2 3] 2) '(1 1 2 2 3 3))
(= (n-plicate [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= (n-plicate [4 5 6] 1) '(4 5 6))
(= (n-plicate [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= (n-plicate [44 33] 2) [44 44 33 33])

;; 34
(defn my-range [inf sup]
  (letfn [(iter [inf acc]
            (if (< inf sup)
              (recur (inc inf) (conj acc inf))
              (reverse acc)))]
    (iter inf nil)))

(= (my-range 1 4) '(1 2 3))
(= (my-range -2 2) '(-2 -1 0 1))
(= (my-range 5 8) '(5 6 7))

;; 35
(= 7 (let [x 5] (+ 2 x)))
(= 7 (let [x 3, y 10] (- y x)))
(= 7 (let [x 21] (let [y 3] (/ x y))))

;; 36
(= 10 (let [x 7, y 3, z 1] (+ x y)))
(= 4  (let [x 7, y 3, z 1] (+ y z)))
(= 1  (let [x 7, y 3, z 1] z))

;; 37
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; 38
(defn maximum [& args]
  (reduce #(if (> %1 %2) %1 %2) args))

(= (maximum 1 8 3 4) 8)
(= (maximum 30 20) 30)
(= (maximum 45 67 11) 67)

;; 39
(defn my-interleave [c1 c2]
  (mapcat #(list %1 %2) c1 c2))

(= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
(= (my-interleave [1 2 3 4] [5]) [1 5])
(= (my-interleave [30 20] [25 15]) [30 25 20 15])

;; 40
(defn my-interpose [i coll]
  (butlast (my-interleave coll (repeat i))))

;; 41
(defn drop-step [coll step]
  (letfn [(iter [coll n acc]
            (cond
              (empty? coll) acc
              (= n step) (recur (next coll) 1 acc)
              :else (recur (next coll) (inc n) (conj acc (first coll)))))]
    (iter coll 1 [])))

(= (drop-step [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (drop-step [:a :b :c :d :e :f] 2) [:a :c :e])
(= (drop-step [1 2 3 4 5 6] 4) [1 2 3 5 6])

;; 42
(defn factorial [n]
  (letfn [(iter [n acc]
            (if (< n 2)
              acc
              (recur (dec n) (* n acc))))]
    (iter n 1)))

(= (factorial 1) 1)
(= (factorial 3) 6)
(= (factorial 5) 120)
(= (factorial 8) 40320)

;; 43
(defn rev-interleave [coll step]
  (apply map list (partition step coll)))

(= (rev-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (rev-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (rev-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

;; 44
(defn rotate [n coll]
  (let [n (mod n (count coll))]
    (concat (drop n coll)
            (take n coll))))

(= (rotate 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (rotate -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (rotate 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (rotate 1 '(:a :b :c)) '(:b :c :a))
(= (rotate -4 '(:a :b :c)) '(:c :a :b))

;; 45
(= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))

;; 46
(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(= 3       ((flip nth) 2 [1 2 3 4 5]))
(= true    ((flip >) 7 8))
(= 4       ((flip quot) 2 8))
(= [1 2 3] ((flip take) [1 2 3 4 5] 3))

;; 47
(contains? #{4 5 6} 4)
(contains? [1 1 1 1 1] 1)
(contains? {4 :a 2 :b} 2)
(not (contains? [1 2 4] 7))

;; 48
(= 6 (some #{7 2 6} [5 6 7 8]))
(= 6 (some #(when (even? %) %) [5 6 7 8]))

;; 49
(defn split [n coll]
  (vector (vec (take n coll)) (vec (drop n coll))))

(= (split 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (split 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (split 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

;; 50
(defn split-by [coll]
  (vals (group-by type coll)))

(= (set (split-by [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (split-by [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (split-by [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

;; 51
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

;; 52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))

;; 53
(defn longest-inc-sub-seq [coll]
  (let [mem (atom [])]
    (letfn [(find-sub-seq [coll acc]
              (cond
                (empty? coll)
                (swap! mem #(conj % acc))

                (> (first coll) (last acc))
                (recur (next coll) (conj acc (first coll)))

                :else
                (do
                  (swap! mem #(conj % acc))
                  (recur (next coll) [(first coll)]))))]
      (reduce #(cond
                 (< (count %2) 2) []
                 (>= (count %1) (count %2)) %1
                 :else %2)
              (find-sub-seq (next coll) [(first coll)])))))

(= (longest-inc-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (longest-inc-sub-seq [5 6 1 3 2 7]) [5 6])
(= (longest-inc-sub-seq [2 3 3 4 5]) [3 4 5])
(= (longest-inc-sub-seq [7 6 5 4]) [])

;; 54
(defn my-partition [n coll]
  (letfn [(part [i coll acc1 acc2]
            (cond
              (= i n)
              (recur 0 coll [] (conj acc2 acc1))

              (empty? coll)
              acc2

              (< i n)
              (recur (inc i) (vec (next coll)) (conj acc1 (first coll)) acc2)))]
    (part 0 coll [] [])))

(= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (my-partition 3 (range 8)) '((0 1 2) (3 4 5)))

;; 55
(defn my-frequencies [coll]
  (apply merge-with + (map #(hash-map % 1) coll)))

(= (my-frequencies [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (my-frequencies [:b :a :b :a :b]) {:a 2, :b 3})
(= (my-frequencies '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

;; 56
(defn my-distinct [coll]
  (let [indexed (map-indexed vector coll)]
    (map second
         (sort-by first
                  (map first
                       (vals
                        (group-by second indexed)))))))

(= (my-distinct [1 2 1 3 1 2 4]) [1 2 3 4])
(= (my-distinct [:a :a :b :b :c :c]) [:a :b :c])
(= (my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (my-distinct (range 50)) (range 50))

;; 57
(= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

;; 58
(defn my-comp [& fns]
  (if-not (empty? fns)
    (let [fn1 (last fns)
          fns (butlast fns)]
      (fn [& args]
        (reduce (fn [acc f] (f acc))
                (apply fn1 args)
                (reverse fns))))
    identity))

(= [3 2 1] ((my-comp rest reverse) [1 2 3 4]))
(= 5       ((my-comp (partial + 3) second) [1 2 3 4]))
(= true    ((my-comp zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

;; 59
(defn my-juxt [& fs]
  (fn [& args]
    (mapv #(apply % args) fs)))

(= [21 6 1]    ((my-juxt + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
(= [2 6 4]     ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; 60
(defn my-reductions
  ([f coll]
   (my-reductions f (first coll) (next coll)))
  ([f val coll]
   (if-not (empty? coll)
     (cons val
           (lazy-seq
            (my-reductions f (f val (first coll)) (next coll))))
     (list val))))

(= (take 5 (my-reductions + (range))) [0 1 3 6 10])
(=         (my-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last   (my-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;; 61
(defn my-zipmap [keys vals]
  (apply merge (map hash-map keys vals)))

(= (my-zipmap [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (my-zipmap [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (my-zipmap [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})

;; 62
(defn my-iterate [f i]
  (cons i
        (lazy-seq
         (my-iterate f (f i)))))

(= (take 5   (my-iterate #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (my-iterate inc 0)) (take 100 (range)))
(= (take 9   (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

;; 63
(defn my-group-by [f coll]
  (apply merge-with into
         (map #(hash-map (f %) [%]) coll)))

(= (my-group-by #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})
(= (my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

;; 64
(= 15 (reduce + [1 2 3 4 5]))
(=  0 (reduce + []))
(=  6 (reduce + 1 [2 3]))

;; 65
(defn coll-type [coll]
  (let [g  (gensym)
        gk (keyword (gensym))
        t  (conj coll [g gk] [gk g])]
    (cond
      (= (get t gk)     g)      :map
      (= (get t [gk g]) [gk g]) :set
      (= (last t)       [gk g]) :vector
      (= (first t)      [gk g]) :list)))

(= :map (coll-type {:a 1, :b 2}))
(= :list (coll-type (range (rand-int 20))))
(= :vector (coll-type [1 2 3 4 5 6]))
(= :set (coll-type #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map coll-type [{} #{} [] ()]))

;; 66
(defn gcd [a b]
  (if (=  b 0)
    a
    (recur b (mod a b))))

(= (gcd 2 4) 2)
(= (gcd 10 5) 5)
(= (gcd 5 7) 1)
(= (gcd 1023 858) 33)

;; 67
(defn primes [n]
  (letfn [(reinsert [composites n prime]
            (let [next-n (+ n prime)]
              (update composites next-n #(conj (or % []) prime))))
          (next-primes [n composites]
            (if-let [primes (get composites n)]
              (recur (inc n)
                     (reduce #(reinsert %1 n %2)
                             (dissoc composites n)
                             primes))
              (cons n
                    (lazy-seq (next-primes (inc n)
                                           (assoc composites (* n n) [n]))))))]
    (take n (next-primes 2 {}))))

(= (primes 2) [2 3])
(= (primes 5) [2 3 5 7 11])
(= (last (primes 100)) 541)

;; 68
(= [7 6 5 4 3]
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

;; 69
(defn my-merge-with [f & ms]
  (reduce
   #(reduce
     (fn [m [k v]]
       (assoc m k
              (if (contains? m k)
                (f (m k) v)
                v)))
     %1 %2)
   ms))

(= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})

;; 70
(defn word-sort [s]
  (sort-by clojure.string/lower-case
           compare
           (clojure.string/split
            (apply str (butlast s))
            #" ")))

(= (word-sort  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (word-sort  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (word-sort  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])

;; 71
(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] reverse rest sort last)
   5)

;; 72
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)

;; 73
(defn winner [board]
  (let [transp     (apply map vector board)
        diagonal   (fn [m]
                     (into []
                           (let [m (into [] m)]
                             (for [pos (range (count m))]
                                 (get-in m [pos pos])))))
        diags       [(diagonal board) (diagonal (reverse transp))]
        find-winner (fn [[e1 e2 e3]]
                        (when (and (not= :e e1) (= e1 e2 e3))
                          e1))
        winner?      (fn [m] (some #(when (not (nil? %)) %)
                                   (mapv find-winner m)))]
    (or (winner? board) (winner? transp) (winner? diags))))

(= nil (winner [[:e :e :e]
                [:e :e :e]
                [:e :e :e]]))
(= :x  (winner [[:x :e :o]
                [:x :e :e]
                [:x :e :o]]))
(= :o  (winner [[:e :x :e]
                [:o :o :o]
                [:x :e :x]]))
(= nil (winner [[:x :e :o]
                [:x :x :e]
                [:o :x :o]]))
(= :x  (winner [[:x :e :e]
                [:o :x :e]
                [:o :e :x]]))
(= :o  (winner [[:x :e :o]
                [:x :o :e]
                [:o :e :x]]))
(= nil (winner [[:x :o :x]
                [:x :o :x]
                [:o :x :o]]))

;; 74
;; This works in Clojure, for Clojurescript see the comments
(defn filter-perfect-squares [s]
  (letfn [(parse [s]
                 (map
                  Integer/parseInt ; Become #(js/parseInt % 10)
                  (clojure.string/split s #",")))
          (test [i]
            (let [sqrt (int (Math/sqrt i))] ; Become js/Math.sqrt
              (= (* sqrt sqrt) i)))]
    (reduce #(str %1 "," %2) (filter test (parse s)))))

(= (filter-perfect-squares "4,5,6,7,8,9") "4,9")
(= (filter-perfect-squares "15,16,25,36,37") "16,25,36")

;; 75
(defn eulers-totient [i]
  (letfn [(gcd [a b]
            (if (=  b 0)
              a
              (recur b (mod a b))))]
    (count (filter #(= (gcd i %) 1) (range i)))))

(= (eulers-totient 1) 1)
(= (eulers-totient 10) (count '(1 3 7 9)) 4)
(= (eulers-totient 40) 16)
(= (eulers-totient 99) 60)

;; 76
(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;; 77
(defn anagram-finder [words]
  (->> words
       (map #(vector (apply str (sort %)) %))
       (group-by first)
       vals
       (map #(set (map second %)))
       (filter #(> (count %) 1))
       set))

(= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; 78
(defn my-trampoline
  ([f] (if (fn? f)
         (recur (f))
         f))
  ([f & args] (my-trampoline (apply f args))))

(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop?(- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
    (my-trampoline triple 2))
  82)
(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
    (map (partial my-trampoline my-even?) (range 6)))
  [true false true false true false])

;; 79
(defn triangle [v]
  (let [r (memoize
            (fn [f vs i]
              (if-let [v (first vs)]
                (let [vs (next vs)
                      val (get v i)]
                  (+ val
                     (if vs
                       (min (f f vs i)
                            (f f vs (inc i)))
                       0)))
                0)))]
    (r r v 0)))

(= (triangle [   [1]
                [2 4]
               [5 1 4]
              [2 3 4 5]])
   (+ 1 2 1 3)
   7)
(= (triangle [     [3]
                  [2 4]
                 [1 9 3]
                [9 9 2 4]
               [4 6 6 7 8]
              [5 7 3 5 1 4]])
   (+ 3 4 3 2 7 1)
   20)

;; 80
(defn perfect-number [n]
  (let [n' (Math/ceil (Math/sqrt n))]
    (letfn [(find-divisors [n i acc]
              (if (<= i n')
                (if (= (mod n i) 0)
                  (recur n (inc i) (conj acc i (/ n i)))
                  (recur n (inc i) acc))
                acc))]
      (= n (reduce + (find-divisors n 2 #{1}))))))

(= (perfect-number 6)    true)
(= (perfect-number 7)    false)
(= (perfect-number 496)  true)
(= (perfect-number 500)  false)
(= (perfect-number 8128) true)

;; 81
(defn my-intersection [s1 & sets]
  (set (filter #(every? (fn [s] (contains? s %)) sets) s1)))

(= (my-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (my-intersection #{0 1 2} #{3 4 5}) #{})
(= (my-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

;; 82

;; (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
;; (= false (__ #{"cot" "hot" "bat" "fat"}))
;; (= false (__ #{"to" "top" "stop" "tops" "toss"}))
;; (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
;; (= true (__ #{"share" "hares" "shares" "hare" "are"}))
;; (= false (__ #{"share" "hares" "hare" "are"}))

;; 83
(defn half-truth [& bs]
  (true?
   (and (not (every? true? bs))
       (some true? bs))))

(= false (half-truth false false))
(= true  (half-truth true false))
(= false (half-truth true))
(= true  (half-truth false true false))
(= false (half-truth true true true))
(= true  (half-truth true true true false))
