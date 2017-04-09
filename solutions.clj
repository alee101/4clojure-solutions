;; Solutions to 4clojure problems with some trivial ones omitted
;; Included multiple solutions for some problems
(require '[clojure.test :refer :all])

;; 19: Last element
(def last-clone (comp first reverse))

(deftest last-clone-test
  (is (= (last-clone [1 2 3 4 5]) 5))
  (is (= (last-clone '(5 4 3)) 3))
  (is (= (last-clone ["b" "c" "d"]) "d")))

;; 20: Penultimate element
(def penultimate (comp second reverse))

(deftest penultimate-test
  (is (= (penultimate (list 1 2 3 4 5)) 4))
  (is (= (penultimate ["a" "b" "c"]) "b"))
  (is (= (penultimate [[1 2] [3 4]]) [1 2])))

;; 21: Nth element
(defn nth-clone [s n]
  ((vec s) n))

(deftest nth-clone-test
  (is (= (nth-clone '(4 5 6 7) 2) 6))
  (is (= (nth-clone [:a :b :c] 0) :a))
  (is (= (nth-clone [1 2 3 4] 1) 2))
  (is (= (nth-clone '([1 2] [3 4] [5 6]) 2) [5 6])))

;; 22: Count a sequence
(defn count-clone [s]
  (reduce + (map (constantly 1) s)))

(defn count-clone [s]
  (reduce (fn [acc _] (inc acc)) 0 s))

(deftest count-clone-test
  (is (= (count-clone '(1 2 3 3 1)) 5))
  (is (= (count-clone "Hello World") 11))
  (is (= (count-clone [[1 2] [3 4] [5 6]]) 3))
  (is (= (count-clone '(13)) 1))
  (is (= (count-clone '(:a :b :c)) 3)))

;; 23: Reverse a sequence
(defn reverse-clone [s]
  (reduce conj '() s))

(defn reverse-clone [s]
  (into '() s))

(deftest reverse-clone-test
  (is (= (reverse-clone [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (reverse-clone (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (reverse-clone [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;; 26: Fibonacci sequence
(defn fib [n]
  (->> (map first (iterate (fn [[prev cur]] [cur (+ prev cur)]) [1 1]))
       (take n)))

(deftest fib-test
  (is (= (fib 3) '(1 1 2)))
  (is (= (fib 6) '(1 1 2 3 5 8)))
  (is (= (fib 8) '(1 1 2 3 5 8 13 21))))

;; 27: Palindrome detector
(defn palindrome? [s]
  (= (seq s) (reverse s)))

(deftest palindrome?-test
  (is (false? (palindrome? '(1 2 3 4 5))))
  (is (true? (palindrome? "racecar")))
  (is (true? (palindrome? [:foo :bar :foo])))
  (is (true? (palindrome? '(1 1 3 3 1 1))))
  (is (false? (palindrome? '(:a :b :c)))))

;; 28: Flatten a sequence
(defn flatten-clone [s]
  (if (sequential? s)
    (mapcat flatten-clone s)
    (vector s)))

(deftest flatten-clone-test
  (is (= (flatten-clone '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten-clone ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (flatten-clone '((((:a))))) '(:a))))

;; 29: Get the caps
(defn get-caps [s]
  (apply str (re-seq #"[A-Z]+" s)))

(deftest get-caps-test
  (is (= (get-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (get-caps "nothing")))
  (is (= (get-caps "$#A(*&987Zf") "AZ")))

;; 30: Compress a sequence
(defn compress-seq [s]
  (->> (partition-by identity s)
       (map first)))

(deftest compress-seq-test
  (is (= (apply str (compress-seq "Leeeeeerrroyyy")) "Leroy"))
  (is (= (compress-seq [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (compress-seq [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

;; 31: Pack a sequence
(def pack-seq (partial partition-by identity))

(deftest pack-seq-test
  (is (= (pack-seq [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (pack-seq [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (pack-seq [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

;; 32: Duplicate a sequence
(defn duplicate-seq [s]
  (mapcat (partial repeat 2) s))

(defn duplicate-seq [s]
  (mapcat #(list % %) s))

(defn duplicate-seq [s]
  (interleave s s))

(deftest duplicate-seq-test
  (is (= (duplicate-seq [1 2 3]) '(1 1 2 2 3 3)))
  (is (= (duplicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

;; 33: Replicate a sequence
(defn replicate-seq [s n]
  (mapcat (partial repeat n) s))

(defn replicate-seq [s n]
  (if (= n 1)
    s
    (apply interleave (repeat n s))))

(deftest replicate-seq-test
  (is (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
  (is (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
  (is (= (replicate-seq [4 5 6] 1) '(4 5 6)))
  (is (= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (replicate-seq [44 33] 2) [44 44 33 33])))

;; 34: Implement range
(defn range-clone [start end]
  (take (- end start) (iterate inc start)))

(deftest range-clone-test
  (is (= (range-clone 1 4) '(1 2 3)))
  (is (= (range-clone -2 2) '(-2 -1 0 1)))
  (is (= (range-clone 5 8) '(5 6 7))))

;; 38: Maximum value
(defn max-clone [& nums]
  (last (sort nums)))

(deftest max-clone-test
  (is (= (max-clone 1 8 3 4) 8))
  (is (= (max-clone 30 20) 30))
  (is (= (max-clone 45 67 11) 67)))

;; 39: Interleave two seqs
(def interleave-clone (partial mapcat list))

(deftest interleave-clone-test
  (is (= (interleave-clone [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (is (= (interleave-clone [1 2] [3 4 5 6]) '(1 3 2 4)))
  (is (= (interleave-clone [1 2 3 4] [5]) [1 5]))
  (is (= (interleave-clone [30 20] [25 15]) [30 25 20 15])))

;; 40: Interpose a seq
(defn interpose-clone [sep s]
  (butlast (interleave s (repeat sep))))

(defn interpose-clone [sep s]
  (next (interleave (repeat sep) s)))

(defn interpose-clone [sep s]
  (butlast (mapcat #(list % sep) s)))

(deftest interpose-clone-test
  (is (= (interpose-clone 0 [1 2 3]) [1 0 2 0 3]))
  (is (= (apply str (interpose-clone ", " ["one" "two" "three"])) "one, two, three"))
  (is (= (interpose-clone :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

;; 41: Drop every nth item
(defn drop-every-nth [s n]
  (keep-indexed (fn [idx ele] (if (pos? (mod (inc idx) n)) ele)) s))

(defn drop-every-nth [s n]
  (apply concat (partition-all (dec n) n s)))

(deftest drop-every-nth-test
  (is (= (drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e]))
  (is (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])))

;; 42: Factorial fun
(defn factorial [n]
  (reduce * (range 1 (inc n))))

(deftest factorial-test
  (is (= (factorial 1) 1))
  (is (= (factorial 3) 6))
  (is (= (factorial 5) 120))
  (is (= (factorial 8) 40320)))

;; 43: Reverse interleave
(defn reverse-interleave [s n]
  (->> (partition n s)
       (apply map list)))

(deftest reverse-interleave-test
  (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

;; 44: Rotate sequence
(defn rotate-seq [n coll]
  (let [len (count coll)
        n (mod n len)]
    (->> (cycle coll)
         (drop n)
         (take len))))

(defn rotate-seq [n coll]
  (let [n (mod n (count coll))]
    (concat (drop n coll) (take n coll))))

(defn rotate-seq [n coll]
  (let [n (mod n (count coll))]
    (->> (split-at n coll)
         reverse
         (apply concat))))

(deftest rotate-seq-test
  (is (= (rotate-seq 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-seq -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-seq 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-seq 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-seq -4 '(:a :b :c)) '(:c :a :b))))

;; 46: Flipping out
(defn flipping-out [f]
  (fn [& args]
    (apply f (reverse args))))

(deftest flipping-out-test
  (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
  (is (= true ((flipping-out >) 7 8)))
  (is (= 4 ((flipping-out quot) 2 8)))
  (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))))

;; 49: Split a sequence
(defn split-at-clone [n coll]
  [(take n coll) (drop n coll)])

(def split-at-clone (juxt take drop))

(deftest split-at-clone-test
  (is (= (split-at-clone 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split-at-clone 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split-at-clone 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

;; 50: Split by type
(defn split-by-type [s]
  (vals (group-by type s)))

(deftest split-by-type-test
  (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

;; 53: Longest increasing sub-seq
;; Note: peek is more efficient than last for vector
(defn longest-inc-sub-seq [[h & t]]
  (loop [longest [], cur [h], s t]
    (let [[h & t] s
          longest (if (>= (count longest) (count cur)) longest cur)
          cur (if (= h (inc (peek cur))) (conj cur h) [h])]
      (if (empty? s)
        (if (>= (count longest) 2) longest [])
        (recur longest cur t)))))

(defn longest-inc-sub-seq [s]
  (let [is-inc? (fn [[a b]] (= (inc a) b))]
    (->> (partition 2 1 s)
         (partition-by is-inc?)
         (filter #(every? is-inc? %))
         (map #(concat (map first %) (list (last (last %)))))
         (#(if (empty? %) [] (apply max-key count %))))))

(deftest longest-inc-sub-seq-test
  (is (= (longest-inc-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3]))
  (is (= (longest-inc-sub-seq [5 6 1 3 2 7]) [5 6]))
  (is (= (longest-inc-sub-seq [2 3 3 4 5]) [3 4 5]))
  (is (= (longest-inc-sub-seq [7 6 5 4]) [])))

;; 54: Partition a sequence
(defn partition-clone [n coll]
  (loop [coll, coll, partitions []]
    (if (< (count coll) n)
      partitions
      (recur (drop n coll) (conj partitions (take n coll))))))

(deftest partition-clone-test
  (is (= (partition-clone 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (partition-clone 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (is (= (partition-clone 3 (range 8)) '((0 1 2) (3 4 5)))))

;; 55: Count occurrences
(defn frequencies-clone [coll]
  (->> (map #(hash-map % 1) coll)
       (apply merge-with +)))

(defn frequencies-clone [coll]
  (reduce (fn [counts x]
            (assoc counts x (inc (get counts x 0))))
          {} coll))

(deftest frequencies-clone-test
  (is (= (frequencies-clone [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (frequencies-clone [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (frequencies-clone '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

;; 56: Find distinct items
(defn distinct-clone [coll]
  (reduce (fn [distincts x]
            (if ((set distincts) x) distincts (conj distincts x))) [] coll))

(deftest distinct-clone-test
  (is (= (distinct-clone [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (distinct-clone [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (distinct-clone '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (distinct-clone (range 50)) (range 50))))

;; 58: Function composition
(defn comp-clone [& fns]
  (reduce (fn [g f]
            (fn [& args] (g (apply f args)))) fns))

(deftest comp-clone-test
  (is (= [3 2 1] ((comp-clone rest reverse) [1 2 3 4])))
  (is (= 5 ((comp-clone (partial + 3) second) [1 2 3 4])))
  (is (= true ((comp-clone zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((comp-clone #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

;; 59: Juxtaposition
(defn juxt-clone [& fns]
  (fn [& args]
    (map (fn [f] (apply f args)) fns)))

(defn juxt-clone [& fns]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fns)))

(deftest juxt-clone-test
  (is (= [21 6 1] ((juxt-clone + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxt-clone #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxt-clone :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

;; 60: Sequence reductions
(defn reductions-clone
  ([f coll]
   (if-let [[h & t] (seq coll)]
     (reductions-clone f h t)
     (list (f))))
  ([f init coll]
   (cons init
         (lazy-seq
          (when-let [[h & t] (seq coll)]
            (reductions-clone f (f init h) t))))))

(deftest reductions-clone-test
  (is (= (take 5 (reductions-clone + (range))) [0 1 3 6 10]))
  (is (= (reductions-clone conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (reductions-clone * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))
  (is (= (reductions-clone + []) '(0))))

;; 61: Map construction
(defn zipmap-clone [ks vs]
  (apply hash-map (interleave ks vs)))

(defn zipmap-clone [ks vs]
  (into {} (map vector ks vs)))

(defn zipmap-clone [keys vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc map (first ks) (first vs))
             (next ks)
             (next vs))
      map)))

(deftest zipmap-clone-test
  (is (= (zipmap-clone [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (zipmap-clone [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (zipmap-clone [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

;; 62: Re-implement iterate
(defn iterate-clone [f x]
  (cons x (lazy-seq (iterate-clone f (f x)))))

(deftest iterate-clone-test
  (is (= (take 5 (iterate-clone #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (iterate-clone inc 0)) (take 100 (range))))
  (is (= (take 9 (iterate-clone #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

;; 63: Group a sequence
(defn group-by-clone [f coll]
  (apply merge-with concat (map #(hash-map (f %) [%]) coll)))

(defn group-by-clone [f coll]
  (reduce (fn [groups x]
            (let [k (f x)]
              (assoc groups k (conj (get groups k []) x))))
          {} coll))

(deftest group-by-clone-test
  (is (= (group-by-clone #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-by-clone #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-by-clone count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

;; 65: Black-box testing
(def black-box-testing (comp {\# :set \{ :map \[ :vector \( :list} first str))

(defn black-box-testing [coll]
  (let [empty-coll (empty coll)]
    (cond
      (= {} empty-coll) :map
      (= #{} empty-coll) :set
      (= [1 2] (conj empty-coll 1 2)) :vector
      :else :list)))

(deftest black-box-testing-test
  (is (= :map (black-box-testing {:a 1, :b 2})))
  (is (= :list (black-box-testing (range (rand-int 20)))))
  (is (= :vector (black-box-testing [1 2 3 4 5 6])))
  (is (= :set (black-box-testing #{10 (rand-int 5)})))
  (is (= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))))

;; 66: Greatest common divisor
(defn gcd [x y]
  (if (zero? y) x (recur y (mod x y))))

(deftest gcd-test
  (is (= (gcd 2 4) 2))
  (is (= (gcd 10 5) 5))
  (is (= (gcd 5 7) 1))
  (is (= (gcd 1023 858) 33)))

;; 67: Prime numbers
(defn primes [n]
  (let [prime? (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))]
    (take n (filter prime? (iterate inc 2)))))

(defn primes [n]
  (letfn [(sieve [s] (cons (first s) (lazy-seq (sieve (remove #(zero? (mod % (first s))) (rest s))))))]
    (take n (sieve (iterate inc 2)))))

(deftest primes-test
  (is (= (primes 2) [2 3]))
  (is (= (primes 5) [2 3 5 7 11]))
  (is (= (last (primes 100)) 541)))

;; 69: Merge with a function
(defn merge-with-clone [f m & maps]
  (reduce (fn [acc [k v]]
            (assoc acc k (if (acc k) (f (acc k) v) v)))
          m (apply concat (map vec maps))))

(deftest merge-with-clone-test
  (is (= (merge-with-clone * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
         {:a 4, :b 6, :c 20}))
  (is (= (merge-with-clone - {1 10, 2 20} {1 3, 2 10, 3 15})
         {1 7, 2 10, 3 15}))
  (is (= (merge-with-clone concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
         {:a [3 4 5], :b [6 7], :c [8 9]})))

;; 70: Word sorting
(defn word-sorting [s]
  (->> (clojure.string/split s #"[^A-Za-z]")
       (sort-by clojure.string/lower-case)))

(deftest word-sorting-test
  (is (= (word-sorting  "Have a nice day.")
         ["a" "day" "Have" "nice"]))
  (is (= (word-sorting  "Clojure is a fun language!")
         ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting  "Fools fall for foolish follies.")
         ["fall" "follies" "foolish" "Fools" "for"])))

;; 73: Analyze a tic-tac-toe board
(defn tic-tac-toe-winner [board]
  (let [winner? (fn [s] (if (apply = s) (first s) nil))
        row-winner? (map winner? board)
        col-winner? (map winner? (apply map vector board))
        lft-diag-winner? (winner? (map get board (range 3)))
        rgt-diag-winner? (winner? (map get board (range 2 -1 -1)))]
    (some #{:x :o} (or (concat row-winner? col-winner? [lft-diag-winner? rgt-diag-winner?])))))

(deftest tic-tac-toe-winner-test
  (is (= nil (tic-tac-toe-winner [[:e :e :e]
                                  [:e :e :e]
                                  [:e :e :e]])))
  (is (= :x (tic-tac-toe-winner [[:x :e :o]
                                 [:x :e :e]
                                 [:x :e :o]])))
  (is (= :o (tic-tac-toe-winner [[:e :x :e]
                                 [:o :o :o]
                                 [:x :e :x]])))
  (is (= nil (tic-tac-toe-winner [[:x :e :o]
                                  [:x :x :e]
                                  [:o :x :o]])))
  (is (= :x (tic-tac-toe-winner [[:x :e :e]
                                 [:o :x :e]
                                 [:o :e :x]])))
  (is (= :o (tic-tac-toe-winner [[:x :e :o]
                                 [:x :o :e]
                                 [:o :e :x]])))
  (is (= nil (tic-tac-toe-winner [[:x :o :x]
                                  [:x :o :x]
                                  [:o :x :o]]))))

;; 74: Filter perfect squares
(defn filter-perfect-squares [s]
  (let [perfect-square? (fn [n]
                          (let [sqrt-n (Math/sqrt n)]
                            (== n (* sqrt-n sqrt-n))))]
    (->> (clojure.string/split s #",")
         (map #(Integer/parseInt %))
         (filter perfect-square?)
         (clojure.string/join ","))))

(deftest filter-perfect-squares-test
  (is (= (filter-perfect-squares "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

;; 75: Euler's totient function
(defn eulers-totient-fn [n]
  (let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))]
    (if (= n 1)
      1
      (count (filter #(= (gcd n %) 1) (range 1 n))))))

(deftest eulers-totient-fn-test
  (is (= (eulers-totient-fn 1) 1))
  (is (= (eulers-totient-fn 10) (count '(1 3 7 9)) 4))
  (is (= (eulers-totient-fn 40) 16))
  (is (= (eulers-totient-fn 99) 60)))

;; 77: Anagram finder
(defn anagrams [words]
  (->> (group-by set words)
       vals
       (filter next)
       (map set)
       set))

(deftest anagrams-test
  (is (= (anagrams ["meat" "mat" "team" "mate" "eat"])
         #{#{"meat" "team" "mate"}}))
  (is (= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
         #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

;; 78: Reimplement trampoline
(defn trampoline-clone
  ([f] (if (fn? f) (recur (f)) f))
  ([f & args] (trampoline-clone (apply f args))))

(defn trampoline-clone
  ([f]
   (let [ret (f)]
     (if (fn? ret)
       (recur ret)
       ret)))
   ([f & args]
    (trampoline-clone #(apply f args))))

(deftest trampoline-clone-test
  (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                 (sub-two [x] #(stop?(- x 2)))
                 (stop? [x] (if (> x 50) x #(triple x)))]
           (trampoline-clone triple 2))
         82))
  (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                 (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
           (map (partial trampoline-clone my-even?) (range 6)))
         [true false true false true false])))

;; 79: Triangle minimal path
(defn triangle-min-path [t]
  (if (empty? t)
    0
    (+ (ffirst t)
       (min
        (triangle-min-path (map butlast (next t)))
        (triangle-min-path (map next (next t)))))))

;; More efficient bottom-up approach
(defn triangle-min-path [t]
  (let [pairwise-mins (fn [s] (map #(apply min %) (partition 2 1 s)))]
    (first
     (reduce (fn [cur-sums cur-row]
               (map + (pairwise-mins cur-sums) cur-row)) (reverse t)))))

(deftest trinagle-min-path-test
  (is (= 7 (triangle-min-path '([1]
                                [2 4]
                                [5 1 4]
                                [2 3 4 5]))))
  (is (= 20 (triangle-min-path '([3]
                                 [2 4]
                                 [1 9 3]
                                 [9 9 2 4]
                                 [4 6 6 7 8]
                                 [5 7 3 5 1 4])))))

;; 80: Perfect numbers
(defn perfect-number? [n]
  (let [divisors (filter #(zero? (mod n %)) (range 1 n))]
    (= n (reduce + divisors))))

(deftest perfect-number?-test
  (is (= (perfect-number? 6) true))
  (is (= (perfect-number? 7) false))
  (is (= (perfect-number? 496) true))
  (is (= (perfect-number? 500) false))
  (is (= (perfect-number? 8128) true)))

;; 81: Set intersection
(defn intersection-clone [s1 s2]
  (set (filter s1 s2)))

(deftest intersection-clone-test
  (is (= (intersection-clone #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (is (= (intersection-clone #{0 1 2} #{3 4 5}) #{}))
  (is (= (intersection-clone #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

;; 82: Word chains
(defn word-chain? [ws]
  (let [has-edge? (fn has-edge? [w1 w2]
                    (cond
                      (and (empty? w1) (empty? w2)) true
                      (= (first w1) (first w2)) (has-edge? (next w1) (next w2))
                      (= (next w1) (seq w2)) true
                      (= (seq w1) (next w2)) true
                      (= (next w1) (next w2)) true
                      :else false))
        neighbors (fn [w] (filter (partial has-edge? w) ws))
        graph (into {} (map #(vector % (neighbors %)) ws))
        has-chain? (fn has-chain? [visited root]
                     (let [visited (conj visited root)
                           children (remove visited (graph root))]
                       (if (empty? children)
                         (= ws visited)
                         (some (partial has-chain? visited) children))))]
    (boolean (some (partial has-chain? #{}) ws))))

(deftest word-chain?-test
  (is (= true (word-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
  (is (= false (word-chain? #{"cot" "hot" "bat" "fat"})))
  (is (= false (word-chain? #{"to" "top" "stop" "tops" "toss"})))
  (is (= true (word-chain? #{"spout" "do" "pot" "pout" "spot" "dot"})))
  (is (= true (word-chain? #{"share" "hares" "shares" "hare" "are"})))
  (is (= false (word-chain? #{"share" "hares" "hare" "are"}))))

;; 83: A half-truth
(defn half-truth? [& bools]
  (and (not-every? true? bools) (not-every? false? bools)))

(defn half-truth? [& bools]
  (boolean (and (some true? bools) (not-every? true? bools))))

(deftest half-truth?-test
  (is (= false (half-truth? false false)))
  (is (= true (half-truth? true false)))
  (is (= false (half-truth? true)))
  (is (= true (half-truth? false true false)))
  (is (= false (half-truth? true true true)))
  (is (= true (half-truth? true true true false))))

;; 84: Transitive closure
(defn transitive-closure [relations]
  (let [new-relations (for [[a x] relations [y b] relations :when (= x y)] [a b])
        updated-relations (into relations new-relations)]
    (if (= updated-relations relations)
      updated-relations
      (recur updated-relations))))

(deftest transitive-closure-test
  (is (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
        (= (transitive-closure divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))
  (is (let [more-legs
            #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
        (= (transitive-closure more-legs)
           #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
             ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))
  (is (let [progeny
            #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
        (= (transitive-closure progeny)
           #{["father" "son"] ["father" "grandson"]
             ["uncle" "cousin"] ["son" "grandson"]}))))

;; 85: Power set
(defn power-set [s]
  (reduce (fn [ps ele] (into ps (map #(conj % ele) ps))) #{#{}} s))

(deftest power-set-test
  (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
  (is (= (power-set #{}) #{#{}}))
  (is (= (power-set #{1 2 3})
         #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
  (is (= (count (power-set (into #{} (range 10)))) 1024)))

;; 86: Happy numbers
(defn happy-number? [n]
  (let [sum-square-digits (fn [n] (->> (map (comp read-string str) (str n))
                                       (map #(* % %))
                                       (reduce +)))]
    (loop [n n, seen #{}]
      (cond
        (= n 1) true
        (seen n) false
        :else (recur (sum-square-digits n) (conj seen n))))))

(deftest happy-number?-test
  (is (= (happy-number? 7) true))
  (is (= (happy-number? 986543210) true))
  (is (= (happy-number? 2) false))
  (is (= (happy-number? 3) false)))

;; 88: Symmetric difference
(defn symmetric-difference [s1 s2]
  (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))

(deftest symmetric-difference-test
  (is (= (symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
  (is (= (symmetric-difference #{:a :b :c} #{}) #{:a :b :c}))
  (is (= (symmetric-difference #{} #{4 5 6}) #{4 5 6}))
  (is (= (symmetric-difference #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

;; 90: Cartesian product
(defn cartesian-product [s1 s2]
  (set (for [x s1, y s2] [x y])))

(deftest cartesian-product-test
  (is (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
         #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
           ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
           ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
  (is (= (cartesian-product #{1 2 3} #{4 5})
         #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
  (is (= 300 (count (cartesian-product (into #{} (range 10))
                                       (into #{} (range 30)))))))

;; 91: Graph connectivity
(defn connected-graph? [edges]
  (let [in-component? (fn [component edge] (some #(contains? component %) edge))
        containing-components (fn [cs edge] (filter #(in-component? % edge) cs))
        components (reduce (fn [cs edge]
                             (if-let [ccs (not-empty (containing-components cs edge))]
                               (-> (apply disj cs ccs)
                                   (conj (apply clojure.set/union (cons (set edge) ccs))))
                               (conj cs (set edge))))
                           #{} edges)]
    (= 1 (count components))))

(deftest connected-graph?-test
  (is (= true (connected-graph? #{[:a :a]})))
  (is (= true (connected-graph? #{[:a :b]})))
  (is (= false (connected-graph? #{[1 2] [2 3] [3 1]
                                     [4 5] [5 6] [6 4]})))
  (is (= true (connected-graph? #{[1 2] [2 3] [3 1]
                                    [4 5] [5 6] [6 4] [3 4]})))
  (is (= false (connected-graph? #{[:a :b] [:b :c] [:c :d]
                                     [:x :y] [:d :a] [:b :e]})))
  (is (= true (connected-graph? #{[:a :b] [:b :c] [:c :d]
                                    [:x :y] [:d :a] [:b :e] [:x :a]}))))

;; 89: Graph tour
(defn graph-tour [edges]
  (let [nodes (set (apply concat edges))
        node-degrees (frequencies (apply concat (map set edges)))
        odd-degrees (count (filter odd? (vals node-degrees)))]
    (and (connected-graph? edges) (or (= odd-degrees 0) (= odd-degrees 2)))))

(deftest graph-tour-test
  (is (= true (graph-tour [[:a :b]])))
  (is (= false (graph-tour [[:a :a] [:b :b]])))
  (is (= false (graph-tour [[:a :b] [:a :b] [:a :c] [:c :a]
                            [:a :d] [:b :d] [:c :d]])))
  (is (= true (graph-tour [[1 2] [2 3] [3 4] [4 1]])))
  (is (= true (graph-tour [[:a :b] [:a :c] [:c :b] [:a :e]
                           [:b :e] [:a :d] [:b :d] [:c :e]
                           [:d :e] [:c :f] [:d :f]])))
  (is (= false (graph-tour [[1 2] [2 3] [2 4] [2 5]]))))

;; 92: Read roman numerals
(defn read-roman-numerals [rn]
  (let [numerals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        pairs (partition 2 1 (concat (map numerals rn) [0]))]
    (reduce (fn [acc [a b]] ((if (>= a b) + -) acc a)) 0 pairs)))

(deftest read-roman-numerals-test
  (is (= 14 (read-roman-numerals "XIV")))
  (is (= 827 (read-roman-numerals "DCCCXXVII")))
  (is (= 3999 (read-roman-numerals "MMMCMXCIX")))
  (is (= 48 (read-roman-numerals "XLVIII"))))

;; 93: Partially flatten a sequence
(defn partial-flatten [s]
  (if (every? sequential? s)
    (mapcat partial-flatten s)
    [s]))

(deftest partial-flatten-test
  (is (= (partial-flatten [["Do"] ["Nothing"]])
         [["Do"] ["Nothing"]]))
  (is (= (partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]])
         [[:a :b] [:c :d] [:e :f]]))
  (is (= (partial-flatten '((1 2)((3 4)((((5 6)))))))
         '((1 2)(3 4)(5 6)))))

;; 94: Game of life
(defn game-of-life [board]
  (let [rows (count board)
        cols (count (first board))
        cells (for [i (range rows) j (range cols)] [i j])
        neighbors (fn [[i j]] (for [di [-1 0 1]
                                    dj [-1 0 1]
                                    :when (not= di dj 0)] [(+ i di) (+ j dj)]))
        live-cell? (fn [cell] (= (get-in board cell) \#))
        live-neighbors (fn [cell] (count (filter live-cell? (neighbors cell))))
        next-state (fn [cell]
                     (let [ln (live-neighbors cell)]
                       (if (live-cell? cell)
                         (if (or (= ln 2) (= ln 3)) \# \space)
                         (if (= ln 3) \# \space))))]
    (map #(apply str %) (partition cols (map next-state cells)))))

(deftest game-of-life-test
  (is (= (game-of-life ["      "
                        " ##   "
                        " ##   "
                        "   ## "
                        "   ## "
                        "      "])
         ["      "
          " ##   "
          " #    "
          "    # "
          "   ## "
          "      "]))
  (is (= (game-of-life ["     "
                        "     "
                        " ### "
                        "     "
                        "     "])
         ["     "
          "  #  "
          "  #  "
          "  #  "
          "     "]))
  (is (= (game-of-life ["      "
                        "      "
                        "  ### "
                        " ###  "
                        "      "
                        "      "])
         ["      "
          "   #  "
          " #  # "
          " #  # "
          "  #   "
          "      "])))

;; 95: To tree, or not to tree
(defn tree? [t]
  (if (sequential? t)
    (and (= (count t) 3) (tree? (second t)) (tree? (last t)))
    (nil? t)))

(deftest tree?-test
  (is (= (tree? '(:a (:b nil nil) nil))
         true))
  (is (= (tree? '(:a (:b nil nil)))
         false))
  (is (= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
         true))
  (is (= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
         false))
  (is (= (tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
         true))
  (is (= (tree? [1 [2 [3 [4 false nil] nil] nil] nil])
         false))
  (is (= (tree? '(:a nil ()))
         false)))

;; 96: Beauty is symmetry
(defn symmetric?
  ([[_ lft rgt]] (symmetric? lft rgt))
  ([[lnode llft lrgt :as lft] [rnode rlft rrgt :as rgt]]
   (or (= nil lft rgt)
       (and (= lnode rnode) (symmetric? llft rrgt) (symmetric? lrgt rlft)))))

(defn symmetric? [t]
  (letfn [(flip-tree [[node lft rgt :as t]]
            (if (sequential? t)
              (list node (flip-tree rgt) (flip-tree lft))
              t))]
    (= t (flip-tree t))))

(deftest symmetric?-test
  (is (= (symmetric? '(:a (:b nil nil) (:b nil nil))) true))
  (is (= (symmetric? '(:a (:b nil nil) nil)) false))
  (is (= (symmetric? '(:a (:b nil nil) (:c nil nil))) false))
  (is (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
         true))
  (is (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
         false))
  (is (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [6 nil nil] nil]] nil]])
         false)))

;; 97: Pascal's triangle
(defn pascals-triangle [n]
  (if (= n 1)
    [1]
    (concat [1] (map #(apply + %) (partition 2 1 (pascals-triangle (dec n)))) [1])))

(defn pascals-triangle [n]
  (nth
   (iterate #(concat [1] (map + % (next %)) [1]) [1])
   (dec n)))

(defn pascals-triangle [n]
  (nth
   (iterate #(map + (concat [0] %) (concat % [0])) [1])
   (dec n)))

(deftest pascals-triangle-test
  (is (= (pascals-triangle 1) [1]))
  (is (= (map pascals-triangle (range 1 6))
         [[1]
          [1 1]
          [1 2 1]
          [1 3 3 1]
          [1 4 6 4 1]]))
  (is (= (pascals-triangle 11)
         [1 10 45 120 210 252 210 120 45 10 1])))

;; 98: Equivalence classes
(defn equivalence-classes [f D]
  (set (map set (vals (group-by f D)))))

(deftest equivalence-classes-test
  (= (equivalence-classes #(* % %) #{-2 -1 0 1 2})
     #{#{0} #{1 -1} #{2 -2}})
  (= (equivalence-classes #(rem % 3) #{0 1 2 3 4 5 })
     #{#{0 3} #{1 4} #{2 5}})
  (= (equivalence-classes identity #{0 1 2 3 4})
     #{#{0} #{1} #{2} #{3} #{4}})
  (= (equivalence-classes (constantly true) #{0 1 2 3 4})
     #{#{0 1 2 3 4}}))

;; 99: Product digits
(defn product-digits [x y]
  (map #(Integer/parseInt (str %)) (str (* x y))))

(defn product-digits [x y]
  (map #(- (int %) (int \0)) (str (* x y))))

(defn product-digits [x y]
  (map (comp read-string str) (str (* x y))))

(deftest product-digits-test
  (is (= (product-digits 1 1) [1]))
  (is (= (product-digits 99 9) [8 9 1]))
  (is (= (product-digits 999 99) [9 8 9 0 1])))

;; 100: Least common multiple
(defn lcm [& nums]
  (let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))]
    (/ (reduce * nums) (reduce gcd nums))))

(deftest lcm-test
  (is (== (lcm 2 3) 6))
  (is (== (lcm 5 3 7) 105))
  (is (== (lcm 1/3 2/5) 2))
  (is (== (lcm 3/4 1/6) 3/2))
  (is (== (lcm 7 5/7 2 3/5) 210)))

;; 101: Levenshtein distance
(def lev-dist
  (memoize
   (fn [w1 w2]
     (cond
       (zero? (count w1)) (count w2)
       (zero? (count w2)) (count w1)
       (= (first w1) (first w2)) (lev-dist (next w1) (next w2))
       :else (inc (min
                   (lev-dist w1 (next w2))
                   (lev-dist (next w1) w2)
                   (lev-dist (next w1) (next w2))))))))

(deftest lev-dist-test
  (is (= (lev-dist "kitten" "sitting") 3))
  (is (= (lev-dist "closure" "clojure") (lev-dist "clojure" "closure") 1))
  (is (= (lev-dist "xyx" "xyyyx") 2))
  (is (= (lev-dist "" "123456") 6))
  (is (= (lev-dist "Clojure" "Clojure") (lev-dist "" "") (lev-dist [] []) 0))
  (is (= (lev-dist [1 2 3 4] [0 2 3 4 5]) 2))
  (is (= (lev-dist '(:a :b :c :d) '(:a :d)) 2))
  (is (= (lev-dist "ttttattttctg" "tcaaccctaccat") 10))
  (is (= (lev-dist "gaattctaatctc" "caaacaaaaaattt") 9)))

;; 102: intoCamelCase
(defn into-camel-case [s]
  (let [tokens (clojure.string/split s #"-")]
    (clojure.string/join "" (cons (first tokens) (map clojure.string/capitalize (next tokens))))))

(defn into-camel-case [s]
  (clojure.string/replace s #"-\w" #(clojure.string/capitalize (last %))))

(deftest into-camel-case-test
  (= (into-camel-case "something") "something")
  (= (into-camel-case "multi-word-key") "multiWordKey")
  (= (into-camel-case "leaveMeAlone") "leaveMeAlone"))

;; 103: Generating k-combinations
(defn k-combinations [k s]
  (set
   (if (= k 1)
     (map hash-set s)
     (let [combs (k-combinations (dec k) s)]
       (for [ele s, comb combs :when (not (comb ele))]
         (conj comb ele))))))

(defn k-combinations [k s]
  (set (filter #(= k (count %)) (power-set s))))

(deftest k-combinations-test
  (is (= (k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (is (= (k-combinations 10 #{4 5 6}) #{}))
  (is (= (k-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
  (is (= (k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                           #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
  (is (= (k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
  (is (= (k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                        #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

;; 104: Write roman numerals
(defn write-roman-numerals [n]
  (let [numerals (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")
        num (some #(when (>= n %) %) (keys numerals))]
    (when-not (nil? num)
      (str (numerals num) (write-roman-numerals (- n num))))))

(deftest write-roman-numerals-test
  (is (= "I" (write-roman-numerals 1)))
  (is (= "XXX" (write-roman-numerals 30)))
  (is (= "IV" (write-roman-numerals 4)))
  (is (= "CXL" (write-roman-numerals 140)))
  (is (= "DCCCXXVII" (write-roman-numerals 827)))
  (is (= "MMMCMXCIX" (write-roman-numerals 3999)))
  (is (= "XLVIII" (write-roman-numerals 48))))

;; 105: Identify keys and values
(defn identify-keys-vals [coll]
  (if-let [[k & t] (seq coll)]
    (let [[vs s] (split-with (complement keyword?) t)]
      (conj (identify-keys-vals s) {k vs}))
    {}))

(deftest identify-keys-vals-test
  (is (= {} (identify-keys-vals [])))
  (is (= {:a [1]} (identify-keys-vals [:a 1])))
  (is (= {:a [1], :b [2]} (identify-keys-vals [:a 1, :b 2])))
  (is (= {:a [1 2 3], :b [], :c [4]} (identify-keys-vals [:a 1 2 3 :b :c 4]))))

;; 106: Number maze
(defn number-maze [start end]
  (let [children (fn [n] (conj (when (even? n) (list (/ n 2))) (* n 2) (+ n 2)))]
    (loop [nodes [start]
           depth 1]
      (if (some #{end} nodes)
        depth
        (recur (mapcat children nodes) (inc depth))))))

(deftest number-maze-test
  (is (= 1 (number-maze 1 1)))   ; 1
  (is (= 3 (number-maze 3 12)))  ; 3 6 12
  (is (= 3 (number-maze 12 3)))  ; 12 6 3
  (is (= 3 (number-maze 5 9)))   ; 5 7 9
  (is (= 9 (number-maze 9 2)))   ; 9 18 20 10 12 6 8 4 2
  (is (= 5 (number-maze 9 12)))  ; 9 11 22 24 12
)

;; 108: Lazy searching
(defn lazy-searching [& colls]
  (let [cur-min (apply min (map first colls))]
    (if (every? (partial = cur-min) (map first colls))
      cur-min
      (apply lazy-searching (map #(drop-while (partial = cur-min) %) colls)))))

(defn lazy-searching [& colls]
  (if (apply = (map first colls))
    (ffirst colls)
    (let [cur-min (apply min (map first colls))]
      (recur (map #(drop-while (partial = cur-min) %) colls)))))

(deftest lazy-searching-test
  (is (= 3 (lazy-searching [3 4 5])))
  (is (= 4 (lazy-searching [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
  (is (= 7 (lazy-searching (range) (range 0 100 7/6) [2 3 5 7 11 13])))
  (is (= 64 (lazy-searching (map #(* % % %) (range)) ;; perfect cubes
                            (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                            (iterate inc 20)))) ;; at least as large as 20
  )

;; 110: Sequence of pronunciations
(defn pronunciations [coll]
  (let [pronunciation (fn [coll] (->> (partition-by identity coll)
                                      (mapcat (juxt count first))))]
    (rest (iterate pronunciation coll))))

(deftest pronunciations-test
  (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciations [1]))))
  (is (= [3 1 2 4] (first (pronunciations [1 1 1 4 4]))))
  (is (= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciations [1]) 6)))
  (is (= 338 (count (nth (pronunciations [3 2]) 15)))))

;; 111: Crossword puzzle
(defn valid-crossword? [s puzzle]
  (->> (concat puzzle (apply map str puzzle))
       (map #(clojure.string/replace % #" |_|#" {" " "", "_" ".", "#" "|"}))
       (some #(re-matches (re-pattern %) s))
       boolean))

(deftest valid-crossword?-test
  (is (= true (valid-crossword? "the" ["_ # _ _ e"])))
  (is (= false (valid-crossword? "the" ["c _ _ _"
                                        "d _ # e"
                                        "r y _ _"])))
  (is (= true (valid-crossword? "joy" ["c _ _ _"
                                       "d _ # e"
                                       "r y _ _"])))
  (is (= false (valid-crossword? "joy" ["c o n j"
                                        "_ _ y _"
                                        "r _ _ #"])))
  (is (= true (valid-crossword? "clojure" ["_ _ _ # j o y"
                                           "_ _ o _ _ _ _"
                                           "_ _ f _ # _ _"]))))

;; 112: Sequs horribilis
(defn sequs-horribilis [n [h & t]]
  (cond
    (nil? h) '()
    (not (sequential? h)) (if (> h n)
                            '()
                            (cons h (sequs-horribilis (- n h) t)))
    :else (list (sequs-horribilis n (concat h t)))))

(defn sequs-horribilis [n [h & t]]
  (cond
    (nil? h) '()
    (sequential? h) (list (sequs-horribilis n (concat h t)))
    (> h n) '()
    :else (cons h (sequs-horribilis (- n h) t))))

(deftest sequs-horribilis-test
  (is (= (sequs-horribilis 10 [1 2 [3 [4 5] 6] 7])
         '(1 2 (3 (4)))))
  (is (= (sequs-horribilis 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
         '(1 2 (3 (4 (5 (6 (7))))))))
  (is (= (sequs-horribilis 9 (range))
         '(0 1 2 3)))
  (is (= (sequs-horribilis 1 [[[[[1]]]]])
         '(((((1)))))))
  (is (= (sequs-horribilis 0 [1 2 [3 [4 5] 6] 7])
         '()))
  (is (= (sequs-horribilis 0 [0 0 [0 [0]]])
         '(0 0 (0 (0)))))
  (is (= (sequs-horribilis 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
         '(-10 (1 (2 3 (4)))))))

;; 113: Making data dance
(defn making-data-dance [& args]
  (when args
    (reify clojure.lang.ISeq
      (seq [_] (distinct args))
      (toString [_] (apply str (interpose ", " (sort args)))))))

(deftest making-data-dance-test
  (= "1, 2, 3" (str (making-data-dance 2 1 3)))
  (= '(2 1 3) (seq (making-data-dance 2 1 3)))
  (= '(2 1 3) (seq (making-data-dance 2 1 3 3 1 2)))
  (= '(1) (seq (apply making-data-dance (repeat 5 1))))
  (= "1, 1, 1, 1, 1" (str (apply making-data-dance (repeat 5 1))))
  (and (= nil (seq (making-data-dance)))
       (=  "" (str (making-data-dance)))))

;; 114: Global take-while
(defn global-take-while [n p [h & t]]
  (when-not (or (zero? n) (and (= 1 n) (p h)))
    (lazy-seq (cons h (global-take-while (if (p h) (dec n) n) p t)))))

(deftest global-take-while-test
  (is (= [2 3 5 7 11 13]
         (global-take-while 4 #(= 2 (mod % 3))
                            [2 3 5 7 11 13 17 19 23])))
  (is (= ["this" "is" "a" "sentence"]
         (global-take-while 3 #(some #{\i} %)
                            ["this" "is" "a" "sentence" "i" "wrote"])))
  (is (= ["this" "is"]
         (global-take-while 1 #{"a"}
                            ["this" "is" "a" "sentence" "i" "wrote"]))))

;; 115: The balance of n
(defn balanced-number? [n]
  (let [digits (map (comp read-string str) (str n))
        half-len (int (/ (count digits) 2))]
    (= (reduce + (take half-len digits)) (reduce + (take-last half-len digits)))))

(deftest test-balanced-number?
  (is (= true (balanced-number? 11)))
  (is (= true (balanced-number? 121)))
  (is (= false (balanced-number? 123)))
  (is (= true (balanced-number? 0)))
  (is (= false (balanced-number? 88099)))
  (is (= true (balanced-number? 89098)))
  (is (= true (balanced-number? 89089)))
  (is (= (take 20 (filter balanced-number? (range)))
    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])))

;; 116: Prime sandwich
(defn balanced-prime? [n]
  (let [prime? (fn [x] (not-any? #(zero? (mod x %)) (range 2 (inc (Math/ceil (Math/sqrt x))))))
        [cur hi] (take 2 (filter prime? (iterate inc n)))]
    (if-let [lo (last (filter prime? (range 2 n)))]
      (and (= n cur) (= n (/ (+ lo hi) 2)))
      false)))

(defn balanced-prime? [n]
  (letfn [(gen-primes [s]
            (let [cur (first s)]
              (lazy-seq (cons cur (gen-primes (remove #(zero? (mod % cur)) s))))))]
    (let [primes (gen-primes (iterate inc 2))
          [lower-primes higher-primes] (split-with (partial > n) primes)]
      (and (= n (first higher-primes)) (not= n 2)
           (= (/ (+ (last lower-primes) (first (rest higher-primes))) 2) n)))))

(deftest balanced-prime?-test
  (is (= false (balanced-prime? 4)))
  (is (= true (balanced-prime? 563)))
  (is (= 1103 (nth (filter balanced-prime? (range)) 15))))

;; 117: For science
(defn maze-solvable? [maze]
  (let [rows (count maze)
        cols (count (first maze))
        neighbors (fn [[i j]] [[(dec i) j] [(inc i) j] [i (dec j)] [i (inc j)]])
        maze-map (->> (for [i (range rows), j (range cols)] { (get-in maze [i j]) [i j]})
                      (apply merge-with conj { \space [], \# [] }))
        start (maze-map \M)
        end (maze-map \C)
        open (conj (set (maze-map \space)) end)
        valid-neighbors (fn [cell] (filter open (neighbors cell)))]
    (loop [visited #{}
           queue (list start)]
      (let [cur (first queue)
            next-moves (remove visited (valid-neighbors cur))
            queue (concat (next queue) next-moves)]
        (cond
          (= end cur) true
          (empty? queue) false
          :else (recur (conj visited cur) queue))))))

(deftest maze-solvable?-test
  (= true  (maze-solvable? ["M   C"]))
  (= false (maze-solvable? ["M # C"]))
  (= true  (maze-solvable? ["#######"
                            "#     #"
                            "#  #  #"
                            "#M # C#"
                            "#######"]))
  (= false (maze-solvable? ["########"
                            "#M  #  #"
                            "#   #  #"
                            "# # #  #"
                            "#   #  #"
                            "#  #   #"
                            "#  # # #"
                            "#  #   #"
                            "#  #  C#"
                            "########"]))
  (= false (maze-solvable? ["M     "
                            "      "
                            "      "
                            "      "
                            "    ##"
                            "    #C"]))
  (= true  (maze-solvable? ["C######"
                            " #     "
                            " #   # "
                            " #   #M"
                            "     # "]))
  (= true  (maze-solvable? ["C# # # #"
                            "        "
                            "# # # # "
                            "        "
                            " # # # #"
                            "        "
                            "# # # #M"])))

;; 118: Re-implement map
(defn map-clone [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (f (first s)) (map-clone f (rest s))))))

(deftest map-clone-test
  (is (= [3 4 5 6 7]
         (map-clone inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
         (map-clone (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
         (->> (map-clone inc (range))
              (drop (dec 1000000))
              (take 2)))))

;; 119: Win at tic-tac-toe
(defn win-tic-tac-toe [player board]
  (let [wins? (fn [player cell i j]
                (if (= :e cell)
                  (= player (tic-tac-toe-winner (assoc-in board [i j] player)))
                  false))]
    (set (for [i (range 3)
               j (range 3)
               :let [cell (get-in board [i j])]
               :when (and (= :e cell) (wins? player cell i j))]
           [i j]))))

(deftest win-tic-tac-toe-test
  (is (= (win-tic-tac-toe :x [[:o :e :e]
                              [:o :x :o]
                              [:x :x :e]])
         #{[2 2] [0 1] [0 2]}))
  (is (= (win-tic-tac-toe :x [[:x :o :o]
                              [:x :x :e]
                              [:e :o :e]])
         #{[2 2] [1 2] [2 0]}))
  (is (= (win-tic-tac-toe :x [[:x :e :x]
                              [:o :x :o]
                              [:e :o :e]])
         #{[2 2] [0 1] [2 0]}))
  (is (= (win-tic-tac-toe :x [[:x :x :o]
                              [:e :e :e]
                              [:e :e :e]])
         #{}))
  (is (= (win-tic-tac-toe :o [[:x :x :o]
                              [:o :e :o]
                              [:x :e :e]])
         #{[2 2] [1 1]})))

;; 120: Sum of square digits
(defn less-than-sum-squared-digits [ints]
  (let [sum-square-digits (fn [n]
                            (->> (map #(Integer/parseInt (str %)) (str n))
                                 (map #(* % %))
                                 (reduce +)))]
    (count (filter #(< % (sum-square-digits %)) ints))))

(defn less-than-sum-squared-digits [ints]
  (let [digits (fn [n] (map #(mod % 10) (take-while (partial < 0) (iterate #(quot % 10) n))))
        sum-square-digits (fn [n] (reduce + (map #(* % %) (digits n))))]
    (count (filter #(< % (sum-square-digits %)) ints))))

(deftest less-than-sum-squared-digits-test
  (is (= 8 (less-than-sum-squared-digits (range 10))))
  (is (= 19 (less-than-sum-squared-digits (range 30))))
  (is (= 50 (less-than-sum-squared-digits (range 100))))
  (is (= 50 (less-than-sum-squared-digits (range 1000)))))

;; 121: Universal computation engine
(defn universal-computation-engine [exp]
  (fn [ctx]
    (let [simplify (fn [exp]
                     (if (seq? exp)
                       ((universal-computation-engine exp) ctx)
                       (get ctx exp exp)))
          ops {'+ + '- - '* * '/ /}]
      (apply (ops (first exp)) (map simplify (next exp))))))

(deftest universal-computation-engine-test
  (is (= 2 ((universal-computation-engine '(/ a b))
            '{b 8 a 16})))
  (is (= 8 ((universal-computation-engine '(+ a b 2))
            '{a 2 b 4})))
  (is (= [6 0 -4]
         (map (universal-computation-engine '(* (+ 2 a)
                                                (- 10 b)))
              '[{a 1 b 8}
                {b 5 a -2}
                {a 2 b 11}])))
  (is (= 1 ((universal-computation-engine '(/ (+ x 2)
                                              (* 3 (+ y 1))))
            '{x 4 y 1}))))

;; 122: Read a binary number
(defn read-binary [s]
  (Integer/parseInt s 2))

(deftest read-binary-test
  (is (= 0     (read-binary "0")))
  (is (= 7     (read-binary "111")))
  (is (= 8     (read-binary "1000")))
  (is (= 9     (read-binary "1001")))
  (is (= 255   (read-binary "11111111")))
  (is (= 1365  (read-binary "10101010101")))
  (is (= 65535 (read-binary "1111111111111111"))))

;; 128: Recognize playing cards
(defn recognize-playing-card [[s r]]
  (let [suits (zipmap "DHSC" [:diamond :heart :spade :club])
        ranks (zipmap "23456789TJQKA" (range 13))]
    (hash-map :suit (suits s) :rank (ranks r))))

(deftest recognize-playing-card-test
  (is (= {:suit :diamond :rank 10} (recognize-playing-card "DQ")))
  (is (= {:suit :heart :rank 3} (recognize-playing-card "H5")))
  (is (= {:suit :club :rank 12} (recognize-playing-card "CA")))
  (is (= (range 13) (map (comp :rank recognize-playing-card str)
                         '[S2 S3 S4 S5 S6 S7
                           S8 S9 ST SJ SQ SK SA]))))

;; 131: Sum some set subsets
(defn eq-subset-sums? [& sets]
  (->> (map power-set sets)
       (map #(remove empty? %))
       (map (fn [subsets] (set (map #(reduce + %) subsets))))
       (apply clojure.set/intersection)
       ((complement empty?))))

(deftest eq-subset-sums?-test
  (= true  (eq-subset-sums? #{-1 1 99}
                            #{-2 2 888}
                            #{-3 3 7777}))
  (= false (eq-subset-sums? #{1}
                            #{2}
                            #{3}
                            #{4}))
  (= true  (eq-subset-sums? #{1}))
  (= false (eq-subset-sums? #{1 -3 51 9}
                            #{0}
                            #{9 2 81 33}))
  (= true  (eq-subset-sums? #{1 3 5}
                            #{9 11 4}
                            #{-3 12 3}
                            #{-3 4 -2 10}))
  (= false (eq-subset-sums? #{-1 -2 -3 -4 -5 -6}
                            #{1 2 3 4 5 6 7 8 9}))
  (= true  (eq-subset-sums? #{1 3 5 7}
                            #{2 4 6 8}))
  (= true  (eq-subset-sums? #{-1 3 -5 7 -9 11 -13 15}
                            #{1 -3 5 -7 9 -11 13 -15}
                            #{1 -1 2 -2 4 -4 8 -8}))
  (= true  (eq-subset-sums? #{-10 9 -8 7 -6 5 -4 3 -2 1}
                            #{10 -9 8 -7 6 -5 4 -3 2 -1})))

;; 132: Insert between two items
(defn insert-between [f v coll]
  (let [pairs (partition-all 2 1 coll)]
    (mapcat (fn [[a b]] (if (and b (f a b)) (list a v) (list a))) pairs)))

(defn insert-between [f v coll]
  (when (seq coll)
    (cons (first coll)
          (mapcat (fn [a b] (if (f a b) [v b] [b])) coll (rest coll)))))

(deftest insert-between-test
  (is (= '(1 :less 6 :less 7 4 3) (insert-between < :less [1 6 7 4 3])))
  (is (= '(2) (insert-between > :more [2])))
  (is (= [0 1 :x 2 :x 3 :x 4]  (insert-between #(and (pos? %) (< % %2)) :x (range 5))))
  (is (empty? (insert-between > :more ())))
  (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
         (take 12 (->> [0 1]
                       (iterate (fn [[a b]] [b (+ a b)]))
                       (map first)         ; fibonacci numbers
                       (insert-between (fn [a b]       ; both even or both odd
                                         (= (mod a 2) (mod b 2)))
                                       :same))))))

;; 134: A nil key
(defn nil-key? [k m]
  (nil? (k m :not-found)))

(defn nil-key? [k m]
  (and (contains? m k) (nil? (k m))))

(deftest nil-key?-test
  (is (true?  (nil-key? :a {:a nil :b 2})))
  (is (false? (nil-key? :b {:a nil :b 2})))
  (is (false? (nil-key? :c {:a nil :b 2}))))

;; 135: Infix calculator
(defn infix-calculator
  ([a] a)
  ([a op b & tokens] (apply infix-calculator (op a b) tokens)))

(defn infix-calculator
  ([a op b] (op a b))
  ([a op b & tokens] (apply infix-calculator (op a b) tokens)))

(deftest infix-calculator-test
  (is (= 7  (infix-calculator 2 + 5)))
  (is (= 42 (infix-calculator 38 + 48 - 2 / 2)))
  (is (= 8  (infix-calculator 10 / 2 - 1 * 2)))
  (is (= 72 (infix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

;; 137: Digits and bases
(defn digits-and-bases [n base]
  (if (< n base)
    [n]
    (conj (digits-and-bases (quot n base) base) (mod n base))))

(deftest digits-and-bases-test
  (is (= [1 2 3 4 5 0 1] (digits-and-bases 1234501 10)))
  (is (= [0] (digits-and-bases 0 11)))
  (is (= [1 0 0 1] (digits-and-bases 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)](digits-and-bases n n))))
  (is (= [16 18 5 24 15 1] (digits-and-bases Integer/MAX_VALUE 42))))

;; 141: Tricky card games
(defn tricky-card-games [trump-suit]
  (fn winner [cards]
    (let [trump-suit (or trump-suit ((first cards) :suit) trump-suit)]
      (apply max-key :rank (filter #(= trump-suit (% :suit)) cards)))))

(deftest tricky-card-games-test
  (is (let [notrump (tricky-card-games nil)]
        (and (= {:suit :club :rank 9} (notrump [{:suit :club :rank 4}
                                                 {:suit :club :rank 9}]))
             (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                                 {:suit :club :rank 10}])))))
  (is (= {:suit :club :rank 10} ((tricky-card-games :club) [{:suit :spade :rank 2}
                                                            {:suit :club :rank 10}])))
  (is (= {:suit :heart :rank 8}
         ((tricky-card-games :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                                      {:suit :diamond :rank 10} {:suit :heart :rank 4}]))))

;; 144: Oscilrate
(defn oscilrate [init & fns]
  (reductions (fn [acc f] (f acc)) init (cycle fns)))

(deftest oscilrate-test
  (is (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))
  (is (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
  (is (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))

;; 146: Trees into tables
(defn tree-to-table [t]
  (into {} (for [[k1 m] t
                 [k2 v] m]
             [[k1 k2] v])))

(deftest tree-to-table-test
  (is (= (tree-to-table '{a {p 1, q 2}
                          b {m 3, n 4}})
         '{[a p] 1, [a q] 2
           [b m] 3, [b n] 4}))
  (is (= (tree-to-table '{[1] {a b c d}
                          [2] {q r s t u v w x}})
         '{[[1] a] b, [[1] c] d,
           [[2] q] r, [[2] s] t,
           [[2] u] v, [[2] w] x}))
  (is (= (tree-to-table '{m {1 [a b c] 3 nil}})
         '{[m 1] [a b c], [m 3] nil})))

;; 147: Pascal's trapezoid
(defn pascals-trapezoid [nums]
  (iterate #(map +' (concat [0] %) (concat % [0])) nums))

(deftest pascals-trapezoid-test
  (is (= (second (pascals-trapezoid [2 3 2])) [2 5 5 2]))
  (is (= (take 5 (pascals-trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
  (is (= (take 2 (pascals-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
  (is (= (take 100 (pascals-trapezoid [2 4 2])) (rest (take 101 (pascals-trapezoid [2 2]))))))

;; 148: The big divide
(defn big-divide [n a b]
  (let [num-multiples #(quot (dec n) %)
        sum-multiples #(*' % (num-multiples %) (inc (num-multiples %)) 1/2)]
    (- (+ (sum-multiples a) (sum-multiples b)) (sum-multiples (* a b)))))

(deftest big-divide-test
  (is (= 0 (big-divide 3 17 11)))
  (is (= 23 (big-divide 10 3 5)))
  (is (= 233168 (big-divide 1000 3 5)))
  (is (= "2333333316666668" (str (big-divide 100000000 3 5))))
  (is (= "110389610389889610389610"
         (str (big-divide (* 10000 10000 10000) 7 11))))
  (is (= "1277732511922987429116"
         (str (big-divide (* 10000 10000 10000) 757 809))))
  (is (= "4530161696788274281"
         (str (big-divide (* 10000 10000 1000) 1597 3571)))))

;; 150: Palindromic numbers
(defn palindromic-numbers [n]
  (letfn [(gen-next-palindrome [n]
            (let [s (str n)
                  len (count s)
                  atoi #(Long. %)
                  half (subs s 0 (Math/ceil (/ len 2)))
                  half-to-palindrome (fn [h] (atoi (str h (subs (clojure.string/reverse h) (if (even? len) 0 1)))))
                  cur-palindrome (half-to-palindrome half)
                  next-palindrome (half-to-palindrome (str (inc (atoi half))))]
              (if (>= cur-palindrome n) cur-palindrome next-palindrome)))]
    (iterate (comp gen-next-palindrome inc) (gen-next-palindrome n))))

(deftest palindromic-numbers-test
  (is (= (take 26 (palindromic-numbers 0))
         [0 1 2 3 4 5 6 7 8 9
          11 22 33 44 55 66 77 88 99
          101 111 121 131 141 151 161]))
  (is (= (take 16 (palindromic-numbers 162))
         [171 181 191 202
          212 222 232 242
          252 262 272 282
          292 303 313 323]))
  (is (= (take 6 (palindromic-numbers 1234550000))
         [1234554321 1234664321 1234774321
          1234884321 1234994321 1235005321]))
  (is (= (first (palindromic-numbers (* 111111111 111111111)))
         (* 111111111 111111111)))
  (is (= (set (take 199 (palindromic-numbers 0)))
         (set (map #(first (palindromic-numbers %)) (range 0 10000)))))
  (is (= true
         (apply < (take 6666 (palindromic-numbers 9999999)))))
  (is (= (nth (palindromic-numbers 0) 10101)
         9102019)))

;; 153: Pairwise disjoint sets
(defn pairwise-disjoint? [sets]
  (apply distinct? (apply concat sets)))

(deftest pairwise-disjoint?-test
  (is (= (pairwise-disjoint? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
         true))
  (is (= (pairwise-disjoint? #{#{:a :b :c :d :e}
                               #{:a :b :c :d}
                               #{:a :b :c}
                               #{:a :b}
                               #{:a}})
         false))
  (is (= (pairwise-disjoint? #{#{[1 2 3] [4 5]}
                               #{[1 2] [3 4 5]}
                               #{[1] [2] 3 4 5}
                               #{1 2 [3 4] [5]}})
         true))
  (is (= (pairwise-disjoint? #{#{'a 'b}
                               #{'c 'd 'e}
                               #{'f 'g 'h 'i}
                               #{''a ''c ''f}})
         true))
  (is (= (pairwise-disjoint? #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                               #{#{:x :y :z} #{:x :y} #{:z} #{}}
                               #{'[:x :y :z] [:x :y] [:z] [] {}}})
         false))
  (is (= (pairwise-disjoint? #{#{(= "true") false}
                               #{:yes :no}
                               #{(class 1) 0}
                               #{(symbol "true") 'false}
                               #{(keyword "yes") ::no}
                               #{(class '1) (int \0)}})
         false))
  (is (= (pairwise-disjoint? #{#{distinct?}
                               #{#(-> %) #(-> %)}
                               #{#(-> %) #(-> %) #(-> %)}
                               #{#(-> %) #(-> %) #(-> %)}})
         true))
  (is (= (pairwise-disjoint? #{#{(#(-> *)) + (quote mapcat) #_ nil}
                               #{'+ '* mapcat (comment mapcat)}
                               #{(do) set contains? nil?}
                               #{, , , #_, , empty?}})
         false)))

;; 156: Map defaults
(defn map-defaults [default ks]
  (zipmap ks (repeat default)))

(deftest map-defaults-test
  (is (= (map-defaults 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
  (is (= (map-defaults "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
  (is (= (map-defaults [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))

;; 157: Indexing sequences
(defn indexing-seq [coll]
  (map-indexed #(vector %2 %1) coll))

(defn indexing-seq [coll]
  (map vector coll (range)))

(deftest indexing-seq-test
  (is (= (indexing-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (indexing-seq [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (indexing-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

;; 158: Decurry
(defn decurry [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

(deftest decurry-test
  (is (= 10 ((decurry (fn [a]
                        (fn [b]
                          (fn [c]
                            (fn [d]
                              (+ a b c d))))))
             1 2 3 4)))
  (is (= 24 ((decurry (fn [a]
                        (fn [b]
                          (fn [c]
                            (fn [d]
                              (* a b c d))))))
             1 2 3 4)))
  (is (= 25 ((decurry (fn [a]
                        (fn [b]
                          (* a b))))
             5 5))))


;; 166: Comparisons
(defn comparisons [lt-op x y]
  (cond (lt-op x y) :lt
        (lt-op y x) :gt
        :else :eq))

(deftest comparisons-test
  (is (= :gt (comparisons < 5 1)))
  (is (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparisons > 0 2)))
  (is (= :gt (comparisons > 0 2))))

;; 168: Infinite matrix
(defn infinite-matrix
  ([f] (infinite-matrix f 0 0))
  ([f m n] (letfn [(row [i j] (lazy-seq (cons (f i j) (row i (inc j)))))
                   (col [i j] (lazy-seq (cons (row i j) (col (inc i) j))))]
                 (col m n)))
  ([f m n s t] (take s (map #(take t %) (infinite-matrix f m n)))))

(deftest test-infinite-matrix
  (is (= (take 5 (map #(take 6 %) (infinite-matrix str)))
         [["00" "01" "02" "03" "04" "05"]
          ["10" "11" "12" "13" "14" "15"]
          ["20" "21" "22" "23" "24" "25"]
          ["30" "31" "32" "33" "34" "35"]
          ["40" "41" "42" "43" "44" "45"]]))
  (is (= (take 6 (map #(take 5 %) (infinite-matrix str 3 2)))
         [["32" "33" "34" "35" "36"]
          ["42" "43" "44" "45" "46"]
          ["52" "53" "54" "55" "56"]
          ["62" "63" "64" "65" "66"]
          ["72" "73" "74" "75" "76"]
          ["82" "83" "84" "85" "86"]]))
  (is (= (infinite-matrix * 3 5 5 7)
         [[15 18 21 24 27 30 33]
          [20 24 28 32 36 40 44]
          [25 30 35 40 45 50 55]
          [30 36 42 48 54 60 66]
          [35 42 49 56 63 70 77]]))
  (is (= (infinite-matrix #(/ % (inc %2)) 1 0 6 4)
         [[1/1 1/2 1/3 1/4]
          [2/1 2/2 2/3 1/2]
          [3/1 3/2 3/3 3/4]
          [4/1 4/2 4/3 4/4]
          [5/1 5/2 5/3 5/4]
          [6/1 6/2 6/3 6/4]]))
  (is (= (class (infinite-matrix (juxt bit-or bit-xor)))
         (class (infinite-matrix (juxt quot mod) 13 21))
         (class (lazy-seq))))
  (is (= (class (nth (infinite-matrix (constantly 10946)) 34))
         (class (nth (infinite-matrix (constantly 0) 5 8) 55))
         (class (lazy-seq))))
  (is (= (let [m 377 n 610 w 987
               check (fn [f s] (every? true? (map-indexed f s)))
               row (take w (nth (infinite-matrix vector) m))
               column (take w (map first (infinite-matrix vector m n)))
               diagonal (map-indexed #(nth %2 %) (infinite-matrix vector m n w w))]
           (and (check #(= %2 [m %]) row)
                (check #(= %2 [(+ m %) n]) column)
                (check #(= %2 [(+ m %) (+ n %)]) diagonal)))
         true)))

;; 171: Intervals
(defn intervals [coll]
  (let [ordered-ints (distinct (sort coll))]
    (loop [intervals []
           cur-head (first ordered-ints)
           [h & t] ordered-ints]
      (cond
        (empty? t) (if h (conj intervals [cur-head h]) intervals)
        (not= (first t) (inc h)) (recur (conj intervals [cur-head h]) (first t) t)
        :else (recur intervals cur-head t)))))

(defn intervals [coll]
  (let [s (apply sorted-set coll)]
    (map vector
         (remove #(s (dec %)) s)
         (remove #(s (inc %)) s))))

(deftest intervals-test
  (is (= (intervals [1 2 3]) [[1 3]]))
  (is (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))
  (is (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))
  (is (= (intervals []) []))
  (is (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
         [[1 4] [6 6] [9 11] [13 17] [19 19]])))

;; 177: Balancing brackets
(defn balancing-brackets [s]
  (let [open? #((set "([{") %)
        closed? #((set ")]}") %)
        matches? (fn [open closed] (= ({\} \{, \] \[, \) \(} closed) open))]
    (loop [cur-stack [], [h & t] s]
      (cond
        (nil? h) (empty? cur-stack)
        (open? h) (recur (conj cur-stack h) t)
        (closed? h) (if (matches? (peek cur-stack) h)
                      (recur (pop cur-stack) t)
                      false)
        :else (recur cur-stack t)))))

(deftest balancing-brackets-test
  (is (balancing-brackets "This string has no brackets."))
  (is (balancing-brackets "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))
  (is (not (balancing-brackets "(start, end]")))
  (is (not (balancing-brackets "())")))
  (is (not (balancing-brackets "[ { ] } ")))
  (is (balancing-brackets "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))
  (is (not (balancing-brackets "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))
  (is (not (balancing-brackets "["))))

;; 178: Best hand
(defn best-hand [hand]
  (let [[suits ranks] (apply map str hand)
        freqs (frequencies (vals (frequencies ranks)))
        rank-strs (map str (concat "A" (range 2 10) "TJQKA"))
        straights (set (map set (partition 5 1 rank-strs)))
        straight? (straights (set (map str ranks)))
        flush? (apply = suits)
        n-of-kind? #(freqs %)]
    (cond
      (and straight? flush?) :straight-flush
      (n-of-kind? 4) :four-of-a-kind
      (and (freqs 3) (freqs 2)) :full-house
      flush? :flush
      straight? :straight
      (n-of-kind? 3) :three-of-a-kind
      (= 2 (freqs 2)) :two-pair
      (n-of-kind? 2) :pair
      :else :high-card)))

(deftest best-hand-test
  (is (= :high-card (best-hand ["HA" "D2" "H3" "C9" "DJ"])))
  (is (= :pair (best-hand ["HA" "HQ" "SJ" "DA" "HT"])))
  (is (= :two-pair (best-hand ["HA" "DA" "HQ" "SQ" "HT"])))
  (is (= :three-of-a-kind (best-hand ["HA" "DA" "CA" "HJ" "HT"])))
  (is (= :straight (best-hand ["HA" "DK" "HQ" "HJ" "HT"])))
  (is (= :straight (best-hand ["HA" "H2" "S3" "D4" "C5"])))
  (is (= :flush (best-hand ["HA" "HK" "H2" "H4" "HT"])))
  (is (= :full-house (best-hand ["HA" "DA" "CA" "HJ" "DJ"])))
  (is (= :four-of-a-kind (best-hand ["HA" "DA" "CA" "SA" "DJ"])))
  (is (= :straight-flush (best-hand ["HA" "HK" "HQ" "HJ" "HT"]))))

;; 195: Parentheses... again
(defn paren-combs [n]
  (if (zero? n)
    #{""}
    (set
     (for [i (range n)
           combs1 (paren-combs i)
           combs2 (paren-combs (- n i 1))]
       (str "(" combs1 ")" combs2)))))

(deftest paren-combs-test
  (is (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (paren-combs n)) [0 1 2])))
  (is (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (paren-combs 3)))
  (is (= 16796 (count (paren-combs 10))))
  (is (= (nth (sort (filter #(.contains ^String % "(()()()())") (paren-combs 9))) 6) "(((()()()())(())))"))
  (is (= (nth (sort (paren-combs 12)) 5000) "(((((()()()()()))))(()))")))

(run-tests)
