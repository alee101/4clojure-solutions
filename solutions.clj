;; Solutions to 4clojure problems with some trivial ones omitted
;; Included multiple solutions for some problems
(require '[clojure.test :refer :all])

;; 19: Last element
(def last-clone (comp first reverse))

;; 20: Penultimate element
(def penultimate (comp second reverse))

;; 21: Nth element
(defn nth-clone [s n]
  ((vec s) n))

;; 22: Count a sequence
(defn count-clone [s]
  (reduce + (map (constantly 1) s)))

(defn count-clone [s]
  (reduce (fn [acc _] (inc acc)) 0 s))

;; 23: Reverse a sequence
(defn reverse-clone [s]
  (reduce conj '() s))

(defn reverse-clone [s]
  (into '() s))

;; 26: Fibonacci sequence
(defn fib [n]
  (->> (map first (iterate (fn [[prev cur]] [cur (+ prev cur)]) [1 1]))
       (take n)))

;; 27: Palindrome detector
(defn palindrome? [s]
  (= (seq s) (reverse s)))

;; 28: Flatten a sequence
(defn flatten-clone [s]
  (if (sequential? s)
    (mapcat flatten-clone s)
    (vector s)))

;; 29: Get the caps
(defn get-caps [s]
  (apply str (re-seq #"[A-Z]+" s)))

;; 30: Compress a sequence
(defn compress-seq [s]
  (->> (partition-by identity s)
       (map first)))

;; 31: Pack a sequence
(def pack-seq (partial partition-by identity))

;; 32: Duplicate a sequence
(defn duplicate-seq [s]
  (mapcat (partial repeat 2) s))

(defn duplicate-seq [s]
  (mapcat #(list % %) s))

(defn duplicate-seq [s]
  (interleave s s))

;; 33: Replicate a sequence
(defn replicate-seq [s n]
  (mapcat (partial repeat n) s))

(defn replicate-seq [s n]
  (if (= n 1)
    s
    (apply interleave (repeat n s))))

;; 34: Implement range
(defn range-clone [start end]
  (take (- end start) (iterate inc start)))

;; 38: Maximum value
(defn max-clone [& nums]
  (last (sort nums)))

;; 39: Interleave two seqs
(def interleave-clone (partial mapcat list))

;; 40: Interpose a seq
(defn interpose-clone [sep s]
  (butlast (interleave s (repeat sep))))

(defn interpose-clone [sep s]
  (next (interleave (repeat sep) s)))

(defn interpose-clone [sep s]
  (butlast (mapcat #(list % sep) s)))

;; 41: Drop every nth item
(defn drop-every-nth [s n]
  (keep-indexed (fn [idx ele] (if (pos? (mod (inc idx) n)) ele)) s))

(defn drop-every-nth [s n]
  (apply concat (partition-all (dec n) n s)))

;; 42: Factorial fun
(defn factorial [n]
  (reduce * (range 1 (inc n))))

;; 43: Reverse interleave
(defn reverse-interleave [s n]
  (->> (partition n s)
       (apply map list)))

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

;; 46: Flipping out
(defn flipping-out [f]
  (fn [& args]
    (apply f (reverse args))))

;; 49: Split a sequence
(defn split-at-clone [n coll]
  [(take n coll) (drop n coll)])

(def split-at-clone (juxt take drop))

;; 50: Split by type
(defn split-by-type [s]
  (vals (group-by type s)))

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

;; 54: Partition a sequence
(defn partition-clone [n coll]
  (loop [coll, coll, partitions []]
    (if (< (count coll) n)
      partitions
      (recur (drop n coll) (conj partitions (take n coll))))))

;; 55: Count occurrences
(defn frequencies-clone [coll]
  (->> (map #(hash-map % 1) coll)
       (apply merge-with +)))

(defn frequencies-clone [coll]
  (reduce (fn [counts x]
            (assoc counts x (inc (get counts x 0))))
          {} coll))

;; 56: Find distinct items
(defn distinct-clone [coll]
  (reduce (fn [distincts x]
            (if ((set distincts) x) distincts (conj distincts x))) [] coll))

;; 58: Function composition
(defn comp-clone [& fns]
  (reduce (fn [g f]
            (fn [& args] (g (apply f args)))) fns))

;; 59: Juxtaposition
(defn juxt-clone [& fns]
  (fn [& args]
    (map (fn [f] (apply f args)) fns)))

(defn juxt-clone [& fns]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fns)))

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

;; 62: Re-implement iterate
(defn iterate-clone [f x]
  (cons x (lazy-seq (iterate-clone f (f x)))))

;; 63: Group a sequence
(defn group-by-clone [f coll]
  (apply merge-with concat (map #(hash-map (f %) [%]) coll)))

(defn group-by-clone [f coll]
  (reduce (fn [groups x]
            (let [k (f x)]
              (assoc groups k (conj (get groups k []) x))))
          {} coll))

;; 65: Black-box testing
(def black-box-testing (comp {\# :set \{ :map \[ :vector \( :list} first str))

(defn black-box-testing [coll]
  (let [empty-coll (empty coll)]
    (cond
      (= {} empty-coll) :map
      (= #{} empty-coll) :set
      (= [1 2] (conj empty-coll 1 2)) :vector
      :else :list)))

;; 66: Greatest common divisor
(defn gcd [x y]
  (if (zero? y) x (recur y (mod x y))))

;; 67: Prime numbers
(defn primes [n]
  (let [prime? (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))]
    (take n (filter prime? (iterate inc 2)))))

(defn primes [n]
  (letfn [(sieve [s] (cons (first s) (lazy-seq (sieve (remove #(zero? (mod % (first s))) (rest s))))))]
    (take n (sieve (iterate inc 2)))))

;; 69: Merge with a function
(defn merge-with-clone [f m & maps]
  (reduce (fn [acc [k v]]
            (assoc acc k (if (acc k) (f (acc k) v) v)))
          m (apply concat (map vec maps))))

;; 70: Word sorting
(defn word-sorting [s]
  (->> (clojure.string/split s #"[^A-Za-z]")
       (sort-by clojure.string/lower-case)))

;; 73: Analyze a tic-tac-toe board
(defn tic-tac-toe-winner [board]
  (let [winner? (fn [s] (if (apply = s) (first s) nil))
        row-winner? (map winner? board)
        col-winner? (map winner? (apply map vector board))
        lft-diag-winner? (winner? (map get board (range 3)))
        rgt-diag-winner? (winner? (map get board (range 2 -1 -1)))]
    (some #{:x :o} (or (concat row-winner? col-winner? [lft-diag-winner? rgt-diag-winner?])))))

;; 74: Filter perfect squares
(defn filter-perfect-squares [s]
  (let [perfect-square? (fn [n]
                          (let [sqrt-n (Math/sqrt n)]
                            (== n (* sqrt-n sqrt-n))))]
    (->> (clojure.string/split s #",")
         (map #(Integer/parseInt %))
         (filter perfect-square?)
         (clojure.string/join ","))))

;; 75: Euler's totient function
(defn eulers-totient-fn [n]
  (let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))]
    (if (= n 1)
      1
      (count (filter #(= (gcd n %) 1) (range 1 n))))))

;; 77: Anagram finder
(defn anagrams [words]
  (->> (group-by set words)
       vals
       (filter next)
       (map set)
       set))

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

;; 80: Perfect numbers
(defn perfect-number? [n]
  (let [divisors (filter #(zero? (mod n %)) (range 1 n))]
    (= n (reduce + divisors))))

;; 81: Set intersection
(defn intersection-clone [s1 s2]
  (set (filter s1 s2)))

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

;; 83: A half-truth
(defn half-truth? [& bools]
  (and (not-every? true? bools) (not-every? false? bools)))

(defn half-truth? [& bools]
  (boolean (and (some true? bools) (not-every? true? bools))))

;; 84: Transitive closure
(defn transitive-closure [relations]
  (let [new-relations (for [[a x] relations [y b] relations :when (= x y)] [a b])
        updated-relations (into relations new-relations)]
    (if (= updated-relations relations)
      updated-relations
      (recur updated-relations))))

;; 85: Power set
(defn power-set [s]
  (reduce (fn [ps ele] (into ps (map #(conj % ele) ps))) #{#{}} s))

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

;; 88: Symmetric difference
(defn symmetric-difference [s1 s2]
  (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))

;; 90: Cartesian product
(defn cartesian-product [s1 s2]
  (set (for [x s1, y s2] [x y])))

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

;; 89: Graph tour
(defn graph-tour [edges]
  (let [nodes (set (apply concat edges))
        node-degrees (frequencies (apply concat (map set edges)))
        odd-degrees (count (filter odd? (vals node-degrees)))]
    (and (connected-graph? edges) (or (= odd-degrees 0) (= odd-degrees 2)))))

;; 92: Read roman numerals
(defn read-roman-numerals [rn]
  (let [numerals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        pairs (partition 2 1 (concat (map numerals rn) [0]))]
    (reduce (fn [acc [a b]] ((if (>= a b) + -) acc a)) 0 pairs)))

;; 93: Partially flatten a sequence
(defn partial-flatten [s]
  (if (every? sequential? s)
    (mapcat partial-flatten s)
    [s]))

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

;; 95: To tree, or not to tree
(defn tree? [t]
  (if (sequential? t)
    (and (= (count t) 3) (tree? (second t)) (tree? (last t)))
    (nil? t)))

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

;; 98: Equivalence classes
(defn equivalence-classes [f D]
  (set (map set (vals (group-by f D)))))

;; 99: Product digits
(defn product-digits [x y]
  (map #(Integer/parseInt (str %)) (str (* x y))))

(defn product-digits [x y]
  (map #(- (int %) (int \0)) (str (* x y))))

(defn product-digits [x y]
  (map (comp read-string str) (str (* x y))))

;; 100: Least common multiple
(defn lcm [& nums]
  (let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))]
    (/ (reduce * nums) (reduce gcd nums))))

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

;; 102: intoCamelCase
(defn into-camel-case [s]
  (let [tokens (clojure.string/split s #"-")]
    (clojure.string/join "" (cons (first tokens) (map clojure.string/capitalize (next tokens))))))

(defn into-camel-case [s]
  (clojure.string/replace s #"-\w" #(clojure.string/capitalize (last %))))

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

;; 104: Write roman numerals
(defn write-roman-numerals [n]
  (let [numerals (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")
        num (some #(when (>= n %) %) (keys numerals))]
    (when-not (nil? num)
      (str (numerals num) (write-roman-numerals (- n num))))))

;; 105: Identify keys and values
(defn identify-keys-vals [coll]
  (if-let [[k & t] (seq coll)]
    (let [[vs s] (split-with (complement keyword?) t)]
      (conj (identify-keys-vals s) {k vs}))
    {}))

;; 106: Number maze
(defn number-maze [start end]
  (let [children (fn [n] (conj (when (even? n) (list (/ n 2))) (* n 2) (+ n 2)))]
    (loop [nodes [start]
           depth 1]
      (if (some #{end} nodes)
        depth
        (recur (mapcat children nodes) (inc depth))))))

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

;; 110: Sequence of pronunciations
(defn pronunciations [coll]
  (let [pronunciation (fn [coll] (->> (partition-by identity coll)
                                      (mapcat (juxt count first))))]
    (rest (iterate pronunciation coll))))

;; 111: Crossword puzzle
(defn valid-crossword? [s puzzle]
  (->> (concat puzzle (apply map str puzzle))
       (map #(clojure.string/replace % #" |_|#" {" " "", "_" ".", "#" "|"}))
       (some #(re-matches (re-pattern %) s))
       boolean))

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

;; 113: Making data dance
(defn making-data-dance [& args]
  (when args
    (reify clojure.lang.ISeq
      (seq [_] (distinct args))
      (toString [_] (apply str (interpose ", " (sort args)))))))

;; 114: Global take-while
(defn global-take-while [n p [h & t]]
  (when-not (or (zero? n) (and (= 1 n) (p h)))
    (lazy-seq (cons h (global-take-while (if (p h) (dec n) n) p t)))))

;; 115: The balance of n
(defn balanced-number? [n]
  (let [digits (map (comp read-string str) (str n))
        half-len (int (/ (count digits) 2))]
    (= (reduce + (take half-len digits)) (reduce + (take-last half-len digits)))))

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

;; 118: Re-implement map
(defn map-clone [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (f (first s)) (map-clone f (rest s))))))

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

;; 121: Universal computation engine
(defn universal-computation-engine [exp]
  (fn [ctx]
    (let [simplify (fn [exp]
                     (if (seq? exp)
                       ((universal-computation-engine exp) ctx)
                       (get ctx exp exp)))
          ops {'+ + '- - '* * '/ /}]
      (apply (ops (first exp)) (map simplify (next exp))))))

;; 122: Read a binary number
(defn read-binary [s]
  (Integer/parseInt s 2))

;; 128: Recognize playing cards
(defn recognize-playing-card [[s r]]
  (let [suits (zipmap "DHSC" [:diamond :heart :spade :club])
        ranks (zipmap "23456789TJQKA" (range 13))]
    (hash-map :suit (suits s) :rank (ranks r))))

;; 131: Sum some set subsets
(defn eq-subset-sums? [& sets]
  (->> (map power-set sets)
       (map #(remove empty? %))
       (map (fn [subsets] (set (map #(reduce + %) subsets))))
       (apply clojure.set/intersection)
       ((complement empty?))))

;; 132: Insert between two items
(defn insert-between [f v coll]
  (let [pairs (partition-all 2 1 coll)]
    (mapcat (fn [[a b]] (if (and b (f a b)) (list a v) (list a))) pairs)))

(defn insert-between [f v coll]
  (when (seq coll)
    (cons (first coll)
          (mapcat (fn [a b] (if (f a b) [v b] [b])) coll (rest coll)))))

;; 134: A nil key
(defn nil-key? [k m]
  (nil? (k m :not-found)))

(defn nil-key? [k m]
  (and (contains? m k) (nil? (k m))))

;; 135: Infix calculator
(defn infix-calculator
  ([a] a)
  ([a op b & tokens] (apply infix-calculator (op a b) tokens)))

(defn infix-calculator
  ([a op b] (op a b))
  ([a op b & tokens] (apply infix-calculator (op a b) tokens)))

;; 137: Digits and bases
(defn digits-and-bases [n base]
  (if (< n base)
    [n]
    (conj (digits-and-bases (quot n base) base) (mod n base))))

;; 141: Tricky card games
(defn tricky-card-games [trump-suit]
  (fn winner [cards]
    (let [trump-suit (or trump-suit ((first cards) :suit) trump-suit)]
      (apply max-key :rank (filter #(= trump-suit (% :suit)) cards)))))

;; 144: Oscilrate
(defn oscilrate [init & fns]
  (reductions (fn [acc f] (f acc)) init (cycle fns)))

;; 146: Trees into tables
(defn tree-to-table [t]
  (into {} (for [[k1 m] t
                 [k2 v] m]
             [[k1 k2] v])))

;; 147: Pascal's trapezoid
(defn pascals-trapezoid [nums]
  (iterate #(map +' (concat [0] %) (concat % [0])) nums))

;; 148: The big divide
(defn big-divide [n a b]
  (let [num-multiples #(quot (dec n) %)
        sum-multiples #(*' % (num-multiples %) (inc (num-multiples %)) 1/2)]
    (- (+ (sum-multiples a) (sum-multiples b)) (sum-multiples (* a b)))))

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

;; 153: Pairwise disjoint sets
(defn pairwise-disjoint? [sets]
  (apply distinct? (apply concat sets)))

;; 156: Map defaults
(defn map-defaults [default ks]
  (zipmap ks (repeat default)))

;; 157: Indexing sequences
(defn indexing-seq [coll]
  (map-indexed #(vector %2 %1) coll))

(defn indexing-seq [coll]
  (map vector coll (range)))

;; 158: Decurry
(defn decurry [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

;; 166: Comparisons
(defn comparisons [lt-op x y]
  (cond (lt-op x y) :lt
        (lt-op y x) :gt
        :else :eq))

;; 168: Infinite matrix
(defn infinite-matrix
  ([f] (infinite-matrix f 0 0))
  ([f m n] (letfn [(row [i j] (lazy-seq (cons (f i j) (row i (inc j)))))
                   (col [i j] (lazy-seq (cons (row i j) (col (inc i) j))))]
                 (col m n)))
  ([f m n s t] (take s (map #(take t %) (infinite-matrix f m n)))))

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

;; 195: Parentheses... again
(defn paren-combs [n]
  (if (zero? n)
    #{""}
    (set
     (for [i (range n)
           combs1 (paren-combs i)
           combs2 (paren-combs (- n i 1))]
       (str "(" combs1 ")" combs2)))))
