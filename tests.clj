(load-file "solutions.clj")
(require '[clojure.test :refer :all])

(deftest last-clone-test
  (is (= (last-clone [1 2 3 4 5]) 5))
  (is (= (last-clone '(5 4 3)) 3))
  (is (= (last-clone ["b" "c" "d"]) "d")))

(deftest penultimate-test
  (is (= (penultimate (list 1 2 3 4 5)) 4))
  (is (= (penultimate ["a" "b" "c"]) "b"))
  (is (= (penultimate [[1 2] [3 4]]) [1 2])))

(deftest nth-clone-test
  (is (= (nth-clone '(4 5 6 7) 2) 6))
  (is (= (nth-clone [:a :b :c] 0) :a))
  (is (= (nth-clone [1 2 3 4] 1) 2))
  (is (= (nth-clone '([1 2] [3 4] [5 6]) 2) [5 6])))

(deftest count-clone-test
  (is (= (count-clone '(1 2 3 3 1)) 5))
  (is (= (count-clone "Hello World") 11))
  (is (= (count-clone [[1 2] [3 4] [5 6]]) 3))
  (is (= (count-clone '(13)) 1))
  (is (= (count-clone '(:a :b :c)) 3)))

(deftest reverse-clone-test
  (is (= (reverse-clone [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (reverse-clone (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (reverse-clone [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

(deftest fib-test
  (is (= (fib 3) '(1 1 2)))
  (is (= (fib 6) '(1 1 2 3 5 8)))
  (is (= (fib 8) '(1 1 2 3 5 8 13 21))))

(deftest palindrome?-test
  (is (false? (palindrome? '(1 2 3 4 5))))
  (is (true? (palindrome? "racecar")))
  (is (true? (palindrome? [:foo :bar :foo])))
  (is (true? (palindrome? '(1 1 3 3 1 1))))
  (is (false? (palindrome? '(:a :b :c)))))

(deftest flatten-clone-test
  (is (= (flatten-clone '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten-clone ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (flatten-clone '((((:a))))) '(:a))))

(deftest get-caps-test
  (is (= (get-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (get-caps "nothing")))
  (is (= (get-caps "$#A(*&987Zf") "AZ")))

(deftest compress-seq-test
  (is (= (apply str (compress-seq "Leeeeeerrroyyy")) "Leroy"))
  (is (= (compress-seq [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (compress-seq [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

(deftest pack-seq-test
  (is (= (pack-seq [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (pack-seq [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (pack-seq [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

(deftest duplicate-seq-test
  (is (= (duplicate-seq [1 2 3]) '(1 1 2 2 3 3)))
  (is (= (duplicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

(deftest replicate-seq-test
  (is (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
  (is (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
  (is (= (replicate-seq [4 5 6] 1) '(4 5 6)))
  (is (= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (replicate-seq [44 33] 2) [44 44 33 33])))

(deftest range-clone-test
  (is (= (range-clone 1 4) '(1 2 3)))
  (is (= (range-clone -2 2) '(-2 -1 0 1)))
  (is (= (range-clone 5 8) '(5 6 7))))

(deftest max-clone-test
  (is (= (max-clone 1 8 3 4) 8))
  (is (= (max-clone 30 20) 30))
  (is (= (max-clone 45 67 11) 67)))

(deftest interleave-clone-test
  (is (= (interleave-clone [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (is (= (interleave-clone [1 2] [3 4 5 6]) '(1 3 2 4)))
  (is (= (interleave-clone [1 2 3 4] [5]) [1 5]))
  (is (= (interleave-clone [30 20] [25 15]) [30 25 20 15])))

(deftest interpose-clone-test
  (is (= (interpose-clone 0 [1 2 3]) [1 0 2 0 3]))
  (is (= (apply str (interpose-clone ", " ["one" "two" "three"])) "one, two, three"))
  (is (= (interpose-clone :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

(deftest drop-every-nth-test
  (is (= (drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e]))
  (is (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])))

(deftest factorial-test
  (is (= (factorial 1) 1))
  (is (= (factorial 3) 6))
  (is (= (factorial 5) 120))
  (is (= (factorial 8) 40320)))

(deftest reverse-interleave-test
  (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(deftest rotate-seq-test
  (is (= (rotate-seq 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-seq -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-seq 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-seq 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-seq -4 '(:a :b :c)) '(:c :a :b))))

(deftest flipping-out-test
  (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
  (is (= true ((flipping-out >) 7 8)))
  (is (= 4 ((flipping-out quot) 2 8)))
  (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))))

(deftest split-at-clone-test
  (is (= (split-at-clone 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split-at-clone 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split-at-clone 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest split-by-type-test
  (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

(deftest longest-inc-sub-seq-test
  (is (= (longest-inc-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3]))
  (is (= (longest-inc-sub-seq [5 6 1 3 2 7]) [5 6]))
  (is (= (longest-inc-sub-seq [2 3 3 4 5]) [3 4 5]))
  (is (= (longest-inc-sub-seq [7 6 5 4]) [])))

(deftest partition-clone-test
  (is (= (partition-clone 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (partition-clone 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (is (= (partition-clone 3 (range 8)) '((0 1 2) (3 4 5)))))

(deftest frequencies-clone-test
  (is (= (frequencies-clone [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (frequencies-clone [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (frequencies-clone '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

(deftest distinct-clone-test
  (is (= (distinct-clone [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (distinct-clone [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (distinct-clone '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (distinct-clone (range 50)) (range 50))))

(deftest comp-clone-test
  (is (= [3 2 1] ((comp-clone rest reverse) [1 2 3 4])))
  (is (= 5 ((comp-clone (partial + 3) second) [1 2 3 4])))
  (is (= true ((comp-clone zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((comp-clone #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

(deftest juxt-clone-test
  (is (= [21 6 1] ((juxt-clone + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxt-clone #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxt-clone :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

(deftest reductions-clone-test
  (is (= (take 5 (reductions-clone + (range))) [0 1 3 6 10]))
  (is (= (reductions-clone conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (reductions-clone * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))
  (is (= (reductions-clone + []) '(0))))

(deftest zipmap-clone-test
  (is (= (zipmap-clone [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (zipmap-clone [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (zipmap-clone [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(deftest iterate-clone-test
  (is (= (take 5 (iterate-clone #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (iterate-clone inc 0)) (take 100 (range))))
  (is (= (take 9 (iterate-clone #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(deftest group-by-clone-test
  (is (= (group-by-clone #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-by-clone #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-by-clone count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(deftest black-box-testing-test
  (is (= :map (black-box-testing {:a 1, :b 2})))
  (is (= :list (black-box-testing (range (rand-int 20)))))
  (is (= :vector (black-box-testing [1 2 3 4 5 6])))
  (is (= :set (black-box-testing #{10 (rand-int 5)})))
  (is (= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))))

(deftest gcd-test
  (is (= (gcd 2 4) 2))
  (is (= (gcd 10 5) 5))
  (is (= (gcd 5 7) 1))
  (is (= (gcd 1023 858) 33)))

(deftest primes-test
  (is (= (primes 2) [2 3]))
  (is (= (primes 5) [2 3 5 7 11]))
  (is (= (last (primes 100)) 541)))

(deftest merge-with-clone-test
  (is (= (merge-with-clone * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
         {:a 4, :b 6, :c 20}))
  (is (= (merge-with-clone - {1 10, 2 20} {1 3, 2 10, 3 15})
         {1 7, 2 10, 3 15}))
  (is (= (merge-with-clone concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
         {:a [3 4 5], :b [6 7], :c [8 9]})))

(deftest word-sorting-test
  (is (= (word-sorting  "Have a nice day.")
         ["a" "day" "Have" "nice"]))
  (is (= (word-sorting  "Clojure is a fun language!")
         ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting  "Fools fall for foolish follies.")
         ["fall" "follies" "foolish" "Fools" "for"])))

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

(deftest filter-perfect-squares-test
  (is (= (filter-perfect-squares "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

(deftest eulers-totient-fn-test
  (is (= (eulers-totient-fn 1) 1))
  (is (= (eulers-totient-fn 10) (count '(1 3 7 9)) 4))
  (is (= (eulers-totient-fn 40) 16))
  (is (= (eulers-totient-fn 99) 60)))

(deftest anagrams-test
  (is (= (anagrams ["meat" "mat" "team" "mate" "eat"])
         #{#{"meat" "team" "mate"}}))
  (is (= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
         #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

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

(deftest perfect-number?-test
  (is (= (perfect-number? 6) true))
  (is (= (perfect-number? 7) false))
  (is (= (perfect-number? 496) true))
  (is (= (perfect-number? 500) false))
  (is (= (perfect-number? 8128) true)))

(deftest intersection-clone-test
  (is (= (intersection-clone #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (is (= (intersection-clone #{0 1 2} #{3 4 5}) #{}))
  (is (= (intersection-clone #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

(deftest word-chain?-test
  (is (= true (word-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
  (is (= false (word-chain? #{"cot" "hot" "bat" "fat"})))
  (is (= false (word-chain? #{"to" "top" "stop" "tops" "toss"})))
  (is (= true (word-chain? #{"spout" "do" "pot" "pout" "spot" "dot"})))
  (is (= true (word-chain? #{"share" "hares" "shares" "hare" "are"})))
  (is (= false (word-chain? #{"share" "hares" "hare" "are"}))))

(deftest half-truth?-test
  (is (= false (half-truth? false false)))
  (is (= true (half-truth? true false)))
  (is (= false (half-truth? true)))
  (is (= true (half-truth? false true false)))
  (is (= false (half-truth? true true true)))
  (is (= true (half-truth? true true true false))))

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

(deftest power-set-test
  (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
  (is (= (power-set #{}) #{#{}}))
  (is (= (power-set #{1 2 3})
         #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
  (is (= (count (power-set (into #{} (range 10)))) 1024)))

(deftest happy-number?-test
  (is (= (happy-number? 7) true))
  (is (= (happy-number? 986543210) true))
  (is (= (happy-number? 2) false))
  (is (= (happy-number? 3) false)))

(deftest symmetric-difference-test
  (is (= (symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
  (is (= (symmetric-difference #{:a :b :c} #{}) #{:a :b :c}))
  (is (= (symmetric-difference #{} #{4 5 6}) #{4 5 6}))
  (is (= (symmetric-difference #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(deftest cartesian-product-test
  (is (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
         #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
           ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
           ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
  (is (= (cartesian-product #{1 2 3} #{4 5})
         #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
  (is (= 300 (count (cartesian-product (into #{} (range 10))
                                       (into #{} (range 30)))))))

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

(deftest read-roman-numerals-test
  (is (= 14 (read-roman-numerals "XIV")))
  (is (= 827 (read-roman-numerals "DCCCXXVII")))
  (is (= 3999 (read-roman-numerals "MMMCMXCIX")))
  (is (= 48 (read-roman-numerals "XLVIII"))))

(deftest partial-flatten-test
  (is (= (partial-flatten [["Do"] ["Nothing"]])
         [["Do"] ["Nothing"]]))
  (is (= (partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]])
         [[:a :b] [:c :d] [:e :f]]))
  (is (= (partial-flatten '((1 2)((3 4)((((5 6)))))))
         '((1 2)(3 4)(5 6)))))

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

(deftest equivalence-classes-test
  (= (equivalence-classes #(* % %) #{-2 -1 0 1 2})
     #{#{0} #{1 -1} #{2 -2}})
  (= (equivalence-classes #(rem % 3) #{0 1 2 3 4 5 })
     #{#{0 3} #{1 4} #{2 5}})
  (= (equivalence-classes identity #{0 1 2 3 4})
     #{#{0} #{1} #{2} #{3} #{4}})
  (= (equivalence-classes (constantly true) #{0 1 2 3 4})
     #{#{0 1 2 3 4}}))

(deftest product-digits-test
  (is (= (product-digits 1 1) [1]))
  (is (= (product-digits 99 9) [8 9 1]))
  (is (= (product-digits 999 99) [9 8 9 0 1])))

(deftest lcm-test
  (is (== (lcm 2 3) 6))
  (is (== (lcm 5 3 7) 105))
  (is (== (lcm 1/3 2/5) 2))
  (is (== (lcm 3/4 1/6) 3/2))
  (is (== (lcm 7 5/7 2 3/5) 210)))

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

(deftest into-camel-case-test
  (= (into-camel-case "something") "something")
  (= (into-camel-case "multi-word-key") "multiWordKey")
  (= (into-camel-case "leaveMeAlone") "leaveMeAlone"))

(deftest k-combinations-test
  (is (= (k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (is (= (k-combinations 10 #{4 5 6}) #{}))
  (is (= (k-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
  (is (= (k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                           #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
  (is (= (k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
  (is (= (k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                        #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

(deftest write-roman-numerals-test
  (is (= "I" (write-roman-numerals 1)))
  (is (= "XXX" (write-roman-numerals 30)))
  (is (= "IV" (write-roman-numerals 4)))
  (is (= "CXL" (write-roman-numerals 140)))
  (is (= "DCCCXXVII" (write-roman-numerals 827)))
  (is (= "MMMCMXCIX" (write-roman-numerals 3999)))
  (is (= "XLVIII" (write-roman-numerals 48))))

(deftest identify-keys-vals-test
  (is (= {} (identify-keys-vals [])))
  (is (= {:a [1]} (identify-keys-vals [:a 1])))
  (is (= {:a [1], :b [2]} (identify-keys-vals [:a 1, :b 2])))
  (is (= {:a [1 2 3], :b [], :c [4]} (identify-keys-vals [:a 1 2 3 :b :c 4]))))

(deftest number-maze-test
  (is (= 1 (number-maze 1 1)))   ; 1
  (is (= 3 (number-maze 3 12)))  ; 3 6 12
  (is (= 3 (number-maze 12 3)))  ; 12 6 3
  (is (= 3 (number-maze 5 9)))   ; 5 7 9
  (is (= 9 (number-maze 9 2)))   ; 9 18 20 10 12 6 8 4 2
  (is (= 5 (number-maze 9 12)))  ; 9 11 22 24 12
)

(deftest lazy-searching-test
  (is (= 3 (lazy-searching [3 4 5])))
  (is (= 4 (lazy-searching [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
  (is (= 7 (lazy-searching (range) (range 0 100 7/6) [2 3 5 7 11 13])))
  (is (= 64 (lazy-searching (map #(* % % %) (range)) ;; perfect cubes
                            (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                            (iterate inc 20)))) ;; at least as large as 20
  )

(deftest pronunciations-test
  (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciations [1]))))
  (is (= [3 1 2 4] (first (pronunciations [1 1 1 4 4]))))
  (is (= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciations [1]) 6)))
  (is (= 338 (count (nth (pronunciations [3 2]) 15)))))

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

(deftest making-data-dance-test
  (= "1, 2, 3" (str (making-data-dance 2 1 3)))
  (= '(2 1 3) (seq (making-data-dance 2 1 3)))
  (= '(2 1 3) (seq (making-data-dance 2 1 3 3 1 2)))
  (= '(1) (seq (apply making-data-dance (repeat 5 1))))
  (= "1, 1, 1, 1, 1" (str (apply making-data-dance (repeat 5 1))))
  (and (= nil (seq (making-data-dance)))
       (=  "" (str (making-data-dance)))))

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

(deftest balanced-prime?-test
  (is (= false (balanced-prime? 4)))
  (is (= true (balanced-prime? 563)))
  (is (= 1103 (nth (filter balanced-prime? (range)) 15))))

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

(deftest map-clone-test
  (is (= [3 4 5 6 7]
         (map-clone inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
         (map-clone (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
         (->> (map-clone inc (range))
              (drop (dec 1000000))
              (take 2)))))

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

(deftest less-than-sum-squared-digits-test
  (is (= 8 (less-than-sum-squared-digits (range 10))))
  (is (= 19 (less-than-sum-squared-digits (range 30))))
  (is (= 50 (less-than-sum-squared-digits (range 100))))
  (is (= 50 (less-than-sum-squared-digits (range 1000)))))

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

(deftest read-binary-test
  (is (= 0     (read-binary "0")))
  (is (= 7     (read-binary "111")))
  (is (= 8     (read-binary "1000")))
  (is (= 9     (read-binary "1001")))
  (is (= 255   (read-binary "11111111")))
  (is (= 1365  (read-binary "10101010101")))
  (is (= 65535 (read-binary "1111111111111111"))))

(deftest recognize-playing-card-test
  (is (= {:suit :diamond :rank 10} (recognize-playing-card "DQ")))
  (is (= {:suit :heart :rank 3} (recognize-playing-card "H5")))
  (is (= {:suit :club :rank 12} (recognize-playing-card "CA")))
  (is (= (range 13) (map (comp :rank recognize-playing-card str)
                         '[S2 S3 S4 S5 S6 S7
                           S8 S9 ST SJ SQ SK SA]))))

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

(deftest nil-key?-test
  (is (true?  (nil-key? :a {:a nil :b 2})))
  (is (false? (nil-key? :b {:a nil :b 2})))
  (is (false? (nil-key? :c {:a nil :b 2}))))

(deftest infix-calculator-test
  (is (= 7  (infix-calculator 2 + 5)))
  (is (= 42 (infix-calculator 38 + 48 - 2 / 2)))
  (is (= 8  (infix-calculator 10 / 2 - 1 * 2)))
  (is (= 72 (infix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(deftest digits-and-bases-test
  (is (= [1 2 3 4 5 0 1] (digits-and-bases 1234501 10)))
  (is (= [0] (digits-and-bases 0 11)))
  (is (= [1 0 0 1] (digits-and-bases 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)](digits-and-bases n n))))
  (is (= [16 18 5 24 15 1] (digits-and-bases Integer/MAX_VALUE 42))))

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

(deftest oscilrate-test
  (is (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))
  (is (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
  (is (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))

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

(deftest pascals-trapezoid-test
  (is (= (second (pascals-trapezoid [2 3 2])) [2 5 5 2]))
  (is (= (take 5 (pascals-trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
  (is (= (take 2 (pascals-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
  (is (= (take 100 (pascals-trapezoid [2 4 2])) (rest (take 101 (pascals-trapezoid [2 2]))))))

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

(deftest map-defaults-test
  (is (= (map-defaults 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
  (is (= (map-defaults "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
  (is (= (map-defaults [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))

(deftest indexing-seq-test
  (is (= (indexing-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (indexing-seq [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (indexing-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

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

(deftest comparisons-test
  (is (= :gt (comparisons < 5 1)))
  (is (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparisons > 0 2)))
  (is (= :gt (comparisons > 0 2))))

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

(deftest intervals-test
  (is (= (intervals [1 2 3]) [[1 3]]))
  (is (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))
  (is (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))
  (is (= (intervals []) []))
  (is (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
         [[1 4] [6 6] [9 11] [13 17] [19 19]])))

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

(run-tests)
