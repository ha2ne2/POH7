;;;; 2016-01-14
;; turime gantai
;; short-hair long-hair pony-tail twin-tail
;; sailor cardigan kneesox
;; megane santa mizugi

(defn read-as-int [] (read-string (read-line)))
(defn read-as-ints [] (map read-string (clojure.string/split (read-line) #" ")))

;;;; Annを指定回数繰り返す
;; (turime 3)
;;=>"AnnAnnAnn"
(defn turime [n]
  (apply str (repeat n "Ann")))
;; ((comp println turime read-as-int))


;;;; 古本屋で持っていない本を買う
;; (gantai '((1 3 4) (2 3 5)))
;; => "2 5"
(defn gantai-reader []
  (let [n (read-as-int)
        _ (read-as-int)
        m1lst (read-as-ints)
        _ (read-as-int)
        m2lst (read-as-ints)]
    (list m1lst m2lst)))

(defn lst-to-str [lst]
  (if (empty? lst)
    "None"
    (clojure.string/join " " lst)))

(defn gantai [[xs ys]]
  (lst-to-str (sort < (filter (complement (into #{} xs)) ys))))
;; ((comp println gantai gantai-reader))


;;;; 二桁の足し算
(defn short-hair []
  (println (+ (read-as-int) (read-as-int))))
;; (short-hair)

;;;; yesかnoか多い方を出力する
(defn long-hair []
  (let [votes (doall (repeatedly 5 read-line))
        yes-count (count (filter #(= "yes" %) votes))]
    (println (if (>= yes-count 3) "yes" "no"))))
;; (long-hair)

;;;; カウントダウン、ただし0の時は強調させる
(defn pony-tail []
  (dorun (map println
              (reverse (concat '("0!!") (range 1 (+ 1 (read-as-int))))))))
;; (pony-tail)


;;;; カフェインコスパの良いエナジードリンクを示す
(defn twin-tail []
  (let [[c1 p1] (read-as-ints)
        [c2 p2] (read-as-ints)]
    (println (if (> (/ c1 p1) (/ c2 p2)) 1 2))))
;; (twin-tail)
;;; input sample
;; 200 250
;; 180 200


;;;; 文字列を_で結合させる
(defn sailor []
  (let [n (read-as-int)
        words (doall (repeatedly n read-line))]
    (println (clojure.string/join "_" words))))
;; (sailor)
;; imput sample
;; 3
;; paiza
;; online
;; hackathon


;; 階乗を出力する
(defn cardigan [n]
  (reduce * (range 1 (+ n 1))))
;; ((comp println cardigan read-as-int))


;; 指定された長さの縞模様を作る
(defn kneesox [width length]
  (apply str
         (take length
               (mapcat identity
                       (repeat (concat (repeat width \R) (repeat width \W)))))))
;; (println (kneesox (read-as-int) (read-as-int)))
;; => RRRWWWRRRW


;;;; 画像Bが画像Aに含まれている。
;;;; 画像Bの画像Aにおける開始ピクセル位置を返せ。
(defn megane-reader []
  (let [n (read-as-int)
        nlst (doall (repeatedly n read-as-ints))
        m (read-as-int)
        mlst (doall (repeatedly m read-as-ints))]
    (list nlst mlst)))

(defn cut [lsts x y size]
  (let [tmp (map #(nthrest % y) (nthrest lsts x))]
    (map #(take size %) (take size tmp))))

(defn megane [[nlsts mlsts]]
  (first
   (for [x (range 0 (+ 1 (- (count nlsts) (count mlsts))))
         y (range 0 (+ 1 (- (count nlsts) (count mlsts))))
         :let [size (count mlsts)]
         :when (= (cut nlsts x y size) mlsts)]
     (str x " " y))))
;; ((comp println megane megane-reader))
;;;; input sample
;; 4
;; 0 0 1 0
;; 0 1 1 0
;; 0 1 0 1
;; 1 1 1 0
;; 3
;; 0 1 1
;; 0 1 0
;; 1 1 1
;; (megane '(((0 0 1 0) 
;;            (0 1 1 0) 
;;            (0 1 0 1) 
;;            (1 1 1 0))
;;           ((0 1 1)
;;            (0 1 0)
;;            (1 1 1))))
;; => "1 0"


;;;; 豆腐を切る。一番小さい豆腐の体積を返す。
(defn santa-reader []
  (let [[x y z n] (read-as-ints)
        cut-info (doall (repeatedly n read-as-ints))
        xs (sort < (map second (filter #(= 0 (first %)) cut-info)))
        ys (sort < (map second (filter #(= 1 (first %)) cut-info)))]
    [`(0 ~@xs ~x) `(0 ~@ys ~y) z]))

(defn min-interval [xs]
  (loop [[f s :as xs] xs
         min 10000000N]
    (if (nil? s) min
        (recur (rest xs)
               (if (< (- s f) min) (- s f) min)))))

(defn santa [[xs ys z]]
  (* (min-interval xs) (min-interval ys) z))
;; ((comp println santa santa-reader))


;;;; 1000000以下の階乗を求める。ただし下位の0は切り捨てる
(defn mizugi [n]
  (loop [m 1
         acc 1]
    (if (> m n)
      (mod acc 1000000000N)
      (recur (inc m)
             (mod (loop [acc (*' acc m)]
                    (if (= (mod acc 10) 0)
                      (recur (/ acc 10))
                      acc))
                  1000000000000N)))))
;; ((comp println str mizugi read-as-int))

