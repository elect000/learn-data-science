(ns learn-data-science.core
  (:gen-class))

;; 適当なデータです。
(def data {:data [] :describe []})

;; 平均 : xs ... x-sequence
(defn mean [xs]
  (/ (reduce + xs)
     (count xs)))

;; 中央値
(defn median [xs]
  (let [n (count xs)
        mid (int (/ n 2))
        sorted-xs (sort xs)]
    (if (odd? n)
      (nth sorted-xs mid)
      (-> sorted-xs
          (drop (dec mid))
          (take 2)
          (mean)))))

;;;; 二乗
(defn sq [x]
  (* x x))

;; 分散
(defn variance [xs]
  (let [xs-mean (mean xs)
        n (count xs)
        square-deviation (fn [x]
                           (sq (- x xs-mean)))]
    (mean (map square-deviation xs))))

;; 標準偏差
(defn standard-deviation [xs]
  (java.lang.Math/sqrt (variance xs)))

;; 分位数 : これは近似的な表し方です
(defn quantile [q xs]
  (let [n (dec (count xs))
        i (-> (* n q)
              (+ 1/2)
              (int))]
    (nth (sort xs) i)))

