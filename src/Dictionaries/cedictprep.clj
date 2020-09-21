(load-file "src/Helpermethods/hjaelpemetoder.clj")
;(load-file "src/lwttermgenerator/hjaelpemetoder.clj")

(def filstier_stat (vector "Junda2005.txt" "Tzai2006.txt" "src/lwttermgenerator/cedictimproved.txt"))

(def cedict_raw_1 (clojure.string/split (slurp (get filstier_stat 2)) (re-pattern "\\n")))
(def cedict_raw_1b (map clojure.string/trim cedict_raw_1))
(def cedict_raw_1c (map #(clojure.string/split % (re-pattern "###")) cedict_raw_1b))
(def cedict_raw (identity cedict_raw_1c))

;; lav en funktion der laver cedict_raw om til en traditionel hashmap
;; (det vil ikke nytte med en simplificeret hashmap. jeg vil f.eks. ikke kunne vide om 发 er 發 eller 髮)
;; der burde derimod ikke vaere flere simplificerede tegn der referere til 1 traditionelt.
;; form: vector med 4 entries:
;; <traditionel> <simplificeret> [Fan4 Wei3 qi2] /Christine Fan (1976-), American-born Taiwanese singer and actress/
(def cedictTradHash
  ((comp
     #(apply hash-map %)
     #(apply concat %)
     (fn [cedictData] (map #(vector (get % 0) %) cedictData))
    )cedict_raw))
;(println (count cedictTradHash))
;(println (take 6 cedictTradHash))
;(println (cedictTradHash "范瑋琪"))
;(def enEntry (cedictTradHash "范瑋琪"))
;(println (type enEntry))
;(println (vector? enEntry))
;(println (count enEntry))
;(println (str "" (get enEntry 0)))
;(println (str "" (get enEntry 1)))
;(println (get enEntry 2))
;(println (get enEntry 3))
;(println (count (get enEntry 0)))



;(defn lineToFirstWord [chineseLine]
;  (first (filter #(not (nil? %)) (map #(cedictTradHash %) (helper_shrinkingChinese chineseLine))))
;  )

;;lav en funktion der tager en linesisk linje samt cedictTradHash
;;og returnere en vector med foerste ord og resten af saetningen
;;(defn lineToFirstWordAndRest [chineseLine] ())
(defn lineToFirstWord [chineseLine]
  (let [naiveAttempt (first (filter #(not (nil? %)) (map #(cedictTradHash %) (helper_shrinkingChinese chineseLine))))]
    (cond
      (= 0 (count chineseLine)) []
      (nil? naiveAttempt) (vector (first chineseLine))
      (nil? (get naiveAttempt 0)) (vector (first chineseLine));;(lineToFirstWord (subs 1 chineseLine))
      :else (vector (get naiveAttempt 0))
      )
   )
  )
;(println (lineToFirstWord "看見那東西了嗎"))
;(println (lineToFirstWord "拜託 老兄 你就別丟人了"))
;(println (lineToFirstWord "哦 不 救命啊"))
;(println (lineToFirstWord "rt 是 先生 dfg "))
;(println (lineToFirstWord " 是 bla"))
;(println (lineToFirstWord ""))
;[看見]
;[拜託]
;[哦]
;[r]
;[ ]
;[]

;(defn lineToFirstWordAndRestVec [chineseLine]
;  (vector (get (lineToFirstWord chineseLine) 0) (subs chineseLine (count (get (lineToFirstWord chineseLine) 0)))))

(defn lineToFirstWordAndRestVec [chineseLine]
  ;(count (get (lineToFirstWord chineseLine) 0))
  (let [lineToFirstWord (lineToFirstWord chineseLine)]
    (cond
      (= 0 (count chineseLine)) ["", ""]
      :else [(get lineToFirstWord 0), (subs chineseLine (count (str "" (get lineToFirstWord 0))))]
      )
    )
  )

;(println "/////////////////////////")

;(println (lineToFirstWordAndRestVec "看見那東西了嗎"))
;(println (lineToFirstWordAndRestVec "拜託 老兄 你就別丟人了"))
;(println (lineToFirstWordAndRestVec "哦 不 救命啊"))
;(println (lineToFirstWordAndRestVec "rt 是 先生 dfg "))
;(println (lineToFirstWordAndRestVec " 是 bla"))
;(println (lineToFirstWordAndRestVec ""))

;(println "********************")
;(println (lineToFirstWordAndRestVec "看見那東西了嗎"))
;(println (lineToFirstWordAndRestVec "ert"))

;; lav en metode der tager en kinesisk linje samt cedictTradHash og returnere
;; en vector af ord
;;(defn lineToWordVec [chineseLine]
;;  ((while
;;    (< 0 (count (get (lineToFirstWordAndRestVec chineseLine) 1)))
;;    (lineToFirstWordAndRestVec chineseLine)
;;    )
;;  )

(defn lineToWordVec [chineseLine]
  (loop [chineseLine chineseLine
         outputVector []]
   (if
     (and (< 0 (count chineseLine)) (not (nil? (get (lineToFirstWordAndRestVec chineseLine) 0))))
     (recur
       (get (lineToFirstWordAndRestVec chineseLine) 1)
       (conj outputVector (get (lineToFirstWordAndRestVec chineseLine) 0))
       )
     outputVector
     )
   )
  )
;(println (lineToWordVec "看見那東西了嗎"))
;(println (lineToWordVec "拜託 老兄 你就別丟人了"))
;(println (lineToWordVec "哦 不 救命啊"))
;(println (lineToWordVec "是 先生 dfg "))

;(println "her starter cedictprep")

(defn linetoUniqueWordsAndChars [kinString]
  (filter #(helper_stringHasChinese %)
  (distinct
    (let [lineToWordVec (lineToWordVec kinString)]
    (flatten
      (map
        (fn [wordOrChar] (if
                           (< 1 (count (str "" wordOrChar)))
                           (conj (vector wordOrChar) (clojure.string/split wordOrChar #""))
                           wordOrChar))
        lineToWordVec
        ))
    )))
  )
;(println (linetoUniqueWordsAndChars "了看見那東西了嗎")) ;the last 了 is not included


;(println "--------- cedictprep slut --------------")


(defn linetoWordAndAllChars [kinString]
  (let [lineToWordVec (lineToWordVec kinString)]
    (flatten
    (map
      (fn [wordOrChar] (if
                    (< 1 (count (str "" wordOrChar)))
                    (conj (vector wordOrChar) (clojure.string/split wordOrChar #""))
                    wordOrChar))
      lineToWordVec
      ))
    )
  )

;(println (linetoWordAndAllChars "看見那東西了嗎"))
;(println (linetoWordAndAllChars "拜託 老兄 你就別丟人了"))
;(println (linetoWordAndAllChars "哦 不 救命啊"))
;(println (linetoWordAndAllChars "是 先生 dfg "))
;(println (count (linetoWordAndAllChars "是 先生 dfg ")))

; lav en funktion der tager en string og returnere en string med simplificerede tegn (andre tegn. f.eks. kommatering skal vaere uaendret)
(defn tradKinStringToSimplifiedKinString
  [stringWithTradCharacters]
  (let [wordAndChrVector (lineToWordVec stringWithTradCharacters)]
    (clojure.string/join "" (map
      (fn [eachWordOrChar]
        (cond
          (not (= nil (cedictTradHash eachWordOrChar))) (get (cedictTradHash eachWordOrChar) 1)
          :else eachWordOrChar))
      wordAndChrVector)
    )
  ))

;(println (tradKinStringToSimplifiedKinString "這景色真是太美了"))
;(println (tradKinStringToSimplifiedKinString "拜託,hr lykke老兄 你就別丟人了"))
;(println (cedictTradHash "拜託"))
;(println (cedictTradHash "這"))

(defn tradKinStringToPinyinKinStringWith*
  [stringWithTradCharacters]
  (let [wordAndChrVector (lineToWordVec stringWithTradCharacters)]
    (clojure.string/join "*" (map
                              (fn [eachWordOrChar]
                                (cond
                                  (not (= nil (cedictTradHash eachWordOrChar))) (get (cedictTradHash eachWordOrChar) 2)
                                  :else eachWordOrChar))
                              wordAndChrVector)
                         )
    ))

;(println (tradKinStringToPinyinKinStringWith* "這景色真是太美了"))
;(println (tradKinStringToPinyinKinStringWith* "拜託,hr lykke老兄 你就別丟人了"))

(defn tradKinStringToTradWordsInfoStringLimmited
  [stringWithTradCharacters]
  (cond
    (= nil stringWithTradCharacters) ""
    (= "" stringWithTradCharacters) ""
    :else
  (let [wordAndChrVector (lineToWordVec stringWithTradCharacters)]
    (clojure.string/join ""
    (map #(str % " \n")
    (filter #(not (= nil %))
    (map #(cedictTradHash %)  (distinct (linetoWordAndAllChars stringWithTradCharacters))) ;wordAndChrVector  ;linetoWordAndAllChars
    ))))))
;(println (tradKinStringToTradWordsInfoStringLimmited "了這景色真是太美了"))
;(println (tradKinStringToTradWordsInfoStringLimmited "拜託,hr lykke老兄 你就別丟人了"))
;(println (tradKinStringToTradWordsInfoStringLimmited ""))
;(println (tradKinStringToTradWordsInfoStringLimmited nil))

;lav en funktion der viser info om alle ord og alle tegn i ordene UANSET om tegnet/ordet er set foer
(defn tradKinStringToTradWordsInfoStringFull
  [stringWithTradCharacters]
  (cond
    (= nil stringWithTradCharacters) ""
    (= "" stringWithTradCharacters) ""
    :else
    (let [wordAndChrVector (lineToWordVec stringWithTradCharacters)]
      (clojure.string/join ""
                           (map #(str % " \n")
                                (filter #(not (= nil %))
                                        (map #(cedictTradHash %)  (linetoWordAndAllChars stringWithTradCharacters)) ;wordAndChrVector  ;linetoWordAndAllChars
                                        ))))))
;(println (tradKinStringToTradWordsInfoStringFull "了這景色真是太美了"))
;(println (tradKinStringToTradWordsInfoStringFull "拜託,hr lykke老兄 你就別丟人了"))

;; naeste opgave: lav en funktion der laeser avengers filen,




;;slut
