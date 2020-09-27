
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;stackoverflow:
;https://stackoverflow.com/questions/10723451/whats-the-one-level-sequence-flattening-function-in-clojure
(defn flatten-one-level [coll]
  (mapcat  #(if (sequential? %) % [%]) coll))


;(. java.lang.Character toCodePoint (first %) (second %))

;;remove newline from end of string
(defn removeNewlineFromEnd [inputString]
  (clojure.string/replace inputString #"\s+$" ""))
;(println (count (removeNewlineFromEnd "a bc \n")))

;remove komma, semikolon, tab and double quote
(defn removeAnkiSpecialChars [inputString]
  (clojure.string/replace inputString #"[,;\t\"]" "")
  )
;(println (removeAnkiSpecialChars "ksjd\tkf;s,s\"f"))

(defn removeAnkiSpecialCharsAndTrailingNewLine [inputString]
  (removeNewlineFromEnd
    (removeAnkiSpecialChars inputString)
    ))
;(println (count (removeAnkiSpecialCharsAndTrailingNewLine "ks\njd\tkf;s,s\"f\n"))) ;should be 10

;;lav en funktion der tager en kinesisk linje samt dens laengde
;;og returnere en vector af linjen med flere og flere bogstaver fjernet
;;(defn shrinkingChineseLineVec [chineseLine lineLength]
;; (map #(subs "看見那東西了嗎" 0 %)
(defn helper_shrinkingChinese [chineseLine]
  (conj (reverse (map #(subs chineseLine 0 %) (drop 1 (range (count chineseLine))))) chineseLine)
  )
;(println (shrinkingChinese "看見那東西了嗎"))

(defn helper_parse-long
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^\d+$" s);(if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))
;(println (helper_parse-long "34"))

(defn helper_filter-by-index [coll idxs]
  (keep-indexed #(when ((set idxs) %1) %2) coll))
;(def testhelper_filter-by-index (list "A", "B", "C", "D", "E", "F", "G", "H"))
;(println (helper_filter-by-index testhelper_filter-by-index (list 0 3 4)))

(defn helper_splitToCodepointChar
  ;"A腦𡳞𨨏B乃匯C𨭎" [A 腦 𡳞 𨨏 B 乃 匯 C 𨭎]
  [kinstring]
  ((comp
     #(clojure.string/split % #" ")
     #(clojure.string/join "" %)
     (fn [x] (map
               #(if
                  (and (>= (int %) 55296) (<= (int %) 56319))
                  % (str % " ")) x))
     ) kinstring))
;(println (helper_splitToCodepointChar "A腦𡳞𨨏B乃匯C𨭎"))

(defn helper_splitToSurogatePair
  ;"A腦𡳞𨨏B乃匯C𨭎" ((65) (33126) (55367 56542) (55394 56847) (66) (20035) (21295) (67) (55394 57166))
  [kinstring]
  ((comp
     (fn [x] (map #(map int %) x))
     #(helper_splitToCodepointChar %)
     ) kinstring))
;(println (helper_splitToSurogatePair "A腦𡳞𨨏B乃匯C𨭎"))

(defn helper_splitToCodepointVec
  ;"A腦𡳞𨨏B乃匯C𨭎" (65 33126 138462 166415 66 20035 21295 67 166734)
  [nested]
  (cond
    (= "" nested) '()
    (= nil nested) '()
    :else
    (map
    #(if
       (and (>= (int (first (str % " "))) 55296) (<= (int (first (str % " "))) 56319))
       (. java.lang.Character toCodePoint (first %) (second %))
       (int (first (str % " "))))
    (helper_splitToCodepointChar
      nested))))
;(println (helper_splitToCodepointVec "A腦𡳞𨨏B乃匯C𨭎"))


;funktion til at omdanne et tal til et tegn: den tager et tal, og omdanner det til
;et tegn fra CJK blokken (1 -> 一 (19968) )
(defn helper_numberToCharInCJKBlock [unicodeOrdinalCedimal] (char (+ unicodeOrdinalCedimal 19967)))
;(println (helper_numberToCharInCJKBlock 1))

;her er en funktion der goer det omvedte af ovenstaaende. Den tager et tegn fra
;CJK blokken (unicode 19968 og frem) og laver den om til et tal
(defn helper_charInCJKBlockToNumber [charInCLKBlock] (- (int charInCLKBlock) 19967))
;(println (helper_charInCJKBlockToNumber (first "一")))


(defn helper_stringHasChinese [inputstring]
    (cond
      (= inputstring nil) false
      (= inputstring "") false
      :else  (< 0 (count (filter #(and (> % 11000) (not (= % 12288))) (helper_splitToCodepointVec (str "" inputstring)))));hvis over 11.000 = ikke kinesisk
  ))
;(println (helper_stringHasChinese ""))                   ;false
;(println (helper_stringHasChinese "gfd234"))             ;false
;(println (helper_stringHasChinese "fds3\uD862\uDE0F25")) ;true
;(println (helper_stringHasChinese "gfd2腦34"))           ;true

(defn helper_stringHasOnlyNumPunktAndNotEmpty [inputstring]
  (and (< 0 (count (str "" inputstring)))
       (= 0
          (count (filter
                   #(< 64 %) ;return true hvis alle tegn er
                   (helper_splitToCodepointVec (str "" inputstring))))
          )
       )
  )
;(println (helper_stringHasOnlyNumPunktAndNotEmpty "023.,+@")) ;true
;(println (helper_stringHasOnlyNumPunktAndNotEmpty "023.,+A")) ;false

(defn helper_compareLength
  [a b] (
          if (= (count a) (count b))
          (compare a b)
          (if (and (= (count a) 1) (> (count b) 1))
            -1
            (if (and (= (count b) 1) (> (count a) 1))
              1
              (if (> (count a) (count b))
                -1
                1
                )
              )
            )
          ))
;(println (helper_compareLength "abc" "abc"))  ;0
;(println (helper_compareLength "abcd" "abc")) ;-1
;(println (helper_compareLength "abc" "abcd")) ;1

;***************************** sorting functions ***************************

;sorting functions to dictionaries
(defn compare-tzai-secondary [a b]
  (if (and (= (get a :tzai) nil) (not= (get b :tzai) nil))
    1
    (if (and (= (get b :tzai) nil) (not= (get a :tzai) nil))
      -1
      (if (and (not= (get a :tzai) nil) (not= (get b :tzai) nil))
        (compare (get a :tzai) (get b :tzai))
        (compare (get a :ordinal) (get b :ordinal))
        )
      )
    )
  )

(defn compare-junda-secondary [a b]
  ;(sort-by (juxt :junda :tzai :cedictsimp :cedicttrad :ordinal) x)
  (if (and (= (get a :junda) nil) (not= (get b :junda) nil))
    1
    (if (and (= (get b :junda) nil) (not= (get a :junda) nil))
      -1
      (if (and (not= (get a :junda) nil) (not= (get b :junda) nil))
        (compare (get a :junda) (get b :junda))
        (compare (get a :ordinal) (get b :ordinal));(compare (get a :tzai) (get b :tzai))
        )
      )
    )
  )

(defn helper_compare-tzai [a b]
  (if (and (= (get a :tzai) nil) (not= (get b :tzai) nil))
    1
    (if (and (= (get b :tzai) nil) (not= (get a :tzai) nil))
      -1
      (if (and (not= (get a :tzai) nil) (not= (get b :tzai) nil))
        (compare (get a :tzai) (get b :tzai))
        (compare-junda-secondary a b)
        )
      )
    )
  )

(defn helper_compare-junda [a b]
  ;(sort-by (juxt :junda :tzai :cedictsimp :cedicttrad :ordinal) x)
  (if (and (= (get a :junda) nil) (not= (get b :junda) nil))
    1
    (if (and (= (get b :junda) nil) (not= (get a :junda) nil))
      -1
      (if (and (not= (get a :junda) nil) (not= (get b :junda) nil))
        (compare (get a :junda) (get b :junda))
        (compare-tzai-secondary a b);(compare (get a :tzai) (get b :tzai))
        )
      )
    )
  )

;lav en funktion der tager to tal og returnere det stoerste
(defn lengthOfShortestList [list1 list2]
  (if
    (> (count list1) (count list2))
    (count list2)
    (count list1)
    )
  )
;(println (lengthOfShortestList (vector 1 2 3) (vector 4 7 3 2 1)))

;(defn compareListsOfTzai [listA listB]
;  (if
;    (and (not (= nil %)) (< 0 (count %)))
;    (first %)
;    0
;    (filter
;      #(not (= 0 %))
;      (map #(compare (nth listA %) (nth listB %))
;           (range (lengthOfShortestList listA listB)))))
;  )

;sorting vectors of tzai numebers for use by contentVecGnerator
(defn compareListsOfTzai [listA listB]
  ((comp
    (fn [comparisonsNotNull] (if
                                (and (not (= nil comparisonsNotNull)) (< 0 (count comparisonsNotNull)))
                                (first comparisonsNotNull)
                                0))

     (fn [listOfComparisons] (filter #(not (= 0 %)) listOfComparisons))
     (fn [rangeOfIndexes] (map #(compare (nth listA %) (nth listB %)) rangeOfIndexes))
    )(range (lengthOfShortestList listA listB)))
  )
;(println (compareListsOfTzai [978 219 219 143 21 19] [878 348 219 156 95 17 10 2]))


;[235 219 143 21 19]
;[878 348 219 156 95 17 10 2]
;[1392 418 298 102 89 30 26 19 3]
;[1343 1105 776 399 199 190 26 19 10 9]

;(defn shortestListIndexes [list1 list2]
;  ([let shortesLength (lengthOfShortestList list1 list2)]
;   (range 0 shortesLength)
;   )
;  )
;(println (lengthOfShortestList (vector 1 2 3) (vector 4 7 3 2 1)))



(defn helper_rundOpTilNul
  [Heltal]
  (if
    (> 0 Heltal)
    0
    Heltal)
  )
;(println (helper_rundOpTilNul 1))
;(println (helper_rundOpTilNul 0))
;(println (helper_rundOpTilNul -1))

(defn helper_CollItemOrEmptyVec
  [listOfItems index]
  (if
    (> 0 index)
    []
    (nth listOfItems index))
  )
;(println (helper_CollItemOrEmptyVec [10 11 12 13 14 15] 3))
;(println (helper_CollItemOrEmptyVec [10 11 12 13 14 15] -1))



;slut