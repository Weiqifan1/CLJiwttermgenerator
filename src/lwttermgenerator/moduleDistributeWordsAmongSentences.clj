(load-file "src/lwttermgenerator/hjaelpemetoder.clj")
(load-file "src/lwttermgenerator/cedictprep.clj")
(load-file "src/lwttermgenerator/tzaiprep.clj")

(println "moduleDistributeWordsAmongSentences.clj")

(def testeMap
  (vector
    (hash-map :keyWordString "一郝的" )
    (hash-map :keyWordString "有禿是" )
    (hash-map :keyWordString "不淪大" )
    (hash-map :keyWordString "我剝在" )
    ))
;(println (sortVectorOfMapsByCharFrequency testeMap :keyWordString))

; 2020-08-02 kl. 17.08
; lav en funktion der tager en saetning og laver tegnene om til frekvenstegn:
;2020-08-02 kl. 17.36 lav en funktion der tager et tegn og laver det om til et CJKBlock tegn>
(defn charToCharInCJKBlock [ordinaryChar] (numberToCharInCJKBlock (get tzai_hashmap (str "" ordinaryChar))))

(println (charToCharInCJKBlock "不"))
;lav en funktion der tager en saetning og returnere en liste af tegn i CLKBlock
(defn sentenceToReverseSortedCharsInCLKBlock [ordinarySentence] (reverse (sort (map #(charToCharInCJKBlock %) ordinarySentence))))

(println (sentenceToReverseSortedCharsInCLKBlock "有禿是"))
(println (map charInCJKBlockToNumber (sentenceToReverseSortedCharsInCLKBlock "有禿是")))

;lav en funktion der tager en liste af saetninger og reverse sortere dem efter tegnenes frekvens
(defn sortSentencesByReverseCharSequence [vectorOfSentences]
  (map first
       (reverse
         (sort-by second
                  (map #(vector (nth % 0) (vec (sentenceToReverseSortedCharsInCLKBlock (nth % 1))))
                       (map (fn [eachSentence] (vector eachSentence eachSentence)) vectorOfSentences))))))

(def chrTestVector (vector "一郝的" "不淪大" "我剝在" "有禿是"))
;(println chrTestVector)
;(println (map (fn [eachString] (map charInCJKBlockToNumber (sentenceToReverseSortedCharsInCLKBlock eachString))) chrTestVector))
;(println (sortSentencesByReverseCharSequence chrTestVector))




;slut