(ns lwttermgenerator.core)
;;  (:gen-class))

(load-file "src/lwttermgenerator/hjaelpemetoder.clj")
(load-file "src/lwttermgenerator/cedictprep.clj")
(load-file "src/lwttermgenerator/tzaiprep.clj")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(println "hello chr")
;;(println (take 3 cedict_raw))
;;(println (count cedict_raw))

;;lav metode til at laese alle linjer i filen:

;(def fil_indhold
;  (with-open [rdr (clojure.java.io/reader "src/iwttermgenerator/test.txt")]
;    (reduce conj [] (line-seq rdr))))

(def Example
  (slurp "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt" ))
  ;(slurp (java.io.FileReader. "test.txt")))
  ;rintln string1))
;(println (count Example))

(def vectorOfLines_1 (clojure.string/split Example #"[\r\n]+"))
;(println (count vectorOfLines_1))

(def setOfChineseWords (set (filter #(stringHasChinese %) (flatten (map #(lineToWordVec %) vectorOfLines_1)))))

; #(and (> % 11000) (not (= % 12288)))

;(println (count vectorOfLines_1))
;(println (take 5 vectorOfLines_1))

;(println (count setOfChineseWords))
;(println (take 5 setOfChineseWords))

;setOfChineseWords er nu paa 672. jeg skal teste om det matcher det reelle tal:
(def dictlookupOfWords (map
                         ;;#(cedictTradHash %)
                         (fn [eachWord]
                           (let [eachWordInfoVec (cedictTradHash eachWord)]
                             (cond
                               (= nil eachWordInfoVec) eachWord
                               (= 4 (count eachWordInfoVec)) eachWordInfoVec
                               :else eachWord
                               )
                             ))
                         setOfChineseWords))

;(println (count dictlookupOfWords))

;(println (type "gfd"))
;(println (filter
;           #(not (vector? %))
;           dictlookupOfWords))

;(println (take 3 dictlookupOfWords))
;([響應 响应 [xiang3 ying4] /to respond to/answer/CL:個|个[ge4]/] [老兄 老兄 [lao3 xiong1] /'old chap' (form of address between male friends)/] [七 七 [qi1] /seven/7/])

; tag vectoren med saetningerne (vectorOfLines_1), og match hvert ord  i (setOfChineseWords) til en saetning.
; laeg de fundne par i en map, saa man til sidst faar en vector af maps
; man skal finde kinesiske tegn fra linje 5 og ned (index 4)
;(println "***************** arbejde med nye funktioner ******************")
;(println (take 5 vectorOfLines_1)) ;(--Transkription episode 1 - avengers senest aendret 2016-04-21 kl. 09.00 漢語 鋼鐵俠降臨 "Ironman is born" 01.01 這景色真是太美了)



(comment
(def MapOfChineseLines
  (map-indexed
    (fn [idx currentLine]
      (let [previousLine (get vectorOfLines_1 (rundOpTilNul (- idx 1)))]
        (if
          (and (= java.lang.String (type currentLine))
               (= java.lang.String (type previousLine))
               (stringHasChinese currentLine)
               (stringHasOnlyNumPunktAndNotEmpty previousLine))
          (hash-map :sentenceTraditional currentLine :sentenceTimeOrPage previousLine)
          )
        )
      );[idx currentLine])
    vectorOfLines_1)))

(defn MapOfChineseLines [stringLinesFromFile]
  (filter
    #(not (nil? %))
          (map-indexed
    (fn [idx currentLine]
      (let [previousLine (get vectorOfLines_1 (rundOpTilNul(- idx 1)))]
        (if
          (and (= java.lang.String (type currentLine))
               (= java.lang.String (type previousLine))
               (stringHasChinese currentLine)
               (stringHasOnlyNumPunktAndNotEmpty previousLine)
               )
          (hash-map
            :sentenceTraditional currentLine
            :sentenceSimplified (tradKinStringToSimplifiedKinString currentLine)
            :sentencePinyin (tradKinStringToPinyinKinStringWith* currentLine)
            :sentenceTimeOrPage previousLine
            :sentenceWordInfoVector (tradKinStringToTradWordsInfoString currentLine) ;denne er korrekt men fylder for meget ift. debugging
            )
          )
        )
      )
    stringLinesFromFile)))

;(println (take 3 (MapOfChineseLines vectorOfLines_1)))

;; 2020-07-21
;; opgaver: jeg har nu ordene for de terms jeg skal bruge.
; naeste skridt er at lave en csv fil med ordene.
; ordene skal desuden i en separat fil til naar jeg senere vil generere ord
; fra andre lister og antage at jeg allerede kender ordene fra denne epsiode.
; CSV fil:
; ordTraditionelt ordSimplificeret OrdPinyin ordOversaettelse ordLydfilFilstig
; liste af komponenttegn med pinyin simp og betydning (deres cedict entries - i 1 kolonne)
; saetningTraditionelt saetningSimplificeret saetningPinyin saetningOversaettelse(bareOversaettelseAfHvertOrd) saetningLydfilStig


(comment
  (defn wordsSortedByFreq [setOfWords]
    (map
      (fn [eachWord]
        (vector eachWord
                (reverse
                  (sort
                    (map (fn [eachChar]
                           (get tzai_hashmap (str "" eachChar));tzai_hashmap
                           ) eachWord))))
        );tzai_hashmap
      setOfWords))
  )

(defn wordsSortedByFreq_beta [setOfWords]
  (map
    (fn [eachWord]
      (vector eachWord
              (vec
                (reverse
                  (sort
                    (map (fn [eachChar]
                           (get tzai_hashmap (str "" eachChar));tzai_hashmap
                           ) eachWord)))))
      );tzai_hashmap
    setOfWords))

;lav en funktion der tager et set af ord, og sortere dem efter hvor almindelige tegnene er (ord med sjaeldnine tegn tilsidst).
;den laeser listen af de traditionelle tegn og laver dem om til en map af ord ordnet efter de sjaeldneste tegns frekvens
(defn wordsSortedByFreq [setOfWords]
  (map #(first %)
  (sort-by second (map
    (fn [eachWordAndNumVector]
      (vector
        (get eachWordAndNumVector 0)
        (clojure.string/join "" (map #(numberToCharInCJKBlock %) (get eachWordAndNumVector 1)))
      ))
  (map
    (fn [eachWord]
      (vector eachWord
              (vec
              (reverse
              (sort
              (map (fn [eachChar]
           (get tzai_hashmap (str "" eachChar));tzai_hashmap
           ) eachWord))))
              )
    );tzai_hashmap
    setOfWords)))))

;numberToCharInCJKBlock
;foerste kinesiske tegn i unicode blokken: 19968

;setOfChineseWords
;(println (count (wordsSortedByFreq_beta (wordsSortedByFreq setOfChineseWords))))
;(println (wordsSortedByFreq_beta (wordsSortedByFreq setOfChineseWords)))
;(println (wordsSortedByFreq setOfChineseWords))

;lav en funktion der tager en saetning og et set af ord, og returnere det sjaeldneste ord fra saettet der findes isaetningen.
(defn getRareestWordListFromString [inputKinString]
  (reverse (wordsSortedByFreq (lineToWordVec inputKinString ))
  ))

;(println "你就不能放鬆點兒嗎")
;(println (getRareestWordListFromString "你就不能放鬆點兒嗎"))

(defn sortVectorOfMapsByCharFrequency [vectorOfMaps relevantKeyword]
  ;(sort-by #(map charInCJKBlockToNumber (relevantKeyword %)) vectorOfMaps);你就不能放鬆點兒嗎
  (sort-by relevantKeyword vectorOfMaps)
  )

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

;jeg har nu en funktion der kan reverse sortere. Det jeg skal nu er at lave ne funktion der
;kan tage en vector af maps, hvor hver map er en linje og dens information. Denne vector skal saa kunne sorteres.

; **********



;lav en funktion der tager en liste af maps, hvor hver map er MapOfChineseLines, og sortere dem efter
;de sjaeldneste ords forekomst.

;lav en funktion der tager en saetning og et setAfOrd, og derefter fjerne et ord fra settet og laegger det i en vector,
;hvor saetningen er foerste item i vectoren. og ordet er i en sepparat vector der er item 2.
;(defn takeWordFromSet [])

;lav en funktion der tager et saet af ord og en liste af ordnede saetninger,
; og fordeler ordene paa saetningerne (ligelidt?)
;ideen er at ordene der hoere til saetningen bliver lagt ind i en vector, med saetningen som vectores foerste item
(comment
(defn fordelOrdPaaSaetninger [setOfWords, vectorOfSentences]
  (map
    (fn [eachSentence]
      ()
      )
    vectorOfSentences)
  ))

(println "slut paa programmet")




;;slut
