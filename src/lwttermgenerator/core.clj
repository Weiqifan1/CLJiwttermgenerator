(ns lwttermgenerator.core)

(load-file "src/Dictionaries/cedictprep.clj")
(load-file "src/Dictionaries/preptzai.clj")
(load-file "src/lwttermgenerator/tzaiprep.clj")
(load-file "src/lwttermgenerator/moduleDistributeWordsAmongSentences.clj")
(load-file "src/StoryAnalysing/readInfoFromFile.clj")

(load-file "src/Helpermethods/hjaelpemetoder.clj")
(load-file "src/ParsingStories/storyToDatastructure.clj")
(load-file "src/DatastructuresToFiles/dataToFileFirstTry.clj")
;moduleDistributeWordsAmongSentences.clj

;; 2020-09-27
;jeg kan nu fint skrive en fil der kan generere tegn og pinyin Anki kort.
;dog laver jeg mange dubletter ifoelge anki. Jeg skal lave en funktion
;tager full story og fjerne linjer der har vaeret der tidligere,
;men tilfoejer de slettede linjers minut tal til den foerste linjes minut tal linje

;herefter skal jeg kunne redigere skriftstoerelse paa mine kort (kan maaske goeres i anki)

;herefter skal jeg finde ud af at tilfoeje lydfiler til mine kort

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
;(writeExample)

;;(println (take 3 cedict_raw))
;;(println (count cedict_raw))


(def Example
  (slurp "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt" ))

(def vectorOfLines_1 (clojure.string/split Example #"[\r\n]+"))
;(println (count vectorOfLines_1))

(def setOfChineseWords (set (filter #(stringHasChinese %) (flatten (map #(lineToWordVec %) vectorOfLines_1)))))
; #(and (> % 11000) (not (= % 12288)))
;(println (count vectorOfLines_1))
;(println (take 5 vectorOfLines_1))
;(println (count setOfChineseWords))
;(println (take 5 setOfChineseWords))

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
