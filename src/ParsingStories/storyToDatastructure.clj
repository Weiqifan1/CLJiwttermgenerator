(load-file "src/Helpermethods/hjaelpemetoder.clj")
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/Dictionaries/cedictprep.clj")

(def raw_story (clean_story_funk "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"))
;(println (count raw_story))
;(println (take 20 raw_story))

;;HUSK - maalet med programmet er at generere CSV filer!

;;;;;;;;;;;;;;;2020-09-17 kl. 8.23
;se koden i bunden. jeg faar index out of bounds exception

;;lav en funktion der tager en raw story og laver den om til en datastruktur.
;datastrukturen skal have foelgende: (jeg tror jeg har brug for en database!)
;   storyNavn (jeg har overvejet at tage et kapittel med)
;   linjeID
;   linje

;(println (tradKinStringToPinyinKinStringWith* "這景色真是太美了"))
;(println (type (tradKinStringToPinyinKinStringWith* "這景色真是太美了"))) ;String
;   infoVector
;(println (tradKinStringToTradWordsInfoStringFull "了這景色真是太美了"))
;(println (type (tradKinStringToTradWordsInfoStringFull "了這景色真是太美了"))) ;String

;lav en hashmap hvor hver entry er en saetning ->
;
;(println (tradKinStringToTradWordsInfoStringLimmited "這景色真是太美了")) ;String
;(println (type (tradKinStringToTradWordsInfoStringLimmited "這景色真是太美了")))

;(println (take 7 raw_story))
;;;;;;;;;;;;;;;;(keep-indexed #(if (odd? %1) %2) [:a :b :c :d :e])

(def miniStory (take 11 raw_story))

(def storyInfo (vector (nth raw_story 0) (nth raw_story 2)))
;(println storyInfo)
(defn contentVec-bata-1 [inputStory]
  (keep-indexed
    (fn [lineIndex, eachLine]
      (hash-map :line eachLine, :lineID (nth inputStory (helper_rundOpTilNul (- lineIndex 1))))
      )
    inputStory)
  )

;;;; task: create a function similar to  contentVec-bata-2 that creates the information for each word and character
;the information for each word and character must include>
;pinYin
;information
;char

(defn createCharInfoHashMap [inputChar]
  (hash-map
    :char inputChar
    :charInfo (tradKinStringToTradWordsInfoStringFull inputChar)
    :charPinYin (tradKinStringToPinyinKinStringWith* inputChar)
    )
  )

;(println (createCharInfoHashMap "美"))

(defn contentVec-bata-2 [inputStory]
  (map #(hash-map
          :line (get % :line)
          :lineID (get % :lineID)
          :components (linetoUniqueWordsAndChars (get % :line));(map createCharInfoHashMap  (linetoUniqueWordsAndChars (get % :line))); ;outCommented Vector
          :lineInfo (tradKinStringToTradWordsInfoStringFull (get % :line))
          :linePinYin (tradKinStringToPinyinKinStringWith* (get % :line))
          )
  (filter
    (fn [inputLine]
      (and
        (helper_stringHasOnlyNumPunktAndNotEmpty (get inputLine :lineID))
        (helper_stringHasChinese (get inputLine :line))
        )
      )
    (contentVec-bata-1 inputStory))))

(defn contentVecGnerator [inputStory]
  (let [eachLineWithID (contentVec-bata-2 inputStory)]
    (vector
      (nth inputStory 0)
      (nth inputStory 1)
      (nth inputStory 2)
      eachLineWithID
      ;(get :line eachLineWithID)
      ;(get eachLineWithID :components)
    )
  ))

;;;;;;;; lav en funktion der tager en saetning og returnere en hashmap med foelgende:
;saetning
;ord og tegn
;(defn senToSenWordsAndChars [inputSentence]
;  (map
;    (fn [input])
;    inputSentence)
;  )
;(println (linetoUniqueWordsAndChars "了看見那東西了嗎"))


;(def miniStory_2 (take 7 raw_story))
;(println (contentVecGnerator miniStory_2))
;(println (contentVecGnerator raw_story))
(println "********************************")
;(println miniStory_2)
;(println raw_story)
;(println (contentVec-bata-2 miniStory))

(println (type (contentVecGnerator miniStory)))
(println (contentVecGnerator miniStory)) ;virker

;dette kunne jeg nok godt proeve at lave om til en csv film
;foerst skal jeg fjerne dubletter.

;;;;;; - 2020-09-18 kl. 6.36
;;;;;;;lav en funktion der iterere gennem alle components, og accumulere tegn, og fjerne tegn i de efterf'lgende
;;;; linjer hvis de forekommer 2. gang.

;slut