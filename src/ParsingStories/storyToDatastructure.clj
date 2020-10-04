(load-file "src/Helpermethods/hjaelpemetoder.clj")
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/Dictionaries/cedictprep.clj")

(def raw_story (clean_story_funk "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"))
;(println (count raw_story))
;(println (take 20 raw_story))

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
          :storyFirstLine (nth inputStory 0)
          :storySecondLine (nth inputStory 1)
          :storyThirdLine (nth inputStory 2)
          :line (get % :line)
          :lineID (get % :lineID)
          :components (linetoUniqueWordsAndChars (get % :line));(map createCharInfoHashMap  (linetoUniqueWordsAndChars (get % :line))); ;outCommented Vector
          :componentsToBeRemovedIfDublet (linetoUniqueWordsAndChars (get % :line))
          :lineInfo (tradKinStringToTradWordsInfoStringFull (get % :line))
          :linePinYin (tradKinStringToPinyinKinStringWith* (get % :line))
          :lineCharsAndTzaiNumbers (stringToCharsAndTzaiNumber (get % :line))
          :lineSortedTzaiNumbers (vec (reverse (stringToTzaiNumbersSortedNoNil (get % :line))))
          )
  (filter
    (fn [inputLine]
      (and
        (helper_stringHasOnlyNumPunktAndNotEmpty (get inputLine :lineID))
        (helper_stringHasChinese (get inputLine :line))
        )
      )
     inputStory)))

;lav en funktion der sortere contentVec-bata-2 saadan at linjerne med sjaeldne tegn kommer sidst
(defn sortedStoryLinesWithInfo [inputStory]
  (sort-by :lineSortedTzaiNumbers compareListsOfTzai (vec (contentVec-bata-2 inputStory)))
  )

;lav en funktion der tager de sorterede hashmaps og cummulativt fjerne tegn fra :componentsToBeRemovedIfDublet
(defn accumulatedComponentsNestedList [inputStory]
  (let [sortedStoryLines (sortedStoryLinesWithInfo inputStory)
        nestedWordAndCharList (map #(get % :componentsToBeRemovedIfDublet) sortedStoryLines)
        nestedAccumWordAndChar (map
                                 (fn [eachRange]
                                   (flatten (map #(nth nestedWordAndCharList %) eachRange)))
                                 (map #(range (+ % 1))
                                      (range 0 (count nestedWordAndCharList))))]
    (map
      (fn [eachIndex]
        (let [accumulatedWordsAndCharacters (vec (helper_CollItemOrEmptyVec nestedAccumWordAndChar (- eachIndex 1)))
              wordsAndCharsEachLine (vec (nth nestedWordAndCharList eachIndex))]
          ;(identity accumulatedWordsAndCharacters) ;([] [你好 你 好 啊 美女 美 女] [你好 你 好 啊 美女 美 女 這 景色 景 色 真是 真 是 太 美 了] [你好 你 好 啊 美女 美 女 這 景色 景 色 真是 真 是 太 美 了 拜託 拜 託 老兄 老 兄 你 就 別 丟人 丟 人 了])
          ;(identity wordsAndCharsEachLine) ;([你好 你 好 啊 美女 美 女] [這 景色 景 色 真是 真 是 太 美 了] [拜託 拜 託 老兄 老 兄 你 就 別 丟人 丟 人 了] [你 就 不能 不 能 放鬆 放 鬆 點兒 點 兒 嗎])
          (filter
            (fn [eachWordOrChar]
              (not
                (contains? (set accumulatedWordsAndCharacters) eachWordOrChar))
              )
            wordsAndCharsEachLine)
          ))
      (range (count nestedWordAndCharList)))))
;(println (accumulatedComponentsNestedList miniStory))
;((你好 你 好 啊 美女 美 女) (這 景色 景 色 真是 真 是 太 了) (拜託 拜 託 老兄 老 兄 就 別 丟人 丟 人) (不能 不 能 放鬆 放 鬆 點兒 點 兒 嗎))
;characters removed (() (美) (你 了) (你 就))

(defn sortedStoryLinesWithInfoWithRemovedComponentDublicates [inputStory]
  (let [storyLineHashmap (sortedStoryLinesWithInfo inputStory)
        lineWordsAndCharsDublicatesRemoved (accumulatedComponentsNestedList inputStory)]
    (map-indexed (fn [idx itm] (merge itm (hash-map :componentsToBeRemovedIfDublet (nth lineWordsAndCharsDublicatesRemoved idx)))) storyLineHashmap)))

;(println (sortedStoryLinesWithInfoWithRemovedComponentDublicates miniStory))

(defn contentVecGnerator [inputStory]
  (let [eachLineWithID (sortedStoryLinesWithInfoWithRemovedComponentDublicates inputStory)]
    (vector
      (nth inputStory 0)
      (nth inputStory 1)
      (nth inputStory 2)
      eachLineWithID
      )
    ))

;denne ser fornuftig ud.
;2020-09-20 kl.21.04
;jeg tror godt jeg kan bruge dette til at lave en csv fil
;opgave: jeg skal lave en csv fil

;(println (contentVecGnerator miniStory))
;(println (contentVecGnerator raw_story))
;(println "********************************")




;(println (sortedStoryLinesWithInfo miniStory))

;slut