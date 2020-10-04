
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/ParsingStories/newstoryToDatastructure.clj")
(load-file "src/Helpermethods/hjaelpemetoder.clj")

;write a function that that takes the sorted info lines and
;successively remove chars from :componentsToBeRemovedIfDublet



(defn removeIdenticalLinesThatOccurLater [sortedInfoLines]
  (filter #(not (= nil %))
  (map-indexed
    (fn [idx itm]
      (if (< 0 idx)
        (if
          (= (get itm :lineSortedTzaiNumbers) (get (nth sortedInfoLines (- idx 1)) :lineSortedTzaiNumbers))
          nil
          itm
          )
        itm))
    sortedInfoLines)
  ))

(defn contentVec-bata-3 [inputStory]
  (filter #(not (nil? %))
  (map-indexed
      (fn [lineIndex, eachLine]
        (let [previousLine (nth inputStory (helper_rundOpTilNul (- lineIndex 1)))
              storyLineIsChinese (and (helper_stringHasChinese eachLine) (helper_stringHasOnlyNumPunktAndNotEmpty previousLine))
              ]
          (if storyLineIsChinese
         (hash-map :line eachLine
                   :lineID previousLine
                   :storyFirstLine (nth inputStory 0)
                   :storySecondLine (nth inputStory 1)
                   :storyThirdLine  (nth inputStory 2)
                   :components (linetoUniqueWordsAndChars eachLine);(map createCharInfoHashMap  (linetoUniqueWordsAndChars (get % :line))); ;outCommented Vector
                   :componentsToBeRemovedIfDublet (linetoUniqueWordsAndChars eachLine)
                   :lineInfo (tradKinStringToTradWordsInfoStringFull eachLine)
                   :linePinYin (tradKinStringToPinyinKinStringWith* eachLine)
                   :lineCharsAndTzaiNumbers (stringToCharsAndTzaiNumber eachLine)
                   :lineSortedTzaiNumbers (vec (reverse (stringToTzaiNumbersSortedNoNil eachLine)))
                  ) nil))
        )
      inputStory)))


(defn newstoryfilePathToAnkiFile [storyFilePath targetAnkiFilepath] ;"src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  (let [
        fullStory (clean_story_funk storyFilePath)
        simpelLineHashMap (contentVec-bata-3 fullStory)
        sortedInfoLines (sortByTzaiNumbers simpelLineHashMap)
        ;removeDublicates (removeIdenticalLinesThatOccurLater sortedInfoLines)
        nestedWordAndCharList (map #(get % :componentsToBeRemovedIfDublet) sortedInfoLines)
        AccumWordAndChar (vec (map
                                 (fn [eachRange]
                                   (vec (distinct (flatten (map #(nth nestedWordAndCharList %) eachRange)))))
                                 (map #(range (+ % 1))
                                      (range 0 (count nestedWordAndCharList)))))
        AccumWordAndCharPreviousIndex (into [[]] AccumWordAndChar)
        newUniqueCharactersLists (map-indexed
                                   (fn [eachIndex eachListOfAccumWordsAndChars]
                                     (filter #(not (nil? %))
                                     (map
                                         #(if (not (contains?
                                                (set (nth AccumWordAndCharPreviousIndex eachIndex ))
                                                %))
                                            %
                                            nil)
                                         eachListOfAccumWordsAndChars))
                                     )
                                   AccumWordAndChar)
        addUniqueWordsAndCarsToMaps (map-indexed
                                      (fn [eachIndex eachMap]
                                        (conj eachMap {:newWordsAndChars (nth newUniqueCharactersLists eachIndex)}))
                                     sortedInfoLines)
        ;2020-10-04 nedenstaaende funktion virker
        removeMapsWithNoNewWordsOrChars (filter
                                          #(not (= [] (% :newWordsAndChars)))
                                          addUniqueWordsAndCarsToMaps)
        ]
    ;(println (nth addUniqueWordsAndCarsToMaps 50))
    (println (count addUniqueWordsAndCarsToMaps))
    (println (take 10 (map #(% :newWordsAndChars) addUniqueWordsAndCarsToMaps)))

    (println (count removeMapsWithNoNewWordsOrChars))
    (println (take 10 (map #(% :newWordsAndChars) removeMapsWithNoNewWordsOrChars)))
    
    (println (count (flatten (map #(% :newWordsAndChars) removeMapsWithNoNewWordsOrChars))))
    (println (take 10 (flatten (map #(% :newWordsAndChars) removeMapsWithNoNewWordsOrChars))))

    ;我
    ;這是我的
    ;不是你一個人
    ;我可不這麼想
    ;很多年以前
    ;如果沒有你們
    ;沒問題
    ;沒問題
    ;沒問題
    ;對吧

    ;(println (take 3 removeDublicates))

    ;(println (count removeNonChineseLineMaps))
    ;(println (count removeDublicateMaps))

    ;(writeArrayToFile ankiRowStringArray targetAnkiFilepath)
    ;(println (testNewStoryToDatastructure "hello fra dataToFileSecondTry"))
    ;(println (first removeNonChineseLineMaps))
    ;(println (map #(get % :line) removeNonChineseLineMaps))
    ;(println (take 3 sortedInfoLines))
    ;(println "test")

    ))


(newstoryfilePathToAnkiFile
  "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  "resources/ankiResultFiles/Example3.txt"
  )


;slut