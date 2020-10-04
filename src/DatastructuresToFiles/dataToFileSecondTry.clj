
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

(defn newstoryfilePathToAnkiFile [storyFilePath targetAnkiFilepath] ;"src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  (let [
        fullStory (clean_story_funk storyFilePath)
        simpelLineHashMap (contentVec-bata-1 fullStory)
        removeNonChineseLineMaps (contentVec-bata-2 simpelLineHashMap)
        sortedInfoLines (sortByTzaiNumbers removeNonChineseLineMaps)
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
                                          #(< 0 (count (% :newWordsAndChars)))
                                          addUniqueWordsAndCarsToMaps)
        ]
    (println (count sortedInfoLines))
    (println (count removeMapsWithNoNewWordsOrChars))

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