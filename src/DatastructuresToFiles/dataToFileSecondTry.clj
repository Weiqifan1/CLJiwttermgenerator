
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/ParsingStories/newstoryToDatastructure.clj")
(load-file "src/Helpermethods/hjaelpemetoder.clj")

(defn newstoryfilePathToAnkiFile [storyFilePath targetAnkiFilepath] ;"src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  (let [
        fullStory (clean_story_funk storyFilePath)
        simpelLineHashMap (contentVec-bata-1 fullStory)
        removeNonChineseLineMaps (contentVec-bata-2 simpelLineHashMap)
        sortedStoryLinesWithInfo (sortedStoryLinesWithInfo removeNonChineseLineMaps)
        ]
    ;(writeArrayToFile ankiRowStringArray targetAnkiFilepath)
    ;(println (testNewStoryToDatastructure "hello fra dataToFileSecondTry"))
    ;(println (first removeNonChineseLineMaps))
    ;(println (map #(get % :line) removeNonChineseLineMaps))
    (println (take 3 removeNonChineseLineMaps))
    ))


(newstoryfilePathToAnkiFile
  "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  "resources/ankiResultFiles/Example3.txt"
  )


;slut