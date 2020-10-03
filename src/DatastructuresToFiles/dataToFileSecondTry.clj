
(defn newstoryfilePathToAnkiFile [storyFilePath targetAnkiFilepath] ;"src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  (let [
        fullStory (clean_story_funk storyFilePath)
        ]
    ;(writeArrayToFile ankiRowStringArray targetAnkiFilepath)
    (println "new fileToAnki")
    ))


(newstoryfilePathToAnkiFile
  "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  "resources/ankiResultFiles/Example3.txt"
  )


;slut