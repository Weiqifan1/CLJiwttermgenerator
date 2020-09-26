
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/ParsingStories/storyToDatastructure.clj")

(println "dataToFIleFirstTry")

;; This program displays Hello World
(defn writeExample []
  (with-open [w (clojure.java.io/writer "resources/ankiResultFiles/Example.txt" :append false)]
    (.write w (str "hello" "world" "there"))))

(def raw_story (clean_story_funk "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"))
(def miniStory (take 11 raw_story))
;(println miniStory)
;(println (take 6 raw_story))
;;(println (contentVecGnerator miniStory))
;;;(println (contentVecGnerator raw_story))
(def dataStrudtuceToWriteToFile (contentVecGnerator miniStory))

;(println (count dataStrudtuceToWriteToFile))
;(println dataStrudtuceToWriteToFile) ;virker

(defn createStringToBeWrittenTofileFromContentVecGenerator
  [inputDataStructure]
  (let [hashmapWithLineInfo (nth inputDataStructure 3)]
    (identity hashmapWithLineInfo)))


(println (createStringToBeWrittenTofileFromContentVecGenerator dataStrudtuceToWriteToFile))

;slut