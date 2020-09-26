
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/ParsingStories/storyToDatastructure.clj")
(load-file "src/Helpermethods/hjaelpemetoder.clj")

(println "dataToFIleFirstTry")

(defn contentVecLineToString_LineFirst
  [dataStructureLine]
  (let [contentLine (get dataStructureLine :line)]
    (str contentLine ";"
         (removeNewlineFromEnd (get dataStructureLine :lineID)) "\n"
         (removeNewlineFromEnd (get dataStructureLine :linePinYin)) "\n"
         (removeNewlineFromEnd (clojure.string/join "" (get dataStructureLine :lineCharsAndTzaiNumbers))) "\n"
         (removeNewlineFromEnd (get dataStructureLine :line)) "\n"
         (removeNewlineFromEnd (get dataStructureLine :lineInfo)) ";"
         )))

(defn contentVecLineToString_PinyinFirst
  [dataStructureLine]
  (let [contentLine (get dataStructureLine :linePinYin)]
    (str contentLine ";"
         (removeNewlineFromEnd (get dataStructureLine :lineID)) "\n"
         (removeNewlineFromEnd (get dataStructureLine :linePinYin)) "\n"
         (removeNewlineFromEnd (clojure.string/join "" (get dataStructureLine :lineCharsAndTzaiNumbers))) "\n"
         (removeNewlineFromEnd (get dataStructureLine :line)) "\n"
         (removeNewlineFromEnd (get dataStructureLine :lineInfo)) ";"
         )))

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
(def dataStructureToWriteToFile (contentVecGnerator miniStory))

;(println (count dataStrudtuceToWriteToFile))
;(println dataStrudtuceToWriteToFile) ;virker

(defn createStringToBeWrittenTofileFromContentVecGenerator
  [inputDataStructure]
  (let [hashmapWithLineInfo (nth inputDataStructure 3)]
    (identity hashmapWithLineInfo)))

(def foersteGodeLinje (nth (createStringToBeWrittenTofileFromContentVecGenerator dataStructureToWriteToFile) 1))
;(println foersteGodeLinje)





;(println (contentVecLineToString_LineFirst foersteGodeLinje))

;(println (createStringToBeWrittenTofileFromContentVecGenerator dataStructureToWriteToFile))

;slut