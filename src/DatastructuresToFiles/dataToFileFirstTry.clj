
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

; createOrCleanseAFile
(defn createOrCleanseFile [filePathString]
  (with-open [w (clojure.java.io/writer filePathString :append false)]
    (.write w "")))

; write input to a file
(defn writeInputToFile [inputString filePathString]
  (with-open [w (clojure.java.io/writer filePathString :append true)]
    (.write w inputString)))

(def raw_story (clean_story_funk "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"))
(def miniStory (take 11 raw_story))
;(println miniStory)
;(println (take 6 raw_story))
;;(println (contentVecGnerator miniStory))
;;;(println (contentVecGnerator raw_story))
(def dataStructureToWriteToFile (contentVecGnerator miniStory))

;(println (count dataStrudtuceToWriteToFile))
;(println dataStrudtuceToWriteToFile) ;virker

;write a function that can write dataStructure lines to a file
;2020-09-26 funktionen virker ikke. Der bliver ikke skrevet til en fil
(defn createStringToBeWrittenTofileFromContentVecGenerator
  [inputDataStructure filePath]
  (let [hashmapWithLineInfo (nth inputDataStructure 3)]
    (map
      (fn [eachHashMap]
        (let [ordinaryLineToString (contentVecLineToString_LineFirst eachHashMap)
              pinyinToString (contentVecLineToString_PinyinFirst eachHashMap)
              ]
          (writeInputToFile ordinaryLineToString filePath
           ;(writeInputToFile ordinaryLineToString filePath)
           ;(writeInputToFile pinyinToString filePath)
            ))
        )
      hashmapWithLineInfo)))
;(createStringToBeWrittenTofileFromContentVecGenerator miniStory "resources/ankiResultFiles/Example2.txt")

;(println (count (createStringToBeWrittenTofileFromContentVecGenerator dataStructureToWriteToFile)))
;(println foersteGodeLinje)
;(println (contentVecLineToString_LineFirst foersteGodeLinje))
;(println (createStringToBeWrittenTofileFromContentVecGenerator dataStructureToWriteToFile))

;;;skriv en ny funktion der kan skrive til en fil





;slut