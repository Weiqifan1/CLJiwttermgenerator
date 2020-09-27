
(load-file "src/StoryAnalysing/readInfoFromFile.clj")
(load-file "src/ParsingStories/storyToDatastructure.clj")
(load-file "src/Helpermethods/hjaelpemetoder.clj")

(println "dataToFIleFirstTry")

(defn contentVecLineToStringVector_LineFirst
  [dataStructureLine storyFirstLine storySecondLine]
  (let [contentLine (get dataStructureLine :line)
        infoLine (str
                   (clojure.string/trim (get dataStructureLine :line)) "\n"
                   (clojure.string/trim (get dataStructureLine :linePinYin)) "\n"
                   (clojure.string/trim (clojure.string/join "" (get dataStructureLine :lineCharsAndTzaiNumbers))) "\n"
                   "newCharsAndWords: "
                   (clojure.string/trim (clojure.string/join " " (get dataStructureLine :componentsToBeRemovedIfDublet))) "\n"
                   (clojure.string/trim storyFirstLine) "\n"
                   (clojure.string/trim storySecondLine) "\n"
                   (clojure.string/trim (get dataStructureLine :lineID)) "\n"
                   (clojure.string/trim (get dataStructureLine :line)) "\n"
                   (clojure.string/trim (get dataStructureLine :lineInfo))
                   )
        ]
    (vector (clojure.string/trim contentLine)
            (clojure.string/trim infoLine)
            (str "sentence" " " (clojure.string/replace storyFirstLine #"\s+" "")))
    ))
;;(clojure.string/replace inputString #"\s+$" ""))

(defn contentVecLineToStringVector_PinyinFirst
  [dataStructureLine storyFirstLine storySecondLine]
  (let [contentLine (get dataStructureLine :linePinYin)
        infoLine (str
                   (clojure.string/trim (get dataStructureLine :line)) "\n"
                   (clojure.string/trim (get dataStructureLine :linePinYin)) "\n"
                   (clojure.string/trim (clojure.string/join "" (get dataStructureLine :lineCharsAndTzaiNumbers))) "\n"
                   "newCharsAndWords: "
                   (clojure.string/trim (clojure.string/join " " (get dataStructureLine :componentsToBeRemovedIfDublet))) "\n"
                   (clojure.string/trim storyFirstLine) "\n"
                   (clojure.string/trim storySecondLine) "\n"
                   (clojure.string/trim (get dataStructureLine :lineID)) "\n"
                   (clojure.string/trim (get dataStructureLine :line)) "\n"
                   (clojure.string/trim (get dataStructureLine :lineInfo))
                   )
        ]
    (vector (clojure.string/trim contentLine)
            (clojure.string/trim infoLine)
            (str "pinyin" " " (clojure.string/replace storyFirstLine #"\s+" "")))
    ))

(def raw_story (clean_story_funk "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"))
(def miniStory (take 11 raw_story))
;(println miniStory)
;(println (take 6 raw_story))
(def dataStructureToWriteToFile (contentVecGnerator miniStory))
(def bigDataStrunctureTowriteTofile (contentVecGnerator raw_story))
;last dataStructureToWriteToFile))
;(println dataStructureToWriteToFile)
;(println (count (last bigDataStrunctureTowriteTofile))) ;virker

;; This program displays Hello World
(defn writeExample []
  (with-open [w (clojure.java.io/writer "resources/ankiResultFiles/Example.txt" :append false)]
    (.write w (str "hello" "world" "there" "lykke"))))

; createOrCleanseAFile
(defn createOrCleanseFile [filePathString]
  (with-open [w (clojure.java.io/writer filePathString :append false)]
    (.write w "")))

; write input to a file
(defn writeInputToFile [inputString filePathString]
  (with-open [w (clojure.java.io/writer filePathString :append true)]
    (.write w inputString)))

;write a function that can write dataStructure lines to a file
;2020-09-26 funktionen virker ikke. Der bliver ikke skrevet til en fil
(defn createStringToBeWrittenTofileFromContentVecGenerator
  [inputDataStructure filePath]
  (let [hashmapWithLineInfo (nth inputDataStructure 3)]
    (map
      (fn [eachHashMap]
        (let [ordinaryLineToString (contentVecLineToStringVector_LineFirst eachHashMap)
              pinyinToString (contentVecLineToStringVector_PinyinFirst eachHashMap)
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
;(println (contentVecLineToStringVector_LineFirst foersteGodeLinje))
;(println (createStringToBeWrittenTofileFromContentVecGenerator dataStructureToWriteToFile))

;(writeExample)

;;;skriv en ny funktion der kan skrive til en fil
(defn writeArrayToFile [inputArrayOfStrings filePathString]
  (do
    (createOrCleanseFile filePathString)
    (writeInputToFile (clojure.string/join "\n" inputArrayOfStrings) filePathString)
    ))
;(writeArrayToFile ["hello" "there" "lykke"] "resources/ankiResultFiles/Example.txt")

;;; 2020-09-27 write a function that takes a Line hashmap and creates an array of strings (3 strings)
;;jeg skal fjerne komma, kolon, tab og doublequote fra alle strenge, og tilfoeje kolon og doublequote bagefter
;removeAnkiSpecialCharsAndTrailingNewLine
;the input should be a string array with 2 or 3 items:
;front side, backside and maybe thirdly: tags separated by space
(defn stringArrayToAnkiLine [inputArray]
  ((comp
    #(clojure.string/join ";" %)
    (fn [cleanedArray]
      (map
        #(str "\"" % "\"")
        cleanedArray))
    #(map removeAnkiSpecialCharsAndTrailingNewLine %)
  )inputArray))
;(println (stringArrayToAnkiLine ["her, e;r sae\ttning\n" "her\ner\ninfo" "sentence avengers"]))

(def miniStoryExampleLine (nth (nth dataStructureToWriteToFile 3) 2))
;(println miniStoryExampleLine)

(def testLine (contentVecLineToStringVector_LineFirst miniStoryExampleLine (nth miniStory 0) (nth miniStory 1)))
;(println testLine)

;write a function that takes a story line hashmap and returns a string array (first line, secondline, tags)
(defn storyLineHashMapNestedStringArray [arrayOfStoryLineMaps storyLine1 storyLine2]
  (
    flatten-one-level ;after this, every 3 items in the list now belog in a vector
   (map
    (fn [eachStoryLineMap]
      (vector
        (contentVecLineToStringVector_LineFirst eachStoryLineMap storyLine1 storyLine2)
        (contentVecLineToStringVector_PinyinFirst eachStoryLineMap storyLine1 storyLine2)
        )
      )
    arrayOfStoryLineMaps)))

(def nestedArray (storyLineHashMapNestedStringArray
                   (nth dataStructureToWriteToFile 3)
                   (nth dataStructureToWriteToFile 0)
                   (nth dataStructureToWriteToFile 1)
                   ))
;(println (last (first nestedArray)))

(defn ankiCellNestedArrayToAnkiLineArray [ankiCellNestedArray]
  (map
    (fn [eachRowArray]
      (stringArrayToAnkiLine eachRowArray))
    ankiCellNestedArray))

;;;;2020-09-27 funtion der tager en anki row string og skriver den til en fil
(defn ankiRowStringArrayToFile [ankiRowStringArray filepath]
  (writeArrayToFile ankiRowStringArray filepath)
  )
;(ankiRowStringArrayToFile  (ankiCellNestedArrayToAnkiLineArray nestedArray) "resources/ankiResultFiles/Example.txt")
;;ovenstaande virker, men er lidt kompliceret. Jeg skal pr've at stoppe processen over i en funktin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(println (contentVecGnerator miniStory))
;(println (contentVecGnerator raw_story))

;2020-09-27
;write a function that takes a storyFilePath and a targetAnkiFilePath,
;and creates the file to be passed to Anki
(defn storyfilePathToAnkiFile [storyFilePath targetAnkiFilepath] ;"src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  (let [
        fullStory (clean_story_funk storyFilePath)
        bigDataStructure (contentVecGnerator fullStory)
        nestedArray (storyLineHashMapNestedStringArray
                      (nth bigDataStructure 3)
                      (nth bigDataStructure 0)
                      (nth bigDataStructure 1)
                      )
        ankiRowStringArray (ankiCellNestedArrayToAnkiLineArray nestedArray)
        ]
    ;(println (take 1 (last bigDataStructure)))
    ;(println (first (nth dataStructureToWriteToFile 3)))
    (writeArrayToFile ankiRowStringArray targetAnkiFilepath)
  ))
(storyfilePathToAnkiFile
  "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"
  "resources/ankiResultFiles/Example.txt"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;slut