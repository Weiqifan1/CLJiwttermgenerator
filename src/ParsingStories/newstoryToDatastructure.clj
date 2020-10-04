
(load-file "src/Helpermethods/hjaelpemetoder.clj")

(defn testNewStoryToDatastructure [inputString] (str "" inputString " " "test"))

(defn contentVec-bata-1 [inputStory]
  (keep-indexed
    (fn [lineIndex, eachLine]
      (hash-map :line eachLine, :lineID (nth inputStory (helper_rundOpTilNul (- lineIndex 1))))
      )
    inputStory)
  )

(defn contentVec-bata-2 [inputStory]
  (vec
  (map #(hash-map
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
         inputStory))))

(defn sortByTzaiNumbers [inputStory]
  (sort-by :lineSortedTzaiNumbers compareListsOfTzai_2 inputStory)
  )


;slut