(load-file "src/Helpermethods/hjaelpemetoder.clj")

(println "hello readInfo")

(defn clean_story_funk [filstig]
  ((comp
     ;(fn [input] (map #(vector (first %) (read-string (second %))) input))
     ;(fn [input] (map #(clojure.string/split % (re-pattern regexSplitLinje)) input))
     ;#(map clojure.string/trim %)
     #(clojure.string/split (slurp %) (re-pattern "\\s+"))
     )
   filstig
   )
  )

(def raw_story (clean_story_funk "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt"))

(println (count raw_story))
(println (take 20 raw_story))



;slut