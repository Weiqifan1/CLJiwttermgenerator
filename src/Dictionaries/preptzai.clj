;(load-file "src/lwttermgenerator/hjaelpemetoder.clj")
(load-file "src/Helpermethods/hjaelpemetoder.clj")

(defn clean_tzai_funk [filstig regexSplitLinje regexSplitDokument]
  ((comp
     (fn [input] (map #(vector (first %) (read-string (second %))) input))
     (fn [input] (map #(clojure.string/split % (re-pattern regexSplitLinje)) input))
     #(map clojure.string/trim %)
     #(clojure.string/split (slurp %) (re-pattern regexSplitDokument))
     )
   filstig
   )
  )

;(def Example
;  (slurp "src\\lwttermgenerator\\Avengers01_01_changedToTrad.txt" ))

(def clean_tzai (clean_tzai_funk "src\\lwttermgenerator\\Tzai2006.txt" "\\s+" "\\r+"))
(def tzaivec (map #(first %) clean_tzai))
(def tzaivec_2 (flatten (vector "的" (drop 1 tzaivec))))
;(println (count tzaivec_2))
;(println (take 10 tzaivec_2))
;(println (count (first (take 1 clean_tzai))))

(def tzai_indexes (map-indexed (fn [idx itm] (vector itm (inc idx))) tzaivec_2))
(def tzai_hashmap (apply hash-map (vec (flatten tzai_indexes))))
;(println (take 5 tzai_hashmap))
;(println (get tzai_hashmap "的"))
;(println (get tzai_hashmap "是"))
;(println (get tzai_hashmap "不"))

;la en funktion der tager en integer og returnere et tegn fra tzai_hashmap
(def tzai_indexes_2 (map-indexed (fn [idx itm] (vector (inc idx) itm)) tzaivec_2))
(def tzai_hashmap_numToChar (apply hash-map (vec (flatten tzai_indexes_2))))
;(println (take 5 tzai_hashmap_numToChar))
;(println (get tzai_hashmap_numToChar 1))
;(println (get tzai_hashmap_numToChar 2))
;(println (get tzai_hashmap_numToChar 3))



;(println (count tzai_hashmap))
;(println (take 3 tzai_hashmap))
;(println (get tzai_hashmap "的"))
;(println (get tzai_hashmap "是"))
;(println (get tzai_hashmap "中"))
;(println (get tzai_hashmap "鷍"))

;(println "her er tzai")


;slut