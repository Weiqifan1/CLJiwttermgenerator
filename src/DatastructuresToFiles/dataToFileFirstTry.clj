
(println "dataToFIleFirstTry")

;; This program displays Hello World
(defn writeExample []
  (with-open [w (clojure.java.io/writer "resources/ankiResultFiles/Example.txt" :append true)]
    (.write w (str "hello" "world"))))

;slut