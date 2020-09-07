(ns battle.core
	(:gen-class)
	(:require [clojure.string :as str])
	(:require [clojure.edn :as edn])
)

(defn debug [x]
	(println x)
	x	
)

(defn validate [castles squad-count army]
	(if (= (count army) castles)
		(if (= (reduce + army) squad-count)
			(every? (fn [x] (>= x 0)) army)
			false
		)
		false
	)
)

(defn parse-int [string]
	(Integer/parseInt (str/trim string))
)

(defn create-army [castles squad-count]
	(fn [line]
		(let [army (edn/read-string line)]
			(if (validate castles squad-count army)
				army
				(println "Invalid army")
			)
		)
	)
)

(defn get-start [castles squad-count seed]
	(let [make-army (create-army castles squad-count)]
		(map make-army (str/split-lines (str/trim-newline seed)))
	)
)



(defn siege [left right value]
	(cond
		(> left right) value
		(< left right) (- value)
		:else 0
	)
)

(defn conflict [left right value]
	(if (first left)
		(+ (siege (first left) (first right) value) (conflict (rest left) (rest right) (+ value 1)))
		0
	)
)

(defn battle [left right]
	(if (> (conflict left right 1) 0) 1 0)
)

(defn campaign [army other-list]
	(let [other (first other-list)]
		(if other
			(let [result (battle army other)]
				(+ result (campaign army (rest other-list)))
			)
			0
		)
	)
)

(defn war [army-list other-list result-list]
	(let [army (first army-list)]
		(if army
			(let 
				[
					wins (campaign army other-list)
					result {:wins wins, :army army}
				]
				(cons result (war (rest army-list) other-list result-list))
			)
			result-list
		)
	)
)

(defn compute-results [castles squad-count start]
	(sort-by :wins #(compare %2 %1) (war start start nil))
)

(defn write-results [writer results]
	(doseq [army results] (.write writer (println-str (get army :army))))
	(doseq [army (take 40 results)] (println army))
)

(defn tweak [castles army]
	(let 
		[
			src (rand-int castles)
			dest (rand-int castles)

			s-value (nth army src)
			d-value (nth army dest)
		]
		(if (and (not (= src dest)) (not (= 0 s-value)))
			(assoc (assoc army src (- s-value 1)) dest (+ d-value 1))
			(tweak castles army)
		)
	)
)

(defn mutate [castles army depth]
	(let [result (tweak castles army)]
		(if (and (< depth 10) (= 0 (rand-int 2)))
			(mutate castles result (+ depth 1))
			result
		)
	)
)

(defn evolve [castles depth army army-list-out]
	(if (> depth 0)
		(cons (mutate castles army 0) (evolve castles (- depth 1) army army-list-out))
		army-list-out
	)
)

(defn evolution-loop [castles army-list-in army-list-out]
	(let [army (first army-list-in)]
		(if army
			(evolve castles 10 army (evolution-loop castles (rest army-list-in) army-list-out))
			army-list-out
		)
	)
)

(defn evolution [castles army-list]
	(evolution-loop castles army-list army-list)
)

(defn -main
  	"538 Battle"
  	[& args]
  	(if (== 4 (count args))
		(try
			(let 
				[
					castles (Integer/parseInt (nth args 0))
					squad-count (Integer/parseInt (nth args 1))
					seed-name (nth args 2)
					results-name (nth args 3)

					start (get-start castles squad-count (slurp seed-name))
					evolved-list (evolution castles start)
				]
				(with-open [writer (clojure.java.io/writer results-name)]
					(write-results writer (compute-results castles squad-count evolved-list))
				)
			)
			(catch NumberFormatException exception 
				(println "Something is not an int")
			)
		)
	  	(println "Usage: battle castles armies seed results")
	)  	
)

