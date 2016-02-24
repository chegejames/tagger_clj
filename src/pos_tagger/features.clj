(ns pos_tagger.features)

(defn join[s] (apply str (interpose ":" s)))

(defn prev_pos[pos i] (get pos (- i 1) "*"))

(defn prev_word[sent i] (get sent (- i 1) "NULL"))

 (defn next_word[sent i] (get sent (+ i 1) "NULL"))

(defn features[sent pos]
  (let [prev_pos #(prev_pos pos %)
        prev_word #(prev_word sent %)
        next_word #(next_word sent %)
        ]
  
  (fn[i]
    (let [word (sent i)
          word-1 (prev_word i)
          word-2 (prev_word (- i 1))
          word+1 (next_word i)
          word+2 (next_word (+ i 1))
          t (pos i)
          t1 (prev_pos i)
          t2 (prev_pos (- i 1))]
    
    {
      ["BIGRAM:POS-2:WORD:POS" t2 word t] 1
      ["BIGRAM:POS-2:POS" t2 t] 1
      ["TRIGRAM:POS-2:POS-1:POS" t2 t1 t] 1
      ["TRIGRAM:POS-2:POS-1:WORD:POS" t2 t1 word t] 1
      ["TRIGRAM:POS-2:POS-1:WORD-1:WORD:POS" t2 t1 word-1 word t] 1
      ["TRIGRAM:POS-2:WORD-2:POS-1:WORD-1:WORD:POS" t2 word-2 t1 word-1 word t] 1
      ["TRIGRAM:POS-2:WORD-1:WORD:POS" t2 word-1 word t] 1
      ["TRIGRAM:POS-2:WORD-1:POS" t2 word-1 t] 1
      ["UNIGRAM:WORD:POS" word t] 1
      ["BIGRAM:POS-1:WORD:POS" t1 word t] 1
      ["BIGRAM:POS-1:POS" t1 t] 1
      ["BIGRAM:WORD-1:POS-1:WORD" word-1 t1  word] 1
      ["BIGRAM:WORD-1:WORD:POS" word-1 word t] 1
      ["BIGRAM:WORD-1:POS" word-1 t] 1
      ["BIGRAM:WORD-1:POS-1:WORD:POS" word-1 t1 word t] 1
      ["BRIRAM:WORD-2:WORD" word-2, word] 1
      ["BIGRAM:POS:WORD+1" t word+1] 1
      ["BIGRAM:WORD:POS:WORD+1" word t word+1] 1
      ["TRIGRAM:WORD-2:POS-1:WORD:POS" word-2 t1 word t] 1
      ["TRIGRAM:WORD-2:POS-1:POS" word-2 t1 t] 1
      ["TRIGRAM:WORD-2:WORD-1:POS" word-2 word-1 t] 1
      ["TRIGRAM:WORD-2:WORD-1:WORD:POS" word-2 word-1 word t] 1
      ["TRIGRAM:WORD:POS:WORD+1:WORD+2" word t word+1 word+2] 1
     }
))))
  


(defn global-features[{:keys [sent pos]}]
  (let [n (range (count sent))
        g (features sent pos)]
    
    (reduce (partial merge-with +) (map g n))))


(defn local-features[sent pos i]
  ((features sent pos) i))
    

