(ns kolmogorov-music.test.champernowne
  (:require [kolmogorov-music.champernowne :as champernowne]
            [midje.sweet :refer :all]))

(fact "The Champernowne word is defined by concatenating the natural numbers base 10."
  (->> (champernowne/word) (take 12))
    => [0 1 2 3 4 5 6 7 8 9 1 0])

(fact "It's similarly defined for other bases."
  (->> (champernowne/word 2) (take 9))
    => [0 1 1 0 1 1 1 0 0]

  (->> (champernowne/word 16) (take 18))
    => [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 1 0])