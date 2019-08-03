(ns overchauffeur.jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        [overtone.live :only [play-buf load-sample freesound-path sample apply-at apply-by now at ctl square perc FREE adsr line:kr sin-osc definst hpf lpf clip2 env-gen]]
        jazz.instruments))

(def tonic (-> triad (root 7) (inversion 1)))
(def fourth (-> triad (root 3)))
(def fifth (-> triad (root 4)))

(def in-the-mood
  (let [bassline #(->> [0 2 4 5 4 7 5 4 2]
                      (phrase [1 1 1 1/2 1/2 1 1 1 1])
                      (where :pitch (comp lower lower))
                      (where :pitch (from %)))
        hook #(->> (-> % vals sort cycle)
                   (phrase (concat (repeat 11 1/2) [5/2])))
        beat (->> (rhythm (cycle [1 1/2 1/2]))
                  (take-while #(-> % :time (< 48)))
                  (all :part :beat))]
    (->>
      (mapthen bassline [0 0 3 0 4 0])
      (with beat)
      (with (mapthen hook [tonic tonic fourth tonic fifth tonic]))
      (where :pitch (comp C major))
      (tempo (comp (scale [2/3 [1/3]]) (partial * 2)))
      (tempo (bpm 100)))))

(comment
  (jam (var in-the-mood))
  (def in-the-mood nil)
)

(defmethod play-note :beat [_]
  (hat))

(defmethod play-note :default
  [{midi :pitch, seconds :duration}]
  (piano midi :duration seconds))
