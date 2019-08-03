(ns overchauffeur.geb
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon interval]]
            [leipzig.scale :refer [lower B minor from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [overtone.inst.drum :as drums]
            [overchauffeur.coding :as coding]))

(def bar-lengths [3.5 3.5 7])

(def geb
  (let [theme (->> "GEB"
                   (map coding/char->ascii)
                   (phrase bar-lengths)
                   (all :theme true))
        ascii #(where :pitch coding/ascii->midi %)
        double-canon (->> theme
                          (canon ascii))
        sample #(->> %
                     (where :pitch char)
                     (all :part :sample))
        triple-canon (->> theme
                          (canon #(with (sample %) (ascii %))))
        robot (->> triple-canon
                   (filter #(-> % :part (= :sample))))
        bass (->> (phrase [2.5 0.5 0.5 2.5 0.5 0.5 6.5 0.5] [-2 -2 -2 -1 -1 -1 0 0])
                  (canon (interval -7))
                  (where :pitch (comp lower lower)))
        alt-bass (->> (phrase (repeat 4 4) (cycle [3 0]))
                      (canon (interval -7))
                      (where :pitch (comp lower lower)))
        rising (->> (phrase [4 4 8] [-2 -1 0])
                    (canon (interval -7))
                    (where :pitch (comp lower lower)))
        riff (->> [-2 -1 0 0]
                  (mapthen #(->> (phrase (repeat 7 1/2)
                                         (interleave [[0 2] [0 2] [0 3] [0 2]] (repeat -3)))
                                 (where :pitch (from %)))))
        whirl (->> (phrase [0.5 2 1] [9 7 4])
                   (where :pitch (from 7))
                   (times 4)
                   (with (phrase (repeat 28 1/4) (cycle [2 3]))))
        hit (->> (phrase [2 0.5 0.5 0.5] [14 [7 11] [7 12] [7 11]])
                 (times 4))
        twiddle (with
                  (phrase (repeat 32 1/2) (cycle [4 2 2 0 -1]))
                  (phrase (repeat 64 1/4) (cycle [4 2 5 4 5 4 7 7])))
        decoration (phrase (repeat 64 1/4) (cycle [7 8 9 11 7 6]))
        beat (->> (phrase [1 1 0.5 0.5 0.5 2.5 0.5 0.5] (repeat 0))
                  (having :part [:kick :clap :snare :kick :snare :kick :kick :snare])
                  (times 2))
        back-beat (->> (phrase (repeat 28 1/4) (repeat 7)) (having :part (repeat :click)))
        scale (->> (phrase (repeat 1/8) (concat (range -14 35 1) (range 35 -28 -1))))]
    (->> []
         (with bass #_double-canon)
         ;(with riff)
         ;(with whirl)
         ;(with robot)
         ;(with beat #_back-beat)
         ;(with hit)
         ;(with (->> scale (with (where :pitch (from 2) scale))))
         ;(with #_alt-bass rising twiddle decoration)
         (times 2)
         (wherever (comp not :theme) :pitch (fnil (comp B minor) 0))
         (tempo (bpm 90)))))

(comment
  (map fx-chorus [0 1])
  (map fx-distortion [0 1] [2 2] [0.18 0.14])
  (volume 0.8)
  (live/jam (var geb))
  (def geb nil)

  (do (recording-start "geb.aiff")
      (live/play geb))
  (recording-stop))

; Instrumentation
(defsynth walker [out-bus 0 freq 0.5]
  (out:kr out-bus (lf-noise1:kr freq)))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk))
(def resonance (mul-add (in:kr random-walk) 700 2500))

(definst overchauffeur [freq 110 dur 1.0 top 1500 vol 0.25]
  (let [inst (-> (sin-osc freq)
                 (* (+ 0.4 (* 0.4 (sin-osc 1.5 0.5))))
                 (+ (* 1/3 (sin-osc (* 2.001 freq))))
                 (+ (* 1/4 (sin-osc (* 3.005 freq))))
                 (+ (* 1/5 (sin-osc (* 4.01 freq))))
                 (+ (sin-osc (* 0.5 freq)))
                 (* 3)
                 (clip2 0.6)
                 (* 8)
                 (clip2 0.9)
                 ;(* (square 3))
                 (rlpf resonance 0.2)
                 (* (env-gen (adsr 0.05 0.2 0.6 0.2) (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.001)
        reverbed (free-verb delayed :damp 0.1 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.3 :mix 0.1 :room 0.2)]
    (+ (-> dryverbed (pan2 -1 0.5)) (pan2 reverbed 1 0.5))))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (overchauffeur seconds)))

(defmethod live/play-note :kick
  [{midi :pitch seconds :duration}]
  (some-> midi (drums/kick4 :amp 5)))

(defmethod live/play-note :snare
  [{midi :pitch seconds :duration}]
  (some-> midi (drums/snare :amp 2)))

(defmethod live/play-note :clap
  [{midi :pitch seconds :duration}]
  (some-> midi (drums/haziti-clap :amp 5)))

(defmethod live/play-note :click
  [{midi :pitch seconds :duration}]
  (some-> midi (drums/closed-hat :amp 0.3)))

(def godel (sample "samples/goedel.aiff"))
(def escher (sample "samples/escher.aiff"))
(def bach (sample "samples/bach.aiff"))

(defn book [initial]
  (({\G godel
     \E escher
     \B bach}
    initial)))

(defmethod live/play-note :sample
  [{initial :pitch}]
  (book initial))
