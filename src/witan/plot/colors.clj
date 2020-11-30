(ns witan.plot.colors
  (:require [clojure2d.color :as color]))

(def orange (nth (color/palette-presets :tableau-20) 2))
(def blue (nth (color/palette-presets :tableau-20) 5))
(def green (nth (color/palette-presets :tableau-20) 4))
(def white (color/color :white))
(def palette (color/palette-presets :tableau-20))
(def points [\V \\
             \^ \|
             \O \/
             \o \A
             \> \x
             \v \S
             \{ \s
             \< \}
             \-])


(defn legend-shape [s]
  (case s
    \^ \v
    \A \V
    \v \^
    \V \A
    \\ \/
    \/ \\
    s))

(defn domain-colors-and-shapes [domain]
  (into {}
        (sequence
         (map (fn [d c s]
                [d {:color c :shape s :legend-shape (legend-shape s)}]))
         domain
         (cycle palette)
         (cycle points))))
