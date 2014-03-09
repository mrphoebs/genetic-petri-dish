(ns quilr.quil
  (:use quil.core)
  (:use quilr.dish))

(defn setup []
  (smooth)
  (frame-rate 1)
  (background 255)
  (stroke (color 255 0 0)))

(defn draw []
  (background 255)
  (genetic-algo (empty-dish 256)))

(defsketch sample
  :title "life"
  :setup setup
  :draw draw
  :size [256 256])
