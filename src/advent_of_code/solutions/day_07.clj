(ns advent-of-code.solutions.day-07
  (:require [clojure.string :refer [split-lines split]]))

(defn update-files
  "Update the known structure of the file system given a terminal output.
   
   The existing files are expected to be given as a map from a string name
   of the file or folder to either an integer - if a file, which represents
   its size - or another map - if a folder, which represents the nested
   folder within it.
   
   The user's current location within the file system is specified as a
   vector of strings, representing folder names, with the outermost folder
   first in the list."
  [[files path] terminal]
  (let [segments (split terminal #" ")]
    (case (first segments)
      "$" (case (second segments)
            "cd" (case (nth segments 2)
                   "/" [files []]
                   ".." [files (->> path butlast (into []))]
                   (let [folder (conj path (nth segments 2))]
                     [(assoc-in files folder {})
                      folder]))
            "ls" [files path])
      "dir" [(assoc-in files
                      (conj path (second segments))
                      {})
             path]
      ;; Otherwise this is a file
      [(assoc-in files
                (conj path (second segments))
                (read-string (first segments)))
       path])))

(defn build-files [terminal-lines]
  (reduce update-files [{} []] terminal-lines))

(defn day-07a [input]
  (->> input
       split-lines
       build-files))

(defn day-07b [input]
  (->> input))
