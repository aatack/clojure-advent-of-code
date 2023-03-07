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

(defn size-of-small-folders [folder]
  (reduce (fn [[folder-size total-size] [_ size-or-folder]]
            (if (integer? size-or-folder)
              [(+ folder-size size-or-folder) total-size]
              (let [[child-folder-size child-total-size]
                    (size-of-small-folders size-or-folder)]
                [(+ folder-size child-folder-size)
                 (+ total-size
                    child-total-size
                    (if (< child-folder-size 100000)
                      child-folder-size
                      0))])))
          [0 0]
          folder))

(defn day-07a [input]
  (second (size-of-small-folders (->> input
                                      split-lines
                                      build-files
                                      first))))

(defn pick-smallest-sufficient-folder [sizes size]
  (prn sizes)
  (let [filtered-sizes (filter #(and % (>= % size)) sizes)]
    (when (not-empty filtered-sizes) (apply min filtered-sizes))))

(defn find-smallest-sufficient-folder
  "Get the size of the folder and the smallest child folder above a threshold.
   
   TODO: this should be modified such that it also takes the root folder into
         consideration, removing the need for the outer call to
         `pick-smallest-sufficient-folder` in the calling function below."
  [folder size]
  (reduce (fn [[folder-size smallest-size] [_ size-or-folder]]
            (if (integer? size-or-folder)
              [(+ folder-size size-or-folder) smallest-size]
              (let [[child-folder-size child-smallest-size]
                    (find-smallest-sufficient-folder size-or-folder size)]
                [(+ folder-size child-folder-size)
                 (pick-smallest-sufficient-folder
                  [smallest-size child-smallest-size child-folder-size]
                  size)])))
          [0 nil]
          folder))

(defn day-07b [input]
  (let [folder (->> input
                    split-lines
                    build-files
                    first)
        size (- 30000000
                (- 70000000 (first (size-of-small-folders folder))))]
    (pick-smallest-sufficient-folder (find-smallest-sufficient-folder folder size)
                                     size)))
