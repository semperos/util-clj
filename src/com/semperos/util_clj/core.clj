;; util-clj
;;
;; This is a collection of Clojure utilities that I want to keep handy.
;; Some of these are my own, some are snagged from elsewhere.

(ns com.semperos.util-clj.core
  (:use [clojure.contrib.duck-streams :only [with-out-writer]]))

;; Thanks to TJoC
(defmethod print-method clojure.lang.PersistentQueue
  [q w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

;; Thanks Clojuredocs.org user
(defn serialize-data
  "Print a data structure to a file so that we may read it in later."
  [data-structure ^String filename]
  (with-out-writer
    (java.io.File. filename)
    (binding [*print-dup* true] (prn data-structure))))