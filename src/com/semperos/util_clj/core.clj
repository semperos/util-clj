;; util-clj
;;
;; This is a collection of Clojure utilities that I want to keep handy.
;; Some of these are my own, some are snagged from elsewhere.

(ns com.semperos.util-clj.core)

(defmethod print-method clojure.lang.PersistentQueue
  [q w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))