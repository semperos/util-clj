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

;; Thanks to John Harrop
(defn memoize-expire
  "Returns a memoized version of a referentially transparent
   function. The memoized version of the function keeps a cache of
   the mapping from arguments to results and, when calls with the
   same arguments are repeated often, has higher performance at
   the expense of higher memory use. Cached results are removed
   from the cache when their time to live value expires (done in
separate thread)."
  [function time-to-live]
  (let [cached-results (ref {})
        last-time (ref (System/currentTimeMillis))
        expiry-queue (ref [])
        process-queue #(when-not (empty? @expiry-queue)
                         (Thread/sleep
                           (:wait (first @expiry-queue)))
                           (dosync
                             (let [args (:item (first (ensure
expiry-queue)))
                                   item (get (ensure cached-results) args)
                                   t (- (:time item) 100)]
                               (if (<= t (System/currentTimeMillis))
                                 (alter cached-results dissoc args))
                               (ref-set expiry-queue
                                 (vec (rest @expiry-queue)))))
                         (recur))
        ts-agent (agent nil)]
    (fn [& args]
      (let [result (if-let [r (get @cached-results args)]
                     (:result r)
                     (apply function args))]
        (dosync
          (let [t (+ time-to-live (System/currentTimeMillis))
                l (max (ensure last-time) (System/currentTimeMillis))
                w (- t l)
                q (ensure expiry-queue)]
            (alter cached-results assoc args
              {:result result :time t})
            (ref-set last-time t)
            (alter expiry-queue conj {:item args :wait w})
            (if (empty? q)
              (send ts-agent (fn [_] (.start (Thread. process-queue)))))))
        result))))