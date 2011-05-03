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

;; Thanks Meikel Brandmeyer, Christophe Grande and others
(declare naive-strategy)

(defn memoize
  "Returns a memoized version of a referentially transparent function.
  The memoized version of the function keeps a cache of the mapping from
  arguments to results and, when calls with the same arguments are repeated
  often, has higher performance at the expense of higher memory use.

  Optionally takes a cache strategy. The strategy is provided as a map
  containing the following keys. All keys are mandatory!

    - :init   – the initial value for the cache and strategy state
    - :cache  – access function to access the cache
    - :lookup – determines whether a value is in the cache or not
    - :hit    – a function called with the cache state and the argument
                list in case of a cache hit
    - :miss   – a function called with the cache state, the argument list
                and the computation result in case of a cache miss

  The default strategy is the naive safe-all strategy."
  ([f] (memoize f naive-strategy))
  ([f strategy]
   (let [{:keys [init cache lookup hit miss]} strategy
         cache-state (atom init)
         hit-or-miss (fn [state args]
                       (if (lookup state args)
                         (hit state args)
                         (miss state args (delay (apply f args)))))]
     (fn [& args]
       (let [cs (swap! cache-state hit-or-miss args)]
         (-> cs cache (get args) deref))))))

(def #^{:doc "The naive safe-all cache strategy for memoize."}
  naive-strategy
  {:init   {}
   :cache  identity
   :lookup contains?
   :hit    (fn [state _] state)
   :miss   assoc})

(defn fifo-cache-strategy
  "Implements a first-in-first-out cache strategy. When the given limit
  is reached, items from the cache will be dropped in insertion order."
  [limit]
  {:init   {:queue (into clojure.lang.PersistentQueue/EMPTY
                         (repeat limit :dummy))
            :cache {}}
   :lookup (fn [state k] (contains? (:cache state) k))
   :cache  :cache
   :hit    (fn [state _] state)
   :miss   (fn [state args result]
             (let [k (-> state :queue peek)]
               (-> state
                 (update-in [:queue] conj args)
                 (update-in [:queue] pop)
                 (update-in [:cache] dissoc k)
                 (assoc-in  [:cache] args result))))})

(defn lru-cache-strategy
  "Implements a LRU cache strategy, which drops the least recently used
  argument lists from the cache. If the given limit of items in the cache
  is reached, the longest unaccessed item is removed from the cache. In
  case there is a tie, the removal order is unspecified."
  [limit]
  {:init   {:lru   (into {} (for [x (range (- limit) 0)] [x x]))
            :tick  0
            :cache {}}
   :cache  :cache
   :lookup (fn [state k] (contains? (:cache state) k))
   :hit    (fn [state args]
             (-> state
               (assoc-in  [:lru]  args (:tick state))
               (update-in [:tick] inc)))
   :miss   (fn [state args result]
             (let [k (apply min-key (:lru state) (keys (:lru state)))]
               (-> state
                 (update-in [:lru]   dissoc k)
                 (update-in [:cache] dissoc k)
                 (assoc-in  [:lru]   args (:tick state))
                 (update-in [:tick]  inc)
                 (assoc-in  [:cache] args result))))})

(defn ttl-cache-strategy
  "Implements a time-to-live cache strategy. Upon access to the cache
  all expired items will be removed. The time to live is defined by
  the given expiry time span. Items will only be removed on function
  call. No background activity is done."
  [ttl]
  (let [dissoc-dead (fn [state now]
                      (let [ks (map key (filter #(> (- now (val %)) ttl)
                                                (:ttl state)))
                            dissoc-ks #(apply dissoc % ks)]
                        (-> state
                          (update-in [:ttl]   dissoc-ks)
                          (update-in [:cache] dissoc-ks))))]
    {:init   {:ttl {} :cache {}}
     :cache  :cache
     :lookup (fn [state args]
               (when-let [t (get (:ttl state) args)]
                 (< (- (System/currentTimeMillis) t) ttl)))
     :hit    (fn [state args]
               (dissoc-dead state (System/currentTimeMillis)))
     :miss   (fn [state args result]
               (let [now (System/currentTimeMillis)]
                 (-> state
                   (dissoc-dead now)
                   (assoc-in  [:ttl]   args now)
                   (assoc-in  [:cache] args result))))}))

(defn lu-cache-strategy
  "Implements a least-used cache strategy. Upon access to the cache
  it will be tracked which items are requested. If the cache size reaches
  the given limit, items with the lowest usage count will be removed. In
  case of ties the removal order is unspecified."
  [limit]
  {:init   {:lu (into {} (for [x (range (- limit) 0)] [x x])) :cache {}}
   :cache  :cache
   :lookup (fn [state k] (contains? (:cache state) k))
   :hit    (fn [state args] (update-in state [:lu args] inc))
   :miss   (fn [state args result]
             (let [k (apply min-key (:lu state) (keys (:lu state)))]
               (-> state
                 (update-in [:lu]    dissoc k)
                 (update-in [:cache] dissoc k)
                 (assoc-in  [:lu]    args 0)
                 (assoc-in  [:cache] args result))))})