(ns com.semperos.util-clj.exception)

(defmacro thrown?
  "Return truthy if the exception in `klass` is thrown, otherwise return falsey (nil) (code adapted from clojure.test"
  [klass & forms]
  `(try ~@forms
        false
        (catch ~klass e#
          true)))