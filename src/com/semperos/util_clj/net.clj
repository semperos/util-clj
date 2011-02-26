(ns com.semperos.util-clj.net)

(defn url-encode
  "Encode a string to URL-formatted string"
  [s]
  (let [protocol (second (re-find #"^(http|https)://" s))
        base (nth (re-find #"^(http|https)://([^/]*)/" s) 2)
        path (nth (re-find #"^(http|https)://[^/]*(/.*?)$" s) 2)]
    (.toString (java.net.URI. protocol base path nil))))