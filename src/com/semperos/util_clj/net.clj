(ns com.semperos.util-clj.net)

(defn uri-encode
  "Encode a string to a properly formatted URI"
  [s]
  (let [protocol (second (re-find #"^(http|https|ftp)://" s))
        base (nth (re-find #"^(http|https|ftp)://([^/]*)/" s) 2)
        path (nth (re-find #"^(http|https|ftp)://[^/]*(/.*?)$" s) 2)]
    (java.net.URI. protocol base path nil)))