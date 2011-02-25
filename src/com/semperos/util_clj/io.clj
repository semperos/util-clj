;; util-clj.io
;;
;; This namespace is dedicated to I/O-specific functionality. Special thanks
;; to nakkaya dot com.

(ns com.semperos.util-clj.io)

(defn fetch-url-text
  "Fetch text-based data at the given url"
  [url]
  (with-open [stream (.openStream (java.net.URL. url))]
    (let  [buf (java.io.BufferedReader.
                (java.io.InputStreamReader. stream))]
      (apply str (line-seq buf)))))

(defn fetch-url-data
  "Fetch binary data at the given url"
  [url]
  (let  [con    (-> url java.net.URL. .openConnection)
         fields (reduce (fn [h v]
                          (assoc h (.getKey v) (into [] (.getValue v))))
                        {} (.getHeaderFields con))
         size   (first (fields "Content-Length"))
         in     (java.io.BufferedInputStream. (.getInputStream con))
         out    (java.io.BufferedOutputStream.
                 (java.io.FileOutputStream. "out.file"))
         buffer (make-array Byte/TYPE 1024)]
    (loop [g (.read in buffer)
           r 0]
      (if-not (= g -1)
        (do
          (println r "/" size)
          (.write out buffer 0 g)
          (recur (.read in buffer) (+ r g)))))
    (.close in)
    (.close out)
    (.disconnect con)))

(defn socket-connection
  "Connect directly to a socket"
  [host port]
  (let [socket (java.net.Socket. host port)
        in (java.io.BufferedReader.
            (java.io.InputStreamReader. (.getInputStream socket)))
        out (java.io.PrintWriter. (.getOutputStream socket))]
    {:in in :out out}))

(comment
  (slurp "some.txt")

  (with-open [rdr (java.io.BufferedReader.
                   (java.io.FileReader. "project.clj"))]
    (let [seq (line-seq rdr)]
      (count seq)))

  (fetch-url-text "http://www.google.com")
  (fetch-url-data "http://www.ctan.org/tex-archive/info/lshort/english/lshort.pdf")

  (spit "myfile.txt")
  (binding [*out* (java.io.FileWriter. "some.dat")]
     (prn {:a :b :c :d}))
)
