(ns under-my-thumb.core
  (use [clojure.java.io :only [as-file]])
  (import (java.awt Font Color Dimension))
  (import (java.awt.font FontRenderContext))
  (import (java.awt.image BufferedImage))
  (import (gui.ava.html.image.generator HtmlImageGenerator)))

(defn extension
  "Get the extension of a filename."
  [fname]
  (when fname
    (let [dot (.lastIndexOf ^String fname ".")]
      (when (and (>= dot 0)
                 (< dot (dec (count fname))))
        (subs fname (inc dot))))))

(def supported-formats {"pdf" :pdf "txt" :txt-plain "mdwn" :text-markdown "png" :img "jpg" :img "jpeg" :img "gif" :img "tiff" :img})

(defmulti make-thumbnail (fn [x _] (get supported-formats (extension x))))

(defn- replace-tabs [s & [n]]
  (let [n (or n 4)]
    (clojure.string/replace s "\t" (apply str (repeat n "nbsp;")))))

(defn- replace-nl [s]
  (clojure.string/replace s "\n" "<br>"))

(defmethod make-thumbnail :txt [fname width]
  "Returns BufferedImage containing a thumbnail of fname renedered with PDFBox."
  (let [s (slurp fname)
        html-s (-> s
                   (replace-tabs 4)
                   (replace-nl))
        igen (doto (HtmlImageGenerator.) (.loadHtml html-s))
        dim (.getSize igen)
        width (min (.getWidth dim) width)
        height (* (/ width (.getWidth dim)) (.getHeight dim))
        simg (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (doto (.createGraphics simg)
      (.drawImage (.getBufferedImage igen) 0 0 width height nil)
      (.dispose))
    simg))

;;  (javax.imageio.ImageIO/write simg "png" (as-file "test.png"))
  
(defmethod make-thumbnail :pdf [fname width]
  "Returns BufferedImage containing a thumbnail of fname renedered to image."
  (print "pdf" ":" width))

(defmethod make-thumbnail :img [fname width]
  "Returns BufferedImage containing a thumbnail of fname rerendered to new size."
  (let [img (javax.imageio.ImageIO/read (as-file fname))
        imgtype java.awt.image.BufferedImage/TYPE_INT_ARGB
        width (min (.getWidth img) width)
        height (* (/ width (.getWidth img)) (.getHeight img))
        simg (java.awt.image.BufferedImage. width height imgtype)
        g (.createGraphics simg)]
    (.drawImage g img 0 0 width height nil)
    (.dispose g)
    simg))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
