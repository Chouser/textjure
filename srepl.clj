(ns srepl
  (:import (javax.swing JTextPane JScrollPane JFrame JSplitPane)
           (java.awt Insets Font Color)
           (javax.swing.text SimpleAttributeSet Style StyleConstants)))

(defn hex-color [s]
  (let [[r g b]
        (map #(Integer/parseInt (apply str %) 16) (partition 2 s))]
    (Color. r g b)))

(defn make-text-pane []
  (doto (JTextPane.)
    (.setCaretPosition 0)
    (.setMargin (Insets. 4 4 4 4))
    (.setCaretColor (hex-color "8b8bff"))
    (.setForeground (hex-color "cfbfad"))
    (.setBackground (hex-color "1e1e27"))
    (.setFont (Font. "Andale Mono" Font/PLAIN 16))))

; -- main --
(def mtp (make-text-pane))

(when *command-line-args*
  (.setText mtp (slurp (first *command-line-args*))))

(doto (JFrame.) (.add mtp) .pack .show)

;(def frame (JFrame.))
;(.pack frame)
;(.setVisible frame true)
;(JSplitPane. JSplitPane/VERTICAL_SPLIT (JScrollPane. textPane) (JScrollPane.)))
;(.add (.getContentPane frame) (JScrollPane. textPane))

;(def defaultStyle (SimpleAttributeSet.))
;(StyleConstants/setForeground defaultStyle (hex-color "cfbfad"))
;(.setLogicalStyle textPane defaultStyle)
;
;(javax.swing.text.DefaultStyledDocument$ElementSpec. defaultStyle, javax.swing.text.DefaultStyledDocument$ElementSpec/ContentType (into-array Character/TYPE "hello") 0 5); (.getDocument textPane)


;(.putClientProperty textPane JTextPane/HONOR_DISPLAY_PROPERTIES true)

