(ns srepl
  (:import (javax.swing JTextPane JScrollPane JFrame JSplitPane
                        SwingUtilities Action KeyStroke)
           (java.awt Insets Font Color Dimension)
           (java.awt.event InputMethodListener)
           (javax.swing.text SimpleAttributeSet StyleConstants
                             JTextComponent)))

(defn hex-color
  "Takes a 6-digit hex string and returns a Color object"
  [s]
  (let [[r g b]
        (map #(Integer/parseInt (apply str %) 16) (partition 2 s))]
    (Color. r g b)))

(declare repl-keymap)

(defn make-text-pane
  "Creates a default-styled GUI text pane to be used as a file editor or REPL"
  []
  (doto (JTextPane.)
    (.setCaretPosition 0)
    (.setMargin (Insets. 4 4 4 4))
    (.setCaretColor (hex-color "8b8bff"))
    (.setForeground (hex-color "cfbfad"))
    (.setBackground (hex-color "1e1e27"))
    (.setFont (Font. "Andale Mono" Font/PLAIN 16))))

(defn make-repl-pane
  "Creates a GUI pane to be used as a REPL"
  []
  (doto (make-text-pane)
    (.setKeymap repl-keymap)))

(defmacro doswing
  "Macro.  Returns nil immediately.  Causes its body to be run later
  in the Swing thread."
  [& body]
  `(SwingUtilities/invokeLater #(do ~@body)))

; -- main --
(doswing
  (def repl-keymap
    #^{:doc "KeyMap to be used in REPL panes"}
    (JTextComponent/addKeymap
      "repl" (JTextComponent/getKeymap JTextComponent/DEFAULT_KEYMAP)))

  (def repl-pane
    #^{:doc "Main REPL pane"}
    (make-repl-pane))

  (def file-pane
    #^{:doc "Main file edit pane"}
    (make-text-pane))

  (def print-style
    #^{:doc "Style to be used for text printed (not typed) to the REPL"}
    (SimpleAttributeSet.))
  (StyleConstants/setForeground print-style (hex-color "808bed"))

  (def err-style
    #^{:doc "Style to be used for error text printed to the REPL"}
    (SimpleAttributeSet.))
  (StyleConstants/setBackground err-style (hex-color "6e2e2e"))
  (StyleConstants/setForeground err-style (hex-color "ffffff"))

  (doto (JFrame.)
    (.add (JSplitPane. JSplitPane/VERTICAL_SPLIT
                      (doto (JScrollPane. repl-pane)
                        (.setPreferredSize (Dimension. 800 300)))
                      (JScrollPane. file-pane)))
    ; onclose do (System/exit 0) ; XXX
    .pack .show))

(when *command-line-args*
  (let [text (slurp (first *command-line-args*))]
    (doswing (.setText file-pane text))))

(def repl-var-defaults
  #^{:doc "Map of vars to be bound for REPLs,
          with their default initial values"}
  {:*ns* (find-ns 'user) ; XXX neither set! nor in-ns seem to work
   :*warn-on-reflection* false
   :*print-meta* false
   :*print-length* 103
   :*print-level* 15
   :*compile-path* "classes"
   :*1 nil
   :*2 nil
   :*3 nil
   :*e nil})

(defmacro repl-binding
  "Macro.  Binds repl-vars thread-locally for the evaluation of body.
  Takes vars as a map of var names and the values to bind to them.
  Note this macro always binds all the vars given in
  repl-var-defaults, but uses the values passed in.  Returns a new map
  reflecting any value changes made by 'set!' in the body.  See repl-eval."
  [vars & body]
  (let [varsym (gensym 'vars)]
    `(let [~varsym ~vars]
       (binding ~(vec (mapcat (fn [[k v]] [(symbol (name k)) (list varsym k)])
                              repl-var-defaults))
         ~@body
         ~(apply hash-map (mapcat (fn [[k v]] [k (symbol (name k))])
                                  repl-var-defaults))))))

(def repl-agent
  #^{:doc "Agent that manages the state of the main REPL. See also repl-pane."}
  (agent repl-var-defaults))

(defn append-to-pane [pane text style]
  (let [doc (.getDocument pane)
        len (.getLength doc)]
    (.insertString doc len text style)
    (.setCaretPosition pane (+ len (.length text)))))

(defn repl-eval
  "Binds the var/value map given by vars and evaluats the Clojure text
  string in that binding context.  Returns a new var/value map
  reflecting any changes made by any 'set!' in the text expression."
  [vars text pane]
  (repl-binding vars
    (try
      (let [ret (load-string text)]
        (append-to-pane pane (str "\n" (prn-str ret)) print-style)
        (set! *3 *2)
        (set! *2 *1)
        (set! *1 ret))
      (catch Throwable e
        (set! *e e)
        (append-to-pane pane
                    (str "\n"
                         (last (take-while identity (iterate #(.getCause %) e)))
                         "\n")
                    err-style)))))

(defn bind-key-fn
  "Adds a keybinding for keystroke-str to keymap, such that when the
  keystroke-str is press, func is called.  The given func will be
  given a single Map; currently the only key is :source specifying the
  Swing component that was the source of the keystroke event.  The
  return value of func is ignored.  bind-key-fn returns nil.
  keystroke-str is interpreted by javax.swing.KeyStroke.getKeyStroke()
  See also bind-key"
  [keymap keystroke-str func]
  (doswing
    (.addActionForKeyStroke keymap
      (KeyStroke/getKeyStroke keystroke-str)
      (proxy [Action] []
        (actionPerformed [e] (func {:source (.getSource e)}))
        (isEnabled [] true)
        (getValue [k] nil)))))

(defmacro bind-key
  "Convenience macro for bind-key-fn.  Captures the local 'event' to
  supply the event Map to the given body.  See also bind-key-fn.

  Example:
  (bind-key-fn repl-keymap \"ENTER\"
    (println \"Enter pressed in\" (:source event)))"
  [keymap keystroke-str & body]
  `(bind-key-fn ~keymap ~keystroke-str (fn [~'event] ~@body)))

(defn unbind-key [keymap keystroke-str]
  "Removes the keybinding for keystroke-str from keymap.  See also bind-key-fn"
  (doswing
    (.removeKeyStrokeBinding keymap (KeyStroke/getKeyStroke keystroke-str))))

(bind-key repl-keymap "ENTER"
  ; events are handled in a swing thread, so it's ok to call .getText directly
  (send-off repl-agent repl-eval
            (last (.split (.getText (:source event)) "\n")) ; XXX slow
            (:source event)))

;  (.addInputMethodListener repl-pane (proxy [InputMethodListener] []
;                                       (caretPositionChanged [e] (prn :caret e))
;                                       (inputMethodTextChanged [e] (prn :input e)))))

;(javax.swing.text.DefaultStyledDocument$ElementSpec. defaultStyle, javax.swing.text.DefaultStyledDocument$ElementSpec/ContentType (into-array Character/TYPE "hello") 0 5); (.getDocument textPane)

;(.putClientProperty textPane JTextPane/HONOR_DISPLAY_PROPERTIES true)

