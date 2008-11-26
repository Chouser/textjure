; srepl.clj -- Clojure REPL and editor using Swing GUI components
; Copyright Nov 2008 Chris Houser <chouser at n01se dot net>
; Released under the GNU GPL version 2

(ns srepl
  (:import (javax.swing JTextPane JScrollPane JFrame JSplitPane
                        SwingUtilities Action KeyStroke Box BoxLayout)
           (java.awt Insets Font Color Dimension)
           (java.io PushbackReader StringReader OutputStream PrintWriter)
           (java.awt.event InputMethodListener)
           (javax.swing.text SimpleAttributeSet StyleConstants
                             JTextComponent))
  (:use [clojure.contrib.def :only (defvar)]))

(defn hex-color
  "Expects a six-hex-digit int and returns a Color object"
  [n]
  (let [[b g r]
        (map #(rem % 256) (take 3 (iterate #(quot % 256) n)))]
    (Color. r g b)))

(defmacro make-map [& body]
  `(with-local-vars [~'*made-map* {}]
     ~@body
     (var-get ~'*made-map*)))

(defmacro do-map [k & doto-body]
  `(let [v# (doto ~@doto-body)]
     (var-set ~'*made-map* (assoc (var-get ~'*made-map*) ~k v#))
     v#))

(declare repl-keymap)

(defn make-text-pane
  "Creates a default-styled GUI text pane to be used as a file editor or REPL"
  []
  (doto (JTextPane.)
    (.setCaretPosition 0)
    (.setMargin (Insets. 4 4 4 4))
    (.setCaretColor (hex-color 0x8b8bff))
    (.setForeground (hex-color 0xcfbfad))
    (.setBackground (hex-color 0x1e1e27))
    (.setFont (Font. "Andale Mono" Font/PLAIN 16))))

(defn make-repl-widget
  "Creates a new REPL context with a GUI widget attached"
  []
  (let [widget (make-map
                 (do-map :outer (Box. BoxLayout/PAGE_AXIS)
                   (.add (doto (JScrollPane. (do-map :log (make-text-pane)))
                           ;(-> .getViewport (.setBackground (hex-color 0)))
                           (.setPreferredSize (Dimension. 800 250))))
                   (.add (JScrollPane.
                           (do-map :input (make-text-pane)
                             (.setFont (Font. "Andale Mono" Font/BOLD 16))
                             (.setKeymap repl-keymap))))))]
    (.putClientProperty (:input widget) :widget widget)
    widget))

(defmacro doswing
  "Macro.  Returns nil immediately.  Causes its body to be run later
  in the Swing thread.  See also doswing-wait"
  [& body]
  `(SwingUtilities/invokeLater #(do ~@body)))

(defmacro doswing-wait
  "Macro.  Causes its body to be run later in the Swing thread, blocks
  until complete.  Returns nil.  See also doswing"
  [& body]
  `(SwingUtilities/invokeAndWait #(do ~@body)))

(defvar style-setters
  {:bold ['setBold identity]
   :fg   ['setForeground #(list 'hex-color %)]
   :bg   ['setBackground #(list 'hex-color %)]}
  "Maps simple style keywords to a vector [method-name, func], where
  method-name is a static method of the AttributeSet class and func
  converts a simple value to the object needed by the method.")

(defmacro def-style [nm & items]
  `(def ~(with-meta nm (assoc ^nm :doc (last items)))
     (doto (new ~'SimpleAttributeSet)
       ~@(for [[k v] (partition 2 items)]
           (let [[method valfn] (style-setters k)]
             (list (symbol "StyleConstants" (name method)) (valfn v)))))))

(def-style input-style :bold true, :fg 0xcfbfad
  "Style to be used for text entered into the REPL by the user")

(def-style print-style :bold false, :fg 0x808bed ; inkpot Statement
  "Style to be used for text printed (not typed) to the REPL")

(def-style err-style :bold false :bg 0x6e2e2e :fg 0xffffff ; inkpot Error
  "Style to be used for error text printed to the REPL")

(def-style debug-style :bold false :fg 0xcd8b00 ; inkpot Comment
  "Style to be used for printing debug text to the REPL")

(def-style no-eol-style :bg 0x404040 :fg 0xffcd8b ; inkpot String
  "Style to be used to indicate a line does not end in newline")

(defn append-to-pane [pane text style]
  (doswing ; insertString is thread-safe, but the other methods are not.
    (let [doc (.getDocument pane)
          len (.getLength doc)]
      (.insertString doc len text style)
      (.setCaretPosition pane (+ len (.length text))))))

(defn make-pane-stream [pane style]
  (let [w (java.io.StringWriter.)]
    (proxy [java.io.Writer] []
      (write [b & [off len]]
        (if (or (integer? b) (string? b))
          (.write w b)
          (.write w b off len)))
      (flush []
        (append-to-pane pane (str w) style)
        (.setLength (.getBuffer w) 0))
      (close []
        ; Since 'close' is pretty meaningless, use it here to signal
        ; completion, meaning buffers should have lines terminated and
        ; be flushed
        (let [buf (.getBuffer w)
              len (.length buf)
              needs-term (and (pos? len)
                              (not= \newline (.charAt buf (dec len))))]
          (.flush this)
          (when needs-term
            (append-to-pane pane "*\n" no-eol-style)))))))

(defn complete-stream [stream]
  (.close stream))

(doswing-wait
  (def repl-keymap
    #^{:doc "KeyMap to be used in REPL panes"}
    (JTextComponent/addKeymap
      "repl" (JTextComponent/getKeymap JTextComponent/DEFAULT_KEYMAP)))

  (def repl-widget
    #^{:doc "Main REPL widget"}
    (make-repl-widget))

  (def file-pane
    #^{:doc "Main file edit pane"}
    (make-text-pane)))

; -- main --
(doswing
  (doto (JFrame.)
    (.add (JSplitPane. JSplitPane/VERTICAL_SPLIT
                      (:outer repl-widget)
                      (JScrollPane. file-pane)))
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    .pack (.setVisible true))
  (.requestFocusInWindow (:input repl-widget)))

(when *command-line-args*
  (let [text (slurp (first *command-line-args*))]
    (doswing (.setText file-pane text))))

(def repl-var-defaults
  #^{:doc "Map of vars to be bound for REPLs,
          with their default initial values"}
  {:*ns* (find-ns 'srepl)
   :*warn-on-reflection* false
   :*print-meta* false
   :*print-length* 103
   :*print-level* 15
   :*compile-path* "classes"
   :*out* nil
   :*err* nil
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

(def repl-out (make-pane-stream (:log repl-widget) debug-style))
(def repl-err (make-pane-stream (:log repl-widget) err-style))

(def repl-agent
  #^{:doc "Agent that manages the state of the main REPL. See also repl-pane."}
  (agent (assoc repl-var-defaults :*out* repl-out :*err repl-err)))

(defn repl-eval
  "Binds the var/value map given by vars and evaluats the Clojure text
  string in that binding context.  Returns a new var/value map
  reflecting any changes made by any 'set!' in the text expression."
  [vars text pane]
  (append-to-pane pane (str text "\n") input-style)
  (repl-binding vars
    (let [reader (PushbackReader. (StringReader. text))]
      (try
        (loop []
          (let [f (read reader false reader false)]
            (when-not (identical? f reader)
              (let [ret (eval f)]
                (append-to-pane pane (prn-str ret) print-style)
                (set! *3 *2)
                (set! *2 *1)
                (set! *1 ret))
              (recur))))
        (catch Throwable e
          (set! *e e)
          (append-to-pane pane
                          (str (last (take-while identity
                                        (iterate #(.getCause %) e))) "\n")
                          err-style))))
    (complete-stream repl-out)
    (complete-stream repl-err)))

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
        (actionPerformed [e] (func {:source (.getSource e)
                                    :widget (.getClientProperty
                                              (.getSource e) :widget)}))
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
            (.getText (:input (:widget event)))
            (:log (:widget event)))
  (.setText (:input (:widget event)) nil))

;  (.addInputMethodListener repl-pane (proxy [InputMethodListener] []
;                                       (caretPositionChanged [e] (prn :caret e))
;                                       (inputMethodTextChanged [e] (prn :input e)))))

;(javax.swing.text.DefaultStyledDocument$ElementSpec. defaultStyle, javax.swing.text.DefaultStyledDocument$ElementSpec/ContentType (into-array Character/TYPE "hello") 0 5); (.getDocument textPane)

;(.putClientProperty textPane JTextPane/HONOR_DISPLAY_PROPERTIES true)

