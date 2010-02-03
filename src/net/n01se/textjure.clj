; srepl.clj -- Clojure REPL and editor using Swing GUI components
; Copyright Nov 2008 Chris Houser <chouser at n01se dot net>
; Released under the GNU GPL version 2

(ns net.n01se.textjure
  (:import (javax.swing JTextPane JScrollPane JFrame JSplitPane
                        SwingUtilities Action KeyStroke)
           (java.awt Insets Font Color Dimension Container
                     GridBagLayout GridBagConstraints)
           (java.io PushbackReader StringReader OutputStream PrintWriter)
           (java.awt.event InputMethodListener)
           (javax.swing.text SimpleAttributeSet StyleConstants
                             JTextComponent DefaultStyledDocument)
           (clojure.lang RT))
  (:use [clojure.contrib.def :only (defvar)]))

(declare repl-keymap repl-widget file-pane)

(defn hex-color
  "Expects a six-hex-digit int and returns a Color object"
  [n]
  (let [[b g r]
        (map #(rem % 256) (take 3 (iterate #(quot % 256) n)))]
    (Color. r g b)))

(defmacro hash-map* [sym & body]
  `(let [~sym {}
         ~@(mapcat (fn [[k v]] [sym `(assoc ~sym ~k ~v)]) (partition 2 body))]
     ~sym))

(defmacro into-map [& body]
  `(with-local-vars [~'*made-map* {}]
     ~@body
     (var-get ~'*made-map*)))

(defmacro do-map [& args]
  (let [[keywords body] (split-with keyword? args)
        obj (gensym)]
    (if keywords
      `(let [~obj (doto ~@body)]
         (var-set ~'*made-map* (conj (var-get ~'*made-map*)
                                     ~@(for [k keywords] [k obj])))
         ~obj)
      `(doto ~@args))))

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

(defmacro assert-swing
  []
  `(assert (SwingUtilities/isEventDispatchThread)))

(defn make-text-pane
  "Creates a default-styled GUI text pane to be used as a file editor or REPL"
  []
  (assert-swing)
  (let [check-filter (fn [doc details]
                       (let [f (.getProperty doc :input-filter)]
                         (or (nil? f) (f details))))
        doc (proxy [DefaultStyledDocument] []
              (insertString [off s attrs]
                (when (check-filter this {:type :insert :offset off :str s})
                  (proxy-super insertString off s attrs)))
              (remove [off len]
                (when (check-filter this {:type :remove :offset off :len len})
                  (proxy-super remove off len))))]
    (doto (proxy [JTextPane] [doc]
            (setSize [dim] (proxy-super setSize
                                        (.width dim)
                                        (.height (.getPreferredSize this)))))
      (.setCaretPosition 0)
      (.setMargin (Insets. 4 4 4 4))
      (.setCaretColor (hex-color 0x8b8bff))
      (.setForeground (hex-color 0xcfbfad))
      (.setBackground (hex-color 0x1e1e27))
      (.setFont (Font. "Andale Mono" Font/PLAIN 16)))))

(defn anchor-page-end [obj]
  "Returns a new Container with component obj inside it, anchored to
  the PAGE_END (usually the bottom)"
  (assert-swing)
  (let [parent (doto (Container.) (.add obj))
        l (GridBagLayout.)
        c (GridBagConstraints.)]
    (set! (.fill c) GridBagConstraints/HORIZONTAL)
    (set! (.anchor c) GridBagConstraints/PAGE_END)
    (set! (.weightx c) 1.0)
    (set! (.weighty c) 1.0)
    (.setConstraints l obj c)
    (.setLayout parent l)
    parent))

(defn fonts-for-char [c]
  (assert-swing)
  (filter #(.canDisplay (Font. % Font/PLAIN 16) c)
          (.getAvailableFontFamilyNames
            (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment))))

(defvar style-setters
  {:bold ['setBold identity]
   :font-family ['setFontFamily identity]
   :size ['setFontSize identity]
   :fg   ['setForeground #(list 'hex-color %)]
   :bg   ['setBackground #(list 'hex-color %)]}
  "Maps simple style keywords to a vector [method-name, func], where
  method-name is a static method of the AttributeSet class and func
  converts a simple value to the object needed by the method.")

(defmacro def-style [nm & items]
  `(def ~(with-meta nm (assoc (meta nm) :doc (last items)))
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

(def-style no-eol-style :bg 0x404040 :fg 0xffcd8b ; based on inkpot String
                        :font-family "DejaVu Sans Mono" :bold false
  "Style to be used to indicate a line does not end in newline")

(def *appending-to-widget* false)

(defn append-to-widget
  "Used to programmatically add text to the REPL widget's log.
  Dynamically binds *appending-to-widget* to true, so that
  input-filters can allow the updates to happen outside the
  user-editable region."
  [widget text style]
  (when (seq text)
    (doswing ; insertString is thread-safe, but the other methods are not.
      (let [pane (:log widget)
            doc (.getDocument pane)]
        (binding [*appending-to-widget* true]
          (.insertString doc @(:log-end widget) text style)
          (swap! (:log-end widget) + (.length text))
          (.setCaretPosition pane (.getLength doc))
          (.setCharacterAttributes doc (.getLength doc) 1 input-style true))))))

(defn make-widget-stream [widget style]
  (let [w (java.io.StringWriter.)]
    (proxy [java.io.Writer] []
      (write [b & [off len]]
        (if (or (integer? b) (string? b))
          (.write w b)
          (.write w b off len)))
      (flush []
        (append-to-widget widget (str w) style)
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
            (append-to-widget widget "◄\n" no-eol-style)))))))

(defn complete-stream [stream]
  (.close stream))

(def repl-var-defaults
  #^{:doc "Map of vars to be bound for REPLs,
          with their default initial values"}
  {:*ns* (find-ns 'net.n01se.textjure)
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

(defn make-repl-widget
  "Creates a new REPL context with a GUI widget attached"
  []
  (assert-swing)
  (let [widget (hash-map* w
                 :log (doto (make-text-pane) (.setKeymap repl-keymap))
                 :outer (doto (JScrollPane. (anchor-page-end (:log w)))
                          (-> .getViewport (.setBackground (hex-color 0))))
                 :log-end (atom 0)
                 :agent (agent (assoc repl-var-defaults
                                 :*out* (make-widget-stream w debug-style)
                                 :*err* (make-widget-stream w err-style))))]
    (append-to-widget widget "Clojure\n" debug-style)
    (.putClientProperty (:log widget) :widget widget)
    (.putProperty (.getDocument (:log widget)) :input-filter
                  (fn [{off :offset}]
                    (or *appending-to-widget* (>= off @(:log-end widget)))))
    widget))

(defn first-cause
  "Return the initial or root cause of the given exception."
  [e]
  (last (take-while identity (iterate #(.getCause %) e))))

(defn report-exception
  "Report an expection for the given repl widget."
  [widget e]
  (set! *e e)
  (append-to-widget widget (str (first-cause e) "\n") err-style))

(defn repl-eval
  "Binds the var/value map given by vars and evaluates the Clojure text
  string in that binding context.  Returns a new var/value map
  reflecting any changes made by any 'set!' in the text expression."
  [vars widget text]
  (repl-binding vars
    (let [eof (Object.)
          forms (with-open [rdr (PushbackReader. (StringReader. text))]
                  (try
                    (doall
                      (take-while #(not= % eof)
                                  (repeatedly #(read rdr false eof false))))
                    (catch Exception e
                      (when (not= (.getMessage (first-cause e))
                                  "EOF while reading")
                        (report-exception widget e)))))]
      (when forms
        (swap! (:log-end widget) + (inc (.length text)))
        (try
          (doseq [f forms]
            (let [ret (eval f)]
              (append-to-widget widget (prn-str ret) print-style)
              (set! *3 *2)
              (set! *2 *1)
              (set! *1 ret)))
          (catch Throwable e
            (report-exception widget e))))
      (complete-stream *out*)
      (complete-stream *err*))))

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

(defn read-stream-lines [strm]
  (loop [buf (StringBuilder.) lines [0 0] p 0]
    (let [c (.read strm)]
      (if (< c 0)
        [(str buf) lines]
        (recur (.append buf (char c))
               (if (== c 10) (conj lines (inc p)) lines)
               (inc p))))))

(defn edit [x]
  (or (if-let [v (resolve x)]
        (let [ns-name (str (.name (.ns v)))
              path (first (re-seq #"^.*(?=/[^/]*$)" (.replace ns-name "." "/")))
              fname (str path "/" (:file (meta v)))]
          (when-let [strm (.getResourceAsStream (RT/baseLoader) fname)]
            (let [[text lines] (with-open [strm strm] (read-stream-lines strm))]
              (doswing
                (doto file-pane
                  (.setText text)
                  (.setCaretPosition (lines (:line (meta v))))
                  (.requestFocusInWindow))))
            fname)))
        (str "Source not found for " x)))

(declare bind-keys)
(def *force-bind?* false)

(defn stroke-fn [stroke]
  (cond
    (instance? Character stroke)
      (fn [state-map named-states next strokes]
        (let [old-branch (state-map next {})]
          (if-let [old-val (old-branch stroke)]
            (bind-keys state-map named-states old-val strokes)
            (let [new-state (gensym 'state)]
              (bind-keys (assoc-in state-map [next stroke] new-state)
                         named-states new-state strokes)))))
    (symbol? stroke)
      (fn [state-map named-states next strokes]
        (assert (nil? strokes))
        (when-let [old-branch (and (not *force-bind?*) (state-map next))]
          (if (old-branch :final)
            (throw (Exception. "New binding would replace existing binding"))
            (throw (Exception. "New binding would eclipse existing binding"))))
        (assoc-in state-map [next :final] stroke))
    :else (throw (Exception. (str "Unknown bind obj " stroke)))))

(defn alt [& options]
  (fn [state-map named-states next strokes]
    (reduce (fn [state-map option]
              (bind-keys state-map named-states next
                         (if (nil? option)
                           strokes
                           (cons option strokes))))
            state-map options)))

(defn bind-keys [state-map named-states next [stroke & strokes :as v]]
  (if-not v
    state-map
    (let [func (if (fn? stroke) stroke (stroke-fn stroke))]
      (if (and (not *force-bind?*) (:final (state-map next)) strokes)
        (throw (Exception. "Existing binding would eclipse new binding"))
        (func state-map named-states next strokes)))))



;(bind-keys (bind-keys {} {} :normal [\" \a \y \y 'go]) {} :normal [\" \a \y \j 'down])
;(bind-keys {} {} :normal [\" (alt nil x) \y 'done])
;(bind-keys {} {} :normal (alt \0 \1 \2 \3 \4 \5 \6 \7 \8 \9))

;(defn digit [state-map named-states next strokes] ...)
;(defgram digit (alt \0 \1 \2 \3 \4 \5 \6 \7 \8 \9))
;(defgram numeric (alt nil [digit numeric]))

;(def alpha (apply alt (map #(char (+ 97 %)) (range 26))))
;(defn register [] (alt nil [\" (append :register alpha)]))
;(defn num-arg [arg-name] (alt nil [(append arg-name (digit)) (num-arg arg-name)]))

;(bind-keys :cmd [(num-arg :early-num) (register) \y
;                 (alt (append :double \y) (movement) (visual-block))])

; -- main --

(defn -main [ & args]
  (doswing-wait
    (defvar repl-keymap
      (JTextComponent/addKeymap
        "repl" (JTextComponent/getKeymap JTextComponent/DEFAULT_KEYMAP))
      "KeyMap to be used in REPL panes")

    (defvar repl-widget (make-repl-widget) "Main REPL widget")
    (defvar file-pane   (make-text-pane)   "Main file edit pane"))

  (when args
    (let [text (slurp (first args))]
      (doswing (.setText file-pane text))))

  (doswing
    (doto (JFrame. "textjure")
      (.add (doto (JSplitPane.
                    JSplitPane/VERTICAL_SPLIT
                    (doto (JScrollPane. file-pane) ; XXX should be in file-widget
                      (-> .getViewport (.setBackground (hex-color 0))))
                    (:outer repl-widget))))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      .pack
      (.setVisible true))
    (.requestFocusInWindow (:log repl-widget)))

  (bind-key repl-keymap "ENTER"
    (assert-swing)
    (let [widget (:widget event)
          doc (.getDocument (:log widget))
          offset @(:log-end widget)
          text (.getText doc offset (- (.getLength doc) offset))]
      (.insertString doc (.getLength doc) "\n" input-style)
      (.setCaretPosition (:log widget) (.getLength doc))
      (send-off (:agent widget) repl-eval widget text))))

        ;(if-let [strm (.getResourceAsStream (RT/baseLoader) filename)]
            ;(with-open [rdr (LineNumberedReader. (InputStreamReader. strm))]
            ;(.setText pane (apply str (for [c (iterate #(.readLine strm))
            ;                                :while (>= c 0)]
            ;                            (char c))))

;  (.addInputMethodListener repl-pane (proxy [InputMethodListener] []
;                                       (caretPositionChanged [e] (prn :caret e))
;                                       (inputMethodTextChanged [e] (prn :input e)))))

;(javax.swing.text.DefaultStyledDocument$ElementSpec. defaultStyle, javax.swing.text.DefaultStyledDocument$ElementSpec/ContentType (into-array Character/TYPE "hello") 0 5); (.getDocument textPane)

;(.putClientProperty textPane JTextPane/HONOR_DISPLAY_PROPERTIES true)

