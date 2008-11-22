(ns net.n01se.textjure
  (:import (com.trolltech.qt.gui QApplication QMainWindow QTextEdit QGridLayout
                                 QTextCursor$MoveOperation)))

(def app (QApplication. (into-array String *command-line-args*)))

(def main-window (doto (QMainWindow.)
                         (.setWindowTitle "Textjure")
                         .show))

(defn move-cursor
  "Update the editor's cursor by applying the given operation.
  Returns the updated cursor.
  Example: (move-cursor editor QTextCursor$MoveOperation/Down)"
  [editor op]
  (let [c (.textCursor (:qedit @editor))]
    (.movePosition c op)
    (.setTextCursor (:qedit @editor) c)
    c))

(def vim-cmd
  {"h" #(move-cursor % QTextCursor$MoveOperation/Left)
   "j" #(move-cursor % QTextCursor$MoveOperation/Down)
   "k" #(move-cursor % QTextCursor$MoveOperation/Up)
   "l" #(move-cursor % QTextCursor$MoveOperation/Right)
   "b" #(move-cursor % QTextCursor$MoveOperation/WordLeft)
   "w" #(move-cursor % QTextCursor$MoveOperation/WordRight)
   "0" #(move-cursor % QTextCursor$MoveOperation/StartOfLine)
   "$" #(move-cursor % QTextCursor$MoveOperation/EndOfLine)
   "G" #(move-cursor % QTextCursor$MoveOperation/End)
   "i" nil})

(defn key-press [editor event]
  (prn event @editor)
  (let [s (.text event)
        handler ((:keymap @editor) (if (seq s) s (.key event)))]
    (cond
      handler (handler editor)
      (<= "0" s "9")
        (dosync (commute (:count-arg @editor)
                         #(+ (* 10 (or % 0)) (- (int (first s)) 48)))))))

(defn edit [editor filename]
  (let [text (slurp filename)]
    (dosync (ref-set (:filename @editor) filename))
    (.setText (:qedit @editor) text)))

(defn make-editor [& args]
  (let [editor (ref nil)]
    (dosync
      (ref-set editor {:qedit (proxy [QTextEdit] []
                                (keyPressEvent [ev]
                                   (when-not (key-press editor ev)
                                     (proxy-super keyPressEvent ev))))
                       :init-args args
                       :filename (ref nil)
                       :count-arg (ref nil)
                       :keymap (ref vim-cmd)}))
    (doto (:qedit @editor)
      (.setFontPointSize 14)
      (.setCursorWidth 4))
    (when (seq args)
      (edit editor (first args)))
    (if (.centralWidget main-window)
      (throw (Exception. "unsupported multiple editors per app"))
      (.setCentralWidget main-window (:qedit @editor)))
    editor))

(apply make-editor *command-line-args*)

;(def main-grid (QGridLayout. main-window))
;(.addWidget main-grid text-edit)

(QApplication/exec)
