(ns hitch2.process-manager)

(defprotocol ProcessManager
  (-get-or-create-process! [pm process-dtor]
    "given a process dtor return an existing running process or create a new running process")
  (-kill-process-by-dtor! [pm process-dtor]
    "Kill process with process-dtor if running."))

(defprotocol Process
  (-send-message! [process msg] "Blocking send message to process. Returns true if put succeeded, false if process is not running.")
  (-kill-process! [process] "Kills the process, returns whether a process was killed"))

#?(:clj

(deftype ProcessManager [^ConcurrentHashMap pdtor->process reg]
  ProcessManager
  (-get-or-create-process! [pm process-dtor]
    (if-some [ps (.get pdtor->process process-dtor)]
      ps
      (.computeIfAbsent pdtor->process process-dtor
        (reify Function
          (apply [_ pd] (reg pd))))))
  (-kill-process-by-dtor! [pm process-dtor]
    (if-some [ps (.remove pdtor->process process-dtor)]
      (do (-kill-process! ps) true)
      false
      ))))

(defn ->pm [reg context]
  (ProcessManager. (atom {}) reg))
