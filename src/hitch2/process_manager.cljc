(ns hitch2.process-manager
  "A process manager manages a map from process descriptors to process
  instances. Process instances implement IProcess, can receive messages, and
  are killable.

  Process instances are created by a process-factory supplied to the process
  manager when it is created. A process-factory is a function from a
  process-descriptor to a running process instance. The process factory is
  always called with locking to ensure that a given process manager does not
  have more than one `(process-factory process-descriptor)` invocation running
  simultaneously. This means it is safe for the process factory to acquire
  and release resources atomically when invoked."

  #?(:clj
     (:import (java.util.concurrent ConcurrentHashMap)
              (java.util.function Function))))

(defprotocol IProcessManager
  (-get-or-create-process! [pm process-dtor]
    "Given a process descriptor return an existing running process or create a
    new running process.")
  (-kill-process-by-dtor! [pm process-dtor]
    "Kill process with process-dtor if running. Returns whether this invocation
    is what killed the process (= true) or if it was already dead or didn't
    exist in the process manager (= false)"))

(defprotocol IProcess
  (-send-message! [process msg]
    "Blocking send message to process. Returns true if send succeeded,
    false if process is not running.")
  (-kill-process! [process]
    "Kills the process, returns whether this invocation is what killed the
    process (= true) or if it was already dead (= false). Must be idempotent as
    it may be called more than once."))

#?(:clj
   (defn- proc-map [] (ConcurrentHashMap.))
   :cljs
   (defn- proc-map [] (volatile! {})))

#?(:clj
   (defn- proc-get! [^ConcurrentHashMap m pdtor]
     (.get m pdtor))
   :cljs
   (defn- proc-get! [m pdtor]
     (get @m pdtor)))

#?(:clj
   (defn- proc-create! [^ConcurrentHashMap m creator pdtor]
     (.computeIfAbsent m pdtor creator))
   :cljs
   (defn- proc-create! [m process-factory pdtor]
     (let [ps (process-factory pdtor)]
       (vswap! m assoc pdtor ps)
       ps)))

#?(:clj
   (defn- proc-dissoc! [^ConcurrentHashMap m pdtor]
     (.remove m pdtor))
   :cljs
   (defn- proc-dissoc! [m pdtor]
     (let [ps (get @m pdtor)]
       (vswap! m dissoc pdtor)
       ps)))


(deftype ProcessManager [pdtor->process process-factory #?(:clj creator)]
  IProcessManager
  (-get-or-create-process! [_ process-dtor]
    (if-some [ps (proc-get! pdtor->process process-dtor)]
      ps
      (proc-create! pdtor->process #?(:clj creator :cljs process-factory)
        process-dtor)))
  (-kill-process-by-dtor! [_ process-dtor]
    (if-some [ps (proc-dissoc! pdtor->process process-dtor)]
      (-kill-process! ps)
      false)))

(defn ->pm [process-factory]
  (->ProcessManager (proc-map) process-factory
    #?(:clj (reify Function (apply [_ pd] (process-factory pd))))))
