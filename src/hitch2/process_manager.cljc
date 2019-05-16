(ns hitch2.process-manager)


{::start (fn [context])
 ::kill  (fn [process context])
 ::type ::manual-managed}
{::get  (fn [context])
 ::type ::external}

(defprotocol GetProcess
  (-get-process [pm process-dtor] "given a process dtor return the process"))

(defprotocol KillProcess
  (-kill-process [pm process-dtor] "given a process dtor kill the process"))


(deftype ProcessManager [pdtor->process reg context]
  GetProcess
  (-get-process [pm process-dtor]
    (let [{impl-type ::type :as impl} (reg (:name pdtor->process))]
      (case impl-type
        ::manual-managed
        (if-some [process (get @pdtor->process process-dtor)]
          process
          (let [process ((::start impl) context)]
            (swap! pdtor->process assoc process-dtor process)
            process))
        ::external (let [getfn (::get impl)]
                     (getfn context)))))
  KillProcess
  (-kill-process [pm process-dtor]
    (let [{impl-type ::type :as impl} (reg (:name pdtor->process))]
      (case impl-type
        ::manual-managed
        (if-some [process (get @pdtor->process process-dtor)]
          (do
            (swap! pdtor->process dissoc process-dtor)
            ((::kill impl) context)
            nil)
          nil)
        ::external nil))))

(defn ->pm [reg context]
  (ProcessManager. (atom {}) reg context))
