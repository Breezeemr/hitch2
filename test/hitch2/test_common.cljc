(ns hitch2.test-common
  #?(:clj (:import (java.io Writer)))
  (:require [hitch2.protocols.selector :as selector-proto]))

(defn return-constant [gv-tracker v]
  v)
(def constant-impl (reify
                     selector-proto/ImplementationKind
                     (-imp-kind [machine] :hitch.selector.kind/halting)
                     selector-proto/HaltingImplementation
                     (-get-halting-fn [sel]
                       return-constant)))

(defn Constant [v]
  (selector-proto/->Selector1 constant-impl v))

;(defrecord Variable [name]
;  hitch2.protocols.selector/Selector
;  (value [s sv state] state)
;  hitch2.protocols.selector/CommandableSelector
;  (command-accumulator [s old-state] old-state)
;  (command-step [s accumulator command]
;    (case (first command)
;      :set! (second command)
;      (hitch2.protocols.selector/map->CommandError {:accumulator accumulator
;                             :error       "Unrecognized Command"})))
;  (command-result [s accumulator]
;    (hitch2.protocols.selector/->StateEffect accumulator nil nil)))
;
;(defrecord SelVec [parents]
;  hitch2.protocols.selector/Selector
;  (value [_ sv _]
;    (let [v (reduce
;              (fn [r parent]
;                (let [x (get sv parent ::not-found)]
;                  (if (= x ::not-found)
;                    (reduced ::not-found)
;                    (conj r x))))
;              [] parents)]
;      (if (= v ::not-found)
;        (hitch2.protocols.selector/->SelectorUnresolved (set parents))
;        (hitch2.protocols.selector/->SelectorValue v (set parents))))))
;
;;; Machine which will var-reset its vars with their own :v param
;(defrecord EchoMachine []
;  hitch2.protocols.selector/Machine
;  (apply-machine-commands [_ g s commands]
;    (reduce
;      (fn [r [cmd sel [varcmd v]]]
;        (case cmd
;          ::hitch2.protocols.selector/var-command
;          (case varcmd
;            :reset! (assoc-in r [:var-reset sel] v))
;          ::hitch2.protocols.selector/child-add (assoc-in r [:var-reset sel] (:initial sel))
;          ::hitch2.protocols.selector/child-del r
;          ::hitch2.protocols.selector/parent-value-change r))
;      {}
;      commands)))
;
;;; The value of an EchoVar
;(defrecord EchoVar [initial]
;  hitch2.protocols.selector/Var
;  (machine-selector [_] (->EchoMachine)))
;
;(defrecord LogMachine [log-volatile]
;  hitch2.protocols.selector/StatefulSelector
;  (create [_]
;    (vswap! log-volatile conj [:create])
;    (hitch2.protocols.selector/->StateEffect {:count 0} nil nil))
;  (destroy [_ s]
;    (vswap! log-volatile conj [:destroy (:count s)])
;    nil)
;  hitch2.protocols.selector/Machine
;  (apply-machine-commands [_ g {:keys [state] :as sn} commands]
;    (vswap! log-volatile conj [:commands (:count state) commands])
;    (reduce (fn [r [type parent-sel [subcmd f]]]
;              (case type
;                ::hitch2.protocols.selector/var-command
;                (case subcmd
;                  :fn (f r g sn)
;                  :log (let [lv (f r g sn)]
;                         (vswap! log-volatile conj [:log (:count state) lv])
;                         r))
;
;                ::hitch2.protocols.selector/parent-value-change
;                (do
;                  (vswap! log-volatile conj [:new-parent-value (:count state) parent-sel (get g parent-sel ::absent)])
;                  r)
;
;                r))
;      {:state (update state :count inc)}
;      commands)))
;
;(defrecord LogVar [log-volatile]
;  hitch2.protocols.selector/Var
;  (machine-selector [_] (->LogMachine log-volatile)))
;
;(defrecord LogVar2 [log-volatile]
;  hitch2.protocols.selector/Var
;  (machine-selector [_] (->LogMachine log-volatile)))
;
;#?(:cljs
;   (extend-protocol IPrintWithWriter
;     LogMachine
;     (-pr-writer [o writer opts]
;       (doto writer
;         (-write "#<LogMachine ")
;         (-write (str (hash (:log-volatile o))))
;         (-write ">")))
;
;     LogVar
;     (-pr-writer [o writer opts]
;       (doto writer
;         (-write "#<LogVar ")
;         (-write (str (hash (:log-volatile o))))
;         (-write ">")))
;
;     LogVar2
;     (-pr-writer [o writer opts]
;       (doto writer
;         (-write "#<LogVar2 ")
;         (-write (str (hash (:log-volatile o))))
;         (-write ">")))))
;
;#?(:clj
;   (defmethod print-method LogMachine [c, ^Writer w]
;     (doto w
;       (.write "#<LogMachine ")
;       (.write (str (hash (:log-volatile c))))
;       (.write ">"))))
;
;#?(:clj
;   (defmethod print-method LogVar [c, ^Writer w]
;     (doto w
;       (.write "#<LogVar ")
;       (.write (str (hash (:log-volatile c))))
;       (.write ">"))))
;
;#?(:clj
;   (defmethod print-method LogVar2 [c, ^Writer w]
;     (doto w
;       (.write "#<LogVar2 ")
;       (.write (str (hash (:log-volatile c))))
;       (.write ">"))))
