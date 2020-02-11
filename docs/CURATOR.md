# Curator
##Introduction
Hitch through it's descriptors provides a way describe your demand for a value
over time and late bound the how to get that value. 

We have covered halting which is a way to derive those values through pure
computation optionally demanding other descriptors. 

Curators give hitch a way to interface between the pure graph and the helter 
skelter outside world. It has its own state and lifecycles that hook into the
functioning of the graph. 

## Curator Node
The curator  node is a structure passed through all of the curator's lifecycle
method. 
```
(defrecord curator-node [state change-focus set-projections
                           async-effects outbox])
```
###state 
State is a place where a curator can keep it's custom state.

###change-focus 
Is a map from descriptor to boolean. periodically hitch will remove the `change-focus`
from the curator-node and make the curator demand/depend on selectors with true
and not depend on selectors with false. 

###set-projections
Is a map from descriptor to anything. It is the means in which the curator sets
the value of a var.

###async-effect 
deprecated

###outbox (effects)
outbox is a map of process-descriptor to an vector of messages to be sent to a 
process.  Messages can be a map of any shape.  The keys :gm and :graph-value will be 
added by the graph to each message.

##LifeCycle Methods
Each method except for init takes a curator-descriptor and returns a function
Init which just returns the inital node
### :hitch2.def.curator/init
Takes the curator-descriptor and returns the inital curator node. This could do 
inital `set-projections` or `change-focus` as well as initalize the state.
### :hitch2.def.curator/tx-init
Every time the a curator's vars change or a command is applied hitch will call
this lifecyles method many more curation changes or commands may be applied
before the transaction is flushed
### :hitch2.def.curator/curation-changes
This function is called every time a var is demanded and the var returns this curator with 
its `get-curator` call.  When a var selects you as it's curator
it expects that you will set it's value through `set-projections` field of the node
### :hitch2.def.curator/observed-value-changes
This function is called every time the value of a demanded descriptor changes.
This will not be called until the curator has demanded other descriptors through
the `change-focus` field of the curator node.
### :hitch2.def.curator/apply-command
This function is called for every command applied to the curator descriptor of a
var managed by the curator descriptor.
### :hitch2.def.curator/flush-tx
This function is called every time a curator transaction was started and the hitch
graph has reached a fixed point. It gives you a chance to `change-focus` or `set-projections`
### :hitch2.def.curator/finalize
function is called every time a curator transaction was started and the hitch
graph has reached a fixed point and all transactions have been flushed. It this It is
function cannot add to `change-focus` or `set-projections` commonly used as one
last chance to use data batched to state to build a large command for the outbox.
### :hitch2.def.curator/de-init
This function is called Every time that a curator is gced. This is currently
disabled
