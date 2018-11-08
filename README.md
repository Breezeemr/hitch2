# hitch2

FIXME: Write a one-line description of your library/project.

## Overview

FIXME: Write a paragraph about the library/project and highlight its goals.

## Ways to Run

### Console versions

    #rebel readline
    script/rebel

    #standard cljs repl
    script/cljs-repl

    # rebel repl
    clj -A:rebel:build

    # test # but you probably prefer the figwheel main way
    clj -A:test

    # cljs repl
    clj -A:cljs

    #clean
    script/clean

See deps.edn for the particulars. But it is very easy(TM).

The later few without figwheel are for a simple cljs repl that is
suitable for isolating bugs, etc.

## Development

To get an interactive development environment run:

    clojure -A:fig:build

From emacs just use `cider-jack-in-cljs` and the dir-locals file will
set you up correctly, (assuming CIDER 18+).

You can see the tests at http://localhost:9500/test.html

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
