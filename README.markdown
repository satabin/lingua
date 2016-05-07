Lingua
======

Lingua is a set of linguistic tools wirtten in Scala. It is divided in several modules.

Module `lexikon`
----------------

The `lexikon` module provides tools to generate morphological lexica out of a dedicated language. For example dictionaries, see the [resources directory](https://github.com/satabin/lingua/tree/master/lexikon/src/main/resources) andhave a look at `.dico` files.

To run the lexicon genrator:

```sh
$ sbt
> project lexikon
> runMain lingua.lexikon.Dikoc lexikon/src/main/resources/français.dico -N /tmp/nfst.dot -F /tmp/fst.dot
```

Then you can render the generated (N)Fst by using graphviz tools.

Module `fst`
------------

This module is a generic [Fst](https://en.wikipedia.org/wiki/Finite_state_transducer) module that can be used independently.
