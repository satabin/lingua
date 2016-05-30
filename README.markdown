Lingua [![Codacy Badge](https://api.codacy.com/project/badge/Grade/cab189517d35400a848cc44348b1757b)](https://www.codacy.com/app/satabin/lingua?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=satabin/lingua&amp;utm_campaign=Badge_Grade)
======

Lingua is a set of linguistic tools written in Scala. It is divided in several modules.

Module `lexikon`
----------------

The `lexikon` module provides tools to generate morphological lexica out of a dedicated description language. For example dictionaries, see the [resources directory](https://github.com/satabin/lingua/tree/master/lexikon/src/main/resources) and have a look at `.dico` files.

To run the lexicon genrator:

```sh
$ sbt
> project lexikon
> runMain lingua.lexikon.DikoMain compile lexikon/src/main/resources/français.dico -N /tmp/nfst.dot -F /tmp/fst.dot
```

Then you can render the generated (N)Fst by using graphviz tools.

This command produces a compiled version of the dictionary in a file named `dikput.diko`. This compiled version can be queried as follows (from the same sbt session)

```sh
> runMain lingua.lexikon.DikoMain query dikoput.diko -q mange
```

Which will return

```scala
Set(DikoEntry(manger,Set(+Sg, @V, +3ème, +G1, +Prés, +Ind)), DikoEntry(manger,Set(+Sg, +1ère, @V, +G1, +Prés, +Ind)))
```

This means that according to this dictionary, `mange` stems to `manger` which is a verb (`@V` category) conjugated at the first person singular of the indicative present, or at the third person of the indicative present.

For more details on available options, run this main class with option `-h`

Module `fst`
------------

This module is a generic [Fst](https://en.wikipedia.org/wiki/Finite_state_transducer) module that can be used independently.
