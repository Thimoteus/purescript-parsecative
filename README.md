# purescript-parsecative

An Alternative parser library for PureScript.

## Why?

`Alternative` = `Applicative` + `Plus` + distributivity + annihilation

Many parsing libraries expose a monadic interface, but this extra power is rarely needed.
However, `Alt`'s `(<|>)` operator is almost always necessary.

In general, eliding a `Monad` instance comes with other benefits: using the free applicative
lets users perform static monoidal analysis while the lack of the `ap = apply` law means
`apply` can lawfully be done in parallel*.

Hence, this library is a **parse**r library that is also appli**cative**.

*In the case of parsing, we can't parallelize `apply` since in `f <*> x`, we need to
know how much of the stream `f` consumed before passing it off to `x`. However,
we do get a parallel `(<|>)`.
