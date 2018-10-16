# purescript-parsecative

An Alternative parser library for PureScript.

## Why?

`Alternative` = `Applicative` + `Plus` + distributivity + annihilation

Many parsing libraries expose a monadic interface, but this extra power is rarely needed.
However, `Alt`'s `(<|>)` operator is almost always necessary.

In general, eliding a `Monad` instance comes with other benefits: using the free applicative
lets users perform static monoidal analysis while the lack of the `ap = apply` law means
`apply` can lawfully be done in parallel.

Hence, this library is a **parse**r library that is also appli**cative**.