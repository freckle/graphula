Things polygraph got right
* Acyclic graphs
* Declarative dependencies
* Random generation
* Generic versions of type classes
* Editable graph
* Not allowing edits after depdencies have been aligned
* Seperating evaluation of the graph from declaration

Things it doesn't have but could
* Wrapping the graph execution to dump state on failure (withGraph :: m ())
* Replay of previous states with a special combinator
* Replay with shrinking to produce the smallest failing graph possible

Things it got wrong
* Doing nearly everything at the type level. The term level is easier to grok for beginners and experts and type errors are much easier to interpret.
* Having invisible semantics that create confusion. The order of the graph is important, but the type level makes that harder to make visible.

Problem statement
We can build acyciclic generated graphs on the term level via a monadic api. This is easy. There is nothing polygraph does today that we can't do on the term level with appropriate combinators. But can we recover future uses via a free or operational monad?
