# What Is It
Graphula is a library for declaratively generating deeply nested relational data. It is a fixture library.

# Goals
## High Level
* This library should be friendly for beginners to use and should have visible intuitive semantics.
* This library should be maintainable by intermediate haskellers.

## Specific
* Describe graphs on the term level. Type level programming is mysterious to anyone but advanced Haskellers. This makes it inappropriate for common interfaces.
* Data should use random generation via Arbitrary. We should leverage the Quick Check ecosystem as much as possible.
* You should be able to declare the value of properties you care about. Practically, generated data should be editable.
* Dependencies should be declarative and canonical. The type checker should reject poorly-formed graphs.
* Term level usage should be idiomatic. Everyday Haskell combinators should work as expected.
* Graphula should dump failed graphs for inspection. Random data highlights edge cases, but is useless when you can't inspect it.
* Graphula should be able to replay failed data dumps to allow refining a test case and a red/green workflow.
* Graphula should generate dependencies for you if you ask. Example: You should be able to ask for a Town and Graphula will generate a County, State, Country, Planet, etc. for you.


## Pie in the Sky
* Graphula should be able to rewind the graph. This allows deletion of entities after run time. This can allow idempotent groups of tests to run in parallel without having to truncate/reset the database inbetween.
* Idempotent tests should be able to leverage quick check shrinking and replay to produce the smallest test failure possible.

# Lessons Learned

## Polygraph
### Problem statement
We can build acyciclic generated graphs on the term level via a monadic api. This is easy. There is nothing polygraph does today that we can't do on the term level with appropriate combinators. But can we recover future uses possibly via a free or operational monad?

### Things polygraph got right
* Acyclic graphs
* Declarative dependencies
* Random generation
* Generic versions of type classes
* Editable graph
* Not allowing edits after depdencies have been aligned
* Seperating evaluation of the graph from declaration

### Things it doesn't have but could
* Wrapping the graph execution to dump state on failure (withGraph :: m ())
* Replay of previous states with a special combinator
* Replay with shrinking to produce the smallest failing graph possible

### Things it got wrong
* Doing nearly everything at the type level. The term level is easier to grok for beginners and experts and type errors are much easier to interpret.
* Having invisible semantics that create confusion. The order of the graph is important, but the type level makes that harder to make visible.

## Factory Girl
### Things factory girl has
* Automatic dependency generation (ask for a banana, get the gorilla, etc, etc). We can likely do this with a self recursive type class.

