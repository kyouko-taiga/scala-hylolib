# Hylolib

This repository implements an adapted version of [Hylo's standard library](https://github.com/hylo-lang/hylo).
The goal of this project is to test Scala's support for type-class oriented programming and mutable value semantics.

## Notes

Here are some general observations about the experiment.

Defining trait refinement hierarchies is problematic because it creates ambigous given instances.
For example, given `Comparable <: Equatable ` and `Hashable <: Equatable`, if we create given instances for `Comparable[Int]` _and_ `Hashable[Int]`, the compiler won't be able to decide which one it should pick when we're asking for `Equatable[Int]`.

The inability to "compute" anything before delegating to the primary constructor is annoying.
Having to define an `apply` method in the companion object seems like a convoluted workaround that is difficult to "discover".

These two definitions don't behave the same way:

```
def f[T](using Equatable[T])(x: T) = ???
def f[T: Equatable](x: T) = ???
```

The latter seems to cause less ambiguous given errors.