# Set category

Function programing aims at build large scale programs based on small composed functions. 
Those smaller functions are the atomic capabilities of the programs. Once composed with each other, they form a bigger capability, yet testable and still composable.

We've already seen [functions compositions](https://github.com/xebia-france/fp_in_scala_4) in previous a session.

We've seen that some abstractions make possible a higher level for programing for a better and safer code re-usage. [Functors and Applicatives](https://github.com/xebia-france/fp_in_scala_2) and [Monads](https://github.com/xebia-france/fp_in_scala_2) are the most famous ones that we've already studies together.

What is interesting is that all of this is supported by a branch of mathematical study, **Cateogry Theory**. 
Another branch study a complementary field, the **Sets**. 
Let's have a look together about this today. We'll talk about weird structures such *SemiGroup*, *Monoid*, some mathematical properties such as associativity or commutativity. 

Those categories will also be implemented with the [Type Class pattern](https://github.com/xebia-france/fp_in_scala_5).


## Getting started

This project uses Scala and :

* [Cats](https://github.com/typelevel/cats): one open-source framework that already implements lots of pure functional abstractions
* [Scalatest](http://www.scalatest.org) for writing assertions.
* [SBT](http://www.scala-sbt.org/) as a build tool.

Let's have a look at the slides first. 