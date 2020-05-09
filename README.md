# Bastion

[![Build Status](https://travis-ci.com/scarisey/bastion.svg?token=FoFxcrjoaHEnbx4sScjP&branch=master)](https://travis-ci.com/scarisey/bastion)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=scarisey_bastion&metric=alert_status)](https://sonarcloud.io/dashboard?id=scarisey_bastion)
[![Latest tag](https://img.shields.io/github/v/tag/scarisey/bastion?sort=semver)](https://github.com/scarisey/bastion/tags)

Bastion is a library to convert types, using when needed custom defined smart constructors. This library should be well suited for Domain Driven Designed applications.

Based on [Magnolia](https://github.com/propensive/magnolia) for typeclass derivation.

## Disclaimer

This project is a way for me to learn typeclass derivation using Magnolia. It's not ready for a production use, and it's too early too know if it will (but I hope so !).


## Table of content

  * [Concepts](#concepts)
  * [Installation](#installation)
  * [Usages](#usages)
    + [Simple mapping](#simple-mapping)
    + [Convert to an ADT](#convert-to-an-adt)
    + [Use your smart constructors](#use-your-smart-constructors)
    + [Lenient case](#lenient-case)
    + [Or combinator on DynamicRepr](#or-combinator-on-dynamicrepr)
  * [Things to do, and perspectives](#things-to-do--and-perspectives)
  * [License](#license)

## Concepts

Converting from a data type A to another data type B could have been represented as a typeclass Convert[A,B]{ def to(a:A):B}.

Derive this typeclass on two type parameters at the same time seems not trivial with Shapeless, and I just don't find how with Magnolia.
As I was trying to implement it, the obvious way for me was to encode the data type A in a simplistic representation, for which the contract is known
by any decoder. Since the majority of the translating labor is to convert fields from a product type A to fields of the same name of a type B, 
I just use the dynamics feature of Scala to achieve this translating.
So yes, this library will warn you of **mismatch on field names only at runtime**.

So there are three types that you will manipulate through this library : 

  * ***DynamicRepr***, which represents the encoding of the type you want to convert from
  * ***Encode***, which is the contract for instances of encoders from a specific type to DynamicRepr 
  * ***Decode***, which is the contract for instances of decoders from any DynamicRepr to a specific type
  
It should be enough the majority of time to make the imports below and call the convert method that will use instance of Encode and Decode in the implicit scope : 
```scala
import bastion._
import bastion.derivation.encode.auto._
import bastion.derivation.decode.auto._
//...
val instanceOfTypeB:TypeB = instanceOfTypeA.convert[TypeB]
```

For more advanced usage, please see below, and the examples [here](https://github.com/scarisey/bastion/tree/master/examples/src/test/scala/dev/scarisey/bastionexamples).

## Installation

![Sonatype Nexus (Releases)](https://img.shields.io/nexus/r/dev.scarisey/bastion-core_2.12?color=green&label=latest%202.12&server=https%3A%2F%2Foss.sonatype.org)
![Sonatype Nexus (Releases)](https://img.shields.io/nexus/r/dev.scarisey/bastion-core_2.13?color=green&label=latest%202.13&server=https%3A%2F%2Foss.sonatype.org)
### Sbt
```sbt
libraryDependencies += "dev.scarisey" %% "bastion-core" % "X.Y.Z"
```

## Usages

### Simple mapping
```scala
import bastion._
import bastion.derivation.encode.auto._
import bastion.derivation.decode.auto._

final case class PersonExternal(
    id: String,
    firstName: String,
    lastName: String,
    birthdate: LocalDate,
    posX: Double,
    posY: Double
  )
final case class Person(id: String, firstName: String, lastName: String, birthdate: LocalDate)

val person:Person = PersonExternal("anId", "firstName", "lastName", LocalDate.parse("1985-01-12"), 44.846565, -0.567351).convert[Person]
```

### Convert to an ADT
```scala
import bastion._
import bastion.derivation.encode.auto._
import bastion.derivation.decode.auto._

case class A1(aField1: String)
case class A2(aField2: Int)
case class A3(aDouble: Double)
  
sealed trait B
case class B0(aBoolean: Boolean) extends B
case class B1(aField1: String)   extends B
case class B2(aField2: Int)      extends B

A1("foo").convert[RecB] // Right(B1("foo"))
A2(42).convert[RecB] // Right(B2(42))
A3(2.0).convert[RecB] //Left(IncorrectSubtype)
```

### Use your smart constructors
```scala
import bastion._
import bastion.derivation.encode.auto._
import bastion.derivation.decode.auto._

case class A(aString:String,anInt:Int)

case class Wrapped private(aField: String)
object Wrapped{
  def apply(s:String):Either[String,Wrapped] = Either.cond(s.length>2,new Wrapped(s),"length > 2")
}
case class B(aString:Wrapped,anInt:Int)

implicit val decodeWrapped:Decode[Wrapped] = Decode.wrapE(Wrapped.apply)

A("foo",42).convert[B] //Right(B(Wrapped("foo"),42))
A("fo",42).convert[B] //Left(WrappedError("length > 2"))
```
See this more complete example : [SmartConstructors](https://github.com/scarisey/bastion/blob/master/examples/src/test/scala/dev/scarisey/bastionexamples/SmartConstructors.scala)

### Lenient case
```scala
import bastion._
import bastion.derivation.encode.configured.auto._
import bastion.derivation.decode.auto._
import bastion.derivation.encode.Configuration.lenient

case class Source(aString: String, anInt: Int, aBoolean: Boolean)
case class Target(an_int: Int, A_String: String)

Source("foo", 42, true).convert[Target] //Target(42,foo)
```

### Or combinator on DynamicRepr
```scala
import bastion._
import derivation.encode.auto._

case class Source1(aField1: Int)
case class Source2(aField2: Int)

case class Target(finalValue: Int)

implicit val decoder: Decode[Target] = Decode.instance(g => (g.aField1 ||| g.aField2).apply(Target.apply))

Source1(42).convert[Target] //Target(42)
Source2(33).convert[Target] //Target(33)
```

## Things to do, and perspectives

  * Some benchmarks need to be done.
  * There are still too much imports to do before being able to convert types.

## License
bastion is licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the "License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
