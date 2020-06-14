/*
 * Copyright 2020 dev.scarisey
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.scarisey.bastionexamples

import bastion._
import bastion.derivation.dynamicrepr.auto._

object ExplicitConversion extends App {
  case class SubSource1(aString: String)
  case class SubSource2(anInt: Int)
  case class Source(sub1: SubSource1, sub2: SubSource2)
  case class Target(field1: Int, field2: String)

  implicit val decoderTarget: Decoder[Target] = Decoder.instance(g => (g.sub2.anInt, g.sub1.aString).apply(Target.apply))

  println(
    Source(SubSource1("foo"), SubSource2(42)).convert[Target] //Right(Target(42,foo))
  )
}

object ErrorInExplicitConversion extends App {
  case class SubSource1(aString: String)
  case class SubSource2(anInt: Int)
  case class Source(sub1: SubSource1, sub2: SubSource2)
  case class Target(field1: Int, field2: String)

  implicit val decoderTarget: Decoder[Target] = Decoder.instance(g => (g.sub32.anInt, g.sub1.aString).apply(Target.apply))

  println(
    Source(SubSource1("foo"), SubSource2(42)).convert[Target]
    //Left(IncorrectPath: applying root.sub32.anInt on ProductDynamicRepr(Source(SubSource1(foo),SubSource2(42))) produces NilDynamicRepr
    // actualDynamicRepr: NilDynamicRepr))
  )
}
