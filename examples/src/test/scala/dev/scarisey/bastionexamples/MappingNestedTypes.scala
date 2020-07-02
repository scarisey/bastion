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

package bastionexamples
import bastion._
import bastion.derivation.dynamicrepr.auto._
import bastion.derivation.decode.auto._

object MappingNestedTypes extends App {
  case class SubSource1(aString: String)
  case class SubSource2(anInt: Int)
  case class Source(sub1: SubSource1, sub2: SubSource2)
  case class SubTarget1(aString: String)
  case class SubTarget2(anInt: Int)
  case class Target(sub1: SubTarget1, sub2: SubTarget2)
  case class Target2(field1: Int, field2: String)

  println(Source(SubSource1("foo"), SubSource2(42)).convert[Target])

  implicit val decoderTarget: Decoder[Target2] =
    Decoder.instance(g => (g.sub32.anInt, g.sub1.aString).apply(Target2.apply))

  println(Source(SubSource1("foo"), SubSource2(42)).convert[Target2])
}
