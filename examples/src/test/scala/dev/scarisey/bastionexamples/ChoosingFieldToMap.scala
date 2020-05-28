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
import derivation.dynamicrepr.auto._

object ChoosingFieldToMap extends App {
  case class Source1(aField1: Int)
  case class Source2(aField2: Int)

  case class Target(finalValue: Int)

  implicit val decoder: Decode[Target] = Decode.instance(g => (g.aField1 ||| g.aField2).apply(Target.apply))

  println(Source1(42).convert[Target])
  println(Source2(33).convert[Target])
}
