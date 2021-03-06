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

object AdtMapping extends App {
  case class RecA1(aField1: String)
  case class RecA2(aField2: Int)
  case class RecA3(aField3: Double)
  sealed trait RecB
  case class RecB0(aField4: Boolean) extends RecB
  case class RecB1(aField1: String)  extends RecB
  case class RecB2(aField2: Int)     extends RecB

  println(RecA1("foo").convert[RecB])
  println(RecA2(42).convert[RecB])
  println(RecA3(2.0).convert[RecB]) //Left(No matching subtypes of RecB for ProductDynamicRepr(RecA3(2.0)))
}
