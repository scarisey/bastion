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

package bastion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import bastion.derivation.dynamicrepr.Configuration.lenient
import bastion.derivation.dynamicrepr.configured.auto._
import bastion.derivation.decode.auto._

class DecodeLenientCaseTest extends AnyFlatSpec with Matchers {
  behavior of "Encode and Decode with lenient case"

  it should "convert a flat structure to another one - insensitive field case" in {
    case class RecA(aString: String, anInt: Int, aBoolean: Boolean)
    case class RecB(an_int: Int, A_String: String)

    RecA("foo", 42, true).convert[RecB] shouldEqual Right(RecB(42, "foo"))
  }
}
