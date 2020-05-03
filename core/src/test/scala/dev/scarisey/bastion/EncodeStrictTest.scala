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

package dev.scarisey.bastion
import dev.scarisey.bastion.derivation.encode.semiauto._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EncodeStrictTest extends AnyFlatSpec with Matchers {
  trait Fixture {
    case class SubA1(sub_string_1: String, sub_int_1: Int)
    case class SubA2(sub_boolean_2: Boolean, sub_double_2: Double)
    case class RecordA(sub_2: SubA2, string_description: String, sub_1: SubA1)
    case class RecordB(stringDescription: String, subA1: Option[SubA1], subA2s: List[SubA2], recA: Either[String, RecordA])

    implicit val encodeSubA1: Encode[SubA1] = deriveEncode[SubA1]
    implicit val encodeSubA2: Encode[SubA2] = deriveEncode[SubA2]
    implicit val encodeA: Encode[RecordA]   = deriveEncode[RecordA]
    implicit val encodeB: Encode[RecordB]   = deriveEncode[RecordB]
  }

  it should "be case sensitive" in new Fixture {
    val recordA = RecordA(SubA2(true, 2.0), "foo", SubA1("s1", 42))
    val repr    = encodeA.to(recordA)
    repr match {
      case ProductDynamicRepr(a) => a shouldBe recordA
      case _                     => fail()
    }
    repr.sub1.subString1 shouldBe NilDynamicRepr
    repr.sub_1.sub_string_1 shouldBe ValueDynamicRepr("s1")
    repr.string_description shouldBe ValueDynamicRepr("foo")
    //incorrect path
    repr.subX.subString1 shouldBe NilDynamicRepr
    repr.subX.subString1 shouldBe NilDynamicRepr
  }
}
