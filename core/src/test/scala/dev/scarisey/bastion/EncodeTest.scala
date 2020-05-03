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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import derivation.encode.Configuration.lenient
import derivation.encode.configured.auto._

class EncodeTest extends AnyFlatSpec with Matchers {
  trait Fixture {
    case class SubA1 private (sub_string_1: String, sub_int_1: Int)
    case class SubA2 private (sub_boolean_2: Boolean, sub_double_2: Double)
    case class RecordA private (sub_2: SubA2, string_description: String, sub_1: SubA1)
    case class RecordB private (
      stringDescription: String,
      subA1: Option[SubA1],
      subA2s: List[SubA2],
      recA: Either[String, RecordA]
    )

    implicit val encodeA = implicitly[Encode[RecordA]] //FIXME implicitly will fail with type ascription, returning null
    implicit val encodeB = implicitly[Encode[RecordB]]
  }

  behavior of "Encode"

  it should "convert to a dynamic representation" in new Fixture {
    val recordA = RecordA(SubA2(true, 2.0), "foo", SubA1("s1", 42))
    val repr    = encodeA.to(recordA)
    repr match {
      case ProductDynamicRepr(a) => a shouldBe recordA
      case _                     => fail()
    }
    repr.sub_1.sub_string_1 match {
      case ValueDynamicRepr(a) => a shouldBe "s1"
      case _                   => fail()
    }
    //incorrect path
    repr.sub_X.sub_string_1 shouldBe NilDynamicRepr
    repr.sub_X.sub_string_1 shouldBe NilDynamicRepr
  }

  it should "convert an ADT to a dynamic representation" in {
    sealed trait RecC
    object RecC {
      final case class RecC1(aDouble: Double) extends RecC
      final case class RecC2(aString: String) extends RecC
      final case class RecC3(anInt: Int)      extends RecC
    }

    val encode  = implicitly[Encode[RecC]]
    val recordC = RecC.RecC2("a string")
    val repr    = encode.to(recordC)
    repr match {
      case ProductDynamicRepr(a) => a shouldBe recordC
      case _                     => fail()
    }
    repr.aDouble shouldBe NilDynamicRepr
    repr.anInt shouldBe NilDynamicRepr
    repr.aString shouldBe ValueDynamicRepr("a string")
  }

  it should "convert a recursive ADT to a dynamic representation" in {
    sealed trait Rec
    final case class RecM(text: String)  extends Rec
    final case class RecR(rs: List[Rec]) extends Rec

    val encode: Encode[Rec] = implicitly[Encode[Rec]]
    val record              = RecR(List(RecM("foo"), RecM("bar")))
    val repr                = encode.to(record)
    repr match {
      case ProductDynamicRepr(a) => a shouldBe record
      case _                     => fail()
    }
    repr.rs match {
      case IterableDynamicRepr(items) =>
        items match {
          case ProductDynamicRepr(a) :: ProductDynamicRepr(b) :: Nil => {
            a shouldBe record.rs(0)
            b shouldBe record.rs(1)
          }
        }
      case _ => fail()
    }
  }

  it should "be case insensitive" in new Fixture {
    val recordA = RecordA(SubA2(true, 2.0), "foo", SubA1("s1", 42))
    val repr    = encodeA.to(recordA)
    repr match {
      case ProductDynamicRepr(a) => a shouldBe recordA
      case _                     => fail()
    }
    repr.sub1.subString1 shouldBe ValueDynamicRepr("s1")
    //incorrect path
    repr.subX.subString1 shouldBe NilDynamicRepr
    repr.subX.subString1 shouldBe NilDynamicRepr
  }

  it should "works with monadic wrapper" in new Fixture {
    val subA1: SubA1      = SubA1("aString", 42)
    val subA2: SubA2      = SubA2(true, 3.0)
    val recA: RecordA     = RecordA(subA2, "foo", subA1)
    val recordB: RecordB  = RecordB("an attempt", Some(subA1), List(subA2), Right(recA))
    val recordBL: RecordB = RecordB("an attempt", None, List(subA2), Left("not a recA"))

    val repr: DynamicRepr = encodeB.to(recordB)
    repr match {
      case ProductDynamicRepr(a) => a shouldBe recordB
      case _                     => fail()
    }

    repr.subA1 match {
      case ProductDynamicRepr(a) => a shouldBe subA1
      case _                     => fail()
    }
    repr.subA1.subString1 shouldBe ValueDynamicRepr(subA1.sub_string_1)
    repr.subA2s match {
      case IterableDynamicRepr(items) => {
        items should have size 1
        items.head match {
          case ProductDynamicRepr(a) => a shouldEqual subA2
          case x                     => fail(x.toString)
        }
      }
      case x => fail(x.toString)
    }
    repr.recA match {
      case ProductDynamicRepr(a) => a shouldBe recA
      case _                     => fail()
    }

    val reprL = encodeB.to(recordBL)
    reprL match {
      case ProductDynamicRepr(a) => a shouldEqual recordBL
      case _                     => fail()
    }

    reprL.subA1 shouldBe NilDynamicRepr
    reprL.recA shouldBe ValueDynamicRepr("not a recA")
  }

}
