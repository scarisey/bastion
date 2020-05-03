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

import java.io
import java.io.File
import java.net.URI
import java.net.URL
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.language.postfixOps
import dev.scarisey.bastion.derivation.encode.auto._
import dev.scarisey.bastion.derivation.decode.auto._

class DecodeTest extends AnyFlatSpec with Matchers {
  trait Fixture

  behavior of "Decode"

  it should "convert a flat structure to another one - same shape" in new Fixture {
    case class RecA(aString: String, anInt: Int, aBoolean: Boolean)
    case class RecB(anInt: Int, aBoolean: Boolean, aString: String)

    RecA("foo", 42, true).convert[RecB] shouldEqual Right(RecB(42, true, "foo"))
  }

  it should "convert a flat structure to another one - part of shape and wrapped values" in new Fixture {
    case class RecA(
      aString: String,
      anInt: Int,
      aBoolean: Boolean,
      strings: List[String],
      optionalString: Option[String],
      eitherIOrS: Either[Int, String],
      eitherJustString: Either[String, Boolean]
    )
    case class Wrapped(value: String)
    case class RecB(
      anInt: Int,
      aString: Wrapped,
      strings: List[Wrapped],
      optionalString: Option[Wrapped],
      eitherIOrS: Either[Int, String],
      eitherJustString: String
    )

    implicit val convWrapped: Decode[Wrapped] = Decode.instance(g => g.apply(Wrapped(_)))

    RecA("toto", 42, true, List("foo", "bar"), Some("content"), Right("baz"), Left("titi")).convert[RecB] shouldEqual Right(
      RecB(42, Wrapped("toto"), List(Wrapped("foo"), Wrapped("bar")), Some(Wrapped("content")), Right("baz"), "titi")
    )
  }

  it should "convert a 2 levels structure to another one - same shape" in new Fixture {
    case class SubA1(aString: String)
    case class SubA2(anInt: Int)
    case class RecA(sub1: SubA1, sub2: SubA2)
    case class SubB1(aString: String)
    case class SubB2(anInt: Int)
    case class RecB(sub1: SubB1, sub2: SubB2)

    RecA(SubA1("foo"), SubA2(42)).convert[RecB] shouldEqual Right(RecB(SubB1("foo"), SubB2(42)))
  }

  it should "convert a flat structure into an adt" in new Fixture {
    case class RecA1(aField1: String)
    case class RecA2(aField2: Int)
    case class RecA3(aDouble: Double)
    sealed trait RecB
    case class RecB0(aBoolean: Boolean) extends RecB
    case class RecB1(aField1: String)   extends RecB
    case class RecB2(aField2: Int)      extends RecB

    RecA1("foo").convert[RecB] shouldEqual Right(RecB1("foo"))
    RecA2(42).convert[RecB] shouldEqual Right(RecB2(42))
    RecA3(2.0).convert[RecB] shouldEqual Left(IncorrectSubtype)
  }

  it should "convert from a recursive structure to another one" in new Fixture {
    sealed trait Specific
    final case class SpecificM(text: String)       extends Specific
    final case class SpecificR(rs: List[Specific]) extends Specific

    sealed trait Target
    final case class TargetM(text: String)     extends Target
    final case class TargetR(rs: List[Target]) extends Target

    SpecificR(List(SpecificM("foo"), SpecificM("bar"))).convert[Target] shouldEqual Right(
      TargetR(List(TargetM("foo"), TargetM("bar")))
    )
  }

  it should "convert a flat structure to another one, using smart constructor" in new Fixture {
    case class RecA private (aString: String, anInt: Int, aBoolean: Boolean)
    case class RecB private (anInt: Int, aBoolean: Boolean, aString: String)
    object RecB {
      def apply(anInt: Int, aBoolean: Boolean, aString: String): Either[List[String], RecB] =
        Either.cond(aString.length <= anInt, new RecB(anInt, aBoolean, aString), List(s"${aString} > $anInt"))
    }

    implicit val convB: Decode[RecB] =
      Decode.instance(g => (g.anInt, g.aBoolean, g.aString).applyE(RecB.apply))

    RecA("foo", 42, true).convert[RecB] shouldEqual RecB(42, true, "foo")
    RecA("fooo", 3, true).convert[RecB] shouldEqual Left(WrappedError(List("fooo > 3")))
  }

  it should "convert a complex structure to another one using smart constructors" in new Fixture {
    case class RecordA(sub_2: SubA2, string_description: String, sub_1: SubA1)
    case class SubA1(sub_string_1: String, sub_int_1: Int)
    case class SubA2(sub_boolean_2: Boolean, sub_double_2: Double)

    case class RecordB(sub1: SubB1, sub2: SubB2)
    case class SubB2(subComputed: Double, subBoolean2: Boolean)
    object SubB2 {
      def apply(subInt1: Int, subDouble2: Double, subBoolean2: Boolean) = new SubB2(subInt1 * subDouble2, subBoolean2)
    }
    case class SubB1(subInt1: Int, subString1: String, stringDescription: String)
    object SubB1 {
      def apply(subInt1: Int, subString1: String, stringDescription: String): Option[SubB1] =
        if (subInt1 > subString1.length) Some(new SubB1(subInt1, subString1, stringDescription)) else None
    }

    //explicit instances
    implicit val conv2: Decode[SubB2] = Decode.instance(g =>
      (g.sub_1.sub_int_1, g.sub_2.sub_double_2, g.sub_2.sub_boolean_2)
        .apply(SubB2.apply)
    )
    implicit val conv1: Decode[SubB1] = Decode.instance(g =>
      (g.sub_1.sub_int_1, g.sub_1.sub_string_1, g.string_description)
        .applyO(SubB1.apply)
    )
    implicit val conv: Decode[RecordB] =
      Decode.instance(g => (g, g).apply(RecordB.apply))

    val recordB     = RecordA(SubA2(true, 2.0), "foo", SubA1("s1", 42)).convert[RecordB]
    val recordBNone = RecordA(SubA2(true, 2.0), "foo", SubA1("astring", 3)).convert[RecordB]

    recordB shouldEqual Right(RecordB(SubB1(42, "s1", "foo").get, SubB2(42, 2.0, true)))
    recordBNone shouldEqual Left(NilSmartConstructorError)

  }

  it should "create decoder for wrapped values" in new Fixture {
    val exception = new IllegalArgumentException("No !")
    case class Wrapped(aField: String)
    object Wrapped {
      def makeEither(aField: String) =
        Either.cond(aField.length < 5, new Wrapped(aField), exception)
      def makeTry(aField: String)    = makeEither(aField).toTry
      def makeOption(aField: String) = makeEither(aField).toOption
    }

    val enc: Decode[Wrapped]  = Decode.wrap(Wrapped(_))
    val encE: Decode[Wrapped] = Decode.wrapE(Wrapped.makeEither)
    val encO: Decode[Wrapped] = Decode.wrapO(Wrapped.makeOption)
    val encT: Decode[Wrapped] = Decode.wrapT(Wrapped.makeTry)

    private val expectedDecodedValue                  = Right(Wrapped("foo"))
    private val dynamicRepr: ValueDynamicRepr[String] = ValueDynamicRepr("foo")
    enc.from(dynamicRepr) shouldEqual expectedDecodedValue
    encE.from(dynamicRepr) shouldEqual expectedDecodedValue
    encO.from(dynamicRepr) shouldEqual expectedDecodedValue
    encT.from(dynamicRepr) shouldEqual expectedDecodedValue

    private val failingDynamicRepr: ValueDynamicRepr[String] = ValueDynamicRepr("failing foo")
    private val expectedFailedDecodedValue                   = Left(WrappedError(exception))
    encE.from(failingDynamicRepr) shouldBe expectedFailedDecodedValue
    encT.from(failingDynamicRepr) shouldBe expectedFailedDecodedValue
    encO.from(failingDynamicRepr) shouldBe Left(NilSmartConstructorError)
  }

  it should "convert to Option in different cases" in new Fixture {
    case class RecA(anInt: Int)
    case class RecB(anInt: Int)

    Some(RecA(42)).convert[Option[RecB]] shouldBe Right(Some(RecB(42)))
    RecA(42).convert[Option[RecB]] shouldBe Right(Some(RecB(42)))
    Option.empty[RecA].convert[Option[RecB]] shouldBe Right(None)
    List(RecA(42), RecA(43)).convert[List[Option[RecB]]] shouldBe Right(List(Some(RecB(42)), Some(RecB(43))))
    List(RecA(42), RecA(43)).convert[Option[List[RecB]]] shouldBe Right(Some(List(RecB(42), RecB(43))))
  }

  it should "convert to Either in different cases" in new Fixture {
    case class RecA(anInt: Int)
    case class RecB(anInt: Int)

    RecA(42).convert[Either[RecB, String]] shouldBe Right(Left(RecB(42)))
    RecA(42).convert[Either[Boolean, RecB]] shouldBe Right(Right(RecB(42)))
  }

  it should "convert to List in different cases" in new Fixture {
    case class RecA(anInt: Int)
    case class RecB(anInt: Int)

    RecA(42).convert[List[RecB]] shouldBe Right(List(RecB(42)))
    List(RecA(42), RecA(43)).convert[List[RecB]] shouldBe Right(List(RecB(42), RecB(43)))
  }

  it should "convert those specific instances - same shape and types, just verifying decode(encode(t)) = t" in new Fixture {
    case class Specific(
      aByte: Byte,
      aShort: Short,
      aLong: Long,
      aFloat: Float,
      aChar: Char,
      aUnit: Unit,
      aBigInt: BigInt,
      aBigDecimal: BigDecimal,
      anUri: URI,
      anURL: URL,
      aDuration: Duration,
      anUUID: UUID,
      anInstant: Instant,
      aLocalDate: LocalDate,
      aLocalTime: LocalTime,
      aLocalDateTime: LocalDateTime,
      aFile: io.File,
      aSerializedUri: String,
      aSerializedURL: String,
      aSerializedDuration: String,
      aSerializedUUID: String,
      aSerializedInstant: String,
      aSerializedLocalDate: String,
      aSerializedLocalTime: String,
      aSerializedLocalDateTime: String,
      aSerializedFile: String
    )

    case class Target(
      aByte: Byte,
      aShort: Short,
      aLong: Long,
      aFloat: Float,
      aChar: Char,
      aUnit: Unit,
      aBigInt: BigInt,
      aBigDecimal: BigDecimal,
      anUri: URI,
      anURL: URL,
      aDuration: Duration,
      anUUID: UUID,
      anInstant: Instant,
      aLocalDate: LocalDate,
      aLocalTime: LocalTime,
      aLocalDateTime: LocalDateTime,
      aFile: io.File,
      aSerializedUri: URI,
      aSerializedURL: URL,
      aSerializedDuration: Duration,
      aSerializedUUID: UUID,
      aSerializedInstant: Instant,
      aSerializedLocalDate: LocalDate,
      aSerializedLocalTime: LocalTime,
      aSerializedLocalDateTime: LocalDateTime,
      aSerializedFile: io.File
    )

    val uuid          = UUID.randomUUID()
    val instant       = Instant.now()
    val localDate     = LocalDate.now()
    val localTime     = LocalTime.now()
    val localDateTime = LocalDateTime.now()
    val file          = File.createTempFile("foo", ".tmp")

    Specific(
      42.toByte,
      4,
      4000L,
      2.0f,
      'C',
      (),
      BigInt(1234567890),
      BigDecimal(123456780.9),
      new URI("urn:isbn:096139210x"),
      new URL("https://www.google.com"),
      42 seconds,
      uuid,
      instant,
      localDate,
      localTime,
      localDateTime,
      file,
      new URI("urn:isbn:096139210x").toString,
      new URL("https://www.google.com").toString,
      (42 seconds).toString,
      uuid.toString,
      instant.toString,
      localDate.toString,
      localTime.toString,
      localDateTime.toString,
      file.toString
    ).convert[Target] shouldEqual Right(
      Target(
        42.toByte,
        4,
        4000L,
        2.0f,
        'C',
        (),
        BigInt(1234567890),
        BigDecimal(123456780.9),
        new URI("urn:isbn:096139210x"),
        new URL("https://www.google.com"),
        42 seconds,
        uuid,
        instant,
        localDate,
        localTime,
        localDateTime,
        file,
        new URI("urn:isbn:096139210x"),
        new URL("https://www.google.com"),
        42 seconds,
        uuid,
        instant,
        localDate,
        localTime,
        localDateTime,
        file
      )
    )
  }

}
