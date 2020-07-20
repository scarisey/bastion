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

import bastion.json._
import derivation.decode.auto._
import scala.language.postfixOps

class EncodeDecodeTest extends AnyFlatSpec with Matchers {
  case class Basics(
    aByte: Byte,
    aShort: Short,
    aLong: Long,
    aFloat: Float,
    aChar: Char,
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

  case class Containers(
    anOption: Option[String],
    anEither: Either[UUID, Option[Double]],
    aList: List[Option[String]],
    aMap: Map[String, Map[String, String]]
  )

  behavior of "Encode to json then decode"

  it should "be identity for all basic types" in {
    val uuid          = UUID.randomUUID()
    val instant       = Instant.now()
    val localDate     = LocalDate.now()
    val localTime     = LocalTime.now()
    val localDateTime = LocalDateTime.now()
    val file          = File.createTempFile("foo", ".tmp")
    val basics = Basics(
      42.toByte,
      4,
      4000L,
      2.0f,
      'C',
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
    )
    val json   = basics.asJson
    val actual = decodeJson[Basics](json)

    actual.fold(err => fail(err.toString), _ shouldEqual basics)
  }

  it should "be identity for Option" in {
    val jsonEmpty = Option.empty[String].asJson
    decodeJson[Option[String]](jsonEmpty) shouldEqual Right(None)

    val jsonFilled = Some("foo").asJson
    decodeJson[Option[String]](jsonFilled) shouldEqual Right(Some("foo"))
  }

  it should "be identity for containers" in {
    val containers = Containers(
      Some("some description"),
      Right(Some(42.0)),
      List(Some("foo"), None, Some("bar")),
      Map("foo" -> Map("bar" -> "text", "baz" -> "text2"))
    )

    implicit val encodeC = BasicJsonEncoder.deriveBasicJsonEncoder[Containers]
    val json             = containers.asJson

    implicit val decoder = Decoder.derive[Containers]
    val actual           = decodeJson[Containers](json)

    actual shouldBe Right(containers)
  }
}
