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
import java.io.File
import java.net.URI
import java.net.URL
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.util.UUID

import bastion.derivation.decode.auto._
import bastion.derivation.encode.auto._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.postfixOps
import scala.concurrent.duration._

class DecodeConversionsTest extends AnyFlatSpec with Matchers {
  trait Fixture {
    case class Source[T](t: T)
    case class Target[T](t: T)
  }

  behavior of "number conversions"

  it should "manage to decode from Int" in new Fixture {
    Source[Int](Int.MaxValue).convert[Target[Int]] shouldEqual Right(Target[Int](Int.MaxValue))
    Source[Int](Int.MaxValue).convert[Target[Long]] shouldEqual Right(Target[Long](Int.MaxValue.toLong))
    Source[Int](Int.MaxValue).convert[Target[Float]] shouldEqual Right(Target[Float](Int.MaxValue.toFloat))
    Source[Int](Int.MaxValue).convert[Target[Double]] shouldEqual Right(Target[Double](Int.MaxValue.toDouble))
    Source[Int](Int.MaxValue).convert[Target[String]] shouldEqual Right(Target[String](Int.MaxValue.toString))
    Source[Int](Int.MaxValue).convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(Int.MaxValue)))
    Source[Int](Int.MaxValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Int.MaxValue)))

    Source[Int](Int.MinValue).convert[Target[Int]] shouldEqual Right(Target[Int](Int.MinValue))
    Source[Int](Int.MinValue).convert[Target[Long]] shouldEqual Right(Target[Long](Int.MinValue.toLong))
    Source[Int](Int.MinValue).convert[Target[Float]] shouldEqual Right(Target[Float](Int.MinValue.toFloat))
    Source[Int](Int.MinValue).convert[Target[Double]] shouldEqual Right(Target[Double](Int.MinValue.toDouble))
    Source[Int](Int.MinValue).convert[Target[String]] shouldEqual Right(Target[String](Int.MinValue.toString))
    Source[Int](Int.MinValue).convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(Int.MinValue)))
    Source[Int](Int.MinValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Int.MinValue)))
  }

  it should "manage to decode from Short" in new Fixture {
    Source[Short](Short.MaxValue).convert[Target[Short]] shouldEqual Right(Target[Short](Short.MaxValue))
    Source[Short](Short.MaxValue).convert[Target[Int]] shouldEqual Right(Target[Int](Short.MaxValue.toInt))
    Source[Short](Short.MaxValue).convert[Target[Long]] shouldEqual Right(Target[Long](Short.MaxValue.toLong))
    Source[Short](Short.MaxValue).convert[Target[Float]] shouldEqual Right(Target[Float](Short.MaxValue.toFloat))
    Source[Short](Short.MaxValue).convert[Target[Double]] shouldEqual Right(Target[Double](Short.MaxValue.toDouble))
    Source[Short](Short.MaxValue).convert[Target[String]] shouldEqual Right(Target[String](Short.MaxValue.toString))
    Source[Short](Short.MaxValue).convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(Short.MaxValue)))
    Source[Short](Short.MaxValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Short.MaxValue)))

    Source[Short](Short.MinValue).convert[Target[Int]] shouldEqual Right(Target[Int](Short.MinValue.toInt))
    Source[Short](Short.MinValue).convert[Target[Long]] shouldEqual Right(Target[Long](Short.MinValue.toLong))
    Source[Short](Short.MinValue).convert[Target[Float]] shouldEqual Right(Target[Float](Short.MinValue.toFloat))
    Source[Short](Short.MinValue).convert[Target[Double]] shouldEqual Right(Target[Double](Short.MinValue.toDouble))
    Source[Short](Short.MinValue).convert[Target[String]] shouldEqual Right(Target[String](Short.MinValue.toString))
    Source[Short](Short.MinValue).convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(Short.MinValue)))
    Source[Short](Short.MinValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Short.MinValue)))
  }

  it should "manage to decode from Long" in new Fixture {
    Source[Long](Long.MaxValue).convert[Target[Long]] shouldEqual Right(Target[Long](Long.MaxValue))
    Source[Long](Long.MaxValue).convert[Target[Float]] shouldEqual Right(Target[Float](Long.MaxValue.toFloat))
    Source[Long](Long.MaxValue).convert[Target[Double]] shouldEqual Right(Target[Double](Long.MaxValue.toDouble))
    Source[Long](Long.MaxValue).convert[Target[String]] shouldEqual Right(Target[String](Long.MaxValue.toString))
    Source[Long](Long.MaxValue).convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(Long.MaxValue)))
    Source[Long](Long.MaxValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Long.MaxValue)))

    Source[Long](Long.MinValue).convert[Target[Long]] shouldEqual Right(Target[Long](Long.MinValue))
    Source[Long](Long.MinValue).convert[Target[Float]] shouldEqual Right(Target[Float](Long.MinValue.toFloat))
    Source[Long](Long.MinValue).convert[Target[Double]] shouldEqual Right(Target[Double](Long.MinValue.toDouble))
    Source[Long](Long.MinValue).convert[Target[String]] shouldEqual Right(Target[String](Long.MinValue.toString))
    Source[Long](Long.MinValue).convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(Long.MinValue)))
    Source[Long](Long.MinValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Long.MinValue)))
  }

  it should "manage to decode from Float" in new Fixture {
    Source[Float](Float.MaxValue).convert[Target[Float]] shouldEqual Right(Target[Float](Float.MaxValue))
    Source[Float](Float.MaxValue).convert[Target[Double]] shouldEqual Right(Target[Double](Float.MaxValue.toDouble))
    Source[Float](Float.MaxValue).convert[Target[String]] shouldEqual Right(Target[String](Float.MaxValue.toString))
    Source[Float](Float.MaxValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Float.MaxValue)))

    Source[Float](Float.MinValue).convert[Target[Float]] shouldEqual Right(Target[Float](Float.MinValue))
    Source[Float](Float.MinValue).convert[Target[Double]] shouldEqual Right(Target[Double](Float.MinValue.toDouble))
    Source[Float](Float.MinValue).convert[Target[String]] shouldEqual Right(Target[String](Float.MinValue.toString))
    Source[Float](Float.MinValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Float.MinValue)))
  }

  it should "manage to decode from Double" in new Fixture {
    Source[Double](Double.MaxValue).convert[Target[Double]] shouldEqual Right(Target[Double](Double.MaxValue.toDouble))
    Source[Double](Double.MaxValue).convert[Target[String]] shouldEqual Right(Target[String](Double.MaxValue.toString))
    Source[Double](Double.MaxValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Double.MaxValue)))

    Source[Double](Double.MinValue).convert[Target[Double]] shouldEqual Right(Target[Double](Double.MinValue.toDouble))
    Source[Double](Double.MinValue).convert[Target[String]] shouldEqual Right(Target[String](Double.MinValue.toString))
    Source[Double](Double.MinValue).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(Double.MinValue)))
  }

  it should "manage to decode from Char" in new Fixture {
    Source[Char]('c').convert[Target[String]] shouldEqual Right(Target[String]("c"))
  }

  it should "manage to decode from Boolean" in new Fixture {
    Source[Boolean](true).convert[Target[String]] shouldEqual Right(Target[String](true.toString))
  }

  it should "manage to decode from BigInt" in new Fixture {
    Source[BigInt](BigInt(42)).convert[Target[String]] shouldEqual Right(Target[String](BigInt(42).toString()))
    Source[BigInt](BigInt(42)).convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(42)))
  }

  it should "manage to decode from BigDecimal" in new Fixture {
    Source[BigDecimal](BigDecimal(42)).convert[Target[String]] shouldEqual Right(Target[String](BigDecimal(42).toString()))
  }

  it should "manage to decode from String" in new Fixture {
    Source[String](Short.MinValue.toString).convert[Target[Short]] shouldEqual Right(Target[Short](Short.MinValue))
    Source[String](Int.MinValue.toString).convert[Target[Int]] shouldEqual Right(Target[Int](Int.MinValue))
    Source[String](Long.MinValue.toString).convert[Target[Long]] shouldEqual Right(Target[Long](Long.MinValue))
    Source[String](Float.MinValue.toString).convert[Target[Float]] shouldEqual Right(Target[Float](Float.MinValue))
    Source[String](Double.MinValue.toString).convert[Target[Double]] shouldEqual Right(Target[Double](Double.MinValue))
    Source[String]("true").convert[Target[Boolean]] shouldEqual Right(Target[Boolean](true))
    Source[String]("3000000000000000000").convert[Target[BigInt]] shouldEqual Right(Target[BigInt](BigInt(3e18.toLong)))
    Source[String]("3.14E18").convert[Target[BigDecimal]] shouldEqual Right(Target[BigDecimal](BigDecimal(3.14e18)))
    Source[String]("urn:isbn:0123456789x").convert[Target[URI]] shouldEqual Right(Target[URI](new URI("urn:isbn:0123456789x")))
    Source[String]("https://www.mozilla.org").convert[Target[URL]] shouldEqual Right(
      Target[URL](new URL("https://www.mozilla.org"))
    )
    Source[String]("30 minutes").convert[Target[Duration]] shouldEqual Right(Target[Duration](30 minutes))
    Source[String]("85e0e171-1547-4c13-b4cf-0e53c97103b7").convert[Target[UUID]] shouldEqual Right(
      Target[UUID](UUID.fromString("85e0e171-1547-4c13-b4cf-0e53c97103b7"))
    )
    val instant        = "2020-05-14T22:17:45.000Z"
    val localDateTime  = "2020-05-14T22:17:45.00"
    val localDate      = "2020-05-14"
    val localTime      = "22:17:45.00"
    val offsetDateTime = "2020-05-14T22:17:45.0000000+02:00"
    val offsetTime     = "22:17:45.0000000+02:00"
    Source[String](instant).convert[Target[Instant]] shouldEqual Right(Target[Instant](Instant.parse(instant)))
    Source[String](localDateTime).convert[Target[LocalDateTime]] shouldEqual Right(
      Target[LocalDateTime](LocalDateTime.parse(localDateTime))
    )
    Source[String](localDate).convert[Target[LocalDate]] shouldEqual Right(Target[LocalDate](LocalDate.parse(localDate)))
    Source[String](localTime).convert[Target[LocalTime]] shouldEqual Right(Target[LocalTime](LocalTime.parse(localTime)))
    Source[String](offsetDateTime).convert[Target[OffsetDateTime]] shouldEqual Right(
      Target[OffsetDateTime](OffsetDateTime.parse(offsetDateTime))
    )
    Source[String](offsetTime).convert[Target[OffsetTime]] shouldEqual Right(Target[OffsetTime](OffsetTime.parse(offsetTime)))
    Source[String]("/tmp/some.file").convert[Target[File]] shouldEqual Right(Target[File](new File("/tmp/some.file")))
  }

  ignore should "handle conversions between date and time types" in {
    //TODO
  }
}
