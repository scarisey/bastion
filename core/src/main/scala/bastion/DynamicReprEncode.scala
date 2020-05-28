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
import java.util.UUID

import bastion.derivation.dynamicrepr.AutoUnlock
import bastion.derivation.dynamicrepr.Configuration
import bastion.derivation.dynamicrepr.EncodeDerivation
import magnolia._

import language.experimental.macros
import scala.concurrent.duration.Duration
import scala.reflect.macros.whitebox

/**
 * Typeclass representing an Encoder from a type A to [[DynamicRepr]].
 */
trait DynamicReprEncode[-A] {
  def to(a: A): DynamicRepr
}
object DynamicReprEncode extends EncodeDerivation {
  /*
  Any val implicits
   */
  implicit val encodeByte: DynamicReprEncode[Byte]       = (a: Byte) => ValueDynamicRepr(a)
  implicit val encodeInt: DynamicReprEncode[Int]         = (a: Int) => ValueDynamicRepr(a)
  implicit val encodeShort: DynamicReprEncode[Short]     = (a: Short) => ValueDynamicRepr(a)
  implicit val encodeLong: DynamicReprEncode[Long]       = (a: Long) => ValueDynamicRepr(a)
  implicit val encodeFloat: DynamicReprEncode[Float]     = (a: Float) => ValueDynamicRepr(a)
  implicit val encodeChar: DynamicReprEncode[Char]       = (a: Char) => ValueDynamicRepr(a)
  implicit val encodeBoolean: DynamicReprEncode[Boolean] = (a: Boolean) => ValueDynamicRepr(a)
  implicit val encodeDouble: DynamicReprEncode[Double]   = (a: Double) => ValueDynamicRepr(a)
  implicit val encodeUnit: DynamicReprEncode[Unit]       = (a: Unit) => ValueDynamicRepr(a)

  /*
  Other basic types
   */
  implicit val encodeString: DynamicReprEncode[String]               = (a: String) => ValueDynamicRepr(a)
  implicit val encodeBigInt: DynamicReprEncode[BigInt]               = (a: BigInt) => ValueDynamicRepr(a)
  implicit val encodeBigDecimal: DynamicReprEncode[BigDecimal]       = (a: BigDecimal) => ValueDynamicRepr(a)
  implicit val encodeUri: DynamicReprEncode[URI]                     = (a: URI) => ValueDynamicRepr(a)
  implicit val encodeUrl: DynamicReprEncode[URL]                     = (a: URL) => ValueDynamicRepr(a)
  implicit val encodeDuration: DynamicReprEncode[Duration]           = (a: Duration) => ValueDynamicRepr(a)
  implicit val encodeUUID: DynamicReprEncode[UUID]                   = (a: UUID) => ValueDynamicRepr(a)
  implicit val encodeInstant: DynamicReprEncode[Instant]             = (a: Instant) => ValueDynamicRepr(a)
  implicit val encodeLocalDate: DynamicReprEncode[LocalDate]         = (a: LocalDate) => ValueDynamicRepr(a)
  implicit val encodeLocalTime: DynamicReprEncode[LocalTime]         = (a: LocalTime) => ValueDynamicRepr(a)
  implicit val encodeLocalDateTime: DynamicReprEncode[LocalDateTime] = (a: LocalDateTime) => ValueDynamicRepr(a)
  implicit val encodeFile: DynamicReprEncode[File]                   = (a: File) => ValueDynamicRepr(a)

  implicit def encodeOption[A: DynamicReprEncode]: DynamicReprEncode[Option[A]] = {
    case Some(value) => implicitly[DynamicReprEncode[A]].to(value)
    case None        => NilDynamicRepr
  }

  implicit def encodeEither[L: DynamicReprEncode, R: DynamicReprEncode]: DynamicReprEncode[Either[L, R]] = {
    case Left(value)  => implicitly[DynamicReprEncode[L]].to(value)
    case Right(value) => implicitly[DynamicReprEncode[R]].to(value)
  }

  implicit def encodeIterable[A: DynamicReprEncode]: DynamicReprEncode[Iterable[A]] =
    (a: Iterable[A]) => IterableDynamicRepr(a.map(implicitly[DynamicReprEncode[A]].to(_)))

  implicit def deriveEncode[T](implicit u: AutoUnlock, configuration: Configuration): DynamicReprEncode[T] =
    macro macroDeriveEncode[T]

  def macroDeriveEncode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree, configuration: c.Tree): c.Tree = {
    val _ = (u, configuration)
    Magnolia.gen[T](c)
  }

}
