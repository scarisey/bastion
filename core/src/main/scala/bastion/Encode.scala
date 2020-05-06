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

import bastion.derivation.encode.AutoUnlock
import bastion.derivation.encode.Configuration
import bastion.derivation.encode.EncodeDerivation
import magnolia._

import language.experimental.macros
import scala.concurrent.duration.Duration
import scala.reflect.macros.whitebox

/**
 * Typeclass representing an Encoder from a type A to [[DynamicRepr]].
 */
trait Encode[-A] {
  def to(a: A): DynamicRepr
}
object Encode extends EncodeDerivation {
  /*
  Any val implicits
   */
  implicit val encodeByte: Encode[Byte]       = (a: Byte) => ValueDynamicRepr(a)
  implicit val encodeInt: Encode[Int]         = (a: Int) => ValueDynamicRepr(a)
  implicit val encodeShort: Encode[Short]     = (a: Short) => ValueDynamicRepr(a)
  implicit val encodeLong: Encode[Long]       = (a: Long) => ValueDynamicRepr(a)
  implicit val encodeFloat: Encode[Float]     = (a: Float) => ValueDynamicRepr(a)
  implicit val encodeChar: Encode[Char]       = (a: Char) => ValueDynamicRepr(a)
  implicit val encodeBoolean: Encode[Boolean] = (a: Boolean) => ValueDynamicRepr(a)
  implicit val encodeDouble: Encode[Double]   = (a: Double) => ValueDynamicRepr(a)
  implicit val encodeUnit: Encode[Unit]       = (a: Unit) => ValueDynamicRepr(a)

  /*
  Other basic types
   */
  implicit val encodeString: Encode[String]               = (a: String) => ValueDynamicRepr(a)
  implicit val encodeBigInt: Encode[BigInt]               = (a: BigInt) => ValueDynamicRepr(a)
  implicit val encodeBigDecimal: Encode[BigDecimal]       = (a: BigDecimal) => ValueDynamicRepr(a)
  implicit val encodeUri: Encode[URI]                     = (a: URI) => ValueDynamicRepr(a)
  implicit val encodeUrl: Encode[URL]                     = (a: URL) => ValueDynamicRepr(a)
  implicit val encodeDuration: Encode[Duration]           = (a: Duration) => ValueDynamicRepr(a)
  implicit val encodeUUID: Encode[UUID]                   = (a: UUID) => ValueDynamicRepr(a)
  implicit val encodeInstant: Encode[Instant]             = (a: Instant) => ValueDynamicRepr(a)
  implicit val encodeLocalDate: Encode[LocalDate]         = (a: LocalDate) => ValueDynamicRepr(a)
  implicit val encodeLocalTime: Encode[LocalTime]         = (a: LocalTime) => ValueDynamicRepr(a)
  implicit val encodeLocalDateTime: Encode[LocalDateTime] = (a: LocalDateTime) => ValueDynamicRepr(a)
  implicit val encodeFile: Encode[File]                   = (a: File) => ValueDynamicRepr(a)

  implicit def encodeOption[A: Encode]: Encode[Option[A]] = {
    case Some(value) => implicitly[Encode[A]].to(value)
    case None        => NilDynamicRepr
  }

  implicit def encodeEither[L: Encode, R: Encode]: Encode[Either[L, R]] = {
    case Left(value)  => implicitly[Encode[L]].to(value)
    case Right(value) => implicitly[Encode[R]].to(value)
  }

  implicit def encodeIterable[A: Encode]: Encode[Iterable[A]] =
    (a: Iterable[A]) => IterableDynamicRepr(a.map(implicitly[Encode[A]].to(_)))

  implicit def deriveEncode[T](implicit u: AutoUnlock, configuration: Configuration): Encode[T] = macro macroDeriveEncode[T]

  def macroDeriveEncode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree, configuration: c.Tree): c.Tree = {
    val _ = (u, configuration)
    Magnolia.gen[T](c)
  }

}
