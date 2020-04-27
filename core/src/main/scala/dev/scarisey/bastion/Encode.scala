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

import java.io.File
import java.net.URI
import java.net.URL
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

import dev.scarisey.bastion.derivation.encode.AutoUnlock
import dev.scarisey.bastion.derivation.encode.EncodeDerivation
import magnolia._

import language.experimental.macros
import scala.concurrent.duration.Duration
import scala.reflect.macros.whitebox

trait Encode[-A] {
  def to(a: A): DynamicRepr
}
object Encode extends EncodeDerivation {
//  def apply[A: Encode]: Encode[A] = implicitly[Encode[A]]

  /*
  Any val implicits
   */
  implicit val genByte: Encode[Byte]   = new Encode[Byte]  { override def to(a: Byte): DynamicRepr  = ValueDynamicRepr(a) }
  implicit val genInt: Encode[Int]     = new Encode[Int]   { override def to(a: Int): DynamicRepr   = ValueDynamicRepr(a) }
  implicit val genShort: Encode[Short] = new Encode[Short] { override def to(a: Short): DynamicRepr = ValueDynamicRepr(a) }
  implicit val genLong: Encode[Long]   = new Encode[Long]  { override def to(a: Long): DynamicRepr  = ValueDynamicRepr(a) }
  implicit val genFloat: Encode[Float] = new Encode[Float] { override def to(a: Float): DynamicRepr = ValueDynamicRepr(a) }
  implicit val genChar: Encode[Char]   = new Encode[Char]  { override def to(a: Char): DynamicRepr  = ValueDynamicRepr(a) }
  implicit val genBoolean: Encode[Boolean] = new Encode[Boolean] {
    override def to(a: Boolean): DynamicRepr = ValueDynamicRepr(a)
  }
  implicit val genDouble: Encode[Double] = new Encode[Double] { override def to(a: Double): DynamicRepr = ValueDynamicRepr(a) }
  implicit val genUnit: Encode[Unit]     = new Encode[Unit]   { override def to(a: Unit): DynamicRepr   = ValueDynamicRepr(a) }

  implicit val genString: Encode[String] = new Encode[String] { override def to(a: String): DynamicRepr = ValueDynamicRepr(a) }

  implicit val genBigInt: Encode[BigInt] = new Encode[BigInt] { override def to(a: BigInt): DynamicRepr = ValueDynamicRepr(a) }

  implicit val genBigDecimal: Encode[BigDecimal] = new Encode[BigDecimal] {
    override def to(a: BigDecimal): DynamicRepr = ValueDynamicRepr(a)
  }

  /*
  Other basic types
   */

  implicit val genUri: Encode[URI] = new Encode[URI] { override def to(a: URI): DynamicRepr = ValueDynamicRepr(a.toString) }

  implicit val genUrl: Encode[URL] = new Encode[URL] { override def to(a: URL): DynamicRepr = ValueDynamicRepr(a.toString) }

  implicit val genDuration: Encode[Duration] = new Encode[Duration] {
    override def to(a: Duration): DynamicRepr = ValueDynamicRepr(a.toString)
  }

  implicit val genUUID: Encode[UUID] = new Encode[UUID] { override def to(a: UUID): DynamicRepr = ValueDynamicRepr(a.toString) }

  implicit val genInstant: Encode[Instant] = new Encode[Instant] {
    override def to(a: Instant): DynamicRepr = ValueDynamicRepr(a.toString)
  }

  implicit val genLocalDate: Encode[LocalDate] = new Encode[LocalDate] {
    override def to(a: LocalDate): DynamicRepr = ValueDynamicRepr(a.toString)
  }
  implicit val genLocalTime: Encode[LocalTime] = new Encode[LocalTime] {
    override def to(a: LocalTime): DynamicRepr = ValueDynamicRepr(a.toString)
  }
  implicit val genLocalDateTime: Encode[LocalDateTime] = new Encode[LocalDateTime] {
    override def to(a: LocalDateTime): DynamicRepr = ValueDynamicRepr(a.toString)
  }

  implicit val genFile: Encode[File] = new Encode[File] { override def to(a: File): DynamicRepr = ValueDynamicRepr(a.toString) }

  implicit def genOption[A: Encode]: Encode[Option[A]] = new Encode[Option[A]] {
    override def to(a: Option[A]): DynamicRepr = a match {
      case Some(value) => implicitly[Encode[A]].to(value)
      case None        => NilDynamicRepr
    }
  }

  implicit def genEither[L: Encode, R: Encode]: Encode[Either[L, R]] = new Encode[Either[L, R]] {
    override def to(a: Either[L, R]): DynamicRepr = a match {
      case Left(value)  => implicitly[Encode[L]].to(value)
      case Right(value) => implicitly[Encode[R]].to(value)
    }
  }

  implicit def genIterable[A: Encode]: Encode[Iterable[A]] = new Encode[Iterable[A]] {
    override def to(a: Iterable[A]): DynamicRepr = IterableDynamicRepr(a.map(implicitly[Encode[A]].to(_)))
  }

  implicit def deriveEncode[T](implicit u: AutoUnlock): Encode[T] = macro macroDeriveEncode[T]

  def macroDeriveEncode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree): c.Tree = {
    val _ = u
    Magnolia.gen[T](c)
  }

}
