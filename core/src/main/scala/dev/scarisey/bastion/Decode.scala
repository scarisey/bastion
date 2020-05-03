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

import scala.concurrent.duration.Duration
import scala.util.Try
import dev.scarisey.bastion.derivation.decode.AutoUnlockD
import dev.scarisey.bastion.derivation.decode.DecodeDerivation
import magnolia._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait Decode[T] {
  def from(g: DynamicRepr): Result[T]
}
object Decode extends DecodeDerivation {

  def instance[A](f: DynamicRepr => Result[A]): Decode[A] = (g: DynamicRepr) => f(g)

  def wrap[A: Decode, R](f: A => R): Decode[R]                = (g: DynamicRepr) => g.apply(f)
  def wrapE[A: Decode, L, R](f: A => Either[L, R]): Decode[R] = (g: DynamicRepr) => g.applyE(f)
  def wrapO[A: Decode, R](f: A => Option[R]): Decode[R]       = (g: DynamicRepr) => g.applyO(f)
  def wrapT[A: Decode, R](f: A => Try[R]): Decode[R]          = (g: DynamicRepr) => g.applyT(f)

  /*
   * AnyVal specific instances
   */
  implicit val genConvByte: Decode[Byte] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Byte => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Byte"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvInt: Decode[Int] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Int => Right(x)
        case _      => Left(UnexpectedEncodeValue(d, "Int"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvShort: Decode[Short] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Short => Right(x)
        case _        => Left(UnexpectedEncodeValue(d, "Short"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvLong: Decode[Long] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Long => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Long"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvFloat: Decode[Float] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Float => Right(x)
        case _        => Left(UnexpectedEncodeValue(d, "Float"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvChar: Decode[Char] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Char => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Char"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvBoolean: Decode[Boolean] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Boolean => Right(x)
        case _          => Left(UnexpectedEncodeValue(d, "Boolean"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvDouble: Decode[Double] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Double => Right(x)
        case _         => Left(UnexpectedEncodeValue(d, "Double"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvUnit: Decode[Unit] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Unit => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Unit"))
      }
    case _ => Left(IncorrectPath)
  }

  /*
   * Other useful specific instances
   */

  implicit val genConvBigInt: Decode[BigInt] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: BigInt => Right(x)
        case _         => Left(UnexpectedEncodeValue(d, "BigInt"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvBigDecimal: Decode[BigDecimal] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: BigDecimal => Right(x)
        case _             => Left(UnexpectedEncodeValue(d, "BigDecimal"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvString: Decode[String] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: String => Right(x)
        case _         => Left(UnexpectedEncodeValue(d, "String"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvUri: Decode[URI] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: URI    => Right(x)
        case x: String => Try(URI.create(x)).toEither.left.map(_ => IncorrectPathOrType(d, "URI"))
        case _         => Left(UnexpectedEncodeValue(d, "URI"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvUrl: Decode[URL] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: URL    => Right(x)
        case x: String => Try(URI.create(x).toURL).toEither.left.map(_ => IncorrectPathOrType(d, "URL"))
        case _         => Left(UnexpectedEncodeValue(d, "URL"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvDuration: Decode[Duration] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Duration => Right(x)
        case x: String   => Right(Duration(x))
        case _           => Left(UnexpectedEncodeValue(d, "Duration"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvUuid: Decode[UUID] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: UUID   => Right(x)
        case x: String => Right(UUID.fromString(x))
        case _         => Left(UnexpectedEncodeValue(d, "UUID"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvInstant: Decode[Instant] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Instant => Right(x)
        case x: String  => Right(Instant.parse(x))
        case _          => Left(UnexpectedEncodeValue(d, "Instant"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvLocalDate: Decode[LocalDate] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalDate => Right(x)
        case x: String    => Right(LocalDate.parse(x))
        case _            => Left(UnexpectedEncodeValue(d, "LocalDate"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvLocalDateTime: Decode[LocalDateTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalDateTime => Right(x)
        case x: String        => Right(LocalDateTime.parse(x))
        case _                => Left(UnexpectedEncodeValue(d, "LocalDateTime"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvLocalTime: Decode[LocalTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalTime => Right(x)
        case x: String    => Right(LocalTime.parse(x))
        case _            => Left(UnexpectedEncodeValue(d, "LocalTime"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit val genConvFile: Decode[File] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: File   => Right(x)
        case x: String => Right(new File(x))
        case _         => Left(UnexpectedEncodeValue(d, "File"))
      }
    case _ => Left(IncorrectPath)
  }

  implicit def genConvOption[A: Decode]: Decode[Option[A]] = new Decode[Option[A]] {
    override def from(g: DynamicRepr): Result[Option[A]] = g match {
      case ProductDynamicRepr(_)  => implicitly[Decode[A]].from(g).map(Some(_))
      case IterableDynamicRepr(_) => implicitly[Decode[A]].from(g).map(Some(_))
      case ValueDynamicRepr(_)    => implicitly[Decode[A]].from(g).map(Some(_))
      case NilDynamicRepr         => Right(None)
    }
  }

  implicit def genConvEither[L: Decode, R: Decode]: Decode[Either[L, R]] = new Decode[Either[L, R]] {
    override def from(g: DynamicRepr): Result[Either[L, R]] = g match {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) =>
        implicitly[Decode[L]].from(g).map(Left(_)) match {
          case Left(_)      => implicitly[Decode[R]].from(g).map(Right(_))
          case r @ Right(_) => r
        }
      case d => Left(UnexpectedEncodeValue(d, "Either"))
    }
  }

  implicit def genConvList[A: Decode]: Decode[List[A]] = new Decode[List[A]] {
    override def from(g: DynamicRepr): Result[List[A]] = g match {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) => implicitly[Decode[A]].from(g).map(List.apply(_))
      case IterableDynamicRepr(items) =>
        items
          .map(implicitly[Decode[A]].from(_))
          .foldLeft(Right(Seq()).asInstanceOf[Result[List[A]]]) {
            case (acc, res) => acc.flatMap(xs => res.map(x => xs ++ Seq(x)))
          }
      case d => Left(UnexpectedEncodeValue(d, "List"))
    }
  }

  implicit def deriveDecode[T](implicit u: AutoUnlockD): Decode[T] = macro macroDeriveDecode[T]

  def macroDeriveDecode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree): c.Tree = {
    val _ = u
    Magnolia.gen[T](c)
  }

}
