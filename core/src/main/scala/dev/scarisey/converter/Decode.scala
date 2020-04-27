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

package dev.scarisey.converter

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

trait Decode[T] {
  def from(g: DynamicRepr): Result[T]
}
object Decode {

//  def apply[T:Decode]:Decode[T] = implicitly[Decode[T]]

  def instance[A](f: DynamicRepr => Result[A]): Decode[A] = (g: DynamicRepr) => f(g)

  /*
   * AnyVal specific instances
   */
  implicit val genConvByte: Decode[Byte] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Byte] =>
      Try(a.asInstanceOf[Byte]).toEither.left.map(_ => IncorrectPathOrType("Byte"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvInt: Decode[Int] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Int] =>
      Try(a.asInstanceOf[Int]).toEither.left.map(_ => IncorrectPathOrType("Int"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvShort: Decode[Short] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Short] =>
      Try(a.asInstanceOf[Short]).toEither.left.map(_ => IncorrectPathOrType("Short"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvLong: Decode[Long] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Long] =>
      Try(a.asInstanceOf[Long]).toEither.left.map(_ => IncorrectPathOrType("Long"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvFloat: Decode[Float] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Float] =>
      Try(a.asInstanceOf[Float]).toEither.left.map(_ => IncorrectPathOrType("Float"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvChar: Decode[Char] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Char] =>
      Try(a.asInstanceOf[Char]).toEither.left.map(_ => IncorrectPathOrType("Char"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvBoolean: Decode[Boolean] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Boolean] =>
      Try(a.asInstanceOf[Boolean]).toEither.left.map(_ => IncorrectPathOrType("Boolean"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvDouble: Decode[Double] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Double] =>
      Try(a.asInstanceOf[Double]).toEither.left.map(_ => IncorrectPathOrType("Double"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvUnit: Decode[Unit] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[Unit] => Right(())
    case _                                           => Left(IncorrectPath)
  }

  /*
   * Other useful specific instances
   */

  implicit val genConvBigInt: Decode[BigInt] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[BigInt] =>
      Try(a.asInstanceOf[BigInt]).toEither.left.map(_ => IncorrectPathOrType("BigInt"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvBigDecimal: Decode[BigDecimal] = {
    case ValueDynamicRepr(a) if a.isInstanceOf[BigDecimal] =>
      Try(a.asInstanceOf[BigDecimal]).toEither.left.map(_ => IncorrectPathOrType("BigDecimal"))
    case _ => Left(IncorrectPath)
  }

  implicit val genConvString: Decode[String] = new Decode[String] {
    override def from(g: DynamicRepr): Result[String] = g match {
      case ValueDynamicRepr(a) if a.isInstanceOf[String] =>
        Try(a.asInstanceOf[String]).toEither.left.map(_ => IncorrectPathOrType("String"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvUri: Decode[URI] = new Decode[URI] {
    override def from(g: DynamicRepr): Result[URI] = g match {
      case ValueDynamicRepr(a) =>
        Try(URI.create(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("URI"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvUrl: Decode[URL] = new Decode[URL] {
    override def from(g: DynamicRepr): Result[URL] = g match {
      case ValueDynamicRepr(a) =>
        Try(URI.create(a.asInstanceOf[String]).toURL).toEither.left.map(_ => IncorrectPathOrType("URL"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvDuration: Decode[Duration] = new Decode[Duration] {
    override def from(g: DynamicRepr): Result[Duration] = g match {
      case ValueDynamicRepr(a) =>
        Try(Duration(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("Duration"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvUuid: Decode[UUID] = new Decode[UUID] {
    override def from(g: DynamicRepr): Result[UUID] = g match {
      case ValueDynamicRepr(a) =>
        Try(UUID.fromString(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("UUID"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvInstant: Decode[Instant] = new Decode[Instant] {
    override def from(g: DynamicRepr): Result[Instant] = g match {
      case ValueDynamicRepr(a) =>
        Try(Instant.parse(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("Instant"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvLocalDate: Decode[LocalDate] = new Decode[LocalDate] {
    override def from(g: DynamicRepr): Result[LocalDate] = g match {
      case ValueDynamicRepr(a) =>
        Try(LocalDate.parse(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("LocalDate"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvLocalDateTime: Decode[LocalDateTime] = new Decode[LocalDateTime] {
    override def from(g: DynamicRepr): Result[LocalDateTime] = g match {
      case ValueDynamicRepr(a) =>
        Try(LocalDateTime.parse(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("LocalDateTime"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvLocalTime: Decode[LocalTime] = new Decode[LocalTime] {
    override def from(g: DynamicRepr): Result[LocalTime] = g match {
      case ValueDynamicRepr(a) =>
        Try(LocalTime.parse(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("LocalTime"))
      case _ => Left(IncorrectPath)
    }
  }

  implicit val genConvFile: Decode[File] = new Decode[File] {
    override def from(g: DynamicRepr): Result[File] = g match {
      case ValueDynamicRepr(a) =>
        Try(new File(a.asInstanceOf[String])).toEither.left.map(_ => IncorrectPathOrType("File"))
      case _ => Left(IncorrectPath)
    }
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
      case IterableDynamicRepr(_) => Left(IncorrectPathOrType("Either"))
      case NilDynamicRepr         => Left(IncorrectPath)
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
      case NilDynamicRepr => Left(IncorrectPath)
    }
  }

}
