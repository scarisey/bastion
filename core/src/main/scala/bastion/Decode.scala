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

import scala.concurrent.duration.Duration
import scala.util.Try
import bastion.derivation.decode.AutoUnlockD
import bastion.derivation.decode.DecodeDerivation
import magnolia._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Typeclass representing a decoder from a DynamicRepr to a type T.
 * Attempting to decode may fail, resulting in [[DecodeError]].
 */
trait Decode[T] {
  def from(g: DynamicRepr): Result[T]
}
object Decode extends DecodeDerivation {

  /**
   * Create your own typeclass instance with this method.
   */
  def instance[A](f: DynamicRepr => Result[A]): Decode[A] = (g: DynamicRepr) => f(g)

  /**
   * For a function f, mapping a type A to R, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   */
  def wrap[A: Decode, R](f: A => R): Decode[R] = (g: DynamicRepr) => g.apply(f)

  /**
   * For a function f, mapping a type A to R, and that can fail, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   * The eventual error L will be wrapped in a [[WrappedError]].
   */
  def wrapE[A: Decode, L, R](f: A => Either[L, R]): Decode[R] = (g: DynamicRepr) => g.applyE(f)

  /**
   * For a function f, mapping a type A to maybe R, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   * The absence of value R will be represented by a [[NilSmartConstructorError]].
   */
  def wrapO[A: Decode, R](f: A => Option[R]): Decode[R] = (g: DynamicRepr) => g.applyO(f)

  /**
   * For a function f, mapping a type A to R, and that can fail, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   * The eventual throwable error will be wrapped in a [[WrappedError]].
   */
  def wrapT[A: Decode, R](f: A => Try[R]): Decode[R] = (g: DynamicRepr) => g.applyT(f)

  /*
   * AnyVal specific instances
   */
  implicit val decodeByte: Decode[Byte] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Byte => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Byte"))
      }
    case d => Left(IncorrectPath(d, "Byte"))
  }

  implicit val decodeInt: Decode[Int] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Int => Right(x)
        case _      => Left(UnexpectedEncodeValue(d, "Int"))
      }
    case d => Left(IncorrectPath(d, "Int"))
  }

  implicit val decodeShort: Decode[Short] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Short => Right(x)
        case _        => Left(UnexpectedEncodeValue(d, "Short"))
      }
    case d => Left(IncorrectPath(d, "Short"))
  }

  implicit val decodeLong: Decode[Long] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Long => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Long"))
      }
    case d => Left(IncorrectPath(d, "Long"))
  }

  implicit val decodeFloat: Decode[Float] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Float => Right(x)
        case _        => Left(UnexpectedEncodeValue(d, "Float"))
      }
    case d => Left(IncorrectPath(d, "Float"))
  }

  implicit val decodeChar: Decode[Char] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Char => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Char"))
      }
    case d => Left(IncorrectPath(d, "Char"))
  }

  implicit val decodeBoolean: Decode[Boolean] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Boolean => Right(x)
        case _          => Left(UnexpectedEncodeValue(d, "Boolean"))
      }
    case d => Left(IncorrectPath(d, "Boolean"))
  }

  implicit val decodeDouble: Decode[Double] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Double => Right(x)
        case _         => Left(UnexpectedEncodeValue(d, "Double"))
      }
    case d => Left(IncorrectPath(d, "Double"))
  }

  implicit val decodeUnit: Decode[Unit] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Unit => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Unit"))
      }
    case d => Left(IncorrectPath(d, "Unit"))
  }

  /*
   * Other useful specific instances
   */

  implicit val decodeBigInt: Decode[BigInt] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: BigInt => Right(x)
        case _         => Left(UnexpectedEncodeValue(d, "BigInt"))
      }
    case d => Left(IncorrectPath(d, "BigInt"))
  }

  implicit val decodeBigDecimal: Decode[BigDecimal] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: BigDecimal => Right(x)
        case _             => Left(UnexpectedEncodeValue(d, "BigDecimal"))
      }
    case d => Left(IncorrectPath(d, "BigDecimal"))
  }

  implicit val decodeString: Decode[String] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: String => Right(x)
        case _         => Left(UnexpectedEncodeValue(d, "String"))
      }
    case d => Left(IncorrectPath(d, "String"))
  }

  implicit val decodeUri: Decode[URI] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: URI    => Right(x)
        case x: String => Try(URI.create(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "URI"))
        case _         => Left(UnexpectedEncodeValue(d, "URI"))
      }
    case d => Left(IncorrectPath(d, "URI"))
  }

  implicit val decodeUrl: Decode[URL] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: URL    => Right(x)
        case x: String => Try(URI.create(x).toURL).toEither.left.map(_ => UnexpectedEncodeValue(d, "URL"))
        case _         => Left(UnexpectedEncodeValue(d, "URL"))
      }
    case d => Left(IncorrectPath(d, "URL"))
  }

  implicit val decodeDuration: Decode[Duration] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Duration => Right(x)
        case x: String   => Right(Duration(x))
        case _           => Left(UnexpectedEncodeValue(d, "Duration"))
      }
    case d => Left(IncorrectPath(d, "Duration"))
  }

  implicit val decodeUuid: Decode[UUID] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: UUID   => Right(x)
        case x: String => Right(UUID.fromString(x))
        case _         => Left(UnexpectedEncodeValue(d, "UUID"))
      }
    case d => Left(IncorrectPath(d, "UUID"))
  }

  implicit val decodeInstant: Decode[Instant] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Instant => Right(x)
        case x: String  => Right(Instant.parse(x))
        case _          => Left(UnexpectedEncodeValue(d, "Instant"))
      }
    case d => Left(IncorrectPath(d, "Instant"))
  }

  implicit val decodeLocalDate: Decode[LocalDate] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalDate => Right(x)
        case x: String    => Right(LocalDate.parse(x))
        case _            => Left(UnexpectedEncodeValue(d, "LocalDate"))
      }
    case d => Left(IncorrectPath(d, "LocalDate"))
  }

  implicit val decodeLocalDateTime: Decode[LocalDateTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalDateTime => Right(x)
        case x: String        => Right(LocalDateTime.parse(x))
        case _                => Left(UnexpectedEncodeValue(d, "LocalDateTime"))
      }
    case d => Left(IncorrectPath(d, "LocalDateTime"))
  }

  implicit val decodeLocalTime: Decode[LocalTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalTime => Right(x)
        case x: String    => Right(LocalTime.parse(x))
        case _            => Left(UnexpectedEncodeValue(d, "LocalTime"))
      }
    case d => Left(IncorrectPath(d, "LocalTime"))
  }

  implicit val decodeFile: Decode[File] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: File   => Right(x)
        case x: String => Right(new File(x))
        case _         => Left(UnexpectedEncodeValue(d, "File"))
      }
    case d => Left(IncorrectPath(d, "File"))
  }

  implicit def decodeOption[A: Decode]: Decode[Option[A]] = new Decode[Option[A]] {
    override def from(g: DynamicRepr): Result[Option[A]] = g match {
      case ProductDynamicRepr(_)  => implicitly[Decode[A]].from(g).map(Some(_))
      case IterableDynamicRepr(_) => implicitly[Decode[A]].from(g).map(Some(_))
      case ValueDynamicRepr(_)    => implicitly[Decode[A]].from(g).map(Some(_))
      case NilDynamicRepr         => Right(None)
    }
  }

  implicit def decodeEither[L: Decode, R: Decode]: Decode[Either[L, R]] = new Decode[Either[L, R]] {
    override def from(g: DynamicRepr): Result[Either[L, R]] = g match {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) =>
        implicitly[Decode[L]].from(g).map(Left(_)) match {
          case Left(_)      => implicitly[Decode[R]].from(g).map(Right(_))
          case r @ Right(_) => r
        }
      case d => Left(UnexpectedEncodeValue(d, "Either"))
    }
  }

  implicit def decodeList[A: Decode]: Decode[List[A]] = new Decode[List[A]] {
    override def from(g: DynamicRepr): Result[List[A]] = g match {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) => implicitly[Decode[A]].from(g).map(List.apply(_))
      case IterableDynamicRepr(items) =>
        items.traverse(implicitly[Decode[A]].from(_))
      case d => Left(UnexpectedEncodeValue(d, "List"))
    }
  }

  implicit def deriveDecode[T](implicit u: AutoUnlockD): Decode[T] = macro macroDeriveDecode[T]

  def macroDeriveDecode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree): c.Tree = {
    val _ = u
    Magnolia.gen[T](c)
  }

}
