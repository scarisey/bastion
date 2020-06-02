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

import scala.concurrent.duration.Duration
import scala.util.Try
import bastion.derivation.decode.AutoUnlockDecode
import bastion.derivation.decode.DecodeDerivation
import magnolia._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Typeclass representing a decoder from a DynamicRepr to a type T.
 * Attempting to decode may fail, resulting in [[DecodeError]].
 */
trait Decoder[T] {
  def from(g: DynamicRepr): Result[T]
}
object Decoder extends DecodeDerivation {

  /**
   * Create your own typeclass instance with this method.
   */
  def instance[A](f: DynamicRepr => Result[A]): Decoder[A] = (g: DynamicRepr) => f(g)

  /**
   * For a function f, mapping a type A to R, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   */
  def wrap[A: Decoder, R](f: A => R): Decoder[R] = (g: DynamicRepr) => g.apply(f)

  /**
   * For a function f, mapping a type A to R, and that can fail, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   * The eventual error L will be wrapped in a [[WrappedError]].
   */
  def wrapE[A: Decoder, L, R](f: A => Either[L, R]): Decoder[R] = (g: DynamicRepr) => g.applyE(f)

  /**
   * For a function f, mapping a type A to maybe R, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   * The absence of value R will be represented by a [[NilSmartConstructorError]].
   */
  def wrapO[A: Decoder, R](f: A => Option[R]): Decoder[R] = (g: DynamicRepr) => g.applyO(f)

  /**
   * For a function f, mapping a type A to R, and that can fail, create an instance of Decode that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decode for type A, enabling decoding of complex types.
   * The eventual throwable error will be wrapped in a [[WrappedError]].
   */
  def wrapT[A: Decoder, R](f: A => Try[R]): Decoder[R] = (g: DynamicRepr) => g.applyT(f)

  /*
   * AnyVal specific instances
   */
  implicit val decodeByte: Decoder[Byte] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Byte => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Byte"))
      }
    case d => Left(IncorrectPath(d, "Byte"))
  }

  implicit val decodeInt: Decoder[Int] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Int    => Right(x)
        case x: Short  => Right(x.toInt)
        case x: String => Try(x.toInt).toEither.left.map(_ => UnexpectedEncodeValue(d, "Int"))
        case _         => Left(UnexpectedEncodeValue(d, "Int"))
      }
    case d => Left(IncorrectPath(d, "Int"))
  }

  implicit val decodeShort: Decoder[Short] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Short  => Right(x)
        case x: String => Try(x.toShort).toEither.left.map(_ => UnexpectedEncodeValue(d, "Short"))
        case _         => Left(UnexpectedEncodeValue(d, "Short"))
      }
    case d => Left(IncorrectPath(d, "Short"))
  }

  implicit val decodeLong: Decoder[Long] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Long   => Right(x)
        case x: Int    => Right(x.toLong)
        case x: Short  => Right(x.toLong)
        case x: String => Try(x.toLong).toEither.left.map(_ => UnexpectedEncodeValue(d, "Long"))
        case _         => Left(UnexpectedEncodeValue(d, "Long"))
      }
    case d => Left(IncorrectPath(d, "Long"))
  }

  implicit val decodeFloat: Decoder[Float] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Float  => Right(x)
        case x: Int    => Right(x.toFloat)
        case x: Short  => Right(x.toFloat)
        case x: Long   => Right(x.toFloat)
        case x: String => Try(x.toFloat).toEither.left.map(_ => UnexpectedEncodeValue(d, "Float"))
        case _         => Left(UnexpectedEncodeValue(d, "Float"))
      }
    case d => Left(IncorrectPath(d, "Float"))
  }

  implicit val decodeChar: Decoder[Char] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Char => Right(x)
        case _       => Left(UnexpectedEncodeValue(d, "Char"))
      }
    case d => Left(IncorrectPath(d, "Char"))
  }

  implicit val decodeBoolean: Decoder[Boolean] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Boolean => Right(x)
        case x: String  => Try(x.toBoolean).toEither.left.map(_ => UnexpectedEncodeValue(d, "Boolean"))
        case _          => Left(UnexpectedEncodeValue(d, "Boolean"))
      }
    case d => Left(IncorrectPath(d, "Boolean"))
  }

  implicit val decodeDouble: Decoder[Double] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Double => Right(x)
        case x: Int    => Right(x.toDouble)
        case x: Short  => Right(x.toDouble)
        case x: Long   => Right(x.toDouble)
        case x: Float  => Right(x.toDouble)
        case x: String => Try(x.toDouble).toEither.left.map(_ => UnexpectedEncodeValue(d, "Double"))
        case _         => Left(UnexpectedEncodeValue(d, "Double"))
      }
    case d => Left(IncorrectPath(d, "Double"))
  }

  implicit val decodeUnit: Decoder[Unit] = {
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

  implicit val decodeBigInt: Decoder[BigInt] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: BigInt => Right(x)
        case x: Int    => Right(BigInt(x))
        case x: Short  => Right(BigInt(x))
        case x: Long   => Right(BigInt(x))
        case x: String => Try(BigInt(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "BigInt"))
        case _         => Left(UnexpectedEncodeValue(d, "BigInt"))
      }
    case d => Left(IncorrectPath(d, "BigInt"))
  }

  implicit val decodeBigDecimal: Decoder[BigDecimal] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: BigDecimal => Right(x)
        case x: Int        => Right(BigDecimal(x))
        case x: Short      => Right(BigDecimal(x))
        case x: Long       => Right(BigDecimal(x))
        case x: Float      => Right(BigDecimal(x))
        case x: Double     => Right(BigDecimal(x))
        case x: BigInt     => Right(BigDecimal(x))
        case x: String     => Try(BigDecimal(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "BigDecimal"))
        case _             => Left(UnexpectedEncodeValue(d, "BigDecimal"))
      }
    case d => Left(IncorrectPath(d, "BigDecimal"))
  }

  implicit val decodeString: Decoder[String] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: String => Right(x)
        case _: Int | _: Short | _: Long | _: Float | _: Double | _: Char | _: Boolean | _: BigInt | _: BigDecimal =>
          Right(a.toString)
        case _: URI | _: URL | _: LocalDate | _: LocalTime | _: LocalDate | _: Instant | _: Duration | _: UUID | _: File =>
          Right(a.toString)
        case _ => Left(UnexpectedEncodeValue(d, "String"))
      }
    case d => Left(IncorrectPath(d, "String"))
  }

  implicit val decodeUri: Decoder[URI] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: URI    => Right(x)
        case x: String => Try(URI.create(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "URI"))
        case _         => Left(UnexpectedEncodeValue(d, "URI"))
      }
    case d => Left(IncorrectPath(d, "URI"))
  }

  implicit val decodeUrl: Decoder[URL] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: URL    => Right(x)
        case x: String => Try(URI.create(x).toURL).toEither.left.map(_ => UnexpectedEncodeValue(d, "URL"))
        case _         => Left(UnexpectedEncodeValue(d, "URL"))
      }
    case d => Left(IncorrectPath(d, "URL"))
  }

  implicit val decodeDuration: Decoder[Duration] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Duration => Right(x)
        case x: String   => Try(Duration(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "Duration"))
        case _           => Left(UnexpectedEncodeValue(d, "Duration"))
      }
    case d => Left(IncorrectPath(d, "Duration"))
  }

  implicit val decodeUuid: Decoder[UUID] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: UUID   => Right(x)
        case x: String => Try(UUID.fromString(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "UUID"))
        case _         => Left(UnexpectedEncodeValue(d, "UUID"))
      }
    case d => Left(IncorrectPath(d, "UUID"))
  }

  implicit val decodeInstant: Decoder[Instant] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: Instant => Right(x)
        case x: String  => Try(Instant.parse(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "Instant"))
        case _          => Left(UnexpectedEncodeValue(d, "Instant"))
      }
    case d => Left(IncorrectPath(d, "Instant"))
  }

  implicit val decodeLocalDate: Decoder[LocalDate] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalDate => Right(x)
        case x: String    => Try(LocalDate.parse(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "LocalDate"))
        case _            => Left(UnexpectedEncodeValue(d, "LocalDate"))
      }
    case d => Left(IncorrectPath(d, "LocalDate"))
  }

  implicit val decodeLocalDateTime: Decoder[LocalDateTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalDateTime => Right(x)
        case x: String        => Try(LocalDateTime.parse(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "LocalDateTime"))
        case _                => Left(UnexpectedEncodeValue(d, "LocalDateTime"))
      }
    case d => Left(IncorrectPath(d, "LocalDateTime"))
  }

  implicit val decodeLocalTime: Decoder[LocalTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: LocalTime => Right(x)
        case x: String    => Try(LocalTime.parse(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "LocalTime"))
        case _            => Left(UnexpectedEncodeValue(d, "LocalTime"))
      }
    case d => Left(IncorrectPath(d, "LocalTime"))
  }

  implicit val decodeOffsetDateTime: Decoder[OffsetDateTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: OffsetDateTime => Right(x)
        case x: String         => Try(OffsetDateTime.parse(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "OffsetDateTime"))
        case _                 => Left(UnexpectedEncodeValue(d, "OffsetDateTime"))
      }
    case d => Left(IncorrectPath(d, "OffsetDateTime"))
  }

  implicit val decodeOffsetTime: Decoder[OffsetTime] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: OffsetTime => Right(x)
        case x: String     => Try(OffsetTime.parse(x)).toEither.left.map(_ => UnexpectedEncodeValue(d, "OffsetTime"))
        case _             => Left(UnexpectedEncodeValue(d, "OffsetTime"))
      }
    case d => Left(IncorrectPath(d, "OffsetTime"))
  }

  implicit val decodeFile: Decoder[File] = {
    case d @ ValueDynamicRepr(a) =>
      a match {
        case x: File                => Right(x)
        case x: String if x != null => Right(new File(x))
        case _                      => Left(UnexpectedEncodeValue(d, "File"))
      }
    case d => Left(IncorrectPath(d, "File"))
  }

  implicit def decodeOption[A: Decoder]: Decoder[Option[A]] = new Decoder[Option[A]] {
    override def from(g: DynamicRepr): Result[Option[A]] = g match {
      case ProductDynamicRepr(_)  => implicitly[Decoder[A]].from(g).map(Some(_))
      case IterableDynamicRepr(_) => implicitly[Decoder[A]].from(g).map(Some(_))
      case ValueDynamicRepr(_)    => implicitly[Decoder[A]].from(g).map(Some(_))
      case NilDynamicRepr         => Right(None)
    }
  }

  implicit def decodeEither[L: Decoder, R: Decoder]: Decoder[Either[L, R]] = new Decoder[Either[L, R]] {
    override def from(g: DynamicRepr): Result[Either[L, R]] = g match {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) =>
        implicitly[Decoder[L]].from(g).map(Left(_)) match {
          case Left(_)      => implicitly[Decoder[R]].from(g).map(Right(_))
          case r @ Right(_) => r
        }
      case d => Left(UnexpectedEncodeValue(d, "Either"))
    }
  }

  implicit def decodeList[A: Decoder]: Decoder[List[A]] = new Decoder[List[A]] {
    override def from(g: DynamicRepr): Result[List[A]] = g match {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) => implicitly[Decoder[A]].from(g).map(List.apply(_))
      case IterableDynamicRepr(items) =>
        items.traverse(implicitly[Decoder[A]].from(_))
      case d => Left(UnexpectedEncodeValue(d, "List"))
    }
  }

  implicit def deriveDecode[T](implicit u: AutoUnlockDecode): Decoder[T] = macro macroDeriveDecode[T]

  def macroDeriveDecode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree): c.Tree = {
    val _ = u
    Magnolia.gen[T](c)
  }

}
