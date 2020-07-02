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
import java.time._
import java.util.UUID

import bastion.derivation.decode.AutoUnlockDecode
import bastion.derivation.decode.DecoderDerivation
import magnolia._

import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.Try
import scala.reflect.runtime.universe._

/**
 * Typeclass representing a decoder from a DynamicRepr to a type T.
 * This typeclass works with a state that will propagate through decoding, enabling precise error report.
 * Attempting to decode may fail, resulting in [[DecodeError]].
 */
trait Decoder[T] {
  def from(decodingState: DecodingState): Result[T]

  def collect[U: WeakTypeTag](f: PartialFunction[T, U]): Decoder[U] =
    Decoder.instance(state =>
      this
        .from(state)
        .flatMap(t =>
          if (f.isDefinedAt(t)) Right(f(t)) else Left(PartialFunctionCollectError(implicitly[WeakTypeTag[U]].tpe.toString))
        )
    )

}
object Decoder extends DecoderDerivation {

  /**
   * Create your own typeclass instance with this method.
   */
  def instance[A](f: DecodingState => Result[A]): Decoder[A] = (decodingState: DecodingState) => f(decodingState)

  /**
   * For a function f that maps a type A to R, create an instance of Decoder that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decoder for type A, enabling decoding of complex types.
   */
  def wrap[A: Decoder, R](f: A => R): Decoder[R] = (decodingState: DecodingState) => decodingState.runDecoder[A].map(f)

  /**
   * For a function f that maps a type A to R, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decoder for type A, enabling decoding of complex types.
   * The eventual error L will be wrapped in a [[WrappedError]].
   */
  def wrapE[A: Decoder, L, R](f: A => Either[L, R]): Decoder[R] =
    (decodingState: DecodingState) => decodingState.runDecoder[A].flatMap(it => f(it).left.map(WrappedError(_)))

  /**
   * For a function f that maps a type A to maybe R, create an instance of Decoder that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decoder for type A, enabling decoding of complex types.
   * The absence of value R will be represented by a [[NilSmartConstructorError]].
   */
  def wrapO[A: Decoder, R](f: A => Option[R]): Decoder[R] =
    (decodingState: DecodingState) => decodingState.runDecoder[A].flatMap(it => f(it).toRight(NilSmartConstructorError))

  /**
   * For a function f that maps a type A to R, and that can fail, create an instance of Decoder that will map a [[DynamicRepr]] to R.
   * This method can use your own instance of Decoder for type A, enabling decoding of complex types.
   * The eventual throwable error will be wrapped in a [[WrappedError]].
   */
  def wrapT[A: Decoder, R](f: A => Try[R]): Decoder[R] =
    (decodingState: DecodingState) => decodingState.runDecoder[A].flatMap(it => f(it).toEither.left.map(WrappedError(_)))

  /*
   * AnyVal specific instances
   */
  implicit val decodeByte: Decoder[Byte] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Byte => state.succeed(x)
          case _       => state.fail
        }
    }
  }

  implicit val decodeInt: Decoder[Int] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Int    => state.succeed(x)
          case x: Short  => state.attempt(x.toInt)
          case x: String => state.attempt(x.toInt)
          case _         => state.fail
        }
    }
  }

  implicit val decodeShort: Decoder[Short] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Short  => state.succeed(x)
          case x: String => state.attempt(x.toShort)
          case _         => state.fail
        }
    }
  }

  implicit val decodeLong: Decoder[Long] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Long   => state.succeed(x)
          case x: Int    => state.attempt(x.toLong)
          case x: Short  => state.attempt(x.toLong)
          case x: String => state.attempt(x.toLong)
          case _         => state.fail
        }
    }
  }

  implicit val decodeFloat: Decoder[Float] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Float  => state.succeed(x)
          case x: Int    => state.attempt(x.toFloat)
          case x: Short  => state.attempt(x.toFloat)
          case x: Long   => state.attempt(x.toFloat)
          case x: String => state.attempt(x.toFloat)
          case _         => state.fail
        }
    }
  }

  implicit val decodeChar: Decoder[Char] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Char => state.succeed(x)
          case _       => state.fail
        }
    }
  }

  implicit val decodeBoolean: Decoder[Boolean] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Boolean => state.succeed(x)
          case x: String  => state.attempt(x.toBoolean)
          case _          => state.fail
        }
    }
  }

  implicit val decodeDouble: Decoder[Double] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Double => state.succeed(x)
          case x: Int    => state.attempt(x.toDouble)
          case x: Short  => state.attempt(x.toDouble)
          case x: Long   => state.attempt(x.toDouble)
          case x: Float  => state.attempt(x.toDouble)
          case x: String => state.attempt(x.toDouble)
          case _         => state.fail
        }
    }
  }

  implicit val decodeUnit: Decoder[Unit] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Unit => state.succeed(x)
          case _       => state.fail
        }
    }
  }

  /*
   * Other useful specific instances
   */

  implicit val decodeBigInt: Decoder[BigInt] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: BigInt => state.succeed(x)
          case x: Int    => state.succeed(BigInt(x))
          case x: Short  => state.succeed(BigInt(x))
          case x: Long   => state.succeed(BigInt(x))
          case x: String => state.attempt(BigInt(x))
          case _         => state.fail
        }
    }
  }

  implicit val decodeBigDecimal: Decoder[BigDecimal] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: BigDecimal => state.succeed(x)
          case x: Int        => state.succeed(BigDecimal(x))
          case x: Short      => state.succeed(BigDecimal(x))
          case x: Long       => state.succeed(BigDecimal(x))
          case x: Float      => state.succeed(BigDecimal(x.toDouble))
          case x: Double     => state.succeed(BigDecimal(x))
          case x: BigInt     => state.succeed(BigDecimal(x))
          case x: String     => state.attempt(BigDecimal(x))
          case _             => state.fail
        }
    }
  }

  implicit val decodeString: Decoder[String] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: String => state.succeed(x)
          case _: Int | _: Short | _: Long | _: Float | _: Double | _: Char | _: Boolean | _: BigInt | _: BigDecimal =>
            state.succeed(a.toString)
          case _: URI | _: URL | _: LocalDate | _: LocalTime | _: LocalDate | _: Instant | _: Duration | _: UUID | _: File =>
            state.succeed(a.toString)
          case _ => state.fail
        }
    }
  }

  implicit val decodeUri: Decoder[URI] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: URI    => state.succeed(x)
          case x: String => state.attempt(URI.create(x))
          case _         => state.fail
        }
    }
  }

  implicit val decodeUrl: Decoder[URL] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: URL    => state.succeed(x)
          case x: String => state.attempt(URI.create(x).toURL)
          case _         => state.fail
        }
    }
  }

  implicit val decodeDuration: Decoder[Duration] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Duration => state.succeed(x)
          case x: String   => state.attempt(Duration(x))
          case _           => state.fail
        }
    }
  }

  implicit val decodeUuid: Decoder[UUID] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: UUID   => state.succeed(x)
          case x: String => state.attempt(UUID.fromString(x))
          case _         => state.fail
        }
    }
  }

  implicit val decodeInstant: Decoder[Instant] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: Instant => state.succeed(x)
          case x: String  => state.attempt(Instant.parse(x))
          case _          => state.fail
        }
    }
  }

  implicit val decodeLocalDate: Decoder[LocalDate] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: LocalDate => state.succeed(x)
          case x: String    => state.attempt(LocalDate.parse(x))
          case _            => state.fail
        }
    }
  }

  implicit val decodeLocalDateTime: Decoder[LocalDateTime] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: LocalDateTime => state.succeed(x)
          case x: String        => state.attempt(LocalDateTime.parse(x))
          case _                => state.fail
        }
    }
  }

  implicit val decodeLocalTime: Decoder[LocalTime] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: LocalTime => state.succeed(x)
          case x: String    => state.attempt(LocalTime.parse(x))
          case _            => state.fail
        }
    }
  }

  implicit val decodeOffsetDateTime: Decoder[OffsetDateTime] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: OffsetDateTime => state.succeed(x)
          case x: String         => state.attempt(OffsetDateTime.parse(x))
          case _                 => state.fail
        }
    }
  }

  implicit val decodeOffsetTime: Decoder[OffsetTime] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: OffsetTime => state.succeed(x)
          case x: String     => state.attempt(OffsetTime.parse(x))
          case _             => state.fail
        }
    }
  }

  implicit val decodeFile: Decoder[File] = instance { state =>
    state.collect {
      case ValueDynamicRepr(a) =>
        a match {
          case x: File                => state.succeed(x)
          case x: String if x != null => state.succeed(new File(x))
          case _                      => state.fail
        }
    }
  }

  implicit def decoderOption[A: Decoder]: Decoder[Option[A]] = instance { decodeState =>
    decodeState.collect {
      case ProductDynamicRepr(_) | IterableDynamicRepr(_) | ValueDynamicRepr(_) =>
        decodeState.runDecoder[A] match {
          case Left(_)      => Right(None)
          case Right(value) => Right(Some(value))
        }
      case NilDynamicRepr => decodeState.succeed(None)
    }

  }

  implicit def decoderEither[L: Decoder, R: Decoder]: Decoder[Either[L, R]] = instance { decodeState =>
    decodeState.collect {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) =>
        decodeState.runDecoder[L].map(Left(_)) match {
          case Left(_)      => decodeState.runDecoder[R].map(Right(_))
          case r @ Right(_) => r
        }
    }
  }

  implicit def decoderList[A: Decoder]: Decoder[List[A]] = instance { decodeState =>
    decodeState.collect {
      case ValueDynamicRepr(_) | ProductDynamicRepr(_) =>
        decodeState.runDecoder[A].map(List(_))
      case IterableDynamicRepr(_) =>
        decodeState.foreach(state => state.runDecoder[A]).map(_.toList)
    }
  }

  implicit def deriveDecode[T](implicit u: AutoUnlockDecode): Decoder[T] = macro macroDeriveDecode[T]

  def macroDeriveDecode[T: c.WeakTypeTag](c: whitebox.Context)(u: c.Tree): c.Tree = {
    val _ = u
    Magnolia.gen[T](c)
  }

}
