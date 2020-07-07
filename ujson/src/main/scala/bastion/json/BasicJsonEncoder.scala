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

package bastion.json

import java.io.File
import java.net.URI
import java.net.URL
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.util.UUID

import bastion.json.BasicJsonEncoder.macroDeriveBasicJsonEncoder
import bastion.json.auto.JsonEncoderAutoDerivation
import magnolia._

import scala.annotation.switch
import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait BasicJsonEncoder[-T] {
  def write(t: T): String
}
object BasicJsonEncoder extends BasicJsonEncoderDerivation with LowPriorityBasicJsonEncoder {
  val Null  = "null"
  val True  = "true"
  val False = "false"

  def string(s: String) = {
    val sb = new StringBuilder
    sb.append('"')
    s.foreach(c => escapeChar(sb, c))
    sb.append('"')
    sb.mkString
  }
  def string(c: Char) = {
    val sb = new StringBuilder
    sb.append('"')
    escapeChar(sb, c)
    sb.append('"')
    sb.mkString
  }
  def keyValue(sb: StringBuilder, k: String, v: String) = sb.append('"').append(k).append('"').append(':').append(v)

  def escapeChar(sb: StringBuilder, c: Char): StringBuilder =
    (c: @switch) match {
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c =>
        if (c < ' ' || (c > '~')) {
          sb.append("\\u")
            .append(toHex((c >> 12) & 15))
            .append(toHex((c >> 8) & 15))
            .append(toHex((c >> 4) & 15))
            .append(toHex(c & 15))
        } else sb.append(c)
    }

  def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar

  /*
  Any val implicits
   */
  implicit val encodeJsonByte: BasicJsonEncoder[Byte]       = (a: Byte) => a.toString
  implicit val encodeJsonInt: BasicJsonEncoder[Int]         = (a: Int) => a.toString
  implicit val encodeJsonShort: BasicJsonEncoder[Short]     = (a: Short) => a.toString
  implicit val encodeJsonLong: BasicJsonEncoder[Long]       = (a: Long) => a.toString
  implicit val encodeJsonFloat: BasicJsonEncoder[Float]     = (a: Float) => a.toString
  implicit val encodeJsonChar: BasicJsonEncoder[Char]       = (a: Char) => string(a)
  implicit val encodeJsonBoolean: BasicJsonEncoder[Boolean] = (a: Boolean) => a.toString
  implicit val encodeJsonDouble: BasicJsonEncoder[Double]   = (a: Double) => a.toString
  implicit val encodeJsonUnit: BasicJsonEncoder[Unit]       = _ => Null

  /*
  Other basic types
   */
  implicit val encodeJsonString: BasicJsonEncoder[String]               = (a: String) => string(a)
  implicit val encodeJsonBigInt: BasicJsonEncoder[BigInt]               = (a: BigInt) => a.underlying().toString(10)
  implicit val encodeJsonBigDecimal: BasicJsonEncoder[BigDecimal]       = (a: BigDecimal) => a.underlying().toString
  implicit val encodeJsonUri: BasicJsonEncoder[URI]                     = (a: URI) => string(a.toString)
  implicit val encodeJsonUrl: BasicJsonEncoder[URL]                     = (a: URL) => string(a.toString)
  implicit val encodeJsonDuration: BasicJsonEncoder[Duration]           = (a: Duration) => string(a.toString)
  implicit val encodeJsonUUID: BasicJsonEncoder[UUID]                   = (a: UUID) => string(a.toString)
  implicit val encodeJsonInstant: BasicJsonEncoder[Instant]             = (a: Instant) => string(a.toString)
  implicit val encodeJsonLocalDate: BasicJsonEncoder[LocalDate]         = (a: LocalDate) => string(a.toString)
  implicit val encodeJsonLocalTime: BasicJsonEncoder[LocalTime]         = (a: LocalTime) => string(a.toString)
  implicit val encodeJsonLocalDateTime: BasicJsonEncoder[LocalDateTime] = (a: LocalDateTime) => string(a.toString)
  implicit val encodeJsonFile: BasicJsonEncoder[File]                   = (a: File) => string(a.toString)

  implicit def encodeJsonOption[A: BasicJsonEncoder]: BasicJsonEncoder[Option[A]] = {
    case Some(value) => implicitly[BasicJsonEncoder[A]].write(value)
    case None        => Null
  }

  implicit def encodeJsonEither[L: BasicJsonEncoder, R: BasicJsonEncoder]: BasicJsonEncoder[Either[L, R]] = {
    case Left(value)  => implicitly[BasicJsonEncoder[L]].write(value)
    case Right(value) => implicitly[BasicJsonEncoder[R]].write(value)
  }

  implicit def encodeJsonMap[V: BasicJsonEncoder]: BasicJsonEncoder[Map[String, V]] =
    (a: Map[String, V]) => {
      val sb = new StringBuilder
      sb.append('{')
      var first = true
      a.foreach({
        case (k, v) => {
          if (first)
            first = false
          else
            sb.append(',')
          keyValue(sb, k, implicitly[BasicJsonEncoder[V]].write(v))
        }
      })
      sb.append('}')
      sb.toString()
    }

  def macroDeriveBasicJsonEncoder[T: c.WeakTypeTag](c: whitebox.Context)(auto: c.Tree): c.Tree = {
    val _ = auto
    Magnolia.gen[T](c)
  }

}

trait LowPriorityBasicJsonEncoder {
  implicit def encodeJsonIterable[A: BasicJsonEncoder]: BasicJsonEncoder[Iterable[A]] =
    (a: Iterable[A]) => a.map(implicitly[BasicJsonEncoder[A]].write(_)).mkString("[", ",", "]")

  implicit def encodeArray[A: BasicJsonEncoder]: BasicJsonEncoder[Array[A]] =
    (a: Array[A]) => {
      val sb = new StringBuilder
      sb.append('[')
      var i      = 0
      val length = a.length
      while (i < length) {
        if (i != 0) sb.append(',')
        sb.append(implicitly[BasicJsonEncoder[A]].write(a.apply(i)))
        i += 1
      }
      sb.append(']')
      sb.toString()
    }
}

trait BasicJsonEncoderDerivation {
  type Typeclass[T] = BasicJsonEncoder[T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] =
    if (ctx.isObject) {
      new Typeclass[T] {
        override def write(t: T): String = BasicJsonEncoder.string(ctx.typeName.short)
      }
    } else {
      new Typeclass[T] {
        override def write(t: T): String = {
          val sb = new StringBuilder
          sb.append('{')
          var first = true
          ctx.parameters.foreach { p =>
            if (first)
              first = false
            else
              sb.append(',')
            BasicJsonEncoder.keyValue(sb, p.label, p.typeclass.write(p.dereference(t)))
          }
          sb.append('}')
          sb.toString()
        }
      }
    }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def write(t: T): String = ctx.dispatch(t)(sub => sub.typeclass.write(sub.cast(t)))
  }

  implicit def deriveBasicJsonEncoder[T](implicit auto: JsonEncoderAutoDerivation): BasicJsonEncoder[T] =
    macro macroDeriveBasicJsonEncoder[T]
}
