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

import magnolia.Magnolia
import magnolia.ReadOnlyCaseClass
import magnolia.SealedTrait
import ujson.Value

import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import auto.JsonEncoderAutoDerivation
import JsonEncoder.macroDeriveJsonEncoder

trait JsonEncoder[-T] {
  def write(t: T): Value
}
object JsonEncoder extends JsonEncoderDerivation with LowPriorityJsonEncoder {

  /*
  Any val implicits
   */
  implicit val encodeJsonByte: JsonEncoder[Byte]       = (a: Byte) => ujson.Num(a)
  implicit val encodeJsonInt: JsonEncoder[Int]         = (a: Int) => ujson.Num(a)
  implicit val encodeJsonShort: JsonEncoder[Short]     = (a: Short) => ujson.Num(a)
  implicit val encodeJsonLong: JsonEncoder[Long]       = (a: Long) => ujson.Num(a)
  implicit val encodeJsonFloat: JsonEncoder[Float]     = (a: Float) => ujson.Num(a)
  implicit val encodeJsonChar: JsonEncoder[Char]       = (a: Char) => ujson.Str(a.toString)
  implicit val encodeJsonBoolean: JsonEncoder[Boolean] = (a: Boolean) => ujson.Bool(a)
  implicit val encodeJsonDouble: JsonEncoder[Double]   = (a: Double) => ujson.Num(a)
  implicit val encodeJsonUnit: JsonEncoder[Unit]       = _ => ujson.Null

  /*
  Other basic types
   */
  implicit val encodeJsonString: JsonEncoder[String]               = (a: String) => ujson.Str(a)
  implicit val encodeJsonBigInt: JsonEncoder[BigInt]               = (a: BigInt) => ujson.Num(a.toDouble)
  implicit val encodeJsonBigDecimal: JsonEncoder[BigDecimal]       = (a: BigDecimal) => ujson.Num(a.toDouble)
  implicit val encodeJsonUri: JsonEncoder[URI]                     = (a: URI) => ujson.Str(a.toString)
  implicit val encodeJsonUrl: JsonEncoder[URL]                     = (a: URL) => ujson.Str(a.toString)
  implicit val encodeJsonDuration: JsonEncoder[Duration]           = (a: Duration) => ujson.Str(a.toString)
  implicit val encodeJsonUUID: JsonEncoder[UUID]                   = (a: UUID) => ujson.Str(a.toString)
  implicit val encodeJsonInstant: JsonEncoder[Instant]             = (a: Instant) => ujson.Str(a.toString)
  implicit val encodeJsonLocalDate: JsonEncoder[LocalDate]         = (a: LocalDate) => ujson.Str(a.toString)
  implicit val encodeJsonLocalTime: JsonEncoder[LocalTime]         = (a: LocalTime) => ujson.Str(a.toString)
  implicit val encodeJsonLocalDateTime: JsonEncoder[LocalDateTime] = (a: LocalDateTime) => ujson.Str(a.toString)
  implicit val encodeJsonFile: JsonEncoder[File]                   = (a: File) => ujson.Str(a.toString)

  implicit def encodeJsonOption[A: JsonEncoder]: JsonEncoder[Option[A]] = {
    case Some(value) => implicitly[JsonEncoder[A]].write(value)
    case None        => ujson.Null
  }

  implicit def encodeJsonEither[L: JsonEncoder, R: JsonEncoder]: JsonEncoder[Either[L, R]] = {
    case Left(value)  => implicitly[JsonEncoder[L]].write(value)
    case Right(value) => implicitly[JsonEncoder[R]].write(value)
  }

  implicit def encodeJsonMap[V: JsonEncoder]: JsonEncoder[Map[String, V]] =
    (a: Map[String, V]) => {
      val map = a.map({ case (k, v) => (k, implicitly[JsonEncoder[V]].write(v)) }).toSeq
      ujson.Obj(collection.mutable.LinkedHashMap(map: _*))
    }

  def macroDeriveJsonEncoder[T: c.WeakTypeTag](c: whitebox.Context)(auto: c.Tree): c.Tree = {
    val _ = auto
    Magnolia.gen[T](c)
  }
}

trait LowPriorityJsonEncoder {
  implicit def encodeJsonIterable[A: JsonEncoder]: JsonEncoder[Iterable[A]] =
    (a: Iterable[A]) => ujson.Arr.from(a.map(implicitly[JsonEncoder[A]].write(_)))
}

trait JsonEncoderDerivation {
  type Typeclass[T] = JsonEncoder[T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] =
    if (ctx.isObject) {
      new Typeclass[T] {
        override def write(t: T): Value = ujson.Str(ctx.typeName.short)
      }
    } else {
      new Typeclass[T] {
        override def write(t: T): Value = {
          val map: Seq[(String, Value)] = ctx.parameters.map(p => (p.label, p.typeclass.write(p.dereference(t))))
          ujson.Obj(collection.mutable.LinkedHashMap(map: _*))
        }
      }
    }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def write(t: T): Value = ctx.dispatch(t)(sub => sub.typeclass.write(sub.cast(t)))
  }

  implicit def deriveJsonEncoder[T](implicit auto: JsonEncoderAutoDerivation): JsonEncoder[T] = macro macroDeriveJsonEncoder[T]
}
