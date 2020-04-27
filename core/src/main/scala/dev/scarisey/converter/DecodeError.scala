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

sealed trait DecodeError {
  def combine(other: DecodeError): DecodeError = (this, other) match {
    case (CumulatedErrors(xs), CumulatedErrors(ys)) => CumulatedErrors(xs ++ ys)
    case (CumulatedErrors(xs), ys)                  => CumulatedErrors(xs :+ ys)
    case (xs, CumulatedErrors(ys))                  => CumulatedErrors(xs :: ys)
    case (xs, ys)                                   => CumulatedErrors(List(xs, ys))
  }
}
final case class IncorrectPathOrType(message: String) extends DecodeError {
  override def toString: String = s"IncorrectPathOrType($message)"
}
case object IncorrectSubtype                                extends DecodeError
case object IncorrectPath                                   extends DecodeError
final case class WrappedError[T](t: T)                      extends DecodeError
final case class CumulatedErrors(errors: List[DecodeError]) extends DecodeError
