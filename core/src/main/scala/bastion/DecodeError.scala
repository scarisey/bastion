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

/**
 * Mapping of type A to type B may result in errors, represented as DecodeError.
 */
sealed trait DecodeError {

  /**
   * Combine two DecodeError to a CumulatedErrors (subtype of DecodeError).
   */
  def combine(other: DecodeError): DecodeError = (this, other) match {
    case (CumulatedErrors(xs), CumulatedErrors(ys)) => CumulatedErrors(xs ++ ys)
    case (CumulatedErrors(xs), ys)                  => CumulatedErrors(xs :+ ys)
    case (xs, CumulatedErrors(ys))                  => CumulatedErrors(xs :: ys)
    case (xs, ys)                                   => CumulatedErrors(List(xs, ys))
  }
}

/**
 * This error may occur when decoding a DynamicRepr to an ADT, and no subtype suits.
 */
case class IncorrectSubtype(typeTag: String, d: DynamicRepr) extends DecodeError {
  override def toString: String = s"No matching subtypes of ${typeTag} for ${d.toString}"
}

/**
 * This error may occur when you transform an existing decoder with collect method and the function you have given does not handle all cases.
 *
 */
case class PartialFunctionCollectError(typeTag: String) extends DecodeError {
  override def toString: String = s"Match error : transforming Decoder[$typeTag] with collect failed"
}

/**
 * This error may occur when attempting to select an incorrect field on a DynamicRepr.
 */
final case class IncorrectPath(decodingState: DecodingState) extends DecodeError {
  override def toString: String = s"IncorrectPath: ${decodingState.toString}"
}

/**
 * This error encapsulate an error T into a DecodeError. It is used in [[DynamicReprTuples]] with all the 'apply' methods.
 */
final case class WrappedError[T](t: T) extends DecodeError

/**
 * Represents an accumulation of errors.
 */
final case class CumulatedErrors(errors: List[DecodeError]) extends DecodeError

/**
 * This error is used in [[DynamicReprTuples]] when encapsulating a smart constructor with 'applyO' into a [[Decoder]].
 */
case object NilSmartConstructorError extends DecodeError
