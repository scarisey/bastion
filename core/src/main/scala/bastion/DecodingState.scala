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

import scala.util.Try
import scala.language.dynamics

/**
 * Used in the decoder typeclass, it helps to propagate state through decoding, enabling helpful error reports, and simplifying instantiation of a decoder.
 * You will find many usage examples in the [[Decoder]] object.
 * <p><pre><code>
 * val decodeInt: Decoder[Int] = instance { state =>
 *     state.collect {
 *        case ValueDynamicRepr(a) =>
 *          a match {
 *            case x: Int    => state.succeed(x)
 *            case x: Short  => state.attempt(x.toInt)
 *            case x: String => state.attempt(x.toInt)
 *            case _         => state.fail
 *          }
 *     }
 * }
 * </code></pre></p>
 */
final case class DecodingState(aggregatedPath: Path, initialDynamicRepr: DynamicRepr, actualDynamicRepr: DynamicRepr)
    extends Dynamic {
  override def toString: String = s"applying ${aggregatedPath.show} on ${initialDynamicRepr} produces $actualDynamicRepr"

  /**
   * Will terminate decoding with the value t.
   */
  def succeed[T](t: T): Result[T] = Right(t)

  /**
   * Will terminate decoding, raising an error with the current path, type and DynamicRepr being decoded.
   */
  def fail[T]: Result[T] = Left(IncorrectPath(this))

  /**
   * Will terminate decoding, with an error if =>T throw an exception (like with fail[T]), or a successful value otherwise.
   */
  def attempt[T](t: => T): Result[T] = Try(t).fold(_ => this.fail[T], this.succeed)

  /**
   * Browse the attribute "field" of the current DynamicRepr being decoded. Can be chained to browse nested ProductDynamicRepr.
   */
  def selectField(field: String): DecodingState = this.copy(
    aggregatedPath = aggregatedPath.selectDynamic(field),
    actualDynamicRepr = actualDynamicRepr.selectDynamic(field)
  )

  /**
   * Nice scala shortcut to selectField, enabling dynamic invocation.
   */
  def selectDynamic(field: String): DecodingState = selectField(field)

  /**
   * Useful when you guess that the current DynamicRepr is an Iterable. It will loop on each item of an IterableDynamicRepr,
   * accessible through a new decoding state for each item, and will return the monadic Result list of decoded items.
   * It is intended to be used like a traverse function for an Iterable, combining all Results.
   */
  def foreach[A](f: DecodingState => Result[A]): Result[Iterable[A]] =
    actualDynamicRepr.fold(
      fromIterable = items =>
        items.zipWithIndex.traverse {
          case (g, i) =>
            f(
              this.copy(
                aggregatedPath = aggregatedPath.item(i),
                actualDynamicRepr = g
              )
            )
        },
      fromValue = (_: Any) => f(this).map(Iterable(_)),
      fromProduct = (_: Any) => f(this).map(Iterable(_)),
      fromNil = Right(Iterable.empty)
    )

  /**
   * Will invoke an instance of a Decoder[D] on the current DynamicRepr being decoded, as long as this instance is in the implicit scope.
   */
  def runDecoder[D: Decoder]: Result[D] =
    implicitly[Decoder[D]].from(this)

  /**
   * Will apply the partial function f on the current DynamicRepr being decoded.
   */
  def collect[T](f: PartialFunction[DynamicRepr, Result[T]]): Result[T] =
    f.applyOrElse(this.actualDynamicRepr, { _: DynamicRepr => this.fail[T] })

  def fold[R](
    fromNil: => Result[R] = this.fail[R],
    fromValue: actualDynamicRepr.T => Result[R] = (_: actualDynamicRepr.T) => this.fail[R],
    fromProduct: actualDynamicRepr.T => Result[R] = (_: actualDynamicRepr.T) => this.fail[R],
    fromIterable: Iterable[DynamicRepr] => Result[R] = (_: Iterable[DynamicRepr]) => this.fail[R]
  ): Result[R] = actualDynamicRepr.fold(fromNil, fromValue, fromProduct, fromIterable)
}
object DecodingState {
  def init(dynamicRepr: DynamicRepr): DecodingState = DecodingState(Path(), dynamicRepr, dynamicRepr)
}
