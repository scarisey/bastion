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

final case class DecodingState(aggregatedPath: Path, initialDynamicRepr: DynamicRepr, actualDynamicRepr: DynamicRepr)
    extends Dynamic {
  override def toString: String = s"applying ${aggregatedPath.show} on ${initialDynamicRepr} produces $actualDynamicRepr"

  def succeed[T](t: T): Result[T]    = Right(t)
  def fail[T]: Result[T]             = Left(IncorrectPath(actualDynamicRepr, this.toString))
  def attempt[T](t: => T): Result[T] = Try(t).fold(_ => this.fail[T], this.succeed)
  def selectField(field: String): DecodingState = this.copy(
    aggregatedPath = aggregatedPath.selectDynamic(field),
    actualDynamicRepr = actualDynamicRepr.selectDynamic(field)
  )
  def selectDynamic(field: String): DecodingState = selectField(field)
  def foreach[A](f: DecodingState => Result[A]): Iterable[Result[A]] =
    actualDynamicRepr match {
      case IterableDynamicRepr(items) =>
        items.zipWithIndex.map({
          case (g, i) =>
            f(
              this.copy(
                aggregatedPath = aggregatedPath.item(i),
                actualDynamicRepr = g
              )
            )
        })
      case _ => Iterable(f(this))
    }

  def runDecoder[D: Decoder]: Result[D] =
    implicitly[Decoder[D]].from(this)

  def collect[T](f: PartialFunction[DynamicRepr, Result[T]]): Result[T] =
    f.applyOrElse(this.actualDynamicRepr, { _: DynamicRepr => this.fail[T] })

}
object DecodingState {
  def init(dynamicRepr: DynamicRepr): DecodingState = DecodingState(Path(), dynamicRepr, dynamicRepr)
}
