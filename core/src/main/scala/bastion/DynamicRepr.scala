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

import scala.language.dynamics

/**
 * DynamicRepr is a simplistic representation of data, meant to be inspected at runtime.
 * For data as product, fields are accessed dynamically using scala.language.dynamics.
 */
sealed trait DynamicRepr extends Dynamic { self =>
  type T
  def selectDynamic(field: String): DynamicRepr
  def fold[R](
    fromNil: => Result[R],
    fromValue: T => Result[R],
    fromProduct: T => Result[R],
    fromIterable: Iterable[DynamicRepr] => Result[R]
  ): Result[R]
}
object DynamicRepr {
  val nil: DynamicRepr = NilDynamicRepr
}

/**
 * Data as product of other DynamicRepr is encoded as ProductDynamicRepr.
 * The data it encodes is the public field a.
 */
abstract class ProductDynamicRepr[A](val a: A) extends DynamicRepr {
  override type T = A
  override def toString: String = s"ProductDynamicRepr(${a.toString})"

  override def equals(obj: Any): Boolean = obj match {
    case ProductDynamicRepr(otherA) => a.equals(otherA)
    case _                          => false
  }

  override def fold[R](
    fromNil: => Result[R],
    fromValue: A => Result[R],
    fromProduct: A => Result[R],
    fromIterable: Iterable[DynamicRepr] => Result[R]
  ): Result[R] = fromProduct(a)
}
object ProductDynamicRepr {
  def unapply[A](arg: ProductDynamicRepr[A]): Option[A] = Some(arg.a)
}

/**
 * Data as a collection of DynamicRepr is encoded as IterableDynamicRepr.
 * The data it encodes is the public field items.
 */
final case class IterableDynamicRepr(items: Iterable[DynamicRepr]) extends DynamicRepr {
  override type T = DynamicRepr
  override def selectDynamic(field: String): DynamicRepr = NilDynamicRepr
  override def toString: String                          = s"IterableDynamicRepr(${items.toString})"

  override def fold[R](
    fromNil: => Result[R],
    fromValue: DynamicRepr => Result[R],
    fromProduct: DynamicRepr => Result[R],
    fromIterable: Iterable[DynamicRepr] => Result[R]
  ): Result[R] = fromIterable(items)
}

/**
 * Data as a single value of DynamicRepr is encoded as ValueDynamicRepr.
 * The data it encodes is the public field a. Its selectDynamic method will always return NilDynamicRepr.
 */
final case class ValueDynamicRepr[A](a: A) extends DynamicRepr {
  override type T = A
  override def selectDynamic(field: String): DynamicRepr = NilDynamicRepr
  override def toString: String                          = s"ValueDynamicRepr(${a.toString})"

  override def fold[R](
    fromNil: => Result[R],
    fromValue: A => Result[R],
    fromProduct: A => Result[R],
    fromIterable: Iterable[DynamicRepr] => Result[R]
  ): Result[R] =
    fromValue(a)
}

/**
 * The absence of data is encoded as NilDynamicRepr.
 */
case object NilDynamicRepr extends DynamicRepr {
  override type T = NilDynamicRepr.type
  override def selectDynamic(field: String): DynamicRepr = this
  override def toString: String                          = "NilDynamicRepr"

  override def fold[R](
    fromNil: => Result[R],
    fromValue: this.T => Result[R],
    fromProduct: this.T => Result[R],
    fromIterable: Iterable[DynamicRepr] => Result[R]
  ): Result[R] = fromNil
}

/**
 * Internal class to encode field representation.
 */
case class FieldKeyRepr(s: String) {
  val repr: List[String] = {
    val tokens = StringCases.tokenize(s)
    StringCases.transformations.map(f => f(tokens)) :+ s
  }

  def ===(other: String): Boolean = repr.contains(other)

  override def toString: String = repr.toString()
}
