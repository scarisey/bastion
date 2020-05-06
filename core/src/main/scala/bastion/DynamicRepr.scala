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
  def selectDynamic(field: String): DynamicRepr

  /**
   * Combinator with the semantic we expect for 'or' : (a ||| b) will return a if a is not [[NilDynamicRepr]], else b.
   */
  def `|||`(other: DynamicRepr): DynamicRepr
}

/**
 * Data as product of other DynamicRepr is encoded as ProductDynamicRepr.
 * The data it encodes is the public field a.
 */
abstract class ProductDynamicRepr[A](val a: A) extends DynamicRepr {
  override def toString: String                       = s"ProductDynamicRepr(${a.toString})"
  override def `|||`(other: DynamicRepr): DynamicRepr = this
}
object ProductDynamicRepr {
  def unapply[A](arg: ProductDynamicRepr[A]): Option[A] = Some(arg.a)
}

/**
 * Data as a collection of DynamicRepr is encoded as IterableDynamicRepr.
 * The data it encodes is the public field items.
 */
final case class IterableDynamicRepr[A](items: Iterable[DynamicRepr]) extends DynamicRepr {
  override def selectDynamic(field: String): DynamicRepr = NilDynamicRepr
  override def toString: String                          = s"IterableDynamicRepr(${items.toString})"
  override def `|||`(other: DynamicRepr): DynamicRepr    = this
}

/**
 * Data as a single value of DynamicRepr is encoded as ValueDynamicRepr.
 * The data it encodes is the public field a. Its selectDynamic method will always return NilDynamicRepr.
 */
final case class ValueDynamicRepr[A](a: A) extends DynamicRepr {
  override def selectDynamic(field: String): DynamicRepr = NilDynamicRepr
  override def toString: String                          = s"ValueDynamicRepr(${a.toString})"
  override def `|||`(other: DynamicRepr): DynamicRepr    = this
}

/**
 * The absence of data is encoded as NilDynamicRepr.
 */
case object NilDynamicRepr extends DynamicRepr {
  override def selectDynamic(field: String): DynamicRepr = this
  override def toString: String                          = "NilDynamicRepr"
  override def `|||`(other: DynamicRepr): DynamicRepr    = other
}

/**
 * Internal class to encode field representation.
 */
case class FieldKeyRepr(s: String) {
  val repr: List[String] = {
    Logger.debug(s"FieldRepr for ${s}")
    val tokens = StringCases.tokenize(s)
    StringCases.transformations.map(f => f(tokens)) :+ s
  }

  def ===(other: String): Boolean = repr.contains(other)

  override def toString: String = repr.toString()
}
