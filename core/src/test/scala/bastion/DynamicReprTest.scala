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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DynamicReprTest extends AnyFlatSpec with Matchers {
  val left                           = Left(WrappedError("ko"))
  val wrongRight                     = Right("ko")
  def wrongRightFunc[T]              = (_: T) => Right("ko")
  def leftFunc[T, R]: T => Result[R] = (_: T) => left

  behavior of "DynamicRepr"

  it should "fold using appropriate functions" in {
    NilDynamicRepr.fold(fromNil = Right("ok"), fromValue = leftFunc, fromIterable = leftFunc, fromProduct = leftFunc) shouldEqual Right(
      "ok"
    )
    NilDynamicRepr.fold(fromNil = left, fromValue = wrongRightFunc, fromIterable = wrongRightFunc, fromProduct = wrongRightFunc) shouldEqual left

    ValueDynamicRepr(42).fold(
      fromNil = left,
      fromValue = (x: Int) => Right(x + 1),
      fromIterable = leftFunc,
      fromProduct = leftFunc
    ) shouldEqual Right(43)

    ValueDynamicRepr(42).fold(
      fromNil = wrongRight,
      fromValue = leftFunc,
      fromIterable = wrongRightFunc,
      fromProduct = wrongRightFunc
    ) shouldEqual left

    new ProductDynamicRepr[Double](42.0) {
      override def selectDynamic(field: String): DynamicRepr = ???
    }.fold(fromNil = left, fromValue = leftFunc, fromProduct = (d: Double) => Right(d + 1), fromIterable = leftFunc) shouldEqual Right(
      43.0
    )

    new ProductDynamicRepr[Double](42.0) {
      override def selectDynamic(field: String): DynamicRepr = ???
    }.fold(fromNil = wrongRight, fromValue = wrongRightFunc, fromProduct = leftFunc, fromIterable = wrongRightFunc) shouldEqual left

    IterableDynamicRepr(List(ValueDynamicRepr(true), ValueDynamicRepr(false))).fold(
      fromNil = left,
      fromProduct = leftFunc,
      fromValue = leftFunc,
      fromIterable = _ => Right(true)
    ) shouldEqual Right(true)

    IterableDynamicRepr(List(ValueDynamicRepr(true), ValueDynamicRepr(false))).fold(
      fromNil = wrongRight,
      fromProduct = wrongRightFunc,
      fromValue = wrongRightFunc,
      fromIterable = leftFunc
    ) shouldEqual left
  }
}
