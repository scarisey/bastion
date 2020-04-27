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

package dev.scarisey.bastion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success
import scala.util.Try

class ProductHelperTest extends AnyFlatSpec with Matchers {
  def sum22[T](wrap: Int => T)(
    field1: Int,
    field2: Int,
    field3: Int,
    field4: Int,
    field5: Int,
    field6: Int,
    field7: Int,
    field8: Int,
    field9: Int,
    field10: Int,
    field11: Int,
    field12: Int,
    field13: Int,
    field14: Int,
    field15: Int,
    field16: Int,
    field17: Int,
    field18: Int,
    field19: Int,
    field20: Int,
    field21: Int,
    field22: Int
  ): T =
    wrap(
      field1 +
        field2 +
        field3 +
        field4 +
        field5 +
        field6 +
        field7 +
        field8 +
        field9 +
        field10 +
        field11 +
        field12 +
        field13 +
        field14 +
        field15 +
        field16 +
        field17 +
        field18 +
        field19 +
        field20 +
        field21 +
        field22
    )
  val tuple22 = (
    ValueDynamicRepr(1),
    ValueDynamicRepr(2),
    ValueDynamicRepr(3),
    ValueDynamicRepr(4),
    ValueDynamicRepr(5),
    ValueDynamicRepr(6),
    ValueDynamicRepr(7),
    ValueDynamicRepr(8),
    ValueDynamicRepr(9),
    ValueDynamicRepr(10),
    ValueDynamicRepr(11),
    ValueDynamicRepr(12),
    ValueDynamicRepr(13),
    ValueDynamicRepr(14),
    ValueDynamicRepr(15),
    ValueDynamicRepr(16),
    ValueDynamicRepr(17),
    ValueDynamicRepr(18),
    ValueDynamicRepr(19),
    ValueDynamicRepr(20),
    ValueDynamicRepr(21),
    ValueDynamicRepr(22)
  )

  it should "apply 22 DynamicRepr arguments to a total function" in {
    val f = sum22[Int](x => x) _
    tuple22.apply(f) shouldEqual Right(253)
  }

  it should "apply 22 DynamicRepr arguments to a function returning Option" in {
    val f = sum22[Option[Int]](x => Some(x)) _
    tuple22.applyO(f) shouldEqual Right(253)
  }

  it should "apply 22 DynamicRepr arguments to a function returning Either" in {
    val f = sum22[Either[String, Int]](x => Right(x)) _
    tuple22.applyE(f) shouldEqual Right(253)
  }

  it should "apply 22 DynamicRepr arguments to a function returning Try" in {
    val f = sum22[Try[Int]](x => Try(x)) _
    tuple22.applyT(f) shouldEqual Right(253)
  }

  it should "apply one DynamicRepr arguments to a total function" in {
    ValueDynamicRepr(42).apply[Int, Int](_ + 2) shouldEqual Right(44)
  }

  it should "apply one DynamicRepr arguments to a function returning Option" in {
    ValueDynamicRepr(42).applyO[Int, Int](x => Some(x + 2)) shouldEqual Right(44)
  }

  it should "apply one DynamicRepr arguments to a function returning Either" in {
    ValueDynamicRepr(42).applyE[Int, String, Int](x => Right(x + 2)) shouldEqual Right(44)
    ValueDynamicRepr(42).applyE[Int, String, Int](x => Left("an error")) shouldEqual Left(WrappedError("an error"))
  }

  it should "apply one DynamicRepr arguments to a function returning Try" in {
    ValueDynamicRepr(42).applyT[Int, Int](x => Success(x + 2)) shouldEqual Right(44)
  }
}
