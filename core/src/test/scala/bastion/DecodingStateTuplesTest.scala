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

import scala.util.Try

class DecodingStateTuplesTest extends AnyFlatSpec with Matchers {
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
    DecodingState.init(ValueDynamicRepr(1)),
    DecodingState.init(ValueDynamicRepr(2)),
    DecodingState.init(ValueDynamicRepr(3)),
    DecodingState.init(ValueDynamicRepr(4)),
    DecodingState.init(ValueDynamicRepr(5)),
    DecodingState.init(ValueDynamicRepr(6)),
    DecodingState.init(ValueDynamicRepr(7)),
    DecodingState.init(ValueDynamicRepr(8)),
    DecodingState.init(ValueDynamicRepr(9)),
    DecodingState.init(ValueDynamicRepr(10)),
    DecodingState.init(ValueDynamicRepr(11)),
    DecodingState.init(ValueDynamicRepr(12)),
    DecodingState.init(ValueDynamicRepr(13)),
    DecodingState.init(ValueDynamicRepr(14)),
    DecodingState.init(ValueDynamicRepr(15)),
    DecodingState.init(ValueDynamicRepr(16)),
    DecodingState.init(ValueDynamicRepr(17)),
    DecodingState.init(ValueDynamicRepr(18)),
    DecodingState.init(ValueDynamicRepr(19)),
    DecodingState.init(ValueDynamicRepr(20)),
    DecodingState.init(ValueDynamicRepr(21)),
    DecodingState.init(ValueDynamicRepr(22))
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

  it should "apply one DynamicRepr argument to a total function" in {
    DecodingState.init(ValueDynamicRepr(42)).apply[Int, Int](_ + 1) shouldEqual Right(43)
  }

  it should "apply one DynamicRepr argument to a function returnin Option" in {
    DecodingState.init(ValueDynamicRepr(42)).applyO[Int, Int](x => Some(x + 1)) shouldEqual Right(43)
  }

  it should "apply one DynamicRepr argument to a function returning Either" in {
    DecodingState.init(ValueDynamicRepr(42)).applyE[Int, Int, Int](x => Right(x + 1)) shouldEqual Right(43)
  }

  it should "apply one DynamicRepr argument to a function returning Try" in {
    DecodingState.init(ValueDynamicRepr(42)).applyT[Int, Int](x => Try(x + 1)) shouldEqual Right(43)
  }
}
