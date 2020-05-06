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
  behavior of "DynamicRepr"
  it should "select left side or right side" in {
    val valueRepr          = ValueDynamicRepr(42)
    val productRepr        = new ProductDynamicRepr(42) { override def selectDynamic(field: String): DynamicRepr = ??? }
    val iterableDynRepr    = IterableDynamicRepr(List(valueRepr))
    val other: DynamicRepr = ValueDynamicRepr(33)

    (valueRepr ||| other) shouldEqual valueRepr
    (productRepr ||| other) shouldEqual productRepr
    (iterableDynRepr ||| other) shouldEqual iterableDynRepr
    (NilDynamicRepr ||| other) shouldEqual other
  }

}
