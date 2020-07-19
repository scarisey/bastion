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

class DecodingStateTest extends AnyFlatSpec with Matchers {
  val stateNil: DecodingState      = DecodingState.init(NilDynamicRepr)
  val stateValue: DecodingState    = DecodingState.init(ValueDynamicRepr(42))
  val stateIterable: DecodingState = DecodingState.init(IterableDynamicRepr(List(ValueDynamicRepr(42), ValueDynamicRepr(43))))
  val stateProduct: DecodingState = DecodingState.init(
    DynamicReprEncode
      .encodeMap[Int]
      .to(
        Map.apply[String, Int](
          "aField1" -> 42,
          "aField2" -> 43
        )
      )
  )

  behavior of "DecodingState"

  it should "traverse current dynamicrepr" in {
    stateNil.foreach(_.runDecoder[Int]) shouldEqual stateNil.succeed(List.empty)
    stateValue.foreach(_.runDecoder[Int]) shouldEqual stateValue.succeed(List(42))
    stateIterable.foreach(_.runDecoder[Int]) shouldEqual stateIterable.succeed(List(42, 43))
    stateProduct.foreach(_.runDecoder[Map[String, Int]]) shouldEqual stateProduct.succeed(
      List(Map("aField1" -> 42, "aField2" -> 43))
    )
  }

  it should "raise a specific error when partial function is not defined on the input of collect" in {
    stateValue.collect { case NilDynamicRepr      => Right("toto") } shouldEqual stateValue.fail[String]
    stateValue.collect { case ValueDynamicRepr(a) => Right(a) } shouldEqual stateValue.succeed(42)
  }

}
