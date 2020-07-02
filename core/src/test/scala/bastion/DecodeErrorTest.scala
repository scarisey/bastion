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

class DecodeErrorTest extends AnyFlatSpec with Matchers {
  val d              = ValueDynamicRepr(42)
  val decodingState  = DecodingState.init(d)
  val decodingState2 = DecodingState.init(NilDynamicRepr)
  it should "combine errors" in {
    CumulatedErrors(List(IncorrectPath(decodingState))).combine(IncorrectPath(decodingState2)) shouldEqual CumulatedErrors(
      List(IncorrectPath(decodingState), IncorrectPath(decodingState2))
    )
    IncorrectPath(decodingState).combine(IncorrectSubtype("", d)) shouldEqual CumulatedErrors(
      List(IncorrectPath(decodingState), IncorrectSubtype("", d))
    )
    IncorrectPath(decodingState).combine(CumulatedErrors(List(IncorrectSubtype("", d)))) shouldEqual CumulatedErrors(
      List(IncorrectPath(decodingState), IncorrectSubtype("", d))
    )
    CumulatedErrors(List(IncorrectPath(decodingState)))
      .combine(CumulatedErrors(List(IncorrectSubtype("", d)))) shouldEqual CumulatedErrors(
      List(IncorrectPath(decodingState), IncorrectSubtype("", d))
    )

  }
}
