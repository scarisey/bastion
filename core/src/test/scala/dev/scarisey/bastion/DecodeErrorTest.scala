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

class DecodeErrorTest extends AnyFlatSpec with Matchers {
  val d = ValueDynamicRepr(42)
  it should "combine errors" in {
    CumulatedErrors(List(UnexpectedEncodeValue(d, "foo"))).combine(UnexpectedEncodeValue(d, "bar")) shouldEqual CumulatedErrors(
      List(UnexpectedEncodeValue(d, "foo"), UnexpectedEncodeValue(d, "bar"))
    )
    UnexpectedEncodeValue(d, "foo").combine(IncorrectSubtype) shouldEqual CumulatedErrors(
      List(UnexpectedEncodeValue(d, "foo"), IncorrectSubtype)
    )
    UnexpectedEncodeValue(d, "foo").combine(CumulatedErrors(List(IncorrectSubtype))) shouldEqual CumulatedErrors(
      List(UnexpectedEncodeValue(d, "foo"), IncorrectSubtype)
    )
    CumulatedErrors(List(UnexpectedEncodeValue(d, "foo")))
      .combine(CumulatedErrors(List(IncorrectSubtype))) shouldEqual CumulatedErrors(
      List(UnexpectedEncodeValue(d, "foo"), IncorrectSubtype)
    )

  }
}
