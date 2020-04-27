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
  it should "combine errors" in {
    CumulatedErrors(List(IncorrectPathOrType("foo"))).combine(IncorrectPathOrType("bar")) shouldEqual CumulatedErrors(
      List(IncorrectPathOrType("foo"), IncorrectPathOrType("bar"))
    )
    IncorrectPathOrType("foo").combine(IncorrectSubtype) shouldEqual CumulatedErrors(
      List(IncorrectPathOrType("foo"), IncorrectSubtype)
    )
    IncorrectPathOrType("foo").combine(CumulatedErrors(List(IncorrectSubtype))) shouldEqual CumulatedErrors(
      List(IncorrectPathOrType("foo"), IncorrectSubtype)
    )
    CumulatedErrors(List(IncorrectPathOrType("foo")))
      .combine(CumulatedErrors(List(IncorrectSubtype))) shouldEqual CumulatedErrors(
      List(IncorrectPathOrType("foo"), IncorrectSubtype)
    )

  }
}
