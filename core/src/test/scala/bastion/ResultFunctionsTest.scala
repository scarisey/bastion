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

class ResultFunctionsTest extends AnyFlatSpec with Matchers {
  behavior of "traversing an Iterable of Result"
  it should "produce a Result of Iterable when all results are ok" in {
    val xs: List[Result[Int]] = List(Right(1), Right(2), Right(3))

    val actualResults: Result[List[Int]] = xs.traverse(identity)
    actualResults shouldEqual Right(List(1, 2, 3))
  }

  it should "produce Left when one result is ko" in {
    val xs: List[Result[Int]] = List(Right(1), Left(NilSmartConstructorError), Right(3))

    val actualResults: Result[List[Int]] = xs.traverse(identity)
    actualResults shouldEqual Left(NilSmartConstructorError)
  }
}
