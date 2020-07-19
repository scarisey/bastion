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

class TraverseTest extends AnyFlatSpec with Matchers {

  behavior of "traverse for Result"

  it should "traverse an Iterable of Results" in {
    val results = List(Right(1), Right(2), Right(3), Right(4)).zipWithIndex.traverse { case (v, i) => v.map(x => (x + 1, i)) }

    results shouldEqual Right(List((2, 0), (3, 1), (4, 2), (5, 3)))
  }

}
