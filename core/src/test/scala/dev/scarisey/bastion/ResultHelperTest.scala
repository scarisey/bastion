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

class ResultHelperTest extends AnyFlatSpec with Matchers {
  val tuple22 = (
    Right[DecodeError, Int](1),
    Right[DecodeError, Int](2),
    Right[DecodeError, Int](3),
    Right[DecodeError, Int](4),
    Right[DecodeError, Int](5),
    Right[DecodeError, Int](6),
    Right[DecodeError, Int](7),
    Right[DecodeError, Int](8),
    Right[DecodeError, Int](9),
    Right[DecodeError, Int](10),
    Right[DecodeError, Int](11),
    Right[DecodeError, Int](12),
    Right[DecodeError, Int](13),
    Right[DecodeError, Int](14),
    Right[DecodeError, Int](15),
    Right[DecodeError, Int](16),
    Right[DecodeError, Int](17),
    Right[DecodeError, Int](18),
    Right[DecodeError, Int](19),
    Right[DecodeError, Int](20),
    Right[DecodeError, Int](21),
    Right[DecodeError, Int](22)
  )

  val tuple22OneFail = (
    Right[DecodeError, Int](1),
    Right[DecodeError, Int](2),
    Right[DecodeError, Int](3),
    Right[DecodeError, Int](4),
    Right[DecodeError, Int](5),
    Right[DecodeError, Int](6),
    Right[DecodeError, Int](7),
    Right[DecodeError, Int](8),
    Right[DecodeError, Int](9),
    Right[DecodeError, Int](10),
    Right[DecodeError, Int](11),
    Right[DecodeError, Int](12),
    Right[DecodeError, Int](13),
    Right[DecodeError, Int](14),
    Right[DecodeError, Int](15),
    Right[DecodeError, Int](16),
    Right[DecodeError, Int](17),
    Right[DecodeError, Int](18),
    Right[DecodeError, Int](19),
    Right[DecodeError, Int](20),
    Right[DecodeError, Int](21),
    Left(WrappedError("an error"))
  )

  val tuple22TwoFails = (
    Right[DecodeError, Int](1),
    Right[DecodeError, Int](2),
    Right[DecodeError, Int](3),
    Right[DecodeError, Int](4),
    Right[DecodeError, Int](5),
    Right[DecodeError, Int](6),
    Right[DecodeError, Int](7),
    Right[DecodeError, Int](8),
    Right[DecodeError, Int](9),
    Right[DecodeError, Int](10),
    Right[DecodeError, Int](11),
    Right[DecodeError, Int](12),
    Right[DecodeError, Int](13),
    Right[DecodeError, Int](14),
    Right[DecodeError, Int](15),
    Right[DecodeError, Int](16),
    Right[DecodeError, Int](17),
    Left(WrappedError("an error")),
    Right[DecodeError, Int](19),
    Right[DecodeError, Int](20),
    Right[DecodeError, Int](21),
    Left(WrappedError("an error"))
  )

  val f: (
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int],
    Result[Int]
  ) => Result[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] =
    ResultHelper.product22 _

  it should "combine 22 ok results" in {
    f.tupled(tuple22) shouldEqual Right((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
  }

  it should "combine 22 results and fail" in {
    f.tupled(tuple22TwoFails) shouldEqual Left(CumulatedErrors(List(WrappedError("an error"), WrappedError("an error"))))
  }
}
