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

package dev.scarisey.bastionexamples

import java.time.LocalDateTime

import bastion._
import derivation.dynamicrepr.auto._
import derivation.decode.auto._

object DefaultConversions extends App {
  case class A(aLocalDateTime: String, aDouble: String, anInteger: String)
  case class B(aLocalDateTime: LocalDateTime, aDouble: Double, anInteger: Int)

  println(
    A("2020-06-04T13:45:00.000", "3.14", "42").convert[B]
  )

  case class SomeLong(aField: Long)
  case class SomeFloat(aField: Float)

  println(
    SomeLong(Long.MaxValue).convert[SomeFloat]
  )
}
