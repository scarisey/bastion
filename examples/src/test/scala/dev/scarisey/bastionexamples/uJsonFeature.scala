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
import bastion._
import json._
import derivation.json._
import derivation.decode.auto._

object uJsonFeature extends App {
  case class FooString(foo: String)
  case class NestedFoo(bar: Double, baz: FooString, items: List[String])
  case class RootFoo(foo: NestedFoo)

  val aJson = s"""{
                 |  "foo": {
                 |    "bar": 806,
                 |    "baz": {
                 |      "foo": "foo again, but not the same"
                 |    },
                 |    "items":["first","second","third"]
                 |  }
                 |}""".stripMargin

  val foo = decode[RootFoo](aJson)
  println(foo) //Right(RootFoo(NestedFoo(806.0,FooString(foo again, but not the same),List(first, second, third))))

  val serializedFoo = foo.map(encodeString[RootFoo](_))
  println(
    serializedFoo
  ) //Right({"foo":{"bar":806,"baz":{"foo":"foo again, but not the same"},"items":["first","second","third"]}})
}
