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
import ujson._
import bastion.json._
import bastion.derivation.json._

class uJsonSerializationTest extends AnyFlatSpec with Matchers {
  behavior of "uJson module when serializing"

  it should "serialize a case class" in {
    val foo = Foo(42.0, FooString("aFoo"), List("baz", "bar"))
    encodeAST[Foo](foo) shouldEqual Obj(("bar", Num(42.0)), ("baz", Obj(("foo", "aFoo"))), ("items", Arr(Str("baz"), Str("bar"))))
    val expectedJsonString = """{"bar":42,"baz":{"foo":"aFoo"},"items":["baz","bar"]}"""
    encodeString[Foo](foo) shouldEqual expectedJsonString
  }
}
case class FooString(foo: String)
case class Foo(bar: Double, baz: FooString, items: List[String])
