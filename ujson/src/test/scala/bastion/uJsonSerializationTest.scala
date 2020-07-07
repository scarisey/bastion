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
import bastion.json._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class uJsonSerializationTest extends AnyFlatSpec with Matchers {
  behavior of "uJson module when serializing"
  it should "serialize a case class" in {
    case class FooString(foo: String)
    case class Foo(bar: Double, baz: FooString, items: List[String])

    val foo                = Foo(42.0, FooString("aFoo"), List.apply("baz", "bar"))
    val expectedJsonString = """{"bar":42.0,"baz":{"foo":"aFoo"},"items":["baz","bar"]}"""
    foo.asJson shouldEqual expectedJsonString
  }

  it should "serialize a specific case of a sealed trait" in {
    sealed trait Bar
    case class BarA(fieldA: String) extends Bar
    case class BarB(fieldB: Int)    extends Bar
    case class FooBar(fieldBar: Bar)

    BarA("bar").asJson shouldEqual """{"fieldA":"bar"}"""
    BarB(42).asJson shouldEqual """{"fieldB":42}"""
    FooBar(BarB(36)).asJson shouldEqual """{"fieldBar":{"fieldB":36}}"""
  }

  it should "serialize enum's like ADT" in {
    sealed trait Baz
    case object Enum1 extends Baz
    case object Enum2 extends Baz
    case object Enum3 extends Baz
    case class FooBaz(fieldBaz: Baz)

    FooBaz(Enum1).asJson shouldEqual """{"fieldBaz":"Enum1"}"""
    FooBaz(Enum2).asJson shouldEqual """{"fieldBaz":"Enum2"}"""
    FooBaz(Enum3).asJson shouldEqual """{"fieldBaz":"Enum3"}"""
  }
}
