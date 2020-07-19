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

import bastion.json.JsonDecoder.parse
import bastion.json.decodeJson
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class uJsonDeserializationTest extends AnyFlatSpec with Matchers {

  trait MixedJsonFixture {
    case class FooString(foo: String)
    case class Foo(bar: Double, baz: FooString, items: List[String])

    val mixed = (i: Int) => s"""{
                               |  "foo": {
                               |    "bar": ${806 + i},
                               |    "baz": {
                               |      "foo": "foo again, but not the same"
                               |    },
                               |    "items":["first","second","third"]
                               |  }
                               |}""".stripMargin
    val mixedItems =
      s"""[
         |${(0 to 9).map(i => mixed(i)).mkString(",")}
         |]""".stripMargin

    val itemAssert = (repr: DynamicRepr, i: Int) => {
      val fooi = repr.selectDynamic("foo")
      fooi.bar shouldEqual ValueDynamicRepr(806 + i)
      fooi.baz.foo shouldEqual ValueDynamicRepr("foo again, but not the same")
      fooi.items shouldEqual IterableDynamicRepr(
        ValueDynamicRepr("first") :: ValueDynamicRepr("second") :: ValueDynamicRepr("third") :: Nil
      )
    }
  }

  behavior of "uJson module"

  it should "parse scalar json values" in {
    val number    = "42"
    val boolean   = "true"
    val string    = "\"foo\""
    val nullValue = "null"

    parse(number) shouldEqual ValueDynamicRepr(42)
    parse(boolean) shouldEqual ValueDynamicRepr(true)
    parse(string) shouldEqual ValueDynamicRepr("foo")
    parse(nullValue) shouldEqual NilDynamicRepr
  }

  it should "parse json objects" in {
    val simple =
      s"""{
         |"foo":"this is some foo",
         |"bar":33,
         |"baz":false,
         |"naz":null
         |}""".stripMargin

    val nested =
      s"""{
         |  "foo": {
         |    "bar": 806,
         |    "baz": {
         |      "foo": "foo again, but not the same"
         |    }
         |  }
         |}""".stripMargin

    parse(simple) match {
      case p @ ProductDynamicRepr(_) =>
        p.foo shouldEqual ValueDynamicRepr("this is some foo")
        p.bar shouldEqual ValueDynamicRepr(33)
        p.baz shouldEqual ValueDynamicRepr(false)
        p.naz shouldEqual NilDynamicRepr
        p.undefined shouldEqual NilDynamicRepr
      case p => fail(p.toString)
    }

    val repr = parse(nested)

    repr.foo.bar shouldEqual ValueDynamicRepr(806)
    repr.foo.baz.foo shouldEqual ValueDynamicRepr("foo again, but not the same")
  }

  it should "parse array" in {
    val anArray = "[1,2,3,4]"
    parse(anArray) shouldEqual IterableDynamicRepr(
      ValueDynamicRepr(1) :: ValueDynamicRepr(2) :: ValueDynamicRepr(3) :: ValueDynamicRepr(4) :: Nil
    )
  }

  it should "parse mixed json types" in new MixedJsonFixture {
    itemAssert(parse(mixed(0)), 0)

    parse(mixedItems) match {
      case IterableDynamicRepr(items) => items.toList.zipWithIndex.map { case (x, i) => itemAssert(x, i) }
      case d                          => fail(d.toString)
    }

  }

  it should "decode to a case class" in new MixedJsonFixture {
    import derivation.decode.auto._

    implicit val decodeFoo: Decoder[List[Foo]] =
      Decoder.instance(state => state.foreach(itemState => itemState.foo.runDecoder[Foo]).map(_.toList))

    val decodedItems = decodeJson[List[Foo]](mixedItems)

    decodedItems.isRight shouldBe true
    decodedItems.foreach(_ should have size 10)
    decodedItems.map(_.foldLeft(0.0)((acc, i) => acc + i.bar)) shouldEqual Right(8105.0)

  }

}
