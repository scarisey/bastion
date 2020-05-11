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

import ujson.JsVisitor
import ujson.Readable
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor
import upickle.core.Util
import upickle.core.Visitor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object uJsonDynamicRepr {
  object JsonVisitor extends JsVisitor[DynamicRepr, DynamicRepr] {
    override def visitArray(length: Int, index: Int): ArrVisitor[DynamicRepr, DynamicRepr] = new JsonArrayVisitor

    override def visitObject(length: Int, index: Int): ObjVisitor[DynamicRepr, DynamicRepr] = new JsonObjectVisitor

    override def visitNull(index: Int): DynamicRepr = NilDynamicRepr

    override def visitFalse(index: Int): DynamicRepr = ValueDynamicRepr(false)

    override def visitTrue(index: Int): DynamicRepr = ValueDynamicRepr(true)

    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): DynamicRepr =
      ValueDynamicRepr(
        if (decIndex != -1 || expIndex != -1) s.toString.toDouble
        else Util.parseIntegralNum(s, decIndex, expIndex, index)
      )

    override def visitString(s: CharSequence, index: Int): DynamicRepr =
      if (s == null) NilDynamicRepr else ValueDynamicRepr(s.toString)

    class JsonObjectVisitor extends ObjVisitor[DynamicRepr, DynamicRepr] {
      type InternalMap = mutable.HashMap[String, DynamicRepr]
      var key: String                                  = _
      val vs: InternalMap                              = mutable.HashMap.empty
      override def visitKey(index: Int): Visitor[_, _] = upickle.core.StringVisitor

      override def visitKeyValue(v: Any): Unit = key = v.toString

      override def subVisitor: Visitor[_, _] = JsonVisitor

      override def visitValue(v: DynamicRepr, index: Int): Unit = vs.update(key, v)

      override def visitEnd(index: Int): DynamicRepr = new ProductDynamicRepr[InternalMap](vs) {
        override def selectDynamic(field: String): DynamicRepr = a.getOrElse(field, NilDynamicRepr)
      }
    }

    class JsonArrayVisitor extends ArrVisitor[DynamicRepr, DynamicRepr] {
      val vs                                 = new ArrayBuffer[DynamicRepr]()
      override def subVisitor: Visitor[_, _] = JsonVisitor

      override def visitValue(v: DynamicRepr, index: Int): Unit = vs += v

      override def visitEnd(index: Int): DynamicRepr = IterableDynamicRepr(vs)
    }
  }

  def parse(t: Readable): DynamicRepr = ujson.transform(t, JsonVisitor)
}
