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

package bastion.derivation

import magnolia._
import upickle.core.Visitor

import scala.language.experimental.macros

package object json extends upickle.AttributeTagged with upickle.implicits.Writers {
  type Typeclass[T] = upickle.default.Writer[T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def write0[V](out: Visitor[_, V], v: T): V = {
      val visitor    = out.visitObject(ctx.parameters.size, -1).narrow
      val keyVisitor = visitor.visitKey(-1)
      ctx.parameters.foreach { p =>
        visitor.visitKeyValue(keyVisitor.visitString(p.label, -1))
        visitor.visitValue(p.typeclass.write(visitor.subVisitor, p.dereference(v)), -1)
      }
      visitor.visitEnd(-1)
    }

  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def write0[V](out: Visitor[_, V], v: T): V = ctx.dispatch(v) { subtype =>
      subtype.typeclass.write(out, subtype.cast(v))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
