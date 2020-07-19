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

package bastion.derivation.dynamicrepr
import bastion.DynamicRepr
import bastion.DynamicReprEncode
import bastion.NilDynamicRepr
import bastion.ProductDynamicRepr
import bastion.ValueDynamicRepr
import magnolia._

trait EncoderDerivation {
  type Typeclass[T] = DynamicReprEncode[T]

  def combine[T](ctx: ReadOnlyCaseClass[DynamicReprEncode, T]): DynamicReprEncode[T] =
    new DynamicReprEncode[T] {
      override def to(a: T): DynamicRepr =
        if (ctx.isObject) {
          ValueDynamicRepr(ctx.typeName.short)
        } else {
          new ProductDynamicRepr[T](a) { self =>
            override def selectDynamic(field: String): DynamicRepr =
              ctx.parameters
                .find(p => p.label == field)
                .map(p => p.typeclass.to(p.dereference(a)))
                .getOrElse(NilDynamicRepr)
          }
        }
    }

  def dispatch[T](ctx: SealedTrait[DynamicReprEncode, T]): DynamicReprEncode[T] =
    new DynamicReprEncode[T] {
      override def to(a: T): DynamicRepr =
        ctx.dispatch(a)(sub => sub.typeclass.to(sub.cast(a)))

    }

}
