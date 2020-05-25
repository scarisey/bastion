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

package bastion.derivation.encode
import bastion.DynamicRepr
import bastion.Encode
import bastion.FieldKeyRepr
import bastion.Logger
import bastion.NilDynamicRepr
import bastion.ProductDynamicRepr
import magnolia._

trait EncodeDerivation {
  type Typeclass[T] = Encode[T]

  def combine[T](ctx: ReadOnlyCaseClass[Encode, T])(implicit configuration: Configuration): Encode[T] = new Encode[T] {
    Logger.debug(s"combine to ${ctx.typeName.full}")
    val fieldsKeyRepr: Map[FieldKeyRepr, ReadOnlyParam[Encode, T]] = if (configuration.lenientCase) {
      Logger.debug(s"Computing fieldsRepr for ${ctx.typeName.full}")
      ctx.parameters.map(p => (FieldKeyRepr(p.label), p)).toMap
    } else {
      Map.empty
    }
    override def to(a: T): DynamicRepr = new ProductDynamicRepr[T](a) { self =>
      override def selectDynamic(field: String): DynamicRepr = {
        val foundParam = if (configuration.lenientCase) {
          fieldsKeyRepr.find { case (k, _) => k === field }.map { case (_, v) => v }
        } else {
          ctx.parameters.find(p => p.label == field)
        }
        foundParam
          .map(p => p.typeclass.to(p.dereference(a)))
          .getOrElse(NilDynamicRepr)
      }
    }
  }

  def dispatch[T](ctx: SealedTrait[Encode, T]): Encode[T] =
    new Encode[T] {
      Logger.debug(s"dispatch to ${ctx.typeName.full}")
      override def to(a: T): DynamicRepr =
        ctx.dispatch(a)(sub => sub.typeclass.to(sub.cast(a)))

    }

}
