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

package dev.scarisey.bastion.derivation.decode
import dev.scarisey.bastion.Decode
import dev.scarisey.bastion.DynamicRepr
import dev.scarisey.bastion.IncorrectSubtype
import dev.scarisey.bastion.Logger
import dev.scarisey.bastion.Result
import magnolia.CaseClass
import magnolia.SealedTrait

trait DecodeDerivation {
  type Typeclass[T] = Decode[T]

  def combine[T](ctx: CaseClass[Decode, T]): Decode[T] = new Decode[T] {
    Logger.debug(s"decode combine to ${ctx.typeName.full}")
    override def from(g: DynamicRepr): Result[T] =
      ctx.constructMonadic(param => param.typeclass.from(g.selectDynamic(param.label)))
  }

  def dispatch[T](ctx: SealedTrait[Decode, T]): Decode[T] = new Decode[T] {
    Logger.debug(s"decode dispatch to ${ctx.typeName.full}")
    override def from(g: DynamicRepr): Result[T] =
      ctx.subtypes
        .foldLeft(Option.empty[Result[T]]) { (res, sub) =>
          res match {
            case s @ Some(_) => s
            case None =>
              sub.typeclass.from(g) match {
                case r @ Right(_) => Some(r)
                case _            => None
              }
          }
        } match {
        case Some(value) => value
        case None        => Left(IncorrectSubtype)
      }

  }

}
