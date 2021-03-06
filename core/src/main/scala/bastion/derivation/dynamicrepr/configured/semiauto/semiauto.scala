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

package bastion.derivation.dynamicrepr.configured

import bastion._
import bastion.derivation.dynamicrepr.EncoderDerivation
import magnolia._

import scala.language.experimental.macros

package object semiauto extends EncoderDerivation {
  def deriveEncode[T]: DynamicReprEncode[T] = macro Magnolia.gen[T]
}
