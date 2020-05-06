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

/**
 * Configure Encode derivation. When deriving an encoder for a type A, Configuration(lenientCase = true) will enable
 * lenient case resolution of DynamicRepr field.
 * @see [[DynamicRepr]], [[Encode]]
 */
case class Configuration(lenientCase: Boolean)
object Configuration {
  implicit val default: Configuration = Configuration(false)
  implicit val lenient: Configuration = Configuration(true)
}
