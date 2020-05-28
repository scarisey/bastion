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

package bastionexamples
import bastion._
import bastion.derivation.dynamicrepr.configured.auto._
import bastion.derivation.decode.auto._
import bastion.derivation.dynamicrepr.Configuration.lenient

object LenientCase extends App {
  case class Source(aString: String, anInt: Int, aBoolean: Boolean)
  case class Target(an_int: Int, A_String: String)

  println(
    Source("foo", 42, true).convert[Target]
  )
}
