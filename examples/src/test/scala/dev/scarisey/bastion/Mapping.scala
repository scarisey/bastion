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

package dev.scarisey.bastion
import java.time.LocalDate

import dev.scarisey.bastion._
import dev.scarisey.bastion.Encode._
import dev.scarisey.bastion.derivation.encode.auto._
import dev.scarisey.bastion.derivation.decode.auto._
import Configuration.default

object Mapping extends App {
  final case class PersonExternal(
    id: String,
    firstName: String,
    lastName: String,
    birthdate: LocalDate,
    posX: Double,
    posY: Double
  )
  final case class Person(id: String, firstName: String, lastName: String, birthdate: LocalDate)

  implicit val encode: Encode[PersonExternal] = deriveEncode[PersonExternal] //FIXME should not be needed
  val person =
    PersonExternal("anId", "firstName", "lastName", LocalDate.parse("1985-01-12"), 44.846565, -0.567351).convert[Person]

  println(person)
}
