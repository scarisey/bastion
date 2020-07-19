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

package dev.scarisey.bastionbenchmark.fixture

import dev.scarisey.bastionbenchmark.fixture.Domain.Birthdate
import dev.scarisey.bastionbenchmark.fixture.Domain.Name
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson
import upickle.default
import upickle.default._

import scala.util.Try

object UPickleConversion {

  implicit val rw: default.ReadWriter[ExternalPerson] = macroRW[ExternalPerson]

  implicit val wName      = writer[String].comap[Name](name => name.value)
  implicit val wBirthdate = writer[String].comap[Birthdate](date => date.value.toString)
  implicit val w          = macroW[Person]

  def encode(person: Person): String = write[Person](person)

  def decode(person: String): Either[SomeInfraError, Person] =
    Try(read[ExternalPerson](person)).toEither.left
      .map(WrapOtherError(_))
      .flatMap(ManualConversion.convert(_))

}
