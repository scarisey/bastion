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

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import dev.scarisey.bastionbenchmark.fixture.Domain.{Contacts, Person}
import dev.scarisey.bastionbenchmark.fixture.External.{ExternalContacts, ExternalPerson}

import scala.util.Try

object JacksonConversion {
  val objectMapper = new ObjectMapper()
  objectMapper.registerModule(DefaultScalaModule)

  def encode(person: Person): String = objectMapper.writeValueAsString(person)
  def decode(person: String): Either[SomeInfraError, Person] =
    Try(objectMapper.readValue(person, classOf[ExternalPerson])).toEither.left
      .map(WrapOtherError(_))
      .flatMap(ManualConversion.convert)
  def decodeContacts(person: String): Either[SomeInfraError, Contacts] =
    Try(objectMapper.readValue(person, classOf[ExternalContacts])).toEither.left
      .map(WrapOtherError(_))
      .flatMap(ManualConversion.convertFromExternalContacts)
}
