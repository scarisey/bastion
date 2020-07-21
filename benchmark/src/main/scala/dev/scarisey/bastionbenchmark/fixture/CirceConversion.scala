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
import dev.scarisey.bastionbenchmark.fixture.Domain.{Contacts, Person}
import dev.scarisey.bastionbenchmark.fixture.External.{ExternalContacts, ExternalPerson}
import io.circe.Encoder

object CirceConversion {
  import io.circe.generic.auto._
  import io.circe.optics.JsonPath._
  import io.circe.parser._
  import io.circe.syntax._
  val getName      = root.name.string
  val getBirthdate = root.birthdate.string

  def decodeUsingOptics(json: String): Either[SomeInfraError, Person] =
    for {
      parsed    <- parse(json).left.map(WrapOtherError(_))
      name      <- getName.getOption(parsed).toRight(ParsingError)
      birthdate <- getBirthdate.getOption(parsed).toRight(ParsingError)
      person    <- ManualConversion.convertFromPrimitives(name, birthdate).left.map(WrapOtherError(_))
    } yield person

  def decodeUsingGenerics(json: String): Either[SomeInfraError, Person] =
    for {
      external <- decode[ExternalPerson](json).left.map(WrapOtherError(_))
      person   <- ManualConversion.convert(external)
    } yield person

  def decodeContactsUsingGenerics(json: String): Either[SomeInfraError, Contacts] =
    for {
      external <- decode[ExternalContacts](json).left.map(WrapOtherError(_))
      contacts <- ManualConversion.convertFromExternalContacts(external)
    } yield contacts

  def encode(person: Person)(implicit encode: Encoder[Person]): String = person.asJson.toString()
}

object CirceMagnoliaConversion {
  import io.circe.magnolia.derivation.decoder.auto._
  import io.circe.parser._

  def decodeUsingMagnolia(json: String): Either[SomeInfraError, Person] =
    for {
      external <- decode[ExternalPerson](json).left.map(WrapOtherError(_))
      person   <- ManualConversion.convert(external)
    } yield person
}
