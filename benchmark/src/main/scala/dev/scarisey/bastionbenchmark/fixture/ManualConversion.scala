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
import java.time.LocalDate

import bastion.DynamicRepr
import bastion.ProductDynamicRepr
import bastion.ValueDynamicRepr
import dev.scarisey.bastionbenchmark.fixture.Domain.Birthdate
import dev.scarisey.bastionbenchmark.fixture.Domain.DomainError
import dev.scarisey.bastionbenchmark.fixture.Domain.Name
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalContact
import dev.scarisey.bastionbenchmark.fixture.External.ExternalContacts
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson

import scala.util.Try

sealed trait SomeInfraError
case object ParsingError                   extends SomeInfraError
case class WrapDomainError(e: DomainError) extends SomeInfraError
case class WrapOtherError[T](e: T)         extends SomeInfraError

object ManualConversion extends Conversion[SomeInfraError] {

  def convertFromPrimitives(nameString: String, birthdateString: String): Either[SomeInfraError, Domain.Person] =
    for {
      name       <- Name(nameString).left.map(WrapDomainError(_))
      parsedDate <- Try(LocalDate.parse(birthdateString)).toEither.left.map(_ => ParsingError)
      birthdate  <- Birthdate(parsedDate).left.map(WrapDomainError(_))
    } yield Person(name, birthdate)

  override def convert(externalPerson: External.ExternalPerson): Either[SomeInfraError, Domain.Person] =
    for {
      name       <- Name(externalPerson.name).left.map(WrapDomainError(_))
      parsedDate <- Try(LocalDate.parse(externalPerson.birthdate)).toEither.left.map(_ => ParsingError)
      birthdate  <- Birthdate(parsedDate).left.map(WrapDomainError(_))
    } yield Person(name, birthdate)

  def encode(externalPerson: ExternalPerson): DynamicRepr = new ProductDynamicRepr[ExternalPerson](externalPerson) {
    override def selectDynamic(field: String): DynamicRepr = field match {
      case "name"      => ValueDynamicRepr(a.name)
      case "birthdate" => ValueDynamicRepr(a.birthdate)
    }
  }

  def convertFromContact(externalContact: ExternalContact): Either[SomeInfraError, Domain.Contact] =
    for {
      person <- convert(externalContact.person)
      phone  <- Domain.Phone.apply(externalContact.phone).left.map(WrapDomainError(_))
      email  <- Domain.Email.apply(externalContact.email).left.map(WrapDomainError(_))
    } yield Domain.Contact.apply(person, phone, email)

  def convertFromExternalContacts(externalContacts: ExternalContacts): Either[SomeInfraError, Domain.Contacts] =
    (for {
      contact <- externalContacts.contacts
    } yield convertFromContact(contact))
      .foldLeft(Right(List.empty).asInstanceOf[Either[SomeInfraError, List[Domain.Contact]]]) {
        case (acc, ctc) =>
          acc.flatMap(cs => ctc.map(c => c :: cs))
      }
      .map(Domain.Contacts.apply(List.empty, _))
}
