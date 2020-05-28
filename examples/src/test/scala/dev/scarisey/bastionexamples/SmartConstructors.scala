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
import java.time.LocalDate
import java.util.UUID
import bastion._
import bastion.derivation.dynamicrepr.auto._
import bastionexamples.SmartConstructors.Domain.DomainError.BlankCity
import bastionexamples.SmartConstructors.Domain.DomainError.BlankName
import bastionexamples.SmartConstructors.Domain.DomainError.BlankStreetName
import bastionexamples.SmartConstructors.Domain.DomainError.BlankZipCode
import bastionexamples.SmartConstructors.Domain.DomainError.NegativeStreetNumber
import bastionexamples.SmartConstructors.Contract.Address
import bastionexamples.SmartConstructors.Contract.Identity

object SmartConstructors extends App {

  object Domain {
    sealed trait DomainError
    object DomainError {
      case object BlankName            extends DomainError
      case object NegativeStreetNumber extends DomainError
      case object BlankStreetName      extends DomainError
      case object BlankZipCode         extends DomainError
      case object BlankCity            extends DomainError
    }

    final case class PersonId private (value: String)
    object PersonId {
      def from(s: String) = new PersonId(s)
      def apply: PersonId = new PersonId(UUID.randomUUID().toString)
    }
    final case class Name(value: String)
    object Name {
      def apply(value: String): Either[BlankName.type, Name] =
        Either.cond(value != null && !value.isEmpty, new Name(value), BlankName)
    }

    final case class StreetNumber(value: Int)
    object StreetNumber {
      def apply(value: Int): Either[NegativeStreetNumber.type, StreetNumber] =
        Either.cond(value > 0, new StreetNumber(value), NegativeStreetNumber)
    }
    final case class StreetName(value: String)
    object StreetName {
      def apply(value: String): Either[BlankStreetName.type, StreetName] =
        Either.cond(value != null && !value.isEmpty, new StreetName(value), BlankStreetName)
    }
    final case class ZipCode(value: String)
    object ZipCode {
      def apply(value: String): Either[BlankZipCode.type, ZipCode] =
        Either.cond(value != null && !value.isEmpty, new ZipCode(value), BlankZipCode)
    }
    final case class City(value: String)
    object City {
      def apply(value: String): Either[BlankCity.type, City] =
        Either.cond(value != null && !value.isEmpty, new City(value), BlankCity)
    }
    final case class Address(streetNumber: StreetNumber, streetName: StreetName, city: City, zipCode: ZipCode)
    final case class Person(personId: PersonId, firstName: Name, lastName: Name, birthdate: LocalDate, address: Address)
  }

  object Contract {
    final case class Identity(personId: String, firstName: String, lastName: String, birthdate: LocalDate)
    final case class Address(streetNumber: Int, streetName: String, city: String, zipCode: String)
    final case class Person(identity: Identity, address: Address)
  }

  implicit val decodePersonId: Decode[Domain.PersonId]         = Decode.wrap(Domain.PersonId.from)
  implicit val decodeName: Decode[Domain.Name]                 = Decode.wrapE(Domain.Name.apply)
  implicit val decodeStreetNumber: Decode[Domain.StreetNumber] = Decode.wrapE(Domain.StreetNumber.apply)
  implicit val decodeStreetName: Decode[Domain.StreetName]     = Decode.wrapE(Domain.StreetName.apply)
  implicit val decodeZipCode: Decode[Domain.ZipCode]           = Decode.wrapE(Domain.ZipCode.apply)
  implicit val decodeCity: Decode[Domain.City]                 = Decode.wrapE(Domain.City.apply)
  implicit val decodeAddress: Decode[Domain.Address] =
    Decode.instance(g => (g.streetNumber, g.streetName, g.city, g.zipCode).apply(Domain.Address.apply))
  implicit val decodePerson: Decode[Domain.Person] = Decode.instance(g =>
    (g.identity.personId, g.identity.firstName, g.identity.lastName, g.identity.birthdate, g.address).apply(Domain.Person.apply)
  )

  println(
    Contract
      .Person(Identity("anId", "firstName", "lastName", LocalDate.now()), Address(42, "rue Robert", "Bordeaux", "33000"))
      .convert[Domain.Person]
  )
}
