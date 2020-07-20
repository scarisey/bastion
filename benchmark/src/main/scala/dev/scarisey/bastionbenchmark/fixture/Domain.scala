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

import dev.scarisey.bastionbenchmark.fixture.Domain.DomainError.BirthdateInFuture
import dev.scarisey.bastionbenchmark.fixture.Domain.DomainError.BlankName

object Domain {
  sealed trait DomainError
  object DomainError {
    case object BlankName                extends DomainError
    case object BirthdateInFuture        extends DomainError
    case object InvalidFrenchPhoneNumber extends DomainError
    case object InvalidEmail             extends DomainError
  }

  final case class Name private (value: String)
  object Name {
    def apply(value: String): Either[BlankName.type, Name] =
      Either.cond(value != null && !value.isEmpty, new Name(value), BlankName)
  }

  final case class Birthdate private (value: LocalDate)
  object Birthdate {
    def apply(value: LocalDate): Either[BirthdateInFuture.type, Birthdate] = Either.cond(
      LocalDate.now().isAfter(value),
      new Birthdate(value),
      BirthdateInFuture
    )
  }

  final case class Phone private (value: String)
  object Phone {
    val regex =
      "^(?:(?:\\+|00)33[\\s.-]{0,3}(?:\\(0\\)[\\s.-]{0,3})?|0)[1-9](?:(?:[\\s.-]?\\d{2}){4}|\\d{2}(?:[\\s.-]?\\d{3}){2})$".r
    def apply(value: String): Either[DomainError.InvalidFrenchPhoneNumber.type, Phone] =
      regex findFirstIn value match {
        case Some(_) => Right(new Phone(value))
        case None    => Left(DomainError.InvalidFrenchPhoneNumber)
      }
  }

  final case class Email private (value: String)
  object Email {
    val regex =
      "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])".r
    def apply(value: String): Either[DomainError.InvalidEmail.type, Email] =
      regex findFirstIn value match {
        case Some(_) => Right(new Email(value))
        case _       => Left(DomainError.InvalidEmail)
      }
  }

  final case class Person(name: Name, birthdate: Birthdate)
  final case class Contact(person: Person, phone: Phone, email: Email)
  final case class Contacts(favorites: List[Contact], all: List[Contact])
}
