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
    case object BlankName         extends DomainError
    case object BirthdateInFuture extends DomainError
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

  final case class Person(name: Name, birthdate: Birthdate)
}
