package dev.scarisey.bastionbenchmark.fixture
import java.time.LocalDate

import dev.scarisey.bastionbenchmark.fixture.Domain.DomainError.BirthdateInFuture
import dev.scarisey.bastionbenchmark.fixture.Domain.DomainError.BlankName

object Domain {
  sealed trait DomainError
  object DomainError {
    case object BlankName   extends DomainError
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

  final case class Person(name: Name,birthdate: Birthdate)
}
