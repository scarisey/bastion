package dev.scarisey.bastionbenchmark.fixture
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson

trait Conversion[+E] {
  def convert(externalPerson: ExternalPerson): Either[E, Person]
}
