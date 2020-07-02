package dev.scarisey.bastionbenchmark.fixture

import com.fasterxml.jackson.databind.ObjectMapper
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson

import scala.util.Try

object JacksonConversion {
  val objectMapper = new ObjectMapper()

  def encode(person: Person): String = objectMapper.writeValueAsString(person)
  def decode(person: String): Either[SomeInfraError, Person] =
    Try(objectMapper.readValue(person, classOf[ExternalPerson])).toEither.left
      .map(WrapOtherError(_))
      .flatMap(ManualConversion.convert)
}
