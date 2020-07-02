package dev.scarisey.bastionbenchmark.fixture

import dev.scarisey.bastionbenchmark.fixture.Domain.{Birthdate, Name, Person}
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson
import upickle.default
import upickle.default._

import scala.util.Try

object UPickleConversion {

  implicit val rw: default.ReadWriter[ExternalPerson] = macroRW[ExternalPerson]

  implicit val wName = writer[String].comap[Name](name => name.value)
  implicit val wBirthdate = writer[String].comap[Birthdate](date => date.value.toString)
  implicit val w = macroW[Person]

  def encode(person: Person): String = write[Person](person)

  def decode(person: String): Either[SomeInfraError, Person] =
    Try(read[ExternalPerson](person)).toEither.left
      .map(WrapOtherError(_))
      .flatMap(ManualConversion.convert(_))

}
