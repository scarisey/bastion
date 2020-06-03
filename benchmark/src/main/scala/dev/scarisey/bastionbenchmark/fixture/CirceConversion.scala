package dev.scarisey.bastionbenchmark.fixture
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson
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

  def encode(person: Person)(implicit encode:Encoder[Person]):String = person.asJson.toString()
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