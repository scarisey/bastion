package dev.scarisey.bastionbenchmark.fixture
import java.time.LocalDate

import bastion.DynamicRepr
import bastion.ProductDynamicRepr
import bastion.ValueDynamicRepr
import dev.scarisey.bastionbenchmark.fixture.Domain.Birthdate
import dev.scarisey.bastionbenchmark.fixture.Domain.DomainError
import dev.scarisey.bastionbenchmark.fixture.Domain.Name
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson

import scala.util.Try

sealed trait SomeInfraError
case object ParsingError                   extends SomeInfraError
case class WrapDomainError(e: DomainError) extends SomeInfraError
case class WrapOtherError[T](e:T) extends SomeInfraError

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
}
