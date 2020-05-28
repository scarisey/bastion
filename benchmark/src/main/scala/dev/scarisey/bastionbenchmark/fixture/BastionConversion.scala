package dev.scarisey.bastionbenchmark.fixture
import bastion.DecodeError
import dev.scarisey.bastionbenchmark.fixture.Domain.{Birthdate, Name, Person}
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson

object BastionConversion extends Conversion[DecodeError]{
  import bastion._
  import derivation.dynamicrepr.auto._
  import derivation.decode.auto._
  import json._

  implicit val decodeName: Decode[Name] = Decode.wrapE(Name.apply)
  implicit val decodeBirthdate:Decode[Birthdate] = Decode.wrapE(Birthdate.apply)

  override def convert(source:ExternalPerson):Either[DecodeError,Person] = source.convert[Person]

  def decodeFromRepr(dynamicRepr: DynamicRepr):Either[DecodeError,Person] = dynamicRepr.convert[Person]

  def encode(person: ExternalPerson)(implicit encode: DynamicReprEncode[ExternalPerson]):DynamicRepr = encode.to(person)

  def decodeFromJson(json:String):Either[DecodeError,Person] = decode[Person](json)
}
