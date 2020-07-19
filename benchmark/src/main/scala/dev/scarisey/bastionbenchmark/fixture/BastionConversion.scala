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
import bastion.DecodeError
import dev.scarisey.bastionbenchmark.fixture.Domain.Birthdate
import dev.scarisey.bastionbenchmark.fixture.Domain.Name
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson

object BastionConversion extends Conversion[DecodeError] {
  import bastion._
  import derivation.decode.auto._
  import derivation.dynamicrepr.auto._
  import json._

  implicit val decodeName: Decoder[Name]           = Decoder.wrapE(Name.apply)
  implicit val decodeBirthdate: Decoder[Birthdate] = Decoder.wrapE(Birthdate.apply)

  override def convert(source: ExternalPerson): Either[DecodeError, Person] = source.convert[Person]

  def decodeFromRepr(dynamicRepr: DynamicRepr): Either[DecodeError, Person] = dynamicRepr.convert[Person]

  def encode(person: ExternalPerson)(implicit encode: DynamicReprEncode[ExternalPerson]): DynamicRepr = encode.to(person)

  def decodeFromJson(json: String): Either[DecodeError, Person] = decodeJson[Person](json)

  def encode(person: Person)(implicit encode: BasicJsonEncoder[Person]): String = encodeJson[Person](person)
}
