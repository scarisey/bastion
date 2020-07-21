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

package dev.scarisey.bastionbenchmark
import java.time.LocalDate
import java.util.concurrent.TimeUnit

import dev.scarisey.bastionbenchmark.fixture.Domain.Birthdate
import dev.scarisey.bastionbenchmark.fixture.Domain.Name
import dev.scarisey.bastionbenchmark.fixture.Domain.Person
import dev.scarisey.bastionbenchmark.fixture.BastionConversion
import dev.scarisey.bastionbenchmark.fixture.CirceConversion
import dev.scarisey.bastionbenchmark.fixture.CirceMagnoliaConversion
import dev.scarisey.bastionbenchmark.fixture.JacksonConversion
import dev.scarisey.bastionbenchmark.fixture.UPickleConversion
import io.circe.Encoder
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Warmup
import io.circe.generic.semiauto._
import bastion.json._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 1, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 1, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Benchmark)
class CirceVsUpickleVsJacksonVsBastion {
  val aJson = """{"name":"John","birthdate":"1985-01-01"}"""
  val jsonContacts =
    """{
      |"contacts":[
      |{"person":{"name":"John1","birthdate":"1990-01-01"},"phone":"01 02 03 04 05","email":"j.doe1@foo.com"},
      |{"person":{"name":"John2","birthdate":"1990-01-02"},"phone":"02 02 03 04 05","email":"j.doe2@foo.com"},
      |{"person":{"name":"John3","birthdate":"1990-01-03"},"phone":"03 02 03 04 05","email":"j.doe3@foo.com"},
      |{"person":{"name":"John4","birthdate":"1990-01-04"},"phone":"04 02 03 04 05","email":"j.doe4@foo.com"},
      |{"person":{"name":"John5","birthdate":"1990-01-05"},"phone":"05 02 03 04 05","email":"j.doe5@foo.com"},
      |{"person":{"name":"John6","birthdate":"1990-01-06"},"phone":"06 02 03 04 05","email":"j.doe6@foo.com"},
      |{"person":{"name":"John7","birthdate":"1990-01-07"},"phone":"07 02 03 04 05","email":"j.doe7@foo.com"},
      |{"person":{"name":"John8","birthdate":"1990-01-08"},"phone":"08 02 03 04 05","email":"j.doe8@foo.com"},
      |{"person":{"name":"John9","birthdate":"1990-01-09"},"phone":"09 02 03 04 05","email":"j.doe9@foo.com"},
      |{"person":{"name":"John0","birthdate":"1990-01-00"},"phone":"00 02 03 04 05","email":"j.doe0@foo.com"}
      |]
      |}""".stripMargin
  val aPerson = (for {
    name      <- Name("John")
    birthdate <- Birthdate(LocalDate.of(1985, 1, 1))
  } yield Person(name, birthdate)).fold(t => throw new IllegalStateException(t.toString), identity)

  implicit val circeNameEncoder: Encoder[Name]           = Encoder.encodeString.contramap(_.toString)
  implicit val circeBirthdateEncoder: Encoder[Birthdate] = Encoder.encodeString.contramap(_.toString)

  val circePersonEncoder: Encoder[Person] = deriveEncoder[Person]
  val bastionJsonEncoder                  = BasicJsonEncoder.deriveBasicJsonEncoder[Person]

  @Benchmark
  def decodeJsonUsingCirceOptics(): Unit = CirceConversion.decodeUsingOptics(aJson)

  @Benchmark
  def decodeJsonUsingCirceGenerics(): Unit = CirceConversion.decodeUsingGenerics(aJson)

  @Benchmark
  def decodeJsonUsingCirceMagnolia(): Unit = CirceMagnoliaConversion.decodeUsingMagnolia(aJson)

  @Benchmark
  def decodeJsonUsingBastionAndUJson(): Unit = BastionConversion.decodeFromJson(aJson)

  @Benchmark
  def decodeUsingJackson(): Unit = JacksonConversion.decode(aJson)

  @Benchmark
  def decodeUsingUPickle(): Unit = UPickleConversion.decode(aJson)

  @Benchmark
  def decodeContactsJsonUsingCirceGenerics(): Unit =
    println(
      CirceConversion.decodeContactsUsingGenerics(jsonContacts)
    )

  @Benchmark
  def decodeContactsUsingBastionAndUJson(): Unit =
    println(
      BastionConversion.decodeContactsFromJson(jsonContacts)
    )

  @Benchmark
  def decodeContactsUsingJackson(): Unit =
    println(
      JacksonConversion.decodeContacts(jsonContacts)
    )

  @Benchmark
  def decodeContactsUsingUPickle(): Unit =
    println(
      UPickleConversion.decodeContacts(jsonContacts)
    )

  @Benchmark
  def encodeJsonUsingCirce(): Unit = CirceConversion.encode(aPerson)(circePersonEncoder)

  @Benchmark
  def encodeJsonUsingBastion(): Unit = BastionConversion.encode(aPerson)(bastionJsonEncoder)

  @Benchmark
  def encodeUsingJackson(): Unit = JacksonConversion.encode(aPerson)

  @Benchmark
  def encodeUsingUPickle(): Unit = UPickleConversion.encode(aPerson)
}
