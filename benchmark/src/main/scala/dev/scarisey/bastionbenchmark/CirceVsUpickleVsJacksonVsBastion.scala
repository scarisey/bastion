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
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Benchmark)
class CirceVsUpickleVsJacksonVsBastion {
  val aJson = """{"name":"John","birthdate":"1985-01-01"}"""
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
  def encodeJsonUsingCirce(): Unit = CirceConversion.encode(aPerson)(circePersonEncoder)

  @Benchmark
  def encodeJsonUsingBastion(): Unit = BastionConversion.encode(aPerson)(bastionJsonEncoder)

  @Benchmark
  def encodeUsingJackson(): Unit = JacksonConversion.encode(aPerson)

  @Benchmark
  def decodeUsingJackson(): Unit = JacksonConversion.decode(aJson)

  @Benchmark
  def encodeUsingUPickle(): Unit = UPickleConversion.encode(aPerson)

  @Benchmark
  def decodeUsingUPickle(): Unit = UPickleConversion.decode(aJson)

}
