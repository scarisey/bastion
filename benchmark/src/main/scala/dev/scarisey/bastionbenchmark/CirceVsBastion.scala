package dev.scarisey.bastionbenchmark
import java.util.concurrent.TimeUnit

import dev.scarisey.bastionbenchmark.fixture.{BastionConversion, CirceConversion, CirceMagnoliaConversion}
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Warmup

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Benchmark)
class CirceVsBastion {

  val aJson = """{"name":"John","birthdate":"1985-01-01"}"""

  @Benchmark
  def decodeJsonUsingCirceOptics(): Unit = CirceConversion.decodeUsingOptics(aJson)

  @Benchmark
  def decodeJsonUsingCirceGenerics(): Unit = CirceConversion.decodeUsingGenerics(aJson)
  @Benchmark
  def decodeJsonUsingCirceMagnolia(): Unit = CirceMagnoliaConversion.decodeUsingMagnolia(aJson)

  @Benchmark
  def decodeJsonUsingBastionAndUJson(): Unit = BastionConversion.decodeFromJson(aJson)
}
