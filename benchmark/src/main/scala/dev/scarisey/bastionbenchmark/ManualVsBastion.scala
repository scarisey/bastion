package dev.scarisey.bastionbenchmark

import java.util.concurrent.TimeUnit

import dev.scarisey.bastionbenchmark.fixture.External.ExternalPerson
import dev.scarisey.bastionbenchmark.fixture.BastionConversion
import dev.scarisey.bastionbenchmark.fixture.ManualConversion
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Benchmark)
class ManualVsBastion {

  @Benchmark
  def convertManually: Unit = ManualConversion.convert(ExternalPerson("John", "1985-01-01"))

  @Benchmark
  def convertUsingDerivedEncodersAndDecoders: Unit = BastionConversion.convert(ExternalPerson("John", "1985-01-01"))

  @Benchmark
  def decodeDynamicReprUsingDerivedDecoders: Unit =
    BastionConversion.decodeFromRepr(ManualConversion.encode(ExternalPerson("John", "1985-01-01")))
}
