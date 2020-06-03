package dev.scarisey.bastionbenchmark
import java.util.concurrent.TimeUnit

import bastion.DynamicReprEncode
import bastion.derivation.dynamicrepr.auto._
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
class EncodeDynamicRepr {

  val encoder = implicitly[DynamicReprEncode[ExternalPerson]]

  @Benchmark
  def encodeToDynamicReprWithAutoDerivedEncoder: Unit = BastionConversion.encode(ExternalPerson("John", "1985-01-01"))(encoder)
  @Benchmark
  def specificManualEncodeToDynamicRepr: Unit = ManualConversion.encode(ExternalPerson("John", "1985-01-01"))
}
