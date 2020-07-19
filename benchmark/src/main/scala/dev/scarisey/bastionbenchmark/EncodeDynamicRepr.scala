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
