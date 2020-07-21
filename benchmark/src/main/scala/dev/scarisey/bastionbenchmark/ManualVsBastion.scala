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

import dev.scarisey.bastionbenchmark.fixture.External.{ExternalContact, ExternalContacts, ExternalPerson}
import dev.scarisey.bastionbenchmark.fixture.{BastionConversion, ManualConversion}
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@State(Scope.Benchmark)
class ManualVsBastion {

  val contacts = ExternalContacts(
    List(
      ExternalContact(ExternalPerson("Foo1", "1990-01-01"), "00 01 02 03 04", "foo1@bar.dev"),
      ExternalContact(ExternalPerson("Foo2", "1990-01-02"), "00 01 02 03 04", "foo2@bar.dev"),
      ExternalContact(ExternalPerson("Foo3", "1990-01-03"), "00 01 02 03 04", "foo3@bar.dev"),
      ExternalContact(ExternalPerson("Foo4", "1990-01-04"), "00 01 02 03 04", "foo4@bar.dev"),
      ExternalContact(ExternalPerson("Foo5", "1990-01-05"), "00 01 02 03 04", "foo5@bar.dev"),
      ExternalContact(ExternalPerson("Foo6", "1990-01-06"), "00 01 02 03 04", "foo6@bar.dev"),
      ExternalContact(ExternalPerson("Foo7", "1990-01-07"), "00 01 02 03 04", "foo7@bar.dev"),
      ExternalContact(ExternalPerson("Foo8", "1990-01-08"), "00 01 02 03 04", "foo8@bar.dev"),
      ExternalContact(ExternalPerson("Foo9", "1990-01-09"), "00 01 02 03 04", "foo9@bar.dev"),
      ExternalContact(ExternalPerson("Foo10", "1990-01-10"), "00 01 02 03 04", "foo10@bar.dev")
    )
  )

  @Benchmark
  def convertManually: Unit = ManualConversion.convert(ExternalPerson("John", "1985-01-01"))

  @Benchmark
  def convertUsingDerivedEncodersAndDecoders: Unit = BastionConversion.convert(ExternalPerson("John", "1985-01-01"))

  @Benchmark
  def decodeDynamicReprUsingDerivedDecoders: Unit =
    BastionConversion.decodeFromRepr(ManualConversion.encode(ExternalPerson("John", "1985-01-01")))

  @Benchmark
  def decodeContactsUsingBastion: Unit = BastionConversion.convertExternalContacts(contacts)

  @Benchmark
  def decodeContactsManually:Unit = ManualConversion.convertFromExternalContacts(contacts)
}
