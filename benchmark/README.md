# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
[info] Benchmark                                                         Mode  Cnt    Score   Error   Units
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingBastionAndUJson  thrpt   10    1,233 ± 0,008  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceGenerics    thrpt   10    1,288 ± 0,016  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceMagnolia    thrpt   10    0,766 ± 0,005  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceOptics      thrpt   10    1,429 ± 0,011  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeUsingJackson              thrpt   10    1,454 ± 0,004  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeUsingUPickle              thrpt   10    1,741 ± 0,027  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingBastion          thrpt   10    2,146 ± 0,031  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingCirce            thrpt   10    1,472 ± 0,014  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeUsingJackson              thrpt   10    1,541 ± 0,004  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeUsingUPickle              thrpt   10    2,678 ± 0,011  ops/us
[info] EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder      thrpt   10  407,054 ± 3,097  ops/us
[info] EncodeDynamicRepr.specificManualEncodeToDynamicRepr              thrpt   10  505,420 ± 9,909  ops/us
[info] ManualVsBastion.convertManually                                  thrpt   10    2,981 ± 0,053  ops/us
[info] ManualVsBastion.convertUsingDerivedEncodersAndDecoders           thrpt   10    1,553 ± 0,017  ops/us
[info] ManualVsBastion.decodeDynamicReprUsingDerivedDecoders            thrpt   10    1,919 ± 0,019  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.

Decoding json using Circe + a specific handwritten function to convert the Case Class representation of json to Domain Case Class
is about the same performance as bastion + uJson that made this labor automatically. 


Encoding json using Circe cannot be compared directly with Bastion when case class constructors are private, 
since Circe derivation cannot be involved in auto mode. 
Most of the work is done manually then with Circe in this benchmark, contrary to the implementation with Bastion using automatic derivation.
