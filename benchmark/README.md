# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
[info] Benchmark                                                         Mode  Cnt    Score   Error   Units
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingBastionAndUJson  thrpt   10    1,203 ± 0,015  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceGenerics    thrpt   10    1,286 ± 0,031  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceMagnolia    thrpt   10    0,906 ± 0,017  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceOptics      thrpt   10    1,307 ± 0,006  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeUsingJackson              thrpt   10    0,368 ± 0,008  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeUsingUPickle              thrpt   10    1,742 ± 0,012  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingBastion          thrpt   10    1,053 ± 0,013  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingCirce            thrpt   10    1,460 ± 0,012  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeUsingJackson              thrpt   10    8,520 ± 0,176  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeUsingUPickle              thrpt   10    2,691 ± 0,017  ops/us
[info] EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder      thrpt   10  383,598 ± 4,516  ops/us
[info] EncodeDynamicRepr.specificManualEncodeToDynamicRepr              thrpt   10  486,772 ± 9,323  ops/us
[info] ManualVsBastion.convertManually                                  thrpt   10    2,875 ± 0,026  ops/us
[info] ManualVsBastion.convertUsingDerivedEncodersAndDecoders           thrpt   10    1,538 ± 0,012  ops/us
[info] ManualVsBastion.decodeDynamicReprUsingDerivedDecoders            thrpt   10    1,959 ± 0,007  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.

Decoding json using Circe and the specific handwritten converter is about the same performance as bastion + uJson,
but I guess we need more benchmarks on this point.

Encoding json using Circe cannot be compared directly with Bastion when case class constructors are private, 
since Circe derivation cannot be involved in auto mode. 
Most of the work is done manually then with Circe in this benchmark, contrary to the implementation with Bastion using automatic derivation.
