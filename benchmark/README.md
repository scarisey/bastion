# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
[info] Benchmark                                                         Mode  Cnt    Score   Error   Units
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingBastionAndUJson  thrpt   10    1,170 ± 0,013  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceGenerics    thrpt   10    1,268 ± 0,010  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceMagnolia    thrpt   10    0,816 ± 0,012  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceOptics      thrpt   10    1,455 ± 0,005  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeUsingJackson              thrpt   10    1,465 ± 0,022  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.decodeUsingUPickle              thrpt   10    1,724 ± 0,022  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingBastion          thrpt   10    1,927 ± 0,006  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingCirce            thrpt   10    1,408 ± 0,018  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeUsingJackson              thrpt   10    1,654 ± 0,013  ops/us
[info] CirceVsUpickleVsJacksonVsBastion.encodeUsingUPickle              thrpt   10    2,163 ± 0,005  ops/us
[info] EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder      thrpt   10  377,705 ± 4,267  ops/us
[info] EncodeDynamicRepr.specificManualEncodeToDynamicRepr              thrpt   10  356,985 ± 3,441  ops/us
[info] ManualVsBastion.convertManually                                  thrpt   10    2,843 ± 0,044  ops/us
[info] ManualVsBastion.convertUsingDerivedEncodersAndDecoders           thrpt   10    1,475 ± 0,022  ops/us
[info] ManualVsBastion.decodeDynamicReprUsingDerivedDecoders            thrpt   10    1,886 ± 0,032  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.

Decoding json using Circe and the specific handwritten converter is about the same performance as bastion + uJson,
but I guess we need more benchmarks on this point.

Encoding json using Circe cannot be compared directly with Bastion when case class constructors are private, 
since Circe derivation cannot be involved in auto mode. 
Most of the work is done manually then with Circe in this benchmark, contrary to the implementation with Bastion using automatic derivation.
