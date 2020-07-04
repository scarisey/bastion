# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
Benchmark                                                         Mode  Cnt    Score    Error   Units
CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingBastionAndUJson  thrpt   10    1,170 ±  0,009  ops/us
CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceGenerics    thrpt   10    1,276 ±  0,014  ops/us
CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceMagnolia    thrpt   10    0,847 ±  0,008  ops/us
CirceVsUpickleVsJacksonVsBastion.decodeJsonUsingCirceOptics      thrpt   10    1,424 ±  0,029  ops/us
CirceVsUpickleVsJacksonVsBastion.decodeUsingJackson              thrpt   10    1,485 ±  0,004  ops/us
CirceVsUpickleVsJacksonVsBastion.decodeUsingUPickle              thrpt   10    1,769 ±  0,038  ops/us
CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingBastion          thrpt   10    1,046 ±  0,014  ops/us
CirceVsUpickleVsJacksonVsBastion.encodeJsonUsingCirce            thrpt   10    1,535 ±  0,018  ops/us
CirceVsUpickleVsJacksonVsBastion.encodeUsingJackson              thrpt   10    1,582 ±  0,020  ops/us
CirceVsUpickleVsJacksonVsBastion.encodeUsingUPickle              thrpt   10    2,950 ±  0,073  ops/us
EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder      thrpt   10  407,005 ±  5,798  ops/us
EncodeDynamicRepr.specificManualEncodeToDynamicRepr              thrpt   10  473,455 ± 18,917  ops/us
ManualVsBastion.convertManually                                  thrpt   10    2,881 ±  0,011  ops/us
ManualVsBastion.convertUsingDerivedEncodersAndDecoders           thrpt   10    1,538 ±  0,025  ops/us
ManualVsBastion.decodeDynamicReprUsingDerivedDecoders            thrpt   10    1,790 ±  0,026  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.

Decoding json using Circe and the specific handwritten converter is about the same performance as bastion + uJson,
but I guess we need more benchmarks on this point.

Encoding json using Circe cannot be compared directly with Bastion when case class constructors are private, 
since Circe derivation cannot be involved in auto mode. 
Most of the work is done manually then with Circe in this benchmark, contrary to the implementation with Bastion using automatic derivation.
