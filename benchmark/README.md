# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
Benchmark                                                     Mode  Cnt    Score    Error   Units
CirceVsBastion.decodeJsonUsingBastionAndUJson                thrpt   10    1,160 ±  0,012  ops/us
CirceVsBastion.decodeJsonUsingCirceGenerics                  thrpt   10    1,326 ±  0,011  ops/us
CirceVsBastion.decodeJsonUsingCirceMagnolia                  thrpt   10    0,862 ±  0,004  ops/us
CirceVsBastion.decodeJsonUsingCirceOptics                    thrpt   10    1,407 ±  0,023  ops/us
CirceVsBastion.encodeJsonUsingBastion                        thrpt   10    1,656 ±  0,007  ops/us
CirceVsBastion.encodeJsonUsingCirce                          thrpt   10    1,551 ±  0,018  ops/us
EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder  thrpt   10  481,323 ± 13,700  ops/us
EncodeDynamicRepr.specificManualEncodeToDynamicRepr          thrpt   10  487,614 ±  4,146  ops/us
ManualVsBastion.convertManually                              thrpt   10    2,855 ±  0,006  ops/us
ManualVsBastion.convertUsingDerivedEncodersAndDecoders       thrpt   10    1,426 ±  0,031  ops/us
ManualVsBastion.decodeDynamicReprUsingDerivedDecoders        thrpt   10    1,499 ±  0,012  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.

Decoding json using Circe and the specific handwritten converter is about the same performance as bastion + uJson,
but I guess we need more benchmarks on this point.

Encoding json using Circe cannot be compared directly with Bastion when case class constructors are private, 
since Circe derivation cannot be involved in auto mode. 
Most of the work is done manually then with Circe in this benchmark, contrary to the implementation with Bastion using automatic derivation.   