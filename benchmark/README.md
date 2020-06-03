# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
Benchmark                                                     Mode  Cnt    Score   Error   Units
CirceVsBastion.decodeJsonUsingBastionAndUJson                thrpt   10    0,979 ± 0,012  ops/us
CirceVsBastion.decodeJsonUsingCirceGenerics                  thrpt   10    0,992 ± 0,010  ops/us
CirceVsBastion.decodeJsonUsingCirceMagnolia                  thrpt   10    0,685 ± 0,016  ops/us
CirceVsBastion.decodeJsonUsingCirceOptics                    thrpt   10    1,100 ± 0,020  ops/us
CirceVsBastion.encodeJsonUsingBastion                        thrpt   10    1,161 ± 0,021  ops/us
CirceVsBastion.encodeJsonUsingCirce                          thrpt   10    1,162 ± 0,012  ops/us
EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder  thrpt   10  282,800 ± 5,915  ops/us
EncodeDynamicRepr.specificManualEncodeToDynamicRepr          thrpt   10  341,667 ± 1,693  ops/us
ManualVsBastion.convertManually                              thrpt   10    2,314 ± 0,023  ops/us
ManualVsBastion.convertUsingDerivedEncodersAndDecoders       thrpt   10    1,289 ± 0,074  ops/us
ManualVsBastion.decodeDynamicReprUsingDerivedDecoders        thrpt   10    1,629 ± 0,017  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.
Decoding directly from a precomputed DynamicRepr results in ~ 75% of the handwritten solution's performances.

Decoding json using Circe and the specific handwritten converter is about the same performance as bastion + uJson,
but I guess we need more benchmarks on this point.

Encoding json using Circe cannot be compared directly with Bastion when case class constructors are private, 
since Circe derivation cannot be involved in auto mode. 
Most of the work is done manually then with Circe in this benchmark, contrary to the implementation with Bastion using automatic derivation.   