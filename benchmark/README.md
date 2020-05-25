# Benchmark results

## Naive benchmarks

These results help me to investigate where are the bottlenecks in Bastion implementation, not to incriminate other libraries.

```text
Benchmark                                                     Mode  Cnt    Score   Error   Units
CirceVsBastion.decodeJsonUsingBastionAndUJson                thrpt   10    0,969 ± 0,077  ops/us
CirceVsBastion.decodeJsonUsingCirceGenerics                  thrpt   10    0,914 ± 0,233  ops/us
CirceVsBastion.decodeJsonUsingCirceMagnolia                  thrpt   10    0,641 ± 0,016  ops/us
CirceVsBastion.decodeJsonUsingCirceOptics                    thrpt   10    0,872 ± 0,031  ops/us
EncodeDynamicRepr.encodeToDynamicReprWithAutoDerivedEncoder  thrpt   10    7,872 ± 0,287  ops/us
EncodeDynamicRepr.specificManualEncodeToDynamicRepr          thrpt   10  273,011 ± 5,745  ops/us
ManualVsBastion.convertManually                              thrpt   10    1,797 ± 0,080  ops/us
ManualVsBastion.convertUsingDerivedEncodersAndDecoders       thrpt   10    0,928 ± 0,079  ops/us
ManualVsBastion.decodeDynamicReprUsingDerivedDecoders        thrpt   10    1,315 ± 0,051  ops/us
```

Compared to a hand written converter, bastion seems to be 2x times slower.
Decoding directly from a precomputed DynamicRepr results in ~ 73% of the handwritten solution's performances.

Decoding json using Circe and the specific handwritten converter is about the same performance as bastion + uJson,
but I guess we need more benchmarks on this point.