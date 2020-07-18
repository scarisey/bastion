#!/bin/sh
java -agentpath:$HOME/YourKit-JavaProfiler-2019.8/bin/linux-x86-64/libyjpagent.so=exceptions=disable,delay=10000 \
-jar target/scala-2.12/benchmarks.jar org.openjdk.jmh.Main .*Manual.*

