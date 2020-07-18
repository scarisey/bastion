#!/bin/sh
FILENAME=$(pwd)/myrecording.jfr
FLAMEGRAPH_DIR=$HOME/git/external/FlameGraph
JFR_FLAMEGRAPH_DIR=$HOME/git/external/flame-graph-jfr

java -XX:+FlightRecorder -XX:StartFlightRecording=filename=myrecording.jfr -jar target/scala-2.12/benchmarks.jar org.openjdk.jmh.Main .*Manual.*

java -jar $JFR_FLAMEGRAPH_DIR/target/flame-graph-jfr-1.0-SNAPSHOT-shaded.jar -i $FILENAME -o output.fld

$JFR_FLAMEGRAPH_DIR/flamegraph.pl output.fld > output.svg