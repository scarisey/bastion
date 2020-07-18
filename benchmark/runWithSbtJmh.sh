#!/bin/sh

OUTPUT_DIR=$(pwd)/profiling
FLAMEGRAPH_DIR=$HOME/git/external/FlameGraph/
JFR_FLAMEGRAPH_DIR=$HOME/git/external/jfr-flame-graph/

mkdir -p $OUTPUT_DIR

#sbt "clean;compile;project benchmark;jmh:compile;assembly"
sbt "project benchmark;jmh:run -prof jmh.extras.JFR:dir=$OUTPUT_DIR,flameGraphDir=$FLAMEGRAPH_DIR,jfrFlameGraphDir=$JFR_FLAMEGRAPH_DIR .*Manual.*"