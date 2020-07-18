#!/bin/sh
FILENAME=$(pwd)/myrecording.jfr
FLAME_GRAPH_DIR=$HOME/git/external/FlameGraph
JFRFLAMEGRAPH=$HOME/git/external/jfr-flame-graph

export ASYNC_PROFILER_DIR=$HOME/git/external/async-profiler


sbt "compile;project benchmark;jmh:run -prof jmh.extras.Async:dir=$FILENAME .*Manual.*"
#sbt "compile;project benchmark;jmh:run -prof jmh.extras.JFR:dir=$FILENAME,flameGraphDir=$FLAMEGRAPH,jfrFlameGraphDir=$JFRFLAMEGRAPH .*Manual.*"

