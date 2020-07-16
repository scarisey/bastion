#!/bin/sh
java -XX:+FlightRecorder -XX:StartFlightRecording=filename=myrecording.jfr -jar target/scala-2.12/benchmarks.jar org.openjdk.jmh.Main .*Manual.*

