#!/bin/sh
java -agentpath:/home/s.carisey@eu.lectra.com/jprofiler11/bin/linux-x64/libjprofilerti.so=port=8849 -jar target/scala-2.12/benchmarks.jar org.openjdk.jmh.Main .*Manual.* 

