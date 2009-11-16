package com.osinka.mongodb.benchmark

import scala.testing.Benchmark
import org.specs.util.SimpleTimer

object overhead {
    val defaultArgs = List(10000, 1)
    val benchmarkSuites = SerializationOverhead :: ConstraintOverheadNoIndex :: ConstraintOverheadWithIndex :: Nil

    def main(args: Array[String]) {
        val List(collectionSize, repeat) = args.toList.map{_.toInt} ::: defaultArgs.drop(args.size).take(defaultArgs.size-args.size)

        for (val suite <- benchmarkSuites)
            suite.runOn(collectionSize) {benchmark => report(benchmark.prefix, benchmark.runBenchmark(repeat)) }
    }

    def report(name: String, latencies: => List[Long]) {
        implicit def longToTimer(l: Long): SimpleTimer = { val t = new SimpleTimer; t.elapsed = l; t }

        // The first run is taken out, it's a warm up
        val sorted = latencies.tail.sort( (a,b) => a < b )

        val (total, count, min, max) = (
            (0L /: sorted)((x, y) => x + y),
            sorted.size,
            sorted.head,
            sorted.last
        )
        val avg = total / count

        System.err.println(name + ", " + count + " iterations:"
                           + " total=[" + total.time + "]"
                           + ", min=["  + min.time   + "]"
                           + ", avg=["  + avg.time   + "]"
                           + ", max=["  + max.time   + "]")
    }
}