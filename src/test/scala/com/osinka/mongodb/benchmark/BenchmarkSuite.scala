package com.osinka.mongodb.benchmark

import scala.testing.Benchmark

abstract class BenchmarkSuite(val name: String) {
    def benchmarks: List[Benchmark]
    
    def setUp(collSize: Int): Unit
    def tearDown: Unit

    def runOn[R](collSize: Int)(f: (Benchmark => R)): List[R] =
        try {
            System.err.println(name)
            setUp(collSize)
            benchmarks map f
        } finally {
            tearDown
        }
}
